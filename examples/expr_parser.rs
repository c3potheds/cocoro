//! Parser combinator library and expression language parser.
//!
//! ## Language
//!
//! ```text
//! expr     = let_expr | add_expr
//! let_expr = "let" ident "=" expr "in" expr
//! add_expr = mul_expr (("+" | "-") mul_expr)*
//! mul_expr = unary   (("*" | "/") unary  )*
//! unary    = "-" unary | atom
//! atom     = number | ident | "(" expr ")"
//! number   = digit+
//! ident    = alpha (alphanum | "_")*
//! ```
//!
//! ## Architecture
//!
//! The combinator library has two layers:
//!
//! ```text
//! Cocoro level — primitive parsers: single dispatch on one suspension event
//!     Just(ch)           accepts one exact character
//!     Keyword(kw)        accepts a keyword with word-boundary check
//!     number_p()         folds digits into i64 via take_while_fold
//!     ident_p()          folds alphanumeric chars into String via take_while_fold
//!
//! PResult level — composition: thread the leftover stream through sequences
//!     map_output         transform the parsed value (built on Cocoro::map)
//!     or                 backtracking alternation (clones the stream)
//! ```
//!
//! Recursive grammar rules (`expr`, `let_expr`, `add_expr`, `mul_expr`,
//! `unary`, `atom`) are plain functions taking `Input<S>` and returning
//! `PResult<Expr, S>`. Returning `impl Parser` would require an infinite type
//! for mutually recursive rules; free functions sidestep this cleanly.
//! Parser-combinator values are invoked with `.parse(input)`; grammar
//! functions call each other directly.

use std::fmt;

use cocoro::Cocoro;
use cocoro::Coro;
use cocoro::MapCocoro;
use cocoro::Suspend;
use cocoro::Suspend::Return;
use cocoro::Suspended;
use cocoro::Yield;

// ============================================================================
// Types
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedEof,
    UnexpectedChar(char),
    ExpectedDigit,
    ExpectedIdent,
    UnexpectedKeyword,
    Expected(&'static str),
    NumberOutOfRange,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedEof => write!(f, "unexpected end of input"),
            Self::UnexpectedChar(c) => write!(f, "unexpected character: {c:?}"),
            Self::ExpectedDigit => write!(f, "expected a digit"),
            Self::ExpectedIdent => write!(f, "expected an identifier"),
            Self::UnexpectedKeyword => write!(f, "unexpected keyword"),
            Self::Expected(s) => write!(f, "expected {s:?}"),
            Self::NumberOutOfRange => write!(f, "number literal out of range"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Num(i64),
    Var(String),
    Neg(Box<Expr>),
    BinOp(Op, Box<Expr>, Box<Expr>),
    Let {
        name: String,
        value: Box<Expr>,
        body: Box<Expr>,
    },
}

impl Expr {
    fn binop(op: Op, lhs: Expr, rhs: Expr) -> Self {
        Self::BinOp(op, Box::new(lhs), Box::new(rhs))
    }

    fn let_expr(name: String, value: Expr, body: Expr) -> Self {
        Self::Let {
            name,
            value: Box::new(value),
            body: Box::new(body),
        }
    }
}

/// Character stream: yields one `char` at a time, returns `()` at EOF.
#[derive(Clone)]
struct Chars<'a>(std::str::Chars<'a>);

impl Coro<(), char, ()> for Chars<'_> {
    type Next = Self;
    type Suspend = Suspend<char, (), Self>;
    fn resume(mut self, _: ()) -> Self::Suspend {
        match self.0.next() {
            Some(ch) => Yield(ch, self),
            None => Return(()),
        }
    }
}

/// A cursor into a character stream: either the next `char` is ready, or EOF.
type Input<S> = Suspend<char, (), S>;

/// A parse result: the parsed value and remaining stream cursor, or an error.
type PResult<T, S> = Result<(T, Input<S>), ParseError>;

fn advance<S: Coro<(), char, (), Next = S>>(stream: S) -> Input<S> {
    stream.resume(()).into_enum()
}

fn skip_ws<S: Coro<(), char, (), Next = S>>(mut input: Input<S>) -> Input<S> {
    loop {
        match input {
            Yield(ch, next) if ch.is_ascii_whitespace() => {
                input = advance(next)
            }
            other => break other,
        }
    }
}

// ============================================================================
// Parser trait
// ============================================================================
//
// `Parser<S>` extends `Cocoro<(), char, (), S>`: a parser IS a visitor for a
// single step of a character stream. When visited against an `Input<S>`, it
// decides in `on_yield` what to do with the next character, or handles EOF
// in `on_return`.
//
// The stream type `S` appears on the trait because `Cocoro<…, S>` must know
// the type of the coroutine continuation passed to `on_yield`. Methods that
// need backtracking (`or`, `repeated`) require `S: Clone`.
//
// `map_output` is the only method built on a general `Cocoro` combinator
// (`Cocoro::map`). All other methods compose at the `PResult` level — they
// thread the leftover stream through sequential parsers, or clone it for
// backtracking. There is no way to express sequencing as a general `Cocoro`
// combinator because `Cocoro` only sees a single suspension event; the
// leftover stream inside `Out = PResult<…>` is opaque to it.

trait Parser<S>:
    Cocoro<(), char, (), S, Out = PResult<Self::Output, S>> + Sized
where
    S: Coro<(), char, (), Next = S>,
{
    type Output;

    /// Run this parser against `input` by visiting the suspension.
    fn parse(self, input: Input<S>) -> PResult<Self::Output, S> {
        input.visit(self)
    }

    /// Transform the parsed output with `f`.
    ///
    /// This is the only `Parser` method built on a general [`Cocoro`]
    /// combinator. [`Cocoro::map`] transforms `Out` — here
    /// `PResult<A, S>` — to any `Out2`. By lifting `f: A -> B` to
    /// `PResult<A> -> PResult<B>` (applying it inside `Ok`, leaving the
    /// leftover stream untouched), we get a `Parser<S, Output = B>` with no
    /// bespoke struct needed. The [`MapCocoro`] wrapper produced by
    /// [`Cocoro::map`] becomes a `Parser<S>` automatically via the blanket
    /// impl below.
    fn map_output<B, F>(self, f: F) -> impl Parser<S, Output = B> + Clone
    where
        F: Fn(Self::Output) -> B + Clone,
        Self: Clone,
    {
        Cocoro::map(self, move |r: PResult<Self::Output, S>| {
            r.map(|(a, rest)| (f(a), rest))
        })
    }

    /// Try `self`; if it fails, restore the stream and try `other`.
    ///
    /// Backtracking is implemented by cloning `Input<S>` before trying the
    /// first branch, which is cheap when `S` is a `Chars` (it holds a
    /// `std::str::Chars` iterator, itself a slice view into the string).
    fn or<Q>(self, other: Q) -> impl Parser<S, Output = Self::Output> + Clone
    where
        Q: Parser<S, Output = Self::Output> + Clone,
        S: Clone,
        Self: Clone,
    {
        Or(self, other)
    }
}

/// [`MapCocoro`] is automatically a `Parser` when its function maps
/// `PResult<P::Output>` to `PResult<B>`. This makes `map_output` compose
/// with `or` and other combinators without a bespoke struct.
impl<S, P, F, B> Parser<S> for MapCocoro<P, F>
where
    S: Coro<(), char, (), Next = S>,
    P: Parser<S>,
    F: FnOnce(PResult<P::Output, S>) -> PResult<B, S>,
{
    type Output = B;
}

// ============================================================================
// PResult-level combinators
// ============================================================================

#[derive(Clone)]
struct Or<P, Q>(P, Q);

impl<S, P, Q> Cocoro<(), char, (), S> for Or<P, Q>
where
    S: Coro<(), char, (), Next = S> + Clone,
    P: Parser<S>,
    Q: Parser<S, Output = P::Output>,
{
    type Out = PResult<P::Output, S>;

    fn on_yield(self, ch: char, next: S) -> Self::Out {
        let Self(p, q) = self;
        let saved = Yield(ch, next.clone());
        match p.on_yield(ch, next) {
            Ok(result) => Ok(result),
            Err(_) => q.parse(saved),
        }
    }

    fn on_return(self, _: ()) -> Self::Out {
        let Self(p, q) = self;
        match p.on_return(()) {
            Ok(result) => Ok(result),
            Err(_) => q.on_return(()),
        }
    }
}

impl<S, P, Q> Parser<S> for Or<P, Q>
where
    S: Coro<(), char, (), Next = S> + Clone,
    P: Parser<S>,
    Q: Parser<S, Output = P::Output>,
{
    type Output = P::Output;
}

// ============================================================================
// take_while_fold
// ============================================================================
//
// A `Cocoro` that calls `resume()` on the `next` it receives in `on_yield`
// acts as a *driver*: it pulls the stream forward from inside the visitor
// role. `TakeWhileFold` is one such driver. It folds all characters matching
// `pred` into an accumulator using `fold`, then returns the accumulated
// value paired with the leftover `Input` (the first non-matching character
// or EOF). This is used by `number_p` and `ident_p` to build their results
// directly without an intermediate `Vec<char>`.

struct TakeWhileFold<Acc, FP, FA> {
    acc: Acc,
    pred: FP,
    fold: FA,
}

impl<S, Acc, FP, FA> Cocoro<(), char, (), S> for TakeWhileFold<Acc, FP, FA>
where
    S: Coro<(), char, (), Next = S> + Clone,
    Acc: Clone,
    FP: Fn(&char) -> bool + Clone,
    FA: Fn(Acc, char) -> Acc + Clone,
{
    type Out = (Acc, Input<S>);

    fn on_yield(self, ch: char, next: S) -> (Acc, Input<S>) {
        let Self { acc, pred, fold } = self;
        if pred(&ch) {
            let acc = fold(acc, ch);
            next.resume(())
                .into_enum()
                .visit(TakeWhileFold { acc, pred, fold })
        } else {
            (acc, Yield(ch, next))
        }
    }

    fn on_return(self, _: ()) -> (Acc, Input<S>) {
        (self.acc, Return(()))
    }
}

fn take_while_fold<Acc, FP, FA>(
    init: Acc,
    pred: FP,
    fold: FA,
) -> TakeWhileFold<Acc, FP, FA>
where
    Acc: Clone,
    FP: Fn(&char) -> bool + Clone,
    FA: Fn(Acc, char) -> Acc + Clone,
{
    TakeWhileFold {
        acc: init,
        pred,
        fold,
    }
}

// ============================================================================
// Primitive parsers
// ============================================================================

/// Accepts one exact character.
#[derive(Clone, Copy)]
struct Just(char);

impl<S: Coro<(), char, (), Next = S>> Cocoro<(), char, (), S> for Just {
    type Out = PResult<char, S>;

    fn on_yield(self, ch: char, next: S) -> PResult<char, S> {
        let Self(expected) = self;
        if ch == expected {
            Ok((ch, advance(next)))
        } else {
            Err(ParseError::UnexpectedChar(ch))
        }
    }

    fn on_return(self, _: ()) -> PResult<char, S> {
        Err(ParseError::UnexpectedEof)
    }
}

impl<S: Coro<(), char, (), Next = S> + Clone> Parser<S> for Just {
    type Output = char;
}

/// Accepts a keyword, requiring that it is not immediately followed by an
/// alphanumeric character or underscore.
///
/// This boundary check prevents "letx" from being parsed as the keyword
/// "let" followed by some other token. Without it, `Keyword("let")` would
/// match the first three characters of "letx", and the parser would try
/// to interpret "x" as the start of the binding name — failing only later,
/// which wastes work and can produce confusing errors.
#[derive(Clone, Copy)]
struct Keyword(&'static str);

impl<S: Coro<(), char, (), Next = S>> Cocoro<(), char, (), S> for Keyword {
    type Out = PResult<(), S>;

    fn on_yield(self, ch: char, next: S) -> PResult<(), S> {
        let Self(kw) = self;
        let mut chars = kw.chars();
        let first = chars.next().expect("keyword must not be empty");
        if ch != first {
            return Err(ParseError::Expected(kw));
        }
        let mut input = advance(next);
        for expected in chars {
            match input {
                Yield(c, next) if c == expected => input = advance(next),
                Yield(_, _) => return Err(ParseError::Expected(kw)),
                Return(()) => return Err(ParseError::UnexpectedEof),
            }
        }
        // Keyword must not be followed by an alphanumeric char or underscore.
        match &input {
            Yield(c, _) if c.is_ascii_alphanumeric() || *c == '_' => {
                Err(ParseError::Expected(kw))
            }
            _ => Ok(((), input)),
        }
    }

    fn on_return(self, _: ()) -> PResult<(), S> {
        Err(ParseError::UnexpectedEof)
    }
}

impl<S: Coro<(), char, (), Next = S> + Clone> Parser<S> for Keyword {
    type Output = ();
}

// ============================================================================
// Grammar combinators (non-recursive, return impl Parser)
// ============================================================================

/// Parse one or more ASCII digits into an `i64`.
///
/// Uses [`take_while_fold`] to accumulate digits directly into a number,
/// avoiding an intermediate `Vec<char>`. The first digit is handled in
/// `on_yield`; subsequent digits are consumed by the fold.
fn number_p<S>() -> impl Parser<S, Output = i64> + Clone
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    #[derive(Clone)]
    struct NumberParser;

    impl<S> Cocoro<(), char, (), S> for NumberParser
    where
        S: Coro<(), char, (), Next = S> + Clone,
    {
        type Out = PResult<i64, S>;

        fn on_yield(self, ch: char, next: S) -> PResult<i64, S> {
            if !ch.is_ascii_digit() {
                return Err(ParseError::ExpectedDigit);
            }
            let init: Result<i64, ParseError> = Ok(ch as i64 - '0' as i64);
            let (result, rest) = next.resume(()).into_enum().visit(
                take_while_fold(init, char::is_ascii_digit, |acc, c| {
                    acc.and_then(|n| {
                        n.checked_mul(10)
                            .and_then(|n| n.checked_add(c as i64 - '0' as i64))
                            .ok_or(ParseError::NumberOutOfRange)
                    })
                }),
            );
            Ok((result?, rest))
        }

        fn on_return(self, _: ()) -> PResult<i64, S> {
            Err(ParseError::UnexpectedEof)
        }
    }

    impl<S> Parser<S> for NumberParser
    where
        S: Coro<(), char, (), Next = S> + Clone,
    {
        type Output = i64;
    }

    NumberParser
}

/// Parse an identifier: a letter followed by zero or more alphanumeric
/// characters or underscores.
///
/// Returns an error if the parsed name is a reserved keyword (`let`, `in`).
/// Uses [`take_while_fold`] to accumulate characters directly into a `String`.
fn ident_p<S>() -> impl Parser<S, Output = String> + Clone
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    #[derive(Clone)]
    struct IdentParser;

    impl<S> Cocoro<(), char, (), S> for IdentParser
    where
        S: Coro<(), char, (), Next = S> + Clone,
    {
        type Out = PResult<String, S>;

        fn on_yield(self, ch: char, next: S) -> PResult<String, S> {
            if !ch.is_ascii_alphabetic() {
                return Err(ParseError::ExpectedIdent);
            }
            let init = String::from(ch);
            let (s, rest) = next.resume(()).into_enum().visit(take_while_fold(
                init,
                |c: &char| c.is_ascii_alphanumeric() || *c == '_',
                |mut acc, c| {
                    acc.push(c);
                    acc
                },
            ));
            if matches!(s.as_str(), "let" | "in") {
                return Err(ParseError::UnexpectedKeyword);
            }
            Ok((s, rest))
        }

        fn on_return(self, _: ()) -> PResult<String, S> {
            Err(ParseError::UnexpectedEof)
        }
    }

    impl<S> Parser<S> for IdentParser
    where
        S: Coro<(), char, (), Next = S> + Clone,
    {
        type Output = String;
    }

    IdentParser
}

// ============================================================================
// Grammar rules (recursive, free functions)
// ============================================================================
//
// Rules that are mutually recursive with `expr` cannot return `impl Parser`
// because the type would be infinite. Free functions avoid this: they take
// `Input<S>` directly and return `PResult<Expr, S>`, calling each other and
// parser combinators without type-level ceremony.
//
// Operator loops (`add_expr`, `mul_expr`) use imperative `loop` + `match`
// rather than `Repeated` because the right-hand side of each operator is
// another grammar rule, not a combinator value. The pattern matches on
// `Input<S>` directly — the same lookahead technique used throughout.

fn expr<S>(input: Input<S>) -> PResult<Expr, S>
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    let input = skip_ws(input);
    let saved = input.clone();
    match let_expr(input) {
        Ok(result) => Ok(result),
        Err(_) => add_expr(saved),
    }
}

/// Parse `let <ident> = <expr> in <expr>`.
fn let_expr<S>(input: Input<S>) -> PResult<Expr, S>
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    let (_, rest) = Keyword("let").parse(input)?;
    let (name, rest) = ident_p().parse(skip_ws(rest))?;
    let (_, rest) = Just('=').parse(skip_ws(rest))?;
    let (value, rest) = expr(skip_ws(rest))?;
    let (_, rest) = Keyword("in").parse(skip_ws(rest))?;
    let (body, rest) = expr(skip_ws(rest))?;
    Ok((Expr::let_expr(name, value, body), rest))
}

/// Parse additive expressions (left-associative `+` and `-`).
fn add_expr<S>(input: Input<S>) -> PResult<Expr, S>
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    let (mut lhs, mut rest) = mul_expr(input)?;
    loop {
        rest = skip_ws(rest);
        match rest {
            Yield('+', next) => {
                let (rhs, r) = mul_expr(skip_ws(advance(next)))?;
                lhs = Expr::binop(Op::Add, lhs, rhs);
                rest = r;
            }
            Yield('-', next) => {
                let (rhs, r) = mul_expr(skip_ws(advance(next)))?;
                lhs = Expr::binop(Op::Sub, lhs, rhs);
                rest = r;
            }
            other => break Ok((lhs, other)),
        }
    }
}

/// Parse multiplicative expressions (left-associative `*` and `/`).
fn mul_expr<S>(input: Input<S>) -> PResult<Expr, S>
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    let (mut lhs, mut rest) = unary(input)?;
    loop {
        rest = skip_ws(rest);
        match rest {
            Yield('*', next) => {
                let (rhs, r) = unary(skip_ws(advance(next)))?;
                lhs = Expr::binop(Op::Mul, lhs, rhs);
                rest = r;
            }
            Yield('/', next) => {
                let (rhs, r) = unary(skip_ws(advance(next)))?;
                lhs = Expr::binop(Op::Div, lhs, rhs);
                rest = r;
            }
            other => break Ok((lhs, other)),
        }
    }
}

/// Parse unary negation (right-associative `- expr`) or an atom.
fn unary<S>(input: Input<S>) -> PResult<Expr, S>
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    match input {
        Yield('-', next) => {
            let (inner, rest) = unary(skip_ws(advance(next)))?;
            Ok((Expr::Neg(Box::new(inner)), rest))
        }
        other => atom(other),
    }
}

/// Parse an atomic expression: number, variable, or parenthesised expression.
///
/// Uses `number_p().map_output(Expr::Num).or(ident_p().map_output(Expr::Var))`
/// to try number first, backtracking to identifier if it fails. This is the
/// `or` combinator in its most natural role: both branches start with
/// overlapping character sets (digits for numbers, letters for identifiers),
/// so a lookahead-free approach requires trying one and restoring on failure.
fn atom<S>(input: Input<S>) -> PResult<Expr, S>
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    match skip_ws(input) {
        Yield('(', next) => {
            let (inner, rest) = expr(skip_ws(advance(next)))?;
            let (_, rest) = Just(')').parse(skip_ws(rest))?;
            Ok((inner, rest))
        }
        other => number_p()
            .map_output(Expr::Num)
            .or(ident_p().map_output(Expr::Var))
            .parse(other),
    }
}

// ============================================================================
// Entry point
// ============================================================================

pub fn parse(input: &str) -> Result<Expr, ParseError> {
    let stream = Chars(input.chars());
    let input = stream.resume(()).into_enum();
    let (result, rest) = expr(input)?;
    match skip_ws(rest) {
        Return(()) => Ok(result),
        Yield(ch, _) => Err(ParseError::UnexpectedChar(ch)),
    }
}

fn main() {
    let cases = [
        "42",
        "x",
        "1 + 2",
        "3 * 4 + 5",
        "1 + 2 * 3",
        "(1 + 2) * 3",
        "-42",
        "--1",
        "let x = 1 in x + 1",
        "let x = 1 in let y = 2 in x + y",
        "let x = let y = 1 in y in x + 1",
        "let f = 10 in f * f - 1",
        // errors
        "",
        "1 +",
        "@",
        "let x = 1 in",
        "let = 1 in 2",
        "letx",
    ];

    for input in &cases {
        println!("{:40} => {:?}", format!("{input:?}"), parse(input));
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // -- Literals --

    #[test]
    fn number_literal() {
        assert_eq!(parse("42"), Ok(Expr::Num(42)));
        assert_eq!(parse("0"), Ok(Expr::Num(0)));
        assert_eq!(parse("123"), Ok(Expr::Num(123)));
        assert_eq!(parse("  42  "), Ok(Expr::Num(42)));
    }

    #[test]
    fn variable() {
        assert_eq!(parse("x"), Ok(Expr::Var("x".into())));
        assert_eq!(parse("foo"), Ok(Expr::Var("foo".into())));
        assert_eq!(parse("x1"), Ok(Expr::Var("x1".into())));
        assert_eq!(parse("my_var"), Ok(Expr::Var("my_var".into())));
    }

    // -- Arithmetic --

    #[test]
    fn addition() {
        assert_eq!(
            parse("1 + 2"),
            Ok(Expr::binop(Op::Add, Expr::Num(1), Expr::Num(2)))
        );
    }

    #[test]
    fn subtraction() {
        assert_eq!(
            parse("10 - 3"),
            Ok(Expr::binop(Op::Sub, Expr::Num(10), Expr::Num(3)))
        );
    }

    #[test]
    fn multiplication() {
        assert_eq!(
            parse("2 * 3"),
            Ok(Expr::binop(Op::Mul, Expr::Num(2), Expr::Num(3)))
        );
    }

    #[test]
    fn division() {
        assert_eq!(
            parse("10 / 2"),
            Ok(Expr::binop(Op::Div, Expr::Num(10), Expr::Num(2)))
        );
    }

    #[test]
    fn left_associativity() {
        // 1 - 2 - 3 = (1 - 2) - 3, not 1 - (2 - 3)
        assert_eq!(
            parse("1 - 2 - 3"),
            Ok(Expr::binop(
                Op::Sub,
                Expr::binop(Op::Sub, Expr::Num(1), Expr::Num(2)),
                Expr::Num(3)
            ))
        );
    }

    #[test]
    fn mul_binds_tighter_than_add() {
        // 1 + 2 * 3 = 1 + (2 * 3)
        assert_eq!(
            parse("1 + 2 * 3"),
            Ok(Expr::binop(
                Op::Add,
                Expr::Num(1),
                Expr::binop(Op::Mul, Expr::Num(2), Expr::Num(3))
            ))
        );
        // 2 * 3 + 1 = (2 * 3) + 1
        assert_eq!(
            parse("2 * 3 + 1"),
            Ok(Expr::binop(
                Op::Add,
                Expr::binop(Op::Mul, Expr::Num(2), Expr::Num(3)),
                Expr::Num(1)
            ))
        );
    }

    // -- Parentheses --

    #[test]
    fn parentheses_override_precedence() {
        assert_eq!(
            parse("(1 + 2) * 3"),
            Ok(Expr::binop(
                Op::Mul,
                Expr::binop(Op::Add, Expr::Num(1), Expr::Num(2)),
                Expr::Num(3)
            ))
        );
    }

    #[test]
    fn nested_parentheses() {
        assert_eq!(parse("((42))"), Ok(Expr::Num(42)));
        assert_eq!(
            parse("((1 + 2)) * ((3))"),
            Ok(Expr::binop(
                Op::Mul,
                Expr::binop(Op::Add, Expr::Num(1), Expr::Num(2)),
                Expr::Num(3)
            ))
        );
    }

    // -- Unary negation --

    #[test]
    fn unary_negation() {
        assert_eq!(parse("-42"), Ok(Expr::Neg(Box::new(Expr::Num(42)))));
        assert_eq!(parse("-x"), Ok(Expr::Neg(Box::new(Expr::Var("x".into())))));
    }

    #[test]
    fn double_negation() {
        assert_eq!(
            parse("--42"),
            Ok(Expr::Neg(Box::new(Expr::Neg(Box::new(Expr::Num(42))))))
        );
    }

    #[test]
    fn negation_in_expression() {
        // -1 + 2 = (-1) + 2 (unary binds tighter than binary)
        assert_eq!(
            parse("-1 + 2"),
            Ok(Expr::binop(
                Op::Add,
                Expr::Neg(Box::new(Expr::Num(1))),
                Expr::Num(2)
            ))
        );
    }

    // -- Let bindings --

    #[test]
    fn let_binding() {
        assert_eq!(
            parse("let x = 1 in x + 1"),
            Ok(Expr::let_expr(
                "x".into(),
                Expr::Num(1),
                Expr::binop(Op::Add, Expr::Var("x".into()), Expr::Num(1))
            ))
        );
    }

    #[test]
    fn let_with_expression_value() {
        assert_eq!(
            parse("let x = 2 * 3 in x + 1"),
            Ok(Expr::let_expr(
                "x".into(),
                Expr::binop(Op::Mul, Expr::Num(2), Expr::Num(3)),
                Expr::binop(Op::Add, Expr::Var("x".into()), Expr::Num(1))
            ))
        );
    }

    #[test]
    fn nested_let() {
        // let x = 1 in let y = 2 in x + y
        assert_eq!(
            parse("let x = 1 in let y = 2 in x + y"),
            Ok(Expr::let_expr(
                "x".into(),
                Expr::Num(1),
                Expr::let_expr(
                    "y".into(),
                    Expr::Num(2),
                    Expr::binop(
                        Op::Add,
                        Expr::Var("x".into()),
                        Expr::Var("y".into())
                    )
                )
            ))
        );
    }

    #[test]
    fn let_in_value_position() {
        // let x = (let y = 1 in y) in x + 1
        // The inner let is parsed greedily: "let y = 1 in y" is the value.
        assert_eq!(
            parse("let x = let y = 1 in y in x + 1"),
            Ok(Expr::let_expr(
                "x".into(),
                Expr::let_expr("y".into(), Expr::Num(1), Expr::Var("y".into())),
                Expr::binop(Op::Add, Expr::Var("x".into()), Expr::Num(1))
            ))
        );
    }

    // -- Keywords vs identifiers --

    #[test]
    fn keyword_prefix_is_valid_identifier() {
        // "letx" is not a keyword: the 'x' after "let" fails the boundary
        // check.
        assert_eq!(parse("letx"), Ok(Expr::Var("letx".into())));
        assert_eq!(parse("inlet"), Ok(Expr::Var("inlet".into())));
        assert_eq!(parse("letting"), Ok(Expr::Var("letting".into())));
    }

    #[test]
    fn bare_keyword_is_an_error() {
        // "let" alone, with no binding, is not a valid expression.
        assert!(parse("let").is_err());
        // "in" is not valid at the start of an expression.
        assert!(parse("in").is_err());
    }

    // -- Whitespace --

    #[test]
    fn whitespace_insensitive() {
        assert_eq!(parse("1+2"), parse("1 + 2"));
        assert_eq!(parse("1*2+3"), parse("1 * 2 + 3"));
        assert_eq!(parse("let  x  =  1  in  x"), parse("let x = 1 in x"));
    }

    // -- Errors --

    #[test]
    fn errors() {
        assert!(parse("").is_err());
        assert!(parse("1 +").is_err());
        assert!(parse("1 @").is_err());
        assert!(parse("1 2").is_err());
        assert!(parse("(1 + 2").is_err());
        assert!(parse(")").is_err());
        assert!(parse("let x = 1 in").is_err());
        assert!(parse("let = 1 in 2").is_err());
    }
}
