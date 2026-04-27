//! Cocoro combinators experiment: which parser transformations generalize?
//!
//! This builds on `parser_cocoro.rs` to ask: which of the parser combinator
//! operations from that example can be expressed as general `Cocoro`
//! combinators, and which are specific to the `PResult` result monad?
//!
//! ## Findings
//!
//! **Generalizes to `Cocoro::map`:** output transformation.
//!
//! The `Map<P, F>` struct from `parser_cocoro.rs` is subsumed by
//! [`MapCocoro`]. Lifting `f: A -> B` to `PResult<A> -> PResult<B>` and
//! passing it to `Cocoro::map` is all that's needed — the `Parser` trait
//! gains a `map_output` default method that does exactly this, with no
//! bespoke struct required.
//!
//! **Does not generalize:** sequential composition and backtracking.
//!
//! `Then`, `Skip`, and `Or` are inherently `PResult`-level. They require
//! access to the leftover stream *inside* `Out`, which is specific to
//! `PResult<T, Tok, S>`. No general `Cocoro` combinator can express "run me,
//! then run another parser on the remaining input" because `Cocoro` only
//! consumes a *single* suspension event — it does not drive the stream.
//!
//! **Free-function Cocoro constructors:** driving combinators.
//!
//! A `Cocoro` that calls `resume()` on the `next` it receives in `on_yield`
//! acts as a *driver*: it drives the stream forward from inside the visitor
//! role. `take_while_fold` is one such constructor. It is analogous to
//! `continue_while` on `Coro` but lives on the consumer side.
//!
//! ## Architecture summary
//!
//! ```text
//! Cocoro<I, Y, R, N>          ← interface: consume one suspension event
//!     map()                   ← the only general combinator (covariant in Out)
//!     take_while_fold()       ← driving constructor (free function)
//!
//! PResult monad               ← where parser composition lives
//!     then / skip / skip_left ← sequence: leftover stream feeds next parser
//!     or                      ← backtrack: clone stream, try alternative
//!     repeated / separated_by ← loop via PResult structure
//! ```
//!
//! ## Grammar
//!
//! Same as the previous parser examples:
//! ```text
//! patterns = pattern (',' pattern)*
//! pattern  = range | literal
//! range    = number ".." number
//! literal  = number | ident
//! number   = digit+
//! ident    = alpha+
//! ```
//!
//! Compare with:
//! - `parser_cocoro.rs` — same architecture, explicit `Map<P, F>` struct

#![allow(dead_code)]

use std::fmt;

use cocoro::Cocoro;
use cocoro::Coro;
use cocoro::MapCocoro;
use cocoro::Suspend;
use cocoro::Suspend::Return;
use cocoro::Suspended;
use cocoro::Yield;

// ---- Types ----

#[derive(Debug, Clone, PartialEq)]
enum ParseError {
    UnexpectedEof,
    UnexpectedChar(char),
    ExpectedDigit,
    ExpectedAlpha,
    ExpectedStr(&'static str),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedEof => write!(f, "unexpected end of input"),
            Self::UnexpectedChar(c) => write!(f, "unexpected character: '{c}'"),
            Self::ExpectedDigit => write!(f, "expected a digit"),
            Self::ExpectedAlpha => {
                write!(f, "expected an alphabetic character")
            }
            Self::ExpectedStr(s) => write!(f, "expected '{s}'"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Pattern {
    Number(u64),
    Ident(String),
    Range(u64, u64),
}

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

type Input<Tok, S> = Suspend<Tok, (), S>;
type PResult<T, Tok, S> = Result<(T, Input<Tok, S>), ParseError>;

fn advance<Tok, S: Coro<(), Tok, (), Next = S>>(stream: S) -> Input<Tok, S> {
    stream.resume(()).into_enum()
}

fn skip_ws<S: Coro<(), char, (), Next = S>>(
    mut input: Input<char, S>,
) -> Input<char, S> {
    loop {
        match input {
            Yield(ch, next) if ch.is_whitespace() => input = advance(next),
            other => break other,
        }
    }
}

// ============================================================================
// PART 1: The Parser trait and the Cocoro-level boundary
// ============================================================================
//
// The `Parser` trait extends `Cocoro`. Its only default method that delegates
// to a general `Cocoro` combinator is `map_output`, which uses `Cocoro::map`.
//
// All other combinators (`then`, `skip`, `or`, `repeated`, …) are defined
// as default methods on the `Parser` trait itself and operate at the
// `PResult` level — they thread the leftover stream through sequences of
// parsers, or clone it for backtracking. There is no way to express these
// as general `Cocoro` combinators because `Cocoro` only sees a single
// suspension event; the stream structure inside `Out` is opaque to it.

trait Parser<Tok, S>:
    Cocoro<(), Tok, (), S, Out = PResult<Self::Output, Tok, S>> + Sized
where
    S: Coro<(), Tok, ()>,
{
    type Output;

    fn parse(self, input: Input<Tok, S>) -> PResult<Self::Output, Tok, S> {
        input.visit(self)
    }

    /// The only `Parser` method built on a general `Cocoro` combinator.
    ///
    /// `Cocoro::map` transforms `Out` — here `PResult<A, Tok, S>` — to any
    /// `Out2`. By lifting `f: A -> B` to `PResult<A> -> PResult<B>` (applying
    /// it inside the `Ok` branch, leaving the leftover stream untouched), we
    /// get a `Parser<Tok, S, Output = B>` with no bespoke struct needed.
    ///
    /// In `parser_cocoro.rs` this required a `Map<P, F>` struct with a
    /// full `Cocoro` impl. Here, `MapCocoro<P, ClosureType>` fills that role
    /// and automatically becomes a `Parser` via the blanket impl below.
    fn map_output<B, F>(self, f: F) -> impl Parser<Tok, S, Output = B> + Clone
    where
        F: Fn(Self::Output) -> B + Clone,
        S: Coro<(), Tok, (), Next = S> + Clone,
        Self: Clone,
    {
        Cocoro::map(self, move |r: PResult<Self::Output, Tok, S>| {
            r.map(|(a, rest)| (f(a), rest))
        })
    }

    // --- PResult-level combinators ---
    //
    // The following methods cannot be expressed as general `Cocoro`
    // combinators. They thread the leftover `Input<Tok, S>` that lives inside
    // `Out = PResult<..., S>`, which is opaque to `Cocoro`.

    fn then<Q>(
        self,
        other: Q,
    ) -> impl Parser<Tok, S, Output = (Self::Output, Q::Output)> + Clone
    where
        Q: Parser<Tok, S> + Clone,
        S: Coro<(), Tok, (), Next = S>,
        Self: Clone,
    {
        Then(self, other)
    }

    fn skip<Q>(
        self,
        other: Q,
    ) -> impl Parser<Tok, S, Output = Self::Output> + Clone
    where
        Q: Parser<Tok, S> + Clone,
        S: Coro<(), Tok, (), Next = S>,
        Self: Clone,
    {
        Skip(self, other)
    }

    fn skip_left<Q>(
        self,
        other: Q,
    ) -> impl Parser<Tok, S, Output = Q::Output> + Clone
    where
        Q: Parser<Tok, S> + Clone,
        S: Coro<(), Tok, (), Next = S>,
        Self: Clone,
    {
        SkipLeft(self, other)
    }

    fn or<Q>(
        self,
        other: Q,
    ) -> impl Parser<Tok, S, Output = Self::Output> + Clone
    where
        Q: Parser<Tok, S, Output = Self::Output> + Clone,
        Tok: Clone,
        S: Coro<(), Tok, (), Next = S> + Clone,
        Self: Clone,
    {
        Or(self, other)
    }

    fn repeated(self) -> impl Parser<Tok, S, Output = Vec<Self::Output>> + Clone
    where
        Tok: Clone,
        S: Coro<(), Tok, (), Next = S> + Clone,
        Self: Clone,
    {
        Repeated(self)
    }

    fn separated_by<Sep: Parser<Tok, S> + Clone>(
        self,
        sep: Sep,
    ) -> impl Parser<Tok, S, Output = Vec<Self::Output>> + Clone
    where
        Tok: Clone,
        S: Coro<(), Tok, (), Next = S> + Clone,
        Self: Clone,
    {
        SeparatedBy(self, sep)
    }
}

/// Blanket `Parser` impl for `Parser<char, S>` to add `padded`.
/// We use a separate extension trait so `padded` only appears on
/// `Parser<char, S>` without polluting the generic `Parser<Tok, S>`.
trait CharParser<S>: Parser<char, S>
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    fn padded(self) -> impl Parser<char, S, Output = Self::Output> + Clone
    where
        Self: Clone,
    {
        Padded(self)
    }
}

impl<S, P> CharParser<S> for P
where
    S: Coro<(), char, (), Next = S> + Clone,
    P: Parser<char, S>,
{
}

/// `MapCocoro<P, F>` is automatically a `Parser` when `F` maps
/// `PResult<P::Output>` to `PResult<B>`. This makes `map_output` above
/// produce a value that can be passed to any combinator expecting a `Parser`.
impl<Tok, S, P, F, B> Parser<Tok, S> for MapCocoro<P, F>
where
    S: Coro<(), Tok, (), Next = S>,
    P: Parser<Tok, S>,
    F: FnOnce(PResult<P::Output, Tok, S>) -> PResult<B, Tok, S>,
{
    type Output = B;
}

// ============================================================================
// PART 2: PResult-level combinators (Then, Skip, SkipLeft, Or, Repeated, …)
// ============================================================================
//
// These are all `Cocoro` impls, but their logic threads the leftover stream
// out of `PResult` and into the next parser. The composition is monad-bind
// on `PResult`, not a general transformation of the `Out` type.

#[derive(Clone)]
struct Then<P, Q>(P, Q);

impl<Tok, S, P, Q> Cocoro<(), Tok, (), S> for Then<P, Q>
where
    S: Coro<(), Tok, (), Next = S>,
    P: Parser<Tok, S>,
    Q: Parser<Tok, S>,
{
    type Out = PResult<(P::Output, Q::Output), Tok, S>;
    fn on_yield(
        self,
        tok: Tok,
        next: S,
    ) -> PResult<(P::Output, Q::Output), Tok, S> {
        let (a, rest) = self.0.on_yield(tok, next)?;
        let (b, rest) = self.1.parse(rest)?;
        Ok(((a, b), rest))
    }
    fn on_return(self, r: ()) -> PResult<(P::Output, Q::Output), Tok, S> {
        let (a, rest) = self.0.on_return(r)?;
        let (b, rest) = self.1.parse(rest)?;
        Ok(((a, b), rest))
    }
}

impl<Tok, S, P, Q> Parser<Tok, S> for Then<P, Q>
where
    S: Coro<(), Tok, (), Next = S>,
    P: Parser<Tok, S>,
    Q: Parser<Tok, S>,
{
    type Output = (P::Output, Q::Output);
}

#[derive(Clone)]
struct Skip<P, Q>(P, Q);

impl<Tok, S, P, Q> Cocoro<(), Tok, (), S> for Skip<P, Q>
where
    S: Coro<(), Tok, (), Next = S>,
    P: Parser<Tok, S>,
    Q: Parser<Tok, S>,
{
    type Out = PResult<P::Output, Tok, S>;
    fn on_yield(self, tok: Tok, next: S) -> PResult<P::Output, Tok, S> {
        let (a, rest) = self.0.on_yield(tok, next)?;
        let (_, rest) = self.1.parse(rest)?;
        Ok((a, rest))
    }
    fn on_return(self, r: ()) -> PResult<P::Output, Tok, S> {
        let (a, rest) = self.0.on_return(r)?;
        let (_, rest) = self.1.parse(rest)?;
        Ok((a, rest))
    }
}

impl<Tok, S, P, Q> Parser<Tok, S> for Skip<P, Q>
where
    S: Coro<(), Tok, (), Next = S>,
    P: Parser<Tok, S>,
    Q: Parser<Tok, S>,
{
    type Output = P::Output;
}

#[derive(Clone)]
struct SkipLeft<P, Q>(P, Q);

impl<Tok, S, P, Q> Cocoro<(), Tok, (), S> for SkipLeft<P, Q>
where
    S: Coro<(), Tok, (), Next = S>,
    P: Parser<Tok, S>,
    Q: Parser<Tok, S>,
{
    type Out = PResult<Q::Output, Tok, S>;
    fn on_yield(self, tok: Tok, next: S) -> PResult<Q::Output, Tok, S> {
        let (_, rest) = self.0.on_yield(tok, next)?;
        let (b, rest) = self.1.parse(rest)?;
        Ok((b, rest))
    }
    fn on_return(self, r: ()) -> PResult<Q::Output, Tok, S> {
        let (_, rest) = self.0.on_return(r)?;
        let (b, rest) = self.1.parse(rest)?;
        Ok((b, rest))
    }
}

impl<Tok, S, P, Q> Parser<Tok, S> for SkipLeft<P, Q>
where
    S: Coro<(), Tok, (), Next = S>,
    P: Parser<Tok, S>,
    Q: Parser<Tok, S>,
{
    type Output = Q::Output;
}

#[derive(Clone)]
struct Padded<P>(P);

impl<S, P> Cocoro<(), char, (), S> for Padded<P>
where
    S: Coro<(), char, (), Next = S> + Clone,
    P: Parser<char, S>,
{
    type Out = PResult<P::Output, char, S>;
    fn on_yield(self, tok: char, next: S) -> PResult<P::Output, char, S> {
        self.0.parse(skip_ws(Yield(tok, next)))
    }
    fn on_return(self, _: ()) -> PResult<P::Output, char, S> {
        self.0.parse(Return(()))
    }
}

impl<S, P> Parser<char, S> for Padded<P>
where
    S: Coro<(), char, (), Next = S> + Clone,
    P: Parser<char, S>,
{
    type Output = P::Output;
}

#[derive(Clone)]
struct Or<P, Q>(P, Q);

impl<Tok, S, P, Q> Cocoro<(), Tok, (), S> for Or<P, Q>
where
    Tok: Clone,
    S: Coro<(), Tok, (), Next = S> + Clone,
    P: Parser<Tok, S>,
    Q: Parser<Tok, S, Output = P::Output>,
{
    type Out = PResult<P::Output, Tok, S>;
    fn on_yield(self, tok: Tok, next: S) -> PResult<P::Output, Tok, S> {
        let saved = Yield(tok.clone(), next.clone());
        match self.0.on_yield(tok, next) {
            Ok(result) => Ok(result),
            Err(_) => self.1.parse(saved),
        }
    }
    fn on_return(self, _: ()) -> PResult<P::Output, Tok, S> {
        match self.0.on_return(()) {
            Ok(result) => Ok(result),
            Err(_) => self.1.on_return(()),
        }
    }
}

impl<Tok, S, P, Q> Parser<Tok, S> for Or<P, Q>
where
    Tok: Clone,
    S: Coro<(), Tok, (), Next = S> + Clone,
    P: Parser<Tok, S>,
    Q: Parser<Tok, S, Output = P::Output>,
{
    type Output = P::Output;
}

#[derive(Clone)]
struct Repeated<P>(P);

impl<Tok, S, P> Cocoro<(), Tok, (), S> for Repeated<P>
where
    Tok: Clone,
    S: Coro<(), Tok, (), Next = S> + Clone,
    P: Parser<Tok, S> + Clone,
{
    type Out = PResult<Vec<P::Output>, Tok, S>;
    fn on_yield(self, tok: Tok, next: S) -> PResult<Vec<P::Output>, Tok, S> {
        let mut results = Vec::new();
        let mut current: Input<Tok, S> = Yield(tok, next);
        loop {
            let saved = current.clone();
            match self.0.clone().parse(current) {
                Ok((item, rest)) => {
                    results.push(item);
                    current = rest;
                }
                Err(_) => {
                    current = saved;
                    break;
                }
            }
        }
        Ok((results, current))
    }
    fn on_return(self, _: ()) -> PResult<Vec<P::Output>, Tok, S> {
        Ok((Vec::new(), Return(())))
    }
}

impl<Tok, S, P> Parser<Tok, S> for Repeated<P>
where
    Tok: Clone,
    S: Coro<(), Tok, (), Next = S> + Clone,
    P: Parser<Tok, S> + Clone,
{
    type Output = Vec<P::Output>;
}

#[derive(Clone)]
struct SeparatedBy<P, Sep>(P, Sep);

impl<Tok, S, P, Sep> Cocoro<(), Tok, (), S> for SeparatedBy<P, Sep>
where
    Tok: Clone,
    S: Coro<(), Tok, (), Next = S> + Clone,
    P: Parser<Tok, S> + Clone,
    Sep: Parser<Tok, S> + Clone,
{
    type Out = PResult<Vec<P::Output>, Tok, S>;
    fn on_yield(self, tok: Tok, next: S) -> PResult<Vec<P::Output>, Tok, S> {
        let (first, mut rest) = self.0.clone().on_yield(tok, next)?;
        let mut results = vec![first];
        loop {
            let saved = rest.clone();
            match self.1.clone().parse(rest) {
                Ok((_, after_sep)) => match self.0.clone().parse(after_sep) {
                    Ok((item, after_item)) => {
                        results.push(item);
                        rest = after_item;
                    }
                    Err(_) => {
                        rest = saved;
                        break;
                    }
                },
                Err(_) => {
                    rest = saved;
                    break;
                }
            }
        }
        Ok((results, rest))
    }
    fn on_return(self, r: ()) -> PResult<Vec<P::Output>, Tok, S> {
        self.0.on_return(r).map(|(first, rest)| (vec![first], rest))
    }
}

impl<Tok, S, P, Sep> Parser<Tok, S> for SeparatedBy<P, Sep>
where
    Tok: Clone,
    S: Coro<(), Tok, (), Next = S> + Clone,
    P: Parser<Tok, S> + Clone,
    Sep: Parser<Tok, S> + Clone,
{
    type Output = Vec<P::Output>;
}

// ============================================================================
// PART 3: A driving Cocoro constructor — `take_while_fold`
// ============================================================================
//
// A `Cocoro` whose `on_yield` calls `resume()` on the `next` it receives is
// a *driver*: it pulls the stream forward from inside the visitor role. This
// is analogous to `continue_while` on `Coro` but lives on the consumer side.
//
// `take_while_fold` constructs such a `Cocoro`. Given a predicate and an
// accumulator function, it folds matching tokens until the predicate fails or
// the stream ends, then returns the accumulator paired with the leftover
// `Input` (so the caller can continue parsing).
//
// The function is used to implement `number_p` and `ident_p` below, replacing
// `Repeated<Digit>` / `Repeated<Alpha>` with a single Cocoro that folds
// directly into the target type instead of building an intermediate `Vec`.

struct TakeWhileFold<Acc, FP, FA> {
    acc: Acc,
    pred: FP,
    fold: FA,
}

impl<Tok, S, Acc, FP, FA> Cocoro<(), Tok, (), S> for TakeWhileFold<Acc, FP, FA>
where
    Tok: Clone,
    S: Coro<(), Tok, (), Next = S> + Clone,
    Acc: Clone,
    FP: Fn(&Tok) -> bool + Clone,
    FA: Fn(Acc, Tok) -> Acc + Clone,
{
    // Returns the accumulated value and the leftover input.
    // The leftover is either `Yield(non_matching_tok, …)` or `Return(())`.
    type Out = (Acc, Input<Tok, S>);

    fn on_yield(self, tok: Tok, next: S) -> (Acc, Input<Tok, S>) {
        if (self.pred)(&tok) {
            let new_acc = (self.fold)(self.acc, tok);
            next.resume(()).into_enum().visit(TakeWhileFold {
                acc: new_acc,
                pred: self.pred,
                fold: self.fold,
            })
        } else {
            // Predicate failed: put the token back as the leftover.
            (self.acc, Yield(tok, next))
        }
    }

    fn on_return(self, _: ()) -> (Acc, Input<Tok, S>) {
        (self.acc, Return(()))
    }
}

/// Construct a `Cocoro` that folds matching tokens into an accumulator.
///
/// Tokens satisfying `pred` are consumed and folded into `init` with `f`.
/// The first non-matching token (or end-of-stream) is returned as the
/// leftover `Input`, allowing the caller to continue parsing.
fn take_while_fold<Tok, Acc, FP, FA>(
    init: Acc,
    pred: FP,
    f: FA,
) -> TakeWhileFold<Acc, FP, FA>
where
    Tok: Clone,
    Acc: Clone,
    FP: Fn(&Tok) -> bool + Clone,
    FA: Fn(Acc, Tok) -> Acc + Clone,
{
    TakeWhileFold {
        acc: init,
        pred,
        fold: f,
    }
}

// ============================================================================
// PART 4: Primitive parsers
// ============================================================================

#[derive(Clone, Copy)]
struct Digit;

impl<S: Coro<(), char, (), Next = S>> Cocoro<(), char, (), S> for Digit {
    type Out = PResult<char, char, S>;
    fn on_yield(self, ch: char, next: S) -> PResult<char, char, S> {
        if ch.is_ascii_digit() {
            Ok((ch, advance(next)))
        } else {
            Err(ParseError::ExpectedDigit)
        }
    }
    fn on_return(self, _: ()) -> PResult<char, char, S> {
        Err(ParseError::UnexpectedEof)
    }
}

impl<S: Coro<(), char, (), Next = S> + Clone> Parser<char, S> for Digit {
    type Output = char;
}

#[derive(Clone, Copy)]
struct Alpha;

impl<S: Coro<(), char, (), Next = S>> Cocoro<(), char, (), S> for Alpha {
    type Out = PResult<char, char, S>;
    fn on_yield(self, ch: char, next: S) -> PResult<char, char, S> {
        if ch.is_ascii_alphabetic() {
            Ok((ch, advance(next)))
        } else {
            Err(ParseError::ExpectedAlpha)
        }
    }
    fn on_return(self, _: ()) -> PResult<char, char, S> {
        Err(ParseError::UnexpectedEof)
    }
}

impl<S: Coro<(), char, (), Next = S> + Clone> Parser<char, S> for Alpha {
    type Output = char;
}

#[derive(Clone, Copy)]
struct Just<Tok>(Tok);

impl<Tok, S> Cocoro<(), Tok, (), S> for Just<Tok>
where
    Tok: PartialEq + Clone,
    S: Coro<(), Tok, (), Next = S>,
{
    type Out = PResult<Tok, Tok, S>;
    fn on_yield(self, tok: Tok, next: S) -> PResult<Tok, Tok, S> {
        if tok == self.0 {
            Ok((tok, advance(next)))
        } else {
            Err(ParseError::ExpectedStr("expected token"))
        }
    }
    fn on_return(self, _: ()) -> PResult<Tok, Tok, S> {
        Err(ParseError::UnexpectedEof)
    }
}

impl<Tok, S> Parser<Tok, S> for Just<Tok>
where
    Tok: PartialEq + Clone,
    S: Coro<(), Tok, (), Next = S> + Clone,
{
    type Output = Tok;
}

#[derive(Clone, Copy)]
struct Token(&'static str);

impl<S: Coro<(), char, (), Next = S>> Cocoro<(), char, (), S> for Token {
    type Out = PResult<(), char, S>;
    fn on_yield(self, ch: char, next: S) -> PResult<(), char, S> {
        let mut chars = self.0.chars();
        let expected = chars.next().expect("Token string must not be empty");
        if ch != expected {
            return Err(ParseError::ExpectedStr(self.0));
        }
        let mut input = advance(next);
        for expected in chars {
            match input {
                Yield(ch, next) if ch == expected => input = advance(next),
                Yield(_, _) => return Err(ParseError::ExpectedStr(self.0)),
                Return(()) => return Err(ParseError::UnexpectedEof),
            }
        }
        Ok(((), input))
    }
    fn on_return(self, _: ()) -> PResult<(), char, S> {
        Err(ParseError::UnexpectedEof)
    }
}

impl<S: Coro<(), char, (), Next = S> + Clone> Parser<char, S> for Token {
    type Output = ();
}

// ============================================================================
// PART 5: Grammar using combinators
// ============================================================================
//
// `number_p` and `ident_p` are rewritten using `take_while_fold` instead of
// `Repeated<Digit>` / `Repeated<Alpha>`. This avoids the intermediate `Vec`
// and shows `take_while_fold` in practical use.
//
// The rest of the grammar (`range_p`, `literal_p`, `pattern_p`, `patterns_p`)
// is identical in structure to `parser_cocoro.rs`. They use `Parser` trait
// default methods backed by `Cocoro::map` (`map_output`) for output transforms,
// and by the PResult-level combinators for sequencing and alternation.

// `TakeWhileFold<Acc, FP, FA>` doesn't implement `Parser` (it doesn't produce
// `PResult`) — it produces `(Acc, Input<Tok, S>)` directly. Grammar functions
// call it via `input.visit(...)` and then lift the result into `PResult`.

fn number_p<S>() -> impl Parser<char, S, Output = u64> + Clone
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    #[derive(Clone)]
    struct NumberCocoro;

    impl<S> Cocoro<(), char, (), S> for NumberCocoro
    where
        S: Coro<(), char, (), Next = S> + Clone,
    {
        type Out = PResult<u64, char, S>;

        fn on_yield(self, ch: char, next: S) -> PResult<u64, char, S> {
            if !ch.is_ascii_digit() {
                return Err(ParseError::ExpectedDigit);
            }
            // Fold the first digit plus all subsequent digits via
            // `take_while_fold`, then lift the result into `PResult`.
            let init = ch as u64 - '0' as u64;
            let (n, rest) = next.resume(()).into_enum().visit(take_while_fold(
                init,
                |c: &char| c.is_ascii_digit(),
                |acc, c| acc * 10 + (c as u64 - '0' as u64),
            ));
            Ok((n, rest))
        }

        fn on_return(self, _: ()) -> PResult<u64, char, S> {
            Err(ParseError::UnexpectedEof)
        }
    }

    impl<S> Parser<char, S> for NumberCocoro
    where
        S: Coro<(), char, (), Next = S> + Clone,
    {
        type Output = u64;
    }

    NumberCocoro
}

fn ident_p<S>() -> impl Parser<char, S, Output = String> + Clone
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    #[derive(Clone)]
    struct IdentCocoro;

    impl<S> Cocoro<(), char, (), S> for IdentCocoro
    where
        S: Coro<(), char, (), Next = S> + Clone,
    {
        type Out = PResult<String, char, S>;

        fn on_yield(self, ch: char, next: S) -> PResult<String, char, S> {
            if !ch.is_ascii_alphabetic() {
                return Err(ParseError::ExpectedAlpha);
            }
            let init = String::from(ch);
            let (s, rest) = next.resume(()).into_enum().visit(take_while_fold(
                init,
                |c: &char| c.is_ascii_alphabetic(),
                |mut acc, c| {
                    acc.push(c);
                    acc
                },
            ));
            Ok((s, rest))
        }

        fn on_return(self, _: ()) -> PResult<String, char, S> {
            Err(ParseError::UnexpectedEof)
        }
    }

    impl<S> Parser<char, S> for IdentCocoro
    where
        S: Coro<(), char, (), Next = S> + Clone,
    {
        type Output = String;
    }

    IdentCocoro
}

fn range_p<S>() -> impl Parser<char, S, Output = Pattern> + Clone
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    number_p()
        .skip(Token("..").padded())
        .then(number_p().padded())
        .map_output(|(lo, hi)| Pattern::Range(lo, hi))
}

fn literal_p<S>() -> impl Parser<char, S, Output = Pattern> + Clone
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    number_p()
        .map_output(Pattern::Number)
        .or(ident_p().map_output(Pattern::Ident))
}

fn pattern_p<S>() -> impl Parser<char, S, Output = Pattern> + Clone
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    range_p().or(literal_p())
}

fn patterns_p<S>() -> impl Parser<char, S, Output = Vec<Pattern>> + Clone
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    pattern_p().padded().separated_by(Just(',').padded())
}

// ---- Entry point ----

fn parse(input: &str) -> Result<Vec<Pattern>, ParseError> {
    let stream = Chars(input.chars());
    let input = skip_ws(stream.resume(()));
    if let Return(()) = input {
        return Ok(vec![]);
    }
    let (result, rest) = patterns_p().parse(input)?;
    match skip_ws(rest) {
        Return(()) => Ok(result),
        Yield(ch, _) => Err(ParseError::UnexpectedChar(ch)),
    }
}

fn main() {
    let cases = [
        "42",
        "hello",
        "1..10",
        "1 .. 100",
        "42, hello, 1..10",
        "abc, 5..20, xyz, 99",
        "",
        "1..",
        "1..abc",
        "@invalid",
    ];

    for input in &cases {
        println!("{:30} => {:?}", format!("\"{input}\""), parse(input));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plain_number() {
        assert_eq!(parse("42"), Ok(vec![Pattern::Number(42)]));
        assert_eq!(parse("0"), Ok(vec![Pattern::Number(0)]));
        assert_eq!(parse("12345"), Ok(vec![Pattern::Number(12345)]));
    }

    #[test]
    fn plain_ident() {
        assert_eq!(
            parse("hello"),
            Ok(vec![Pattern::Ident("hello".to_string())])
        );
        assert_eq!(parse("x"), Ok(vec![Pattern::Ident("x".to_string())]));
    }

    #[test]
    fn range_pattern() {
        assert_eq!(parse("1..10"), Ok(vec![Pattern::Range(1, 10)]));
        assert_eq!(parse("0..100"), Ok(vec![Pattern::Range(0, 100)]));
        assert_eq!(parse("  5 .. 20  "), Ok(vec![Pattern::Range(5, 20)]));
    }

    #[test]
    fn backtracking_number_not_range() {
        assert_eq!(parse("42"), Ok(vec![Pattern::Number(42)]));
    }

    #[test]
    fn comma_separated() {
        assert_eq!(
            parse("42, hello, 1..10"),
            Ok(vec![
                Pattern::Number(42),
                Pattern::Ident("hello".to_string()),
                Pattern::Range(1, 10),
            ])
        );
    }

    #[test]
    fn comma_separated_mixed() {
        assert_eq!(
            parse("abc, 5..20, xyz, 99"),
            Ok(vec![
                Pattern::Ident("abc".to_string()),
                Pattern::Range(5, 20),
                Pattern::Ident("xyz".to_string()),
                Pattern::Number(99),
            ])
        );
    }

    #[test]
    fn empty_input() {
        assert_eq!(parse(""), Ok(vec![]));
        assert_eq!(parse("   "), Ok(vec![]));
    }

    #[test]
    fn whitespace_handling() {
        assert_eq!(parse("  42  "), Ok(vec![Pattern::Number(42)]));
        assert_eq!(parse("  1 .. 10  "), Ok(vec![Pattern::Range(1, 10)]));
    }

    #[test]
    fn errors() {
        assert!(parse("1..").is_err());
        assert!(parse("@").is_err());
        assert!(parse("42 @").is_err());
    }

    #[test]
    fn error_incomplete_range_with_alpha() {
        assert!(parse("1..abc").is_err());
    }
}
