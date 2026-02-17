//! Parser as Cocoro: using the visitor pattern for parsing.
//!
//! This is the third version of the same parser example. The previous versions:
//! - `backtracking_parser.rs`: standalone functions taking `Input<S>`
//! - `parser_trait.rs`: a `Parser<Tok>` trait with `parse<S>(&self, input)`
//!
//! This version makes the parser *itself* a `Cocoro` — the visitor that a
//! suspended coroutine dispatches to. Instead of `parser.parse(input)`, you
//! write `input.visit(parser)`. The parser receives either `on_yield(tok,
//! next)` or `on_return(())` and produces a parse result.
//!
//! The key trade-off: the stream type `S` is now a type parameter on the
//! `Parser` trait (and therefore on every combinator struct), rather than being
//! hidden behind a generic method. This makes the types more verbose but makes
//! the connection to `Cocoro` explicit.
//!
//! The same grammar is parsed:
//! ```text
//! patterns = pattern (',' pattern)*
//! pattern  = range | literal
//! range    = number ".." number
//! literal  = number | ident
//! number   = digit+
//! ident    = alpha+
//! ```
//!
//! Compare the three styles:
//! - **backtracking_parser**: `or_else(input, range, literal)`
//! - **parser_trait**: `range_p().or(literal_p()).parse(input)`
//! - **parser_cocoro**: `input.visit(range_p().or(literal_p()))`

#![allow(dead_code)]

use std::fmt;

use cocoro::Cocoro;
use cocoro::Coro;
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

// ---- Parser trait ----

/// A parser over a token stream `S` that yields tokens of type `Tok`.
///
/// This trait extends `Cocoro`: a `Parser` *is* a visitor that receives either
/// the next token (`on_yield`) or end-of-stream (`on_return`), and produces a
/// `PResult`.
///
/// Because `Cocoro::on_yield` takes `self` by value, parsers are consumed on
/// use. For combinators like `repeated()` that need to reuse a parser, the
/// parser must implement `Clone`.
trait Parser<Tok, S>:
    Cocoro<(), Tok, (), S, Out = PResult<Self::Output, Tok, S>> + Sized
where
    S: Coro<(), Tok, ()>,
{
    type Output;

    /// Convenience method: parse an input stream.
    fn parse(self, input: Input<Tok, S>) -> PResult<Self::Output, Tok, S> {
        input.visit(self)
    }
}

/// Wrapper that carries the stream type `S`, enabling combinator method chains.
///
/// Because `S` is a type parameter on `Parser`, Rust can't infer which `S` to
/// use when calling trait methods on a value like `Digit` (which implements
/// `Parser<char, S>` for *all* `S`). This wrapper pins `S` at construction
/// time, and provides combinator methods as inherent impls where method
/// resolution is unambiguous.
///
/// This is the main ergonomic workaround for putting `S` on the trait.
#[derive(Clone)]
struct Bind<Tok, S, P> {
    parser: P,
    _phantom: core::marker::PhantomData<(Tok, S)>,
}

/// Bind a parser to a specific stream type, enabling combinator method chains.
fn bind<Tok, S, P: Parser<Tok, S>>(parser: P) -> Bind<Tok, S, P>
where
    S: Coro<(), Tok, ()>,
{
    Bind {
        parser,
        _phantom: core::marker::PhantomData,
    }
}

impl<Tok, S, P> Bind<Tok, S, P>
where
    S: Coro<(), Tok, (), Next = S> + Clone,
    P: Parser<Tok, S>,
{
    fn parse(self, input: Input<Tok, S>) -> PResult<P::Output, Tok, S> {
        self.parser.parse(input)
    }

    fn map<F, B>(self, f: F) -> Bind<Tok, S, Map<P, F>>
    where
        F: Fn(P::Output) -> B,
    {
        bind(Map(self.parser, f))
    }

    fn then<Q: Parser<Tok, S>>(
        self,
        other: Bind<Tok, S, Q>,
    ) -> Bind<Tok, S, Then<P, Q>> {
        bind(Then(self.parser, other.parser))
    }

    fn skip<Q: Parser<Tok, S>>(
        self,
        other: Bind<Tok, S, Q>,
    ) -> Bind<Tok, S, Skip<P, Q>> {
        bind(Skip(self.parser, other.parser))
    }

    fn skip_left<Q: Parser<Tok, S>>(
        self,
        other: Bind<Tok, S, Q>,
    ) -> Bind<Tok, S, SkipLeft<P, Q>> {
        bind(SkipLeft(self.parser, other.parser))
    }

    fn or<Q: Parser<Tok, S, Output = P::Output>>(
        self,
        other: Bind<Tok, S, Q>,
    ) -> Bind<Tok, S, Or<P, Q>>
    where
        Tok: Clone,
    {
        bind(Or(self.parser, other.parser))
    }

    fn repeated(self) -> Bind<Tok, S, Repeated<P>>
    where
        Tok: Clone,
        P: Clone,
    {
        bind(Repeated(self.parser))
    }

    fn repeated1(self) -> Bind<Tok, S, Repeated1<P>>
    where
        Tok: Clone,
        P: Clone,
    {
        bind(Repeated1(self.parser))
    }

    fn separated_by<Sep: Parser<Tok, S> + Clone>(
        self,
        sep: Bind<Tok, S, Sep>,
    ) -> Bind<Tok, S, SeparatedBy<P, Sep>>
    where
        Tok: Clone,
        P: Clone,
    {
        bind(SeparatedBy(self.parser, sep.parser))
    }
}

impl<S, P> Bind<char, S, P>
where
    S: Coro<(), char, (), Next = S> + Clone,
    P: Parser<char, S>,
{
    fn padded(self) -> Bind<char, S, Padded<P>> {
        bind(Padded(self.parser))
    }
}

// ---- Combinator structs ----
//
// Each combinator must implement both `Cocoro` (the visitor) and `Parser`
// (which is just a marker trait with an `Output` associated type, since the
// `parse` method has a default implementation that calls `input.visit(self)`).
//
// The `Cocoro` impl provides `on_yield` and `on_return`. These are where the
// actual parsing logic lives. The `Parser` impl is a trivial blanket that just
// declares `Output`.

// Map: transform the output of a parser.

#[derive(Clone)]
struct Map<P, F>(P, F);

impl<Tok, S, P, F, B> Cocoro<(), Tok, (), S> for Map<P, F>
where
    S: Coro<(), Tok, (), Next = S>,
    P: Parser<Tok, S>,
    F: Fn(P::Output) -> B,
{
    type Out = PResult<B, Tok, S>;
    fn on_yield(self, tok: Tok, next: S) -> PResult<B, Tok, S> {
        let (a, rest) = self.0.on_yield(tok, next)?;
        Ok(((self.1)(a), rest))
    }
    fn on_return(self, r: ()) -> PResult<B, Tok, S> {
        let (a, rest) = self.0.on_return(r)?;
        Ok(((self.1)(a), rest))
    }
}

impl<Tok, S, P, F, B> Parser<Tok, S> for Map<P, F>
where
    S: Coro<(), Tok, (), Next = S>,
    P: Parser<Tok, S>,
    F: Fn(P::Output) -> B,
{
    type Output = B;
}

// Then: sequence two parsers, return (A, B).

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

// Skip: run two parsers, keep first's output.

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

// SkipLeft: run two parsers, keep second's output.

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

// Padded: skip whitespace before parsing (char-only).

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

// Or: try first parser, backtrack on failure, try second.

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

// Repeated: zero or more matches.

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

// Repeated1: one or more matches.

#[derive(Clone)]
struct Repeated1<P>(P);

impl<Tok, S, P> Cocoro<(), Tok, (), S> for Repeated1<P>
where
    Tok: Clone,
    S: Coro<(), Tok, (), Next = S> + Clone,
    P: Parser<Tok, S> + Clone,
{
    type Out = PResult<Vec<P::Output>, Tok, S>;
    fn on_yield(self, tok: Tok, next: S) -> PResult<Vec<P::Output>, Tok, S> {
        let (first, mut current) = self.0.clone().on_yield(tok, next)?;
        let mut results = vec![first];
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
    fn on_return(self, r: ()) -> PResult<Vec<P::Output>, Tok, S> {
        self.0.on_return(r).map(|(first, rest)| (vec![first], rest))
    }
}

impl<Tok, S, P> Parser<Tok, S> for Repeated1<P>
where
    Tok: Clone,
    S: Coro<(), Tok, (), Next = S> + Clone,
    P: Parser<Tok, S> + Clone,
{
    type Output = Vec<P::Output>;
}

// SeparatedBy: one or more items separated by a delimiter.

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

// ---- Primitive parser structs ----

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

// ---- Convenience constructors ----

fn digit_p() -> Digit {
    Digit
}

fn alpha_p() -> Alpha {
    Alpha
}

fn just(ch: char) -> Just<char> {
    Just(ch)
}

fn token(s: &'static str) -> Token {
    Token(s)
}

// ---- Grammar using combinators ----
//
// Because S is a type parameter on Parser (not just on parse()), the grammar
// functions must also be generic over S. This is the main ergonomic cost of
// the Cocoro-based approach compared to parser_trait.rs.
//
// Additionally, Rust's type inference can't always determine which S to use
// when chaining combinator methods. The `on::<S>()` helper constrains a
// parser to a specific stream type, nudging inference at the start of a chain.

fn number_p<S>() -> Bind<char, S, impl Parser<char, S, Output = u64> + Clone>
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    bind::<char, S, _>(digit_p())
        .repeated1()
        .map(|chars: Vec<char>| {
            chars
                .iter()
                .fold(0u64, |n, &c| n * 10 + (c as u64 - '0' as u64))
        })
}

fn ident_p<S>() -> Bind<char, S, impl Parser<char, S, Output = String> + Clone>
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    bind::<char, S, _>(alpha_p())
        .repeated1()
        .map(|chars: Vec<char>| chars.into_iter().collect::<String>())
}

fn range_p<S>() -> Bind<char, S, impl Parser<char, S, Output = Pattern> + Clone>
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    number_p::<S>()
        .skip(bind::<char, S, _>(token("..")).padded())
        .then(number_p::<S>().padded())
        .map(|(lo, hi)| Pattern::Range(lo, hi))
}

fn literal_p<S>()
-> Bind<char, S, impl Parser<char, S, Output = Pattern> + Clone>
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    number_p::<S>()
        .map(Pattern::Number)
        .or(ident_p::<S>().map(Pattern::Ident))
}

fn pattern_p<S>()
-> Bind<char, S, impl Parser<char, S, Output = Pattern> + Clone>
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    range_p::<S>().or(literal_p::<S>())
}

fn patterns_p<S>()
-> Bind<char, S, impl Parser<char, S, Output = Vec<Pattern>> + Clone>
where
    S: Coro<(), char, (), Next = S> + Clone,
{
    pattern_p::<S>()
        .padded()
        .separated_by(bind::<char, S, _>(just(',')).padded())
}

// ---- Entry point ----

fn parse(input: &str) -> Result<Vec<Pattern>, ParseError> {
    let stream = Chars(input.chars());
    let input = skip_ws(stream.resume(()));
    match input {
        Return(()) => return Ok(vec![]),
        _ => {}
    }
    let parser = patterns_p();
    // In this version, we could also write: input.visit(parser)
    let (result, rest) = parser.parse(input)?;
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
