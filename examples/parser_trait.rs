//! Parser trait with combinators built on cocoro's `Suspend` type.
//!
//! This is a companion to `examples/backtracking_parser.rs`. That example
//! implements parsers as standalone functions with signature
//! `fn(Input<S>) -> PResult<T, S>`. This example abstracts that pattern into a
//! `Parser` trait with combinator methods, similar to chumsky or nom.
//!
//! The `Parser` trait is generic over the token type (`Tok`), so the same
//! combinators work for `char` streams, `u8` streams, or any other token type.
//! The char-specific primitives (`Digit`, `Alpha`, `Padded`) only implement
//! `Parser<char>`.
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
//! Compare the two styles:
//! - **backtracking_parser**: `or_else(input, range, literal)`
//! - **parser_trait**: `range_p().or(literal_p())`

#![allow(dead_code)]

use std::fmt;

use cocoro::Coro;
use cocoro::FixedPointCoro;
use cocoro::Suspend;
use cocoro::Suspend::Return;
use cocoro::Suspended;
use cocoro::Yield;

// ---- Types (shared with backtracking_parser) ----

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

fn advance<Tok, S: FixedPointCoro<(), Tok, ()>>(stream: S) -> Input<Tok, S> {
    stream.resume(()).into_enum()
}

fn skip_ws<S: FixedPointCoro<(), char, ()>>(
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

trait Parser<Tok> {
    type Output;

    fn parse<S: FixedPointCoro<(), Tok, ()> + Clone>(
        &self,
        input: Input<Tok, S>,
    ) -> PResult<Self::Output, Tok, S>;

    fn map<F, B>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output) -> B,
    {
        Map(self, f)
    }

    fn then<Q>(self, other: Q) -> Then<Self, Q>
    where
        Self: Sized,
        Q: Parser<Tok>,
    {
        Then(self, other)
    }

    fn skip<Q>(self, other: Q) -> Skip<Self, Q>
    where
        Self: Sized,
        Q: Parser<Tok>,
    {
        Skip(self, other)
    }

    fn skip_left<Q>(self, other: Q) -> SkipLeft<Self, Q>
    where
        Self: Sized,
        Q: Parser<Tok>,
    {
        SkipLeft(self, other)
    }

    fn or<Q>(self, other: Q) -> Or<Self, Q>
    where
        Self: Sized,
        Q: Parser<Tok, Output = Self::Output>,
    {
        Or(self, other)
    }

    fn repeated(self) -> Repeated<Self>
    where
        Self: Sized,
    {
        Repeated(self)
    }

    fn repeated1(self) -> Repeated1<Self>
    where
        Self: Sized,
    {
        Repeated1(self)
    }

    fn separated_by<Sep>(self, sep: Sep) -> SeparatedBy<Self, Sep>
    where
        Self: Sized,
        Sep: Parser<Tok>,
    {
        SeparatedBy(self, sep)
    }
}

/// `padded()` is only available for char parsers since whitespace is a char
/// concept.
trait CharParserExt: Parser<char> + Sized {
    fn padded(self) -> Padded<Self> {
        Padded(self)
    }
}

impl<P: Parser<char>> CharParserExt for P {}

// ---- Combinator structs ----

#[derive(Clone)]
struct Map<P, F>(P, F);

impl<Tok, P, F, B> Parser<Tok> for Map<P, F>
where
    P: Parser<Tok>,
    F: Fn(P::Output) -> B,
{
    type Output = B;
    fn parse<S: FixedPointCoro<(), Tok, ()> + Clone>(
        &self,
        input: Input<Tok, S>,
    ) -> PResult<B, Tok, S> {
        let (a, rest) = self.0.parse(input)?;
        Ok(((self.1)(a), rest))
    }
}

#[derive(Clone)]
struct Then<P, Q>(P, Q);

impl<Tok, P, Q> Parser<Tok> for Then<P, Q>
where
    P: Parser<Tok>,
    Q: Parser<Tok>,
{
    type Output = (P::Output, Q::Output);
    fn parse<S: FixedPointCoro<(), Tok, ()> + Clone>(
        &self,
        input: Input<Tok, S>,
    ) -> PResult<Self::Output, Tok, S> {
        let (a, rest) = self.0.parse(input)?;
        let (b, rest) = self.1.parse(rest)?;
        Ok(((a, b), rest))
    }
}

#[derive(Clone)]
struct Skip<P, Q>(P, Q);

impl<Tok, P, Q> Parser<Tok> for Skip<P, Q>
where
    P: Parser<Tok>,
    Q: Parser<Tok>,
{
    type Output = P::Output;
    fn parse<S: FixedPointCoro<(), Tok, ()> + Clone>(
        &self,
        input: Input<Tok, S>,
    ) -> PResult<Self::Output, Tok, S> {
        let (a, rest) = self.0.parse(input)?;
        let (_, rest) = self.1.parse(rest)?;
        Ok((a, rest))
    }
}

#[derive(Clone)]
struct SkipLeft<P, Q>(P, Q);

impl<Tok, P, Q> Parser<Tok> for SkipLeft<P, Q>
where
    P: Parser<Tok>,
    Q: Parser<Tok>,
{
    type Output = Q::Output;
    fn parse<S: FixedPointCoro<(), Tok, ()> + Clone>(
        &self,
        input: Input<Tok, S>,
    ) -> PResult<Self::Output, Tok, S> {
        let (_, rest) = self.0.parse(input)?;
        let (b, rest) = self.1.parse(rest)?;
        Ok((b, rest))
    }
}

#[derive(Clone)]
struct Padded<P>(P);

impl<P: Parser<char>> Parser<char> for Padded<P> {
    type Output = P::Output;
    fn parse<S: FixedPointCoro<(), char, ()> + Clone>(
        &self,
        input: Input<char, S>,
    ) -> PResult<Self::Output, char, S> {
        self.0.parse(skip_ws(input))
    }
}

#[derive(Clone)]
struct Or<P, Q>(P, Q);

impl<Tok: Clone, P, Q> Parser<Tok> for Or<P, Q>
where
    P: Parser<Tok>,
    Q: Parser<Tok, Output = P::Output>,
{
    type Output = P::Output;
    fn parse<S: FixedPointCoro<(), Tok, ()> + Clone>(
        &self,
        input: Input<Tok, S>,
    ) -> PResult<Self::Output, Tok, S> {
        let saved = input.clone();
        match self.0.parse(input) {
            Ok(result) => Ok(result),
            Err(_) => self.1.parse(saved),
        }
    }
}

#[derive(Clone)]
struct Repeated<P>(P);

impl<Tok: Clone, P: Parser<Tok>> Parser<Tok> for Repeated<P> {
    type Output = Vec<P::Output>;
    fn parse<S: FixedPointCoro<(), Tok, ()> + Clone>(
        &self,
        input: Input<Tok, S>,
    ) -> PResult<Self::Output, Tok, S> {
        let mut results = Vec::new();
        let mut current = input;
        loop {
            let saved = current.clone();
            match self.0.parse(current) {
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
}

#[derive(Clone)]
struct Repeated1<P>(P);

impl<Tok: Clone, P: Parser<Tok>> Parser<Tok> for Repeated1<P> {
    type Output = Vec<P::Output>;
    fn parse<S: FixedPointCoro<(), Tok, ()> + Clone>(
        &self,
        input: Input<Tok, S>,
    ) -> PResult<Self::Output, Tok, S> {
        let (first, mut current) = self.0.parse(input)?;
        let mut results = vec![first];
        loop {
            let saved = current.clone();
            match self.0.parse(current) {
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
}

#[derive(Clone)]
struct SeparatedBy<P, Sep>(P, Sep);

impl<Tok: Clone, P, Sep> Parser<Tok> for SeparatedBy<P, Sep>
where
    P: Parser<Tok>,
    Sep: Parser<Tok>,
{
    type Output = Vec<P::Output>;
    fn parse<S: FixedPointCoro<(), Tok, ()> + Clone>(
        &self,
        input: Input<Tok, S>,
    ) -> PResult<Self::Output, Tok, S> {
        let (first, mut rest) = self.0.parse(input)?;
        let mut results = vec![first];
        loop {
            let saved = rest.clone();
            match self.1.parse(rest) {
                Ok((_, after_sep)) => match self.0.parse(after_sep) {
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
}

// ---- Primitive parser structs ----

#[derive(Clone, Copy)]
struct Digit;

impl Parser<char> for Digit {
    type Output = char;
    fn parse<S: FixedPointCoro<(), char, ()> + Clone>(
        &self,
        input: Input<char, S>,
    ) -> PResult<char, char, S> {
        match input {
            Yield(ch, next) if ch.is_ascii_digit() => Ok((ch, advance(next))),
            Yield(_, _) => Err(ParseError::ExpectedDigit),
            Return(()) => Err(ParseError::UnexpectedEof),
        }
    }
}

#[derive(Clone, Copy)]
struct Alpha;

impl Parser<char> for Alpha {
    type Output = char;
    fn parse<S: FixedPointCoro<(), char, ()> + Clone>(
        &self,
        input: Input<char, S>,
    ) -> PResult<char, char, S> {
        match input {
            Yield(ch, next) if ch.is_ascii_alphabetic() => {
                Ok((ch, advance(next)))
            }
            Yield(_, _) => Err(ParseError::ExpectedAlpha),
            Return(()) => Err(ParseError::UnexpectedEof),
        }
    }
}

#[derive(Clone, Copy)]
struct Just<Tok>(Tok);

impl<Tok: PartialEq + Clone + fmt::Debug> Parser<Tok> for Just<Tok> {
    type Output = Tok;
    fn parse<S: FixedPointCoro<(), Tok, ()> + Clone>(
        &self,
        input: Input<Tok, S>,
    ) -> PResult<Tok, Tok, S> {
        match input {
            Yield(tok, next) if tok == self.0 => Ok((tok, advance(next))),
            Yield(_, _) => Err(ParseError::ExpectedStr("expected token")),
            Return(()) => Err(ParseError::UnexpectedEof),
        }
    }
}

#[derive(Clone, Copy)]
struct Token(&'static str);

impl Parser<char> for Token {
    type Output = ();
    fn parse<S: FixedPointCoro<(), char, ()> + Clone>(
        &self,
        mut input: Input<char, S>,
    ) -> PResult<(), char, S> {
        for expected in self.0.chars() {
            match input {
                Yield(ch, next) if ch == expected => input = advance(next),
                Yield(_, _) => return Err(ParseError::ExpectedStr(self.0)),
                Return(()) => return Err(ParseError::UnexpectedEof),
            }
        }
        Ok(((), input))
    }
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

fn number_p() -> impl Parser<char, Output = u64> + Clone {
    digit_p().repeated1().map(|chars: Vec<char>| {
        chars
            .iter()
            .fold(0u64, |n, &c| n * 10 + (c as u64 - '0' as u64))
    })
}

fn ident_p() -> impl Parser<char, Output = String> + Clone {
    alpha_p()
        .repeated1()
        .map(|chars: Vec<char>| chars.into_iter().collect::<String>())
}

fn range_p() -> impl Parser<char, Output = Pattern> + Clone {
    number_p()
        .skip(token("..").padded())
        .then(number_p().padded())
        .map(|(lo, hi)| Pattern::Range(lo, hi))
}

fn literal_p() -> impl Parser<char, Output = Pattern> + Clone {
    number_p()
        .map(Pattern::Number)
        .or(ident_p().map(Pattern::Ident))
}

fn pattern_p() -> impl Parser<char, Output = Pattern> + Clone {
    range_p().or(literal_p())
}

fn patterns_p() -> impl Parser<char, Output = Vec<Pattern>> + Clone {
    pattern_p().padded().separated_by(just(',').padded())
}

// ---- Entry point ----

fn parse(input: &str) -> Result<Vec<Pattern>, ParseError> {
    let stream = Chars(input.chars());
    let input = skip_ws(stream.resume(()));
    if let Return(()) = input {
        return Ok(vec![]);
    }
    let parser = patterns_p();
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
