//! Backtracking parser combinators built on cocoro's `Suspend` type.
//!
//! This extends the approach from `examples/parser_combinators.rs` with
//! backtracking support. The LL(1) parser in that example can't handle grammars
//! where productions share multi-token prefixes. Here we add `attempt` and
//! `or_else` combinators that clone the `Input<S>` before speculative parsing
//! and restore it on failure.
//!
//! Grammar parsed:
//! ```text
//! patterns = pattern (',' pattern)*
//! pattern  = range | literal
//! range    = number ".." number
//! literal  = number | ident
//! number   = digit+
//! ident    = alpha+
//! ```
//!
//! The interesting case: `range` and `literal` both start with `number`, so
//! parsing `pattern` requires trying `range` first, then backtracking to
//! `literal` if `..` doesn't follow.
//!
//! The backtracking mechanism relies on `Suspend` deriving `Clone`. Since
//! `Input<S> = Suspend<char, (), S>`, cloning it captures the full parser
//! state at a point in the stream, allowing us to restore it later.

use std::fmt;

use cocoro::Coro;
use cocoro::FixedPointCoro;
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

/// Character stream: yields chars one at a time, returns `()` at EOF.
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

/// The "cursor" into a character stream.
type Input<S> = Suspend<char, (), S>;

/// A parse result: the parsed value plus the remaining stream, or an error.
type PResult<T, S> = Result<(T, Input<S>), ParseError>;

/// Advance the stream by one step.
fn advance<S: FixedPointCoro<(), char, ()>>(stream: S) -> Input<S> {
    stream.resume(()).into_enum()
}

// ---- Backtracking combinators ----

/// Try a parser speculatively. On failure, return both the error and the
/// original (cloned) input so the caller can try an alternative.
fn attempt<S, T>(
    input: Input<S>,
    parser: impl FnOnce(Input<S>) -> PResult<T, S>,
) -> Result<(T, Input<S>), (ParseError, Input<S>)>
where
    S: FixedPointCoro<(), char, ()>,
    Input<S>: Clone,
{
    let saved = input.clone();
    match parser(input) {
        Ok(result) => Ok(result),
        Err(e) => Err((e, saved)),
    }
}

/// Try `first`; if it fails, try `second` on the restored input.
fn or_else<S, T>(
    input: Input<S>,
    first: impl FnOnce(Input<S>) -> PResult<T, S>,
    second: impl FnOnce(Input<S>) -> PResult<T, S>,
) -> PResult<T, S>
where
    S: FixedPointCoro<(), char, ()>,
    Input<S>: Clone,
{
    match attempt(input, first) {
        Ok(result) => Ok(result),
        Err((_err, restored)) => second(restored),
    }
}

// ---- Primitives ----

/// Skip whitespace characters.
fn skip_ws<S: FixedPointCoro<(), char, ()>>(mut input: Input<S>) -> Input<S> {
    loop {
        match input {
            Yield(ch, next) if ch.is_whitespace() => input = advance(next),
            other => break other,
        }
    }
}

/// Consume a single ASCII digit.
fn digit<S: FixedPointCoro<(), char, ()>>(input: Input<S>) -> PResult<char, S> {
    match input {
        Yield(ch, next) if ch.is_ascii_digit() => Ok((ch, advance(next))),
        Yield(_, _) => Err(ParseError::ExpectedDigit),
        Return(()) => Err(ParseError::UnexpectedEof),
    }
}

/// Consume a single ASCII alphabetic character.
fn alpha<S: FixedPointCoro<(), char, ()>>(input: Input<S>) -> PResult<char, S> {
    match input {
        Yield(ch, next) if ch.is_ascii_alphabetic() => Ok((ch, advance(next))),
        Yield(_, _) => Err(ParseError::ExpectedAlpha),
        Return(()) => Err(ParseError::UnexpectedEof),
    }
}

/// Consume the string "..".
fn dot_dot<S: FixedPointCoro<(), char, ()>>(input: Input<S>) -> PResult<(), S> {
    let input = match input {
        Yield('.', next) => advance(next),
        Yield(_, _) => return Err(ParseError::ExpectedStr("..")),
        Return(()) => return Err(ParseError::UnexpectedEof),
    };
    match input {
        Yield('.', next) => Ok(((), advance(next))),
        Yield(_, _) => Err(ParseError::ExpectedStr("..")),
        Return(()) => Err(ParseError::UnexpectedEof),
    }
}

// ---- Grammar rules ----

/// number = digit+
fn number<S: FixedPointCoro<(), char, ()>>(input: Input<S>) -> PResult<u64, S> {
    let (first, mut stream) = digit(input)?;
    let mut n = (first as u64) - ('0' as u64);

    let stream = loop {
        match stream {
            Yield(ch, next) if ch.is_ascii_digit() => {
                n = n * 10 + (ch as u64 - '0' as u64);
                stream = advance(next);
            }
            other => break other,
        }
    };

    Ok((n, stream))
}

/// ident = alpha+
fn ident<S: FixedPointCoro<(), char, ()>>(
    input: Input<S>,
) -> PResult<String, S> {
    let (first, mut stream) = alpha(input)?;
    let mut s = String::new();
    s.push(first);

    let stream = loop {
        match stream {
            Yield(ch, next) if ch.is_ascii_alphabetic() => {
                s.push(ch);
                stream = advance(next);
            }
            other => break other,
        }
    };

    Ok((s, stream))
}

/// range = number ".." number
fn range<S: FixedPointCoro<(), char, ()>>(
    input: Input<S>,
) -> PResult<Pattern, S> {
    let (lo, rest) = number(input)?;
    let rest = skip_ws(rest);
    let ((), rest) = dot_dot(rest)?;
    let rest = skip_ws(rest);
    let (hi, rest) = number(rest)?;
    Ok((Pattern::Range(lo, hi), rest))
}

/// literal = number | ident
fn literal<S: FixedPointCoro<(), char, ()>>(
    input: Input<S>,
) -> PResult<Pattern, S>
where
    Input<S>: Clone,
{
    or_else(
        input,
        |i| number(i).map(|(n, rest)| (Pattern::Number(n), rest)),
        |i| ident(i).map(|(s, rest)| (Pattern::Ident(s), rest)),
    )
}

/// pattern = range | literal
///
/// This is where backtracking matters: `range` starts with `number`, so does
/// `literal`. We try `range` first; if `..` isn't found after the first number,
/// we backtrack and try `literal`.
fn pattern<S: FixedPointCoro<(), char, ()>>(
    input: Input<S>,
) -> PResult<Pattern, S>
where
    Input<S>: Clone,
{
    or_else(input, range, literal)
}

/// patterns = pattern (',' pattern)*
fn patterns<S: FixedPointCoro<(), char, ()>>(
    input: Input<S>,
) -> PResult<Vec<Pattern>, S>
where
    Input<S>: Clone,
{
    let (first, mut rest) = pattern(skip_ws(input))?;
    let mut results = vec![first];

    loop {
        rest = skip_ws(rest);
        match rest {
            Yield(',', next) => {
                let next_input = skip_ws(advance(next));
                let (p, r) = pattern(next_input)?;
                results.push(p);
                rest = r;
            }
            other => {
                rest = other;
                break;
            }
        }
    }

    Ok((results, rest))
}

// ---- Entry point ----

fn parse(input: &str) -> Result<Vec<Pattern>, ParseError> {
    let stream = Chars(input.chars());
    let input = skip_ws(stream.resume(()));
    // Allow empty input
    match input {
        Return(()) => return Ok(vec![]),
        _ => {}
    }
    let (result, rest) = patterns(input)?;
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
        // "42" starts parsing as a range (number first), but there's no ".."
        // so it backtracks to literal → number.
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
        // Incomplete range: "1.." fails because second number is missing
        assert!(parse("1..").is_err());
        // Invalid character
        assert!(parse("@").is_err());
        // Trailing junk
        assert!(parse("42 @").is_err());
    }

    #[test]
    fn error_incomplete_range_with_alpha() {
        // "1..abc" fails because the second part of range isn't a number
        assert!(parse("1..abc").is_err());
    }
}
