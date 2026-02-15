//! Parser combinators built on cocoro's `Suspend` type.
//!
//! This example explores using `Suspend<Y, R, N>` as a natural "cursor" for
//! parser combinators. Each parser is a function:
//!
//!     fn parse(input: Suspend<char, (), S>) -> Result<(T, Suspend<char, (),
//! S>), ParseError>
//!
//! The key insight: `Suspend` is either `Yield(char, next_stream)` or
//! `Return(())`. A parser can inspect the next character *without consuming it*
//! by matching `Yield(ch, next)` and returning the whole value if it doesn't
//! want that character. This gives us single-character lookahead for free —
//! the `Suspend` value acts as a "peekable cursor" into the stream.
//!
//! Each pattern match on `Suspend` is equivalent to a `Cocoro` implementation:
//! matching `Yield(ch, next)` corresponds to `on_yield`, and matching
//! `Return(())` corresponds to `on_return`. But plain pattern matching is more
//! ergonomic than implementing the trait for each parsing step.
//!
//! Grammar parsed:
//! ```text
//! expr   = term (('+' | '-') term)*
//! term   = factor (('*' | '/') factor)*
//! factor = number | '(' expr ')'
//! number = digit+
//! ```
//!
//! Compare this (~150 lines of parser logic) with `examples/calculator.rs`
//! (~340 lines using manual `Coro` state machines + a separate recursive
//! descent parser).

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
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedEof => write!(f, "unexpected end of input"),
            Self::UnexpectedChar(c) => write!(f, "unexpected character: '{c}'"),
            Self::ExpectedDigit => write!(f, "expected a digit"),
        }
    }
}

/// Character stream: yields chars one at a time, returns `()` at EOF.
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

/// The "cursor" into a character stream — either a char is available, or EOF.
type Input<S> = Suspend<char, (), S>;

/// A parse result: the parsed value plus the remaining stream, or an error.
type PResult<T, S> = Result<(T, Input<S>), ParseError>;

/// Advance the stream by one step, converting to the concrete `Suspend` enum.
fn advance<S: FixedPointCoro<(), char, ()>>(stream: S) -> Input<S> {
    stream.resume(()).into_enum()
}

// ---- Primitives ----

/// Skip whitespace characters, stopping at the first non-whitespace or EOF.
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

/// Consume a specific expected character.
fn expect<S: FixedPointCoro<(), char, ()>>(
    input: Input<S>,
    expected: char,
) -> PResult<char, S> {
    match input {
        Yield(ch, next) if ch == expected => Ok((ch, advance(next))),
        Yield(ch, _) => Err(ParseError::UnexpectedChar(ch)),
        Return(()) => Err(ParseError::UnexpectedEof),
    }
}

// ---- Grammar rules ----

/// number = digit+
fn number<S: FixedPointCoro<(), char, ()>>(input: Input<S>) -> PResult<i64, S> {
    let (first, mut stream) = digit(input)?;
    let mut n = (first as i64) - ('0' as i64);

    // Consume additional digits via lookahead: inspect each char, and if it's
    // not a digit, break *without consuming it* — `other` preserves the full
    // Yield(char, next) value for the caller.
    let stream = loop {
        match stream {
            Yield(ch, next) if ch.is_ascii_digit() => {
                n = n * 10 + (ch as i64 - '0' as i64);
                stream = advance(next);
            }
            other => break other,
        }
    };

    Ok((n, stream))
}

/// factor = number | '(' expr ')'
fn factor<S: FixedPointCoro<(), char, ()>>(input: Input<S>) -> PResult<i64, S> {
    match input {
        Yield('(', next) => {
            let (n, rest) = expr(skip_ws(advance(next)))?;
            let (_, rest) = expect(skip_ws(rest), ')')?;
            Ok((n, rest))
        }
        // Not '(' — pass the *entire* Suspend value to number(), preserving
        // the character we just inspected.
        other => number(other),
    }
}

/// term = factor (('*' | '/') factor)*
fn term<S: FixedPointCoro<(), char, ()>>(input: Input<S>) -> PResult<i64, S> {
    let (mut result, mut rest) = factor(input)?;

    loop {
        rest = skip_ws(rest);
        match rest {
            Yield('*', next) => {
                let (n, r) = factor(skip_ws(advance(next)))?;
                result *= n;
                rest = r;
            }
            Yield('/', next) => {
                let (n, r) = factor(skip_ws(advance(next)))?;
                result /= n;
                rest = r;
            }
            other => {
                rest = other;
                break;
            }
        }
    }

    Ok((result, rest))
}

/// expr = term (('+' | '-') term)*
fn expr<S: FixedPointCoro<(), char, ()>>(input: Input<S>) -> PResult<i64, S> {
    let (mut result, mut rest) = term(input)?;

    loop {
        rest = skip_ws(rest);
        match rest {
            Yield('+', next) => {
                let (n, r) = term(skip_ws(advance(next)))?;
                result += n;
                rest = r;
            }
            Yield('-', next) => {
                let (n, r) = term(skip_ws(advance(next)))?;
                result -= n;
                rest = r;
            }
            other => {
                rest = other;
                break;
            }
        }
    }

    Ok((result, rest))
}

// ---- Entry point ----

fn parse(input: &str) -> Result<i64, ParseError> {
    let stream = Chars(input.chars());
    let input = skip_ws(stream.resume(()));
    let (result, rest) = expr(input)?;
    match skip_ws(rest) {
        Return(()) => Ok(result),
        Yield(ch, _) => Err(ParseError::UnexpectedChar(ch)),
    }
}

fn main() {
    let cases = [
        "42",
        "1 + 2",
        "3 * 4 + 5",
        "1 + 2 * 3",
        "(1 + 2) * 3",
        "10 - 3 + 5",
        "  100  +  200  ",
        "2 * (3 + 4) - 1",
        "",
        "1 + ",
        "1 + @",
        "1 2",
        "(1 + 2",
    ];

    for input in &cases {
        println!("{:24} => {:?}", format!("\"{input}\""), parse(input));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numbers() {
        assert_eq!(parse("42"), Ok(42));
        assert_eq!(parse("0"), Ok(0));
        assert_eq!(parse("12345"), Ok(12345));
    }

    #[test]
    fn addition_subtraction() {
        assert_eq!(parse("1 + 2"), Ok(3));
        assert_eq!(parse("10 - 3"), Ok(7));
        assert_eq!(parse("1 + 2 + 3"), Ok(6));
        assert_eq!(parse("10 - 3 + 5"), Ok(12));
    }

    #[test]
    fn multiplication_division() {
        assert_eq!(parse("2 * 3"), Ok(6));
        assert_eq!(parse("10 / 2"), Ok(5));
        assert_eq!(parse("2 * 3 * 4"), Ok(24));
    }

    #[test]
    fn precedence() {
        assert_eq!(parse("1 + 2 * 3"), Ok(7));
        assert_eq!(parse("2 * 3 + 1"), Ok(7));
        assert_eq!(parse("10 - 2 * 3"), Ok(4));
    }

    #[test]
    fn parentheses() {
        assert_eq!(parse("(1 + 2) * 3"), Ok(9));
        assert_eq!(parse("2 * (3 + 4)"), Ok(14));
        assert_eq!(parse("((1 + 2))"), Ok(3));
        assert_eq!(parse("(((42)))"), Ok(42));
    }

    #[test]
    fn whitespace() {
        assert_eq!(parse("  42  "), Ok(42));
        assert_eq!(parse("  1  +  2  "), Ok(3));
    }

    #[test]
    fn errors() {
        assert!(parse("").is_err());
        assert!(parse("1 +").is_err());
        assert!(parse("1 @").is_err());
        assert!(parse("1 2").is_err());
        assert!(parse("(1 + 2").is_err());
        assert!(parse(")").is_err());
    }
}
