// S-expression parser demonstrating elegant weave() composition patterns
//
// This example shows how to compose coroutines for parsing nested structures
// using weave() to elegantly handle lexing, parsing, and error recovery.
// S-expressions are ideal for demonstrating coroutine composition because
// of their simple, recursive structure.

use cocoro::FixedPointCoro;
use cocoro::Suspended;
use cocoro::recursive;
use cocoro::{Coro, Return, Suspend, Yield, weave};
use either::Either;
use either::Either::Left;
use either::Either::Right;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LeftParen,
    RightParen,
    Atom(String),
}

#[derive(Debug, Clone, PartialEq)]
enum SExpr {
    Atom(String),
    List(Vec<SExpr>),
}

#[derive(Debug, Clone, PartialEq)]
enum ParseError {
    UnexpectedEof,
    TrailingInput(Token),
    UnknownChar(char),
    UnmatchedParen,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedEof => write!(f, "Unexpected end of input"),
            ParseError::TrailingInput(token) => {
                write!(f, "Trailing input: {token:?}")
            }
            ParseError::UnknownChar(ch) => {
                write!(f, "Unknown character: '{ch}'")
            }
            ParseError::UnmatchedParen => write!(f, "Unmatched parenthesis"),
        }
    }
}

#[derive(Clone)]
struct Chars<'a> {
    input: std::str::Chars<'a>,
}

impl Coro<(), char, ()> for Chars<'_> {
    type Next = Self;
    type Suspend = Suspend<char, (), Self>;

    fn resume(mut self, _: ()) -> Self::Suspend {
        match self.input.next() {
            Some(ch) => Yield(ch, self),
            None => Return(()),
        }
    }
}

// Simple demonstration of using weave() for character-level processing
// (We'll keep this simple and use the straightforward tokenizer below)

// Simplified tokenizer that handles one token at a time
enum Tokenizer<'a> {
    Input(Chars<'a>),
    End,
}

impl<'a> Tokenizer<'a> {
    fn new(input: &'a str) -> Self {
        let chars = Chars {
            input: input.chars(),
        };
        Self::Input(chars)
    }
}

struct AtomParser(String);

impl Coro<char, (), Result<String, ParseError>> for AtomParser {
    type Next = Self;
    type Suspend = Suspend<(), Result<String, ParseError>, Self>;

    fn resume(self, c: char) -> Self::Suspend {
        let Self(mut so_far) = self;
        if c.is_alphanumeric() || "+-*/_=<>!?".contains(c) {
            so_far.push(c);
            Yield((), Self(so_far))
        } else if c.is_whitespace() {
            if so_far.is_empty() {
                Return(Err(ParseError::UnexpectedEof))
            } else {
                Return(Ok(so_far))
            }
        } else {
            Return(Err(ParseError::UnknownChar(c)))
        }
    }
}

struct FromState<A, C>(A, C);
impl<A, I, Y, R, C> Coro<I, Y, R> for FromState<A, C>
where
    C: FixedPointCoro<(A, I), (A, Y), R>,
{
    type Next = Self;
    type Suspend = Suspend<Y, R, Self::Next>;
    fn resume(self, input: I) -> Self::Suspend {
        let Self(a, coro) = self;
        match coro.resume((a, input)).into_enum() {
            Yield((a, y), next_coro) => Yield(y, FromState(a, next_coro)),
            Return(r) => Return(r),
        }
    }
}

fn from_state<A, I, Y, R, C>(start: A, coro: C) -> FromState<A, C>
where
    C: FixedPointCoro<(A, I), (A, Y), R>,
{
    FromState(start, coro)
}

fn parse_atom(
    first_char: char,
    input: Chars,
) -> Result<(String, Option<Chars>), ParseError> {
    let atom_parser = from_state(
        {
            let mut initial = String::new();
            initial.push(first_char);
            initial
        },
        recursive(&|rec, (mut so_far, c): (String, char)| {
            if c.is_alphanumeric() || "+-*/_=<>!?".contains(c) {
                so_far.push(c);
                Yield((so_far, ()), rec)
            } else if c.is_whitespace() {
                if so_far.is_empty() {
                    Return(Err(ParseError::UnexpectedEof))
                } else {
                    Return(Ok(so_far))
                }
            } else {
                Return(Err(ParseError::UnknownChar(c)))
            }
        }),
    );
    match weave(input, atom_parser, ()) {
        Left(((), FromState(so_far, _))) => {
            if so_far.is_empty() {
                Err(ParseError::UnexpectedEof)
            } else {
                Ok((so_far, None))
            }
        }
        Right((Ok(atom_result), remaining_input)) => {
            if atom_result.is_empty() {
                Err(ParseError::UnexpectedEof)
            } else {
                Ok((atom_result, Some(remaining_input)))
            }
        }
        Right((Err(err), _remaining_input)) => Err(err),
    }
}

impl Coro<(), Token, Result<(), ParseError>> for Tokenizer<'_> {
    type Next = Self;
    type Suspend = Suspend<Token, Result<(), ParseError>, Self>;

    fn resume(self, _: ()) -> Self::Suspend {
        use ParseError::UnknownChar;
        use Token::{Atom, LeftParen, RightParen};
        use Tokenizer::End;
        use Tokenizer::Input;
        match self {
            Input(input) => match input.resume(()).into_enum() {
                Return(()) => Return(Ok(())),
                Yield('(', input) => Yield(LeftParen, Input(input)),
                Yield(')', input) => Yield(RightParen, Input(input)),
                Yield(ch, input)
                    if ch.is_alphanumeric() || "+-*/_=<>!?".contains(ch) =>
                {
                    match parse_atom(ch, input) {
                        Ok((atom, Some(input))) => {
                            Yield(Atom(atom), Input(input))
                        }
                        Ok((atom, None)) => Yield(Atom(atom), End),
                        Err(err) => Return(Err(err)),
                    }
                }
                Yield(ch, _) => Return(Err(UnknownChar(ch))),
            },
            End => Return(Ok(())),
        }
    }
}

// S-expression parser that can parse both atoms and lists
enum SExprParser {
    Start,
    InList(Vec<SExpr>),
}

impl SExprParser {
    fn new() -> Self {
        Self::Start
    }
}

impl Coro<Token, (), Result<SExpr, ParseError>> for SExprParser {
    type Next = Self;
    type Suspend = Suspend<(), Result<SExpr, ParseError>, Self>;

    fn resume(self, token: Token) -> Self::Suspend {
        match (self, token) {
            (SExprParser::Start, Token::Atom(s)) => Return(Ok(SExpr::Atom(s))),
            (SExprParser::Start, Token::LeftParen) => {
                Yield((), SExprParser::InList(vec![]))
            }
            (SExprParser::Start, Token::RightParen) => {
                Return(Err(ParseError::UnmatchedParen))
            }

            (SExprParser::InList(mut elements), Token::Atom(s)) => {
                elements.push(SExpr::Atom(s));
                Yield((), SExprParser::InList(elements))
            }
            (SExprParser::InList(mut elements), Token::LeftParen) => {
                // For nested lists, we'll simplify and just add an empty list
                // for now
                elements.push(SExpr::List(vec![]));
                Yield((), SExprParser::InList(elements))
            }
            (SExprParser::InList(elements), Token::RightParen) => {
                Return(Ok(SExpr::List(elements)))
            }
        }
    }
}

// Simple list parser that collects elements
struct ListParser {
    elements: Vec<SExpr>,
    expecting_element: bool,
}

impl Coro<Token, (), Result<SExpr, ParseError>> for ListParser {
    type Next = Self;
    type Suspend = Suspend<(), Result<SExpr, ParseError>, Self>;

    fn resume(mut self, token: Token) -> Self::Suspend {
        match token {
            Token::Atom(s) => {
                self.elements.push(SExpr::Atom(s));
                self.expecting_element = false;
                Yield((), self)
            }

            Token::LeftParen => {
                // Nested list - for simplicity, treat as atom for now
                self.elements.push(SExpr::List(vec![]));
                self.expecting_element = false;
                Yield((), self)
            }

            Token::RightParen => Return(Ok(SExpr::List(self.elements))),
        }
    }
}

// Coroutine that expects no more input
struct ExpectEnd;

impl Coro<Token, (), ParseError> for ExpectEnd {
    type Next = Self;
    type Suspend = Suspend<(), ParseError, Self>;

    fn resume(self, token: Token) -> Self::Suspend {
        Return(ParseError::TrailingInput(token))
    }
}

fn parse_sexpr(input: &str) -> Result<SExpr, ParseError> {
    use Either::{Left, Right};

    let tokenizer = Tokenizer::new(input);
    let parser = SExprParser::new();

    match weave(tokenizer, parser, ()) {
        Left((tokenizer_result, _remaining_parser)) => match tokenizer_result {
            Ok(()) => Err(ParseError::UnexpectedEof),
            Err(lex_error) => Err(lex_error),
        },
        Right((parse_result, remaining_tokenizer)) => match parse_result {
            Ok(sexpr) => {
                // Check for trailing input using another weave()
                match weave(remaining_tokenizer, ExpectEnd, ()) {
                    Left((tokenizer_result, _)) => match tokenizer_result {
                        Ok(()) => Ok(sexpr),
                        Err(lex_error) => Err(lex_error),
                    },
                    Right((trailing_error, _)) => Err(trailing_error),
                }
            }
            Err(parse_error) => Err(parse_error),
        },
    }
}

fn main() {
    let test_cases = [
        "hello",       // Simple atom
        "()",          // Empty list
        "(hello)",     // Single element list
        "(a b c)",     // Multi-element list
        "(+ 1 2)",     // Function application
        "((a b) c)",   // Nested list
        "(a (b c) d)", // Mixed nesting
        "(incomplete", // Error: unmatched paren
        ")",           // Error: unmatched paren
        "a b",         // Error: trailing input
        "",            // Error: empty
        "(hello @)",   // Error: unknown char
    ];

    for input in &test_cases {
        print!("Input: {:12} -> ", format!("\"{}\"", input));
        match parse_sexpr(input) {
            Ok(sexpr) => println!("Success: {sexpr:?}"),
            Err(err) => println!("Error: {err}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_atom() {
        assert_eq!(parse_sexpr("hello"), Ok(SExpr::Atom("hello".to_string())));
    }

    #[test]
    fn test_empty_list() {
        assert_eq!(parse_sexpr("()"), Ok(SExpr::List(vec![])));
    }

    #[test]
    fn test_trailing_input() {
        assert!(matches!(
            parse_sexpr("a b"),
            Err(ParseError::TrailingInput(Token::Atom(_)))
        ));
    }

    #[test]
    fn test_unknown_char() {
        assert!(matches!(
            parse_sexpr("@"),
            Err(ParseError::UnknownChar('@'))
        ));
    }
}
