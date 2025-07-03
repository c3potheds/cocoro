// A simple calculator example demonstrating lexer/parser composition with weave()
//
// This example shows how to compose coroutines using weave() for a complete
// parsing pipeline that handles tokenization and AST construction separately,
// with full error handling and information preservation.

use cocoro::{weave, Coro, Return, Suspend, Yield};
use either::Either;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Number(i64),
    Plus,
    Minus,
    Star,
    Slash,
    LeftParen,
    RightParen,
    End,
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Number(i64),
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
enum ParseError {
    UnexpectedToken(Token),
    UnexpectedEof,
    TrailingInput(Token),
    UnknownChar(char),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(token) => {
                write!(f, "Unexpected token: {token:?}")
            }
            ParseError::UnexpectedEof => write!(f, "Unexpected end of input"),
            ParseError::TrailingInput(token) => {
                write!(f, "Trailing input: {token:?}")
            }
            ParseError::UnknownChar(ch) => {
                write!(f, "Unknown character: '{ch}'")
            }
        }
    }
}

// Simple lexer that tokenizes character by character
struct Lexer<'a> {
    input: std::str::Chars<'a>,
    current: Option<char>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        let mut chars = input.chars();
        let current = chars.next();
        Lexer {
            input: chars,
            current,
        }
    }

    fn advance(&mut self) {
        self.current = self.input.next();
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self) -> i64 {
        let mut num = 0;
        while let Some(ch) = self.current {
            if ch.is_ascii_digit() {
                num = num * 10 + (ch as i64 - '0' as i64);
                self.advance();
            } else {
                break;
            }
        }
        num
    }

    fn next_token(&mut self) -> Result<Token, ParseError> {
        self.skip_whitespace();

        match self.current {
            None => Ok(Token::End),
            Some('0'..='9') => Ok(Token::Number(self.read_number())),
            Some('+') => {
                self.advance();
                Ok(Token::Plus)
            }
            Some('-') => {
                self.advance();
                Ok(Token::Minus)
            }
            Some('*') => {
                self.advance();
                Ok(Token::Star)
            }
            Some('/') => {
                self.advance();
                Ok(Token::Slash)
            }
            Some('(') => {
                self.advance();
                Ok(Token::LeftParen)
            }
            Some(')') => {
                self.advance();
                Ok(Token::RightParen)
            }
            Some(ch) => Err(ParseError::UnknownChar(ch)),
        }
    }
}

impl Coro<(), Token, Result<(), ParseError>> for Lexer<'_> {
    type Next = Self;
    type Suspend = Suspend<Token, Result<(), ParseError>, Self>;

    fn resume(mut self, _: ()) -> Self::Suspend {
        match self.next_token() {
            Ok(Token::End) => Return(Ok(())),
            Ok(token) => Yield(token, self),
            Err(err) => Return(Err(err)),
        }
    }
}

enum Parser {
    WaitingForToken,
    Parsing(Vec<Token>), // Accumulate tokens until we can parse
}

impl Parser {
    fn new() -> Self {
        Self::WaitingForToken
    }

    fn try_parse(tokens: &[Token]) -> Result<Expr, ParseError> {
        if tokens.is_empty() {
            return Err(ParseError::UnexpectedEof);
        }

        let mut parser = ExprParser::new(tokens);
        parser.parse_expression()
    }
}

// Simple recursive descent parser with operator precedence
struct ExprParser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> ExprParser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<&Token> {
        if self.pos < self.tokens.len() {
            let token = &self.tokens[self.pos];
            self.pos += 1;
            Some(token)
        } else {
            None
        }
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_addition()?;
        // Successfully parsed - return the expression
        Ok(expr)
    }

    // Addition and subtraction (lowest precedence)
    fn parse_addition(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_multiplication()?;

        while let Some(op) = self.current() {
            match op {
                Token::Plus | Token::Minus => {
                    let bin_op = match op {
                        Token::Plus => BinOp::Add,
                        Token::Minus => BinOp::Sub,
                        _ => unreachable!(),
                    };
                    self.advance();
                    let right = self.parse_multiplication()?;
                    left = Expr::Binary {
                        op: bin_op,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }

        Ok(left)
    }

    // Multiplication and division (higher precedence)
    fn parse_multiplication(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_primary()?;

        while let Some(op) = self.current() {
            match op {
                Token::Star | Token::Slash => {
                    let bin_op = match op {
                        Token::Star => BinOp::Mul,
                        Token::Slash => BinOp::Div,
                        _ => unreachable!(),
                    };
                    self.advance();
                    let right = self.parse_primary()?;
                    left = Expr::Binary {
                        op: bin_op,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }

        Ok(left)
    }

    // Primary expressions (numbers and parentheses)
    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.advance() {
            Some(Token::Number(n)) => Ok(Expr::Number(*n)),
            Some(Token::LeftParen) => {
                let expr = self.parse_expression()?;
                match self.advance() {
                    Some(Token::RightParen) => Ok(expr),
                    Some(token) => {
                        Err(ParseError::UnexpectedToken(token.clone()))
                    }
                    None => Err(ParseError::UnexpectedEof),
                }
            }
            Some(token) => Err(ParseError::UnexpectedToken(token.clone())),
            None => Err(ParseError::UnexpectedEof),
        }
    }
}

impl Coro<Token, (), Result<Expr, ParseError>> for Parser {
    type Next = Self;
    type Suspend = Suspend<(), Result<Expr, ParseError>, Self>;

    fn resume(mut self, token: Token) -> Self::Suspend {
        match self {
            Parser::WaitingForToken => {
                // Start accumulating tokens
                self = Parser::Parsing(vec![token]);
                Suspend::Yield((), self)
            }
            Parser::Parsing(mut tokens) => {
                tokens.push(token);

                // Check if we can parse a complete expression
                let mut parser = ExprParser::new(&tokens);
                match parser.parse_expression() {
                    Ok(expr) => {
                        // Debug: print parsing info
                        if tokens.len() > 3 {
                            eprintln!(
                                "Parsed {:?} from tokens {:?}, consumed {}/{}",
                                expr,
                                tokens,
                                parser.pos,
                                tokens.len()
                            );
                        }

                        // Check if we consumed all tokens
                        if parser.pos == tokens.len() {
                            Suspend::Return(Ok(expr))
                        } else {
                            // Continue accumulating - we might have more to parse
                            self = Parser::Parsing(tokens);
                            Suspend::Yield((), self)
                        }
                    }
                    Err(_) => {
                        // Can't parse yet, continue accumulating
                        self = Parser::Parsing(tokens);
                        Suspend::Yield((), self)
                    }
                }
            }
        }
    }
}

impl Parser {
    // Handle end of input by trying to parse what we have
    fn try_complete(&self) -> Result<Expr, ParseError> {
        match &self {
            Parser::WaitingForToken => Err(ParseError::UnexpectedEof),
            Parser::Parsing(tokens) => Self::try_parse(tokens),
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

fn parse_expression(input: &str) -> Result<Expr, ParseError> {
    use Either::{Left, Right};
    let lexer = Lexer::new(input);
    let parser = Parser::new();

    match weave(lexer, parser, ()) {
        // Lexer finished, try to complete parsing with what we have
        Left((lexer_result, remaining_parser)) => match lexer_result {
            Ok(()) => remaining_parser.try_complete(),
            Err(lex_error) => Err(lex_error),
        },
        Right((result, remaining_lexer)) => match result {
            // Successful parse, check for trailing input
            Ok(expr) => match weave(remaining_lexer, ExpectEnd, ()) {
                Left((lexer_result, _)) => match lexer_result {
                    Ok(()) => Ok(expr), // No trailing input
                    Err(lex_error) => Err(lex_error),
                },
                Right((trailing_error, _)) => Err(trailing_error),
            },
            Err(parse_error) => Err(parse_error),
        },
    }
}

fn main() {
    let test_cases = [
        "42",
        "1 + 2",
        "3 * 4 + 5",
        "1 + 2 * 3",
        "(1 + 2) * 3",
        "1 + ",    // Error: incomplete
        "1 + 2 3", // Error: trailing input
        "",        // Error: empty
        "1 + @",   // Error: unknown character
    ];

    for input in &test_cases {
        print!("Input: {:10} -> ", format!("\"{}\"", input));
        match parse_expression(input) {
            Ok(expr) => println!("Success: {expr:?}"),
            Err(err) => println!("Error: {err}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_number() {
        assert_eq!(parse_expression("42"), Ok(Expr::Number(42)));
    }

    #[test]
    fn test_addition() {
        assert_eq!(
            parse_expression("1 + 2"),
            Ok(Expr::Binary {
                op: BinOp::Add,
                left: Box::new(Expr::Number(1)),
                right: Box::new(Expr::Number(2)),
            })
        );
    }

    #[test]
    fn test_trailing_input() {
        assert!(matches!(
            parse_expression("1 + 2 3"),
            Err(ParseError::TrailingInput(Token::Number(3)))
        ));
    }

    #[test]
    fn test_incomplete_expression() {
        assert!(matches!(
            parse_expression("1 +"),
            Err(ParseError::UnexpectedEof)
        ));
    }
}
