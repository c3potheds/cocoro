// Simple demonstration of weave() composition patterns
//
// This example focuses on the key insights about weave() composition:
// 1. Information preservation - no data is lost
// 2. Clear error semantics
// 3. Elegant trailing input detection
// 4. Bidirectional composition patterns

use std::fmt;

use cocoro::Coro;
use cocoro::Return;
use cocoro::Suspend;
use cocoro::Yield;
use cocoro::weave;
use either::Either;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Word(String),
    Number(i32),
    End,
}

#[derive(Debug, Clone, PartialEq)]
enum ParseResult {
    Words(Vec<String>),
}

#[derive(Debug, Clone, PartialEq)]
enum Error {
    UnknownChar(char),
    EmptyInput,
    TrailingTokens(Token),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnknownChar(ch) => write!(f, "Unknown character: '{ch}'"),
            Error::EmptyInput => write!(f, "Empty input"),
            Error::TrailingTokens(token) => {
                write!(f, "Trailing tokens: {token:?}")
            }
        }
    }
}

// Simple word tokenizer
struct WordTokenizer<'a> {
    chars: std::str::Chars<'a>,
    current: Option<char>,
    finished: bool,
}

impl<'a> WordTokenizer<'a> {
    fn new(input: &'a str) -> Self {
        let mut chars = input.chars();
        let current = chars.next();
        Self {
            chars,
            current,
            finished: false,
        }
    }

    fn advance(&mut self) {
        self.current = self.chars.next();
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

    fn read_word(&mut self) -> String {
        let mut word = String::new();
        while let Some(ch) = self.current {
            if ch.is_alphabetic() {
                word.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        word
    }

    fn read_number(&mut self) -> i32 {
        let mut num = 0;
        while let Some(ch) = self.current {
            if ch.is_ascii_digit() {
                num = num * 10 + (ch as i32 - '0' as i32);
                self.advance();
            } else {
                break;
            }
        }
        num
    }

    fn next_token(&mut self) -> Result<Token, Error> {
        self.skip_whitespace();

        match self.current {
            None => Ok(Token::End),
            Some(ch) if ch.is_alphabetic() => Ok(Token::Word(self.read_word())),
            Some(ch) if ch.is_ascii_digit() => {
                Ok(Token::Number(self.read_number()))
            }
            Some(ch) => Err(Error::UnknownChar(ch)),
        }
    }
}

impl Coro<(), Token, Result<(), Error>> for WordTokenizer<'_> {
    type Next = Self;
    type Suspend = Suspend<Token, Result<(), Error>, Self>;

    fn resume(mut self, _: ()) -> Self::Suspend {
        if self.finished {
            Return(Ok(()))
        } else {
            match self.next_token() {
                Ok(Token::End) => {
                    // Yield the End token and mark as finished for next resume
                    let mut next_self = self;
                    next_self.finished = true;
                    Yield(Token::End, next_self)
                }
                Ok(token) => Yield(token, self),
                Err(err) => Return(Err(err)),
            }
        }
    }
}

// Parser that collects only words
struct WordCollector {
    words: Vec<String>,
}

impl WordCollector {
    fn new() -> Self {
        Self { words: Vec::new() }
    }
}

impl Coro<Token, (), ParseResult> for WordCollector {
    type Next = Self;
    type Suspend = Suspend<(), ParseResult, Self>;

    fn resume(mut self, token: Token) -> Self::Suspend {
        match token {
            Token::Word(word) => {
                self.words.push(word);
                Yield((), self)
            }
            Token::Number(_) => {
                // Skip numbers, continue collecting
                Yield((), self)
            }
            Token::End => Return(ParseResult::Words(self.words)),
        }
    }
}

// Parser that stops at first number (demonstrates early termination)
struct StopAtNumber {
    words: Vec<String>,
}

impl StopAtNumber {
    fn new() -> Self {
        Self { words: Vec::new() }
    }
}

impl Coro<Token, (), ParseResult> for StopAtNumber {
    type Next = Self;
    type Suspend = Suspend<(), ParseResult, Self>;

    fn resume(mut self, token: Token) -> Self::Suspend {
        match token {
            Token::Word(word) => {
                self.words.push(word);
                Yield((), self)
            }
            Token::Number(_) => {
                // Stop processing when we see a number
                Return(ParseResult::Words(self.words))
            }
            Token::End => Return(ParseResult::Words(self.words)),
        }
    }
}

// Trailing input detector
struct ExpectEnd;

impl Coro<Token, (), Error> for ExpectEnd {
    type Next = Self;
    type Suspend = Suspend<(), Error, Self>;

    fn resume(self, token: Token) -> Self::Suspend {
        Return(Error::TrailingTokens(token))
    }
}

// Demonstrate different weave() composition patterns
fn demo_word_collection(input: &str) -> Result<ParseResult, Error> {
    let tokenizer = WordTokenizer::new(input);
    let parser = WordCollector::new();

    match weave(tokenizer, parser, ()) {
        Either::Left((tokenizer_result, _)) => match tokenizer_result {
            Ok(()) => Err(Error::EmptyInput),
            Err(err) => Err(err),
        },
        Either::Right((parse_result, remaining_tokenizer)) => {
            // Demonstrate information preservation: check for trailing input
            match weave(remaining_tokenizer, ExpectEnd, ()) {
                Either::Left((tokenizer_result, _)) => match tokenizer_result {
                    Ok(()) => Ok(parse_result),
                    Err(err) => Err(err),
                },
                Either::Right((trailing_error, _)) => Err(trailing_error),
            }
        }
    }
}

fn demo_early_termination(
    input: &str,
) -> Result<(ParseResult, Vec<Token>), Error> {
    let tokenizer = WordTokenizer::new(input);
    let parser = StopAtNumber::new();

    match weave(tokenizer, parser, ()) {
        Either::Left((tokenizer_result, _)) => match tokenizer_result {
            Ok(()) => Err(Error::EmptyInput),
            Err(err) => Err(err),
        },
        Either::Right((parse_result, mut remaining_tokenizer)) => {
            // Demonstrate information preservation: collect remaining tokens
            let mut remaining_tokens = Vec::new();

            loop {
                match remaining_tokenizer.resume(()) {
                    Yield(token, next) => {
                        remaining_tokens.push(token);
                        remaining_tokenizer = next;
                    }
                    Return(Ok(())) => break,
                    Return(Err(err)) => return Err(err),
                }
            }

            Ok((parse_result, remaining_tokens))
        }
    }
}

fn main() {
    println!("=== Word Collection Demo ===");
    let test_cases = [
        "hello world foo", // All words
        "hello 123 world", // Mixed (numbers ignored)
        "123 456",         // All numbers
        "",                // Empty
        "hello @",         // Error: unknown char
    ];

    for input in &test_cases {
        print!("Input: {:15} -> ", format!("\"{}\"", input));
        match demo_word_collection(input) {
            Ok(result) => println!("Success: {result:?}"),
            Err(err) => println!("Error: {err}"),
        }
    }

    println!("\n=== Early Termination Demo ===");
    let termination_cases = [
        "hello world 123 foo bar", // Should stop at 123, preserve "foo bar"
        "one two three",           // No numbers, should process all
        "123 hello world",         // Number first, should stop immediately
        "",                        // Empty
    ];

    for input in &termination_cases {
        print!("Input: {:20} -> ", format!("\"{}\"", input));
        match demo_early_termination(input) {
            Ok((result, remaining)) => {
                println!("Result: {result:?}, Remaining: {remaining:?}")
            }
            Err(err) => println!("Error: {err}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_word_collection() {
        assert_eq!(
            demo_word_collection("hello world"),
            Ok(ParseResult::Words(vec![
                "hello".to_string(),
                "world".to_string()
            ]))
        );
    }

    #[test]
    fn test_early_termination_preserves_info() {
        let (result, remaining) =
            demo_early_termination("hello 123 world").unwrap();
        assert_eq!(result, ParseResult::Words(vec!["hello".to_string()]));
        assert_eq!(
            remaining,
            vec![Token::Word("world".to_string()), Token::End]
        );
    }
}
