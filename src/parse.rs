//! A parser for the assembly language.
#![cfg(ignore)]

use std::io;
use std::io::{Bytes, Read};

struct Lexer<R> {
    src: Bytes<R>,
    current: Option<u8>,
    /// A position just after `current`.
    pos: Position,
}

/// A line and a column start from 1.
/// They are never 0.
struct Position {
    line: usize,
    column: usize,
}

struct Located<T> {
    inner: T,
    pos: Position,
}

enum Token {
    Register(usize),
    Word(u32),
}

enum LexError {
    Io(io::Error, Position),
    IllegalByte(u8, Position),
}

fn is_whitespace(b: u8) -> bool {
    match b {
        b' ' | b'\n' => true,
        _ => false,
    }
}

fn is_digit_start(b: u8) -> bool {
    match b {
        b'1'...b'9' => true,
        _ => false,
    }
}

fn is_digit(b: u8) -> bool {
    b == b'0' || is_digit_start(b)
}

macro_rules! wrapped_try {
    ( $x:expr, $f:expr ) => {
        {
            match $x {
                Err(e) => Some(Err(e)),
                Ok(a) => $f(a),
            }
        }
    }
}

impl<R: Read> Lexer<R> {
    fn new(src: Bytes<R>) -> Lexer<R> {
        Lexer {
            src,
            current: None,
            pos: Position::default(),
        }
    }

    fn read_byte(&mut self) -> Result<(), LexError> {
        match self.src.next() {
            None => self.current = None,
            Some(r) => match r {
                Err(e) => return Err(LexError::from_io_error(e, self.pos)),
                Ok(b) => {
                    self.pos.increment(b == b'\n');
                    self.current = Some(b);
                }
            },
        }
        Ok(())
    }

    fn read_byte_option(&mut self) -> Option<Result<u8, LexError>> {
        match self.src.next() {
            None => {
                self.current = None;
                return None;
            }
            Some(r) => match r {
                Err(e) => return Some(Err(LexError::from_io_error(e, self.pos))),
                Ok(b) => {
                    self.pos.increment(b == b'\n');
                    self.current = Some(b);
                    return Some(Ok(b));
                }
            },
        }
    }

    fn lex(&mut self) -> Option<Result<Located<Token>, LexError>> {
        loop {
            match self.read_byte_option()? {
                Err(e) => Some(Err(e)),
                Ok(b) => {
                    let start = self.pos;
                    if is_whitespace(b) {
                        continue;
                    }
                    return Some(
                        match b {
                            _ if is_digit_start(b) => self.lex_word(),
                            b'R' => self.lex_register(),
                            _ => Err(LexError::IllegalByte(b, self.pos)),
                        }.map(|t| Located {
                            inner: t,
                            pos: start,
                        }),
                    );
                }
            }
        }
    }

    fn read_nat<T>(&mut self, start: T) -> Result<T, LexError> {
        let mut n = start;
        loop {
            match self.read_byte_option() {
                None => break,
                Some(x @ Err(_)) => return x,
                Some(Ok(b)) if is_digit(b) => n = n * 10 + (b - b'0') as T,
                Some(Ok(_)) => break,
            }
        }
        Ok(n)
    }

    fn lex_word(&mut self) -> Option<Result<Token, LexError>> {
        let mut w: u32 = 0;
        while self.current.is_some() {
            let b = self.current.unwrap();
            if !is_digit(b) {
                break;
            }
            w = w * 10 + (b - b'0') as u32;
            if let Err(e) = self.read_byte() {
                return Some(Err(e));
            }
        }
        Some(Ok(Token::Word(w)))
    }

    fn lex_register(&mut self) -> Option<Result<Token, LexError>> {
        match self.read_byte_option() {
            None => Some(Err(LexError::Expect(
                "digit to form a register literal".to_string(),
            ))),
            Some(r) => match r {
                Err(e) => Some(Err(e)),
                Ok(b) if is_digit_start(b) => wrapped_try!(
                    self.read_nat((b - b'0') as usize),
                    |n| Some(Ok(Token::Register(n)))
                ),
            },
        }
    }
}

impl Default for Position {
    fn default() -> Position {
        Position { line: 1, column: 1 }
    }
}

impl Position {
    fn increment(&mut self, newline: bool) {
        if newline {
            self.column = 1;
            self.line += 1;
        } else {
            self.column += 1;
        }
    }
}

impl LexError {
    fn from_io_error(e: io::Error, p: Position) -> LexError {
        LexError::Io(e, p)
    }
}
