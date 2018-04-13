//! A functional parser for the untyped assembly language.

use std::io;
use std::io::{Bytes, Read};
use std::marker::PhantomData;

struct Lexer<R> {
    phantom: PhantomData<R>,
}

enum Lex {
    Lexed(u8),
    EOF,
    Other(u8),
    Error(LexError),
}

enum LexError {
    Io(io::Error),
}

struct Cont<R>(Lex, Bytes<R>);

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

impl<R> Cont<R> {
    fn map<F>(self, f: F) -> Self
    where
        F: Fn(Lex) -> Lex,
    {
        Cont(f(self.0), self.1)
    }

    fn then<F>(self, mut f: F) -> Self
    where
        F: FnMut(u8, Bytes<R>) -> Self,
    {
        match self {
            Cont(Lex::Lexed(b), src) => f(b, src),
            c => c,
        }
    }
}

impl<R: Read> Lexer<R> {
    fn read_any(mut src: Bytes<R>) -> Cont<R> {
        match src.next() {
            None => Cont(Lex::EOF, src),
            Some(Err(e)) => Cont(Lex::Error(LexError::from(e)), src),
            Some(Ok(b)) => Cont(Lex::Lexed(b), src),
        }
    }

    fn read_with_condition<F>(src: Bytes<R>, f: F) -> Cont<R>
    where
        F: Fn(u8) -> bool,
    {
        Lexer::read_any(src).map(|l| match l {
            Lex::Lexed(b) if !f(b) => Lex::Other(b),
            _ => l,
        })
    }

    fn read_digit_start(src: Bytes<R>) -> Cont<R> {
        Lexer::read_with_condition(src, is_digit_start)
    }

    fn read_digit(src: Bytes<R>) -> Cont<R> {
        Lexer::read_with_condition(src, is_digit)
    }

    fn read_whitespace(src: Bytes<R>) -> Cont<R> {
        Lexer::read_with_condition(src, is_whitespace)
    }

    fn skip_whitespace(src: Bytes<R>) -> Cont<R> {
        match Lexer::read_whitespace(src) {
            Cont(Lex::Lexed(_), src) => Lexer::skip_whitespace(src),
            c => c,
        }
    }

    fn repeat<F>(src: Bytes<R>, f: F, v: &mut Vec<u8>) -> Cont<R>
    where
        F: Fn(Bytes<R>) -> Cont<R>,
    {
        match f(src) {
            Cont(Lex::Lexed(b), src) => {
                v.push(b);
                Lexer::repeat(src, f, v)
            }
            c => c,
        }
    }

    fn lex_nat(src: Bytes<R>, v: &mut Vec<u8>) -> Cont<R> {
        Lexer::read_digit_start(src).then(|b, src| {
            v.push(b);
            Lexer::repeat(src, Lexer::read_digit, v)
        })
    }
}

impl From<io::Error> for LexError {
    fn from(e: io::Error) -> LexError {
        LexError::Io(e)
    }
}
