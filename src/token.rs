use std::{rc::Rc, usize};

#[derive(Debug, PartialEq)]
pub struct Token {
    pub src: Rc<String>,
    pub offset: usize,
    pub len: usize,
    pub kind: TokenType,
}

impl<'a> Token {
    pub fn new(src: Rc<String>, kind: TokenType, offset: usize, len: usize) -> Self {
        Token {
            src,
            offset,
            len,
            kind,
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
    // Single Character Tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Star,
    Div,
    Plus,
    Minus,
    Comma,
    Dot,
    Semi,
    Colon,

    // One or two character tokens
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,

    // Literal
    Identifier,
    String,
    Number,

    // Keywords
    True,
    False,
    Print,
    Function,
    Import,
    Export,
    If,
    Else,
    Return,
    Let,
    Const,

    // Other
    Comment,
    Eof,
    Skip,
}
