use std::usize;

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    pub lexme: &'a str,
    pub offset: usize,
    pub kind: TokenType,
}

impl<'a> Token<'a> {
    pub fn new(lexme: &'a str, kind: TokenType, offset: usize) -> Self {
        Token {
            lexme,
            offset,
            kind,
        }
    }

    pub fn eof(offset: usize) -> Self {
        Token {
            lexme: "",
            kind: TokenType::Eof,
            offset,
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
    Export,
    If,
    Else,
    Return,
    Let,

    // Other
    Comment,
    Eof,
    Skip,
}
