use std::usize;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub lexme: &'static str,
    pub offset: usize,
    pub kind: TokenType,
}

impl Token {
    pub fn new(lexme: &'static str, kind: TokenType, offset: usize) -> Self {
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
}
