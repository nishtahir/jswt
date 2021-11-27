use std::usize;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub file: String,
    pub lexme: &'static str,
    pub offset: usize,
    pub kind: TokenType,
}

impl Token {
    pub fn new(file: &str, lexme: &'static str, kind: TokenType, offset: usize) -> Self {
        Token {
            file: file.to_owned(),
            lexme,
            offset,
            kind,
        }
    }

    pub fn eof(file: &str, offset: usize) -> Self {
        Token {
            file: file.to_owned(),
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
    Slash,
    Plus,
    Minus,
    Comma,
    Dot,
    Semi,
    Colon,
    At,

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
