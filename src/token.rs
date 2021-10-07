use std::usize;

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub lexme: &'a str,
    pub offset: usize,
    pub kind: TokenType,
}

impl<'a> Token<'a> {
    pub fn new(lexme: &'a str, kind: TokenType, offset: usize) -> Self {
        return Token {
            lexme,
            kind,
            offset,
        };
    }

    pub fn eof(offset: usize) -> Self {
        return Token {
            lexme: "",
            kind: TokenType::Eof,
            offset,
        };
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Function,
    If,
    Else,
    Return,
    True,
    False,
    Let,
    Identifier,
    String,
    Number,
    Eof,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Plus,
    Minus,
    Div,
    Star,
    Less,
    Greater,
    Equal,
    Bang,
    LessEqual,
    GreaterEqual,
    EqualEqual,
    BangEqual,
    Semi,
    Comment,
}
