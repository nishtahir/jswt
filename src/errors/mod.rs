mod code;

pub use code::{code_frame, location_from_offset, Location, NodeLocation};

use crate::{ast::span::Span, tokenizer::TokenType};

#[derive(Debug, Clone)]
pub enum TokenizerError {
    UnreconizedToken {
        file: String,
        token: &'static str,
        offset: usize,
    },
    UnexpectedEof,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    MismatchedToken {
        expected: TokenType,
        actual: TokenType,
        span: Span,
    },
    NoViableAlternative {
        expected: Vec<TokenType>,
        actual: TokenType,
        span: Span,
    },
}
