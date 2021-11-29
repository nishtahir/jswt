use jswt_ast::Span;
use jswt_tokenizer::TokenType;

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
