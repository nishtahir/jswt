use jswt_common::Span;
use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(span: Span, kind: TokenType) -> Self {
        Token { kind, span }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
    // Single Character Tokens
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Star,
    Slash,
    Comma,
    Dot,
    Semi,
    Colon,
    At,

    // One or two character tokens
    And,
    Or,
    Not,
    Plus,
    PlusPlus,
    MinusMinus,
    Minus,
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
    Integer,
    Float,
    HexInteger,

    // Keywords
    True,
    False,
    Function,
    Import,
    Export,
    If,
    Else,
    Return,
    Let,
    Const,
    While,
    For,
    Class,
    Constructor,
    This,
    New,

    // Other
    WhiteSpace,
    Comment,
    Eof,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LeftBracket => f.write_str("["),
            TokenType::RightBracket => f.write_str("]"),
            TokenType::LeftParen => f.write_str("("),
            TokenType::RightParen => f.write_str(")"),
            TokenType::LeftBrace => f.write_str("{"),
            TokenType::RightBrace => f.write_str("}"),
            TokenType::Star => f.write_str("*"),
            TokenType::Slash => f.write_str("/"),
            TokenType::Plus => f.write_str("+"),
            TokenType::PlusPlus => f.write_str("++"),
            TokenType::Minus => f.write_str("-"),
            TokenType::MinusMinus => f.write_str("--"),
            TokenType::Comma => f.write_str("),"),
            TokenType::Dot => f.write_str("."),
            TokenType::Semi => f.write_str(";"),
            TokenType::Colon => f.write_str(":"),
            TokenType::At => f.write_str("@"),
            TokenType::And => f.write_str("&"),
            TokenType::Or => f.write_str("|"),
            TokenType::Not => f.write_str("!"),
            TokenType::Less => f.write_str("<"),
            TokenType::LessEqual => f.write_str("<="),
            TokenType::Greater => f.write_str(">"),
            TokenType::GreaterEqual => f.write_str(">="),
            TokenType::Equal => f.write_str("="),
            TokenType::EqualEqual => f.write_str("=="),
            TokenType::Bang => f.write_str("!"),
            TokenType::BangEqual => f.write_str("!="),
            TokenType::True => f.write_str("true"),
            TokenType::False => f.write_str("false"),
            TokenType::Import => f.write_str("import"),
            TokenType::Export => f.write_str("export"),
            TokenType::Function => f.write_str("function"),
            TokenType::If => f.write_str("if"),
            TokenType::Else => f.write_str("else"),
            TokenType::Return => f.write_str("return"),
            TokenType::Let => f.write_str("let"),
            TokenType::Const => f.write_str("const"),
            TokenType::While => f.write_str("while"),
            TokenType::For => f.write_str("for"),
            TokenType::Class => f.write_str("class"),
            TokenType::Constructor => f.write_str("constructor"),
            TokenType::This => f.write_str("this"),
            TokenType::New => f.write_str("new"),
            TokenType::Identifier => f.write_str("identifier"),
            TokenType::String => f.write_str("string"),
            TokenType::Integer => f.write_str("integer"),
            TokenType::Float => f.write_str("float"),
            TokenType::HexInteger => f.write_str("hex literal"),
            // Non punctuation. We shouldn't be trying
            // to serialize these
            TokenType::WhiteSpace => todo!(),
            TokenType::Comment => todo!(),
            TokenType::Eof => todo!(),
        }
    }
}
