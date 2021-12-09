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
    Plus,
    Minus,
    Comma,
    Dot,
    Semi,
    Colon,
    At,

    // One or two character tokens
    And,
    Or,
    Not,
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
    HexNumber,

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

    // Other
    WhiteSpace,
    Comment,
}

impl From<TokenType> for &'static str {
    fn from(ty: TokenType) -> Self {
        match ty {
            TokenType::LeftBracket => "[",
            TokenType::RightBracket => "]",
            TokenType::LeftParen => "(",
            TokenType::RightParen => ")",
            TokenType::LeftBrace => "{",
            TokenType::RightBrace => "}",
            TokenType::Star => "*",
            TokenType::Slash => "/",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Comma => ",",
            TokenType::Dot => ".",
            TokenType::Semi => ";",
            TokenType::Colon => ":",
            TokenType::At => "@",
            TokenType::And => "&",
            TokenType::Or => "|",
            TokenType::Not => "!",
            TokenType::Less => "<",
            TokenType::LessEqual => "<=",
            TokenType::Greater => ">",
            TokenType::GreaterEqual => ">=",
            TokenType::Equal => "=",
            TokenType::EqualEqual => "==",
            TokenType::Bang => "!",
            TokenType::BangEqual => "!=",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::Import => "import",
            TokenType::Export => "export",
            TokenType::Function => "function",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::Return => "return",
            TokenType::Let => "let",
            TokenType::Const => "const",
            TokenType::While => "while",
            // Non punctuation. We shouldn't be trying 
            // to serialize these
            TokenType::Identifier => todo!(),
            TokenType::String => todo!(),
            TokenType::Number => todo!(),
            TokenType::HexNumber => todo!(),
            TokenType::WhiteSpace => todo!(),
            TokenType::Comment => todo!(),
        }
    }
}
