#[derive(Debug)]
pub enum TokenizerError {
    InvalidCharacter,
    UnexpectedEof,
}

#[derive(Debug)]
pub enum ParseError<'a> {
    SyntaxError(&'a str),
}
