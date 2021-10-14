#[derive(Debug)]
pub enum TokenizerError {
    InvalidCharacter,
    UnexpectedEof,
    UnexpectedToken,
}

#[derive(Debug)]
pub enum ParseError<'a> {
    SyntaxError(&'a str),
}
