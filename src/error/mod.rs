#[derive(Debug)]
pub enum TokenizerError {
    UnexpectedToken(usize),
}

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(String),
}