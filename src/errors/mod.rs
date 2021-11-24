mod code;

pub use code::{code_frame, location_from_offset, Location, NodeLocation};

#[derive(Debug)]
pub enum TokenizerError {
    UnreconizedToken {
        file: String,
        token: &'static str,
        offset: usize,
    },
    UnexpectedEof,
}

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(String),
}
