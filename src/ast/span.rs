#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    pub file: String,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(file: &str, start: usize, end: usize) -> Self {
        Span {
            file: file.to_owned(),
            start,
            end,
        }
    }
}

pub trait Spanable {}
