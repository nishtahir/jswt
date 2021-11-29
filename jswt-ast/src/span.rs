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

pub trait Spannable {
    fn span(&self) -> Span;
}

/// Produces a new Span that encompases both spans
impl std::ops::Add<Span> for Span {
    type Output = Span;

    fn add(self, rhs: Span) -> Span {
        Span {
            file: self.file,
            start: self.start,
            end: rhs.end,
        }
    }
}

impl std::ops::Sub<Span> for Span {
    type Output = Span;

    fn sub(self, rhs: Span) -> Span {
        Span {
            file: self.file,
            start: self.start,
            end: rhs.start,
        }
    }
}
