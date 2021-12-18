use std::{
    borrow::Cow,
    ops::{Add, Sub},
};

/// Descriptor for a span of text that can be located
/// in a Source file.
#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub struct Span {
    pub file: Cow<'static, str>,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(file: Cow<'static, str>, start: usize, end: usize) -> Self {
        Span { file, start, end }
    }
}

/// Generic descriptor of a resource that has a [Span]
pub trait Spannable {
    fn span(&self) -> Span;
}

/// This is syntactic sugar for combining spans
/// Produces a new Span that encompases both spans
/// Span {5, 6} + Span {6, 10} = Span {5, 10}
/// The file in the resulting span is always always the lhs
impl Add<Span> for Span {
    type Output = Span;
    fn add(self, rhs: Span) -> Span {
        Span {
            file: self.file,
            start: self.start,
            end: rhs.end,
        }
    }
}

/// Subtracting a span
impl Sub<Span> for Span {
    type Output = Span;

    fn sub(self, rhs: Span) -> Span {
        Span {
            file: self.file,
            start: self.start,
            end: rhs.start,
        }
    }
}
