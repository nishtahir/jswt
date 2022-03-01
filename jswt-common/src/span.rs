use std::{
    borrow::Cow,
    fmt,
    ops::{Add, Sub},
};

use crate::fs;

/// Generic descriptor of a resource that has a [Span]
pub trait Spannable {
    fn span(&self) -> Span;
}

/// Descriptor for a span of text that can be located
/// in a Source file.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Span {
    pub file: Cow<'static, str>,
    pub module: Cow<'static, str>,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(
        file: Cow<'static, str>,
        module: Cow<'static, str>,
        start: usize,
        end: usize,
    ) -> Self {
        Span {
            file,
            module,
            start,
            end,
        }
    }

    pub fn synthetic() -> Self {
        Span {
            file: "".into(),
            module: "".into(),
            start: 0,
            end: 0,
        }
    }

    pub fn lexme(&self) -> &'static str {
        &fs::from_span(&self)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Span")
            .field("file", &self.file)
            .field("module", &self.module)
            .field("start", &self.start)
            .field("end", &self.end)
            .field("[content]", &fs::from_span(&self).to_owned())
            .finish()
    }
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
            module: self.module,
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
            module: self.module,
            start: self.start,
            end: rhs.start,
        }
    }
}
