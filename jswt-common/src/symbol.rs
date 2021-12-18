use crate::Type;

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub ty: Type,
    pub name: &'static str,
}

impl Symbol {
    pub fn new(ty: Type, name: &'static str) -> Self {
        Self { ty, name }
    }
}
