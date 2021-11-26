#[derive(Debug, PartialEq)]
pub struct Symbol {
    ty: Type,
    name: &'static str,
}

impl Symbol {
    pub fn new(ty: Type, name: &'static str) -> Self {
        Self { ty, name }
    }
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Function,
    Number,
    Unknown,
}
