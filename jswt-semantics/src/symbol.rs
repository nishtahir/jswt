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

#[derive(Debug, PartialEq)]
pub enum Type {
    Function,
    // TODO - add builtins
    Unknown,
}

impl Type {
    /// Returns `true` if the type is [`Function`].
    ///
    /// [`Function`]: Type::Function
    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function)
    }
}
