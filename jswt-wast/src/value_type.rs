#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ValueType {
    I32,
}

impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::I32 => f.write_str("i32"),
        }
    }
}
