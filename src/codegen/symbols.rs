struct Scope {
    symbols: Vec<Symbol>,
}

struct Symbol {
    name: String,
}

struct SymbolTable {
    stack: Vec<Scope>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { stack: vec![] }
    }

    pub fn push() {

    }
}
