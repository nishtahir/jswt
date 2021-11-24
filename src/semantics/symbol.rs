use std::collections::HashMap;

#[derive(Default)]
pub struct SymbolTable {
    table: Vec<HashMap<&'static str, Symbol>>,
}

impl SymbolTable {
    pub fn push_scope(&mut self) {
        self.table.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.table.pop();
    }

    pub fn depth(&self) -> usize {
        self.table.len()
    }

    pub fn define(&mut self, symbol: Symbol) {
        // TODO - make this safe
        let current_scope = self.table.last_mut().unwrap();
        current_scope.insert(symbol.name, symbol);
    }

    pub fn lookup(&self, name: &'static str) -> Option<&Symbol> {
        // Apparently descending ranges aren't a thing.
        // as of 1.53, trying to use one doesn't generate a warning
        // https://github.com/rust-lang/rust/issues/70925
        for idx in (0..self.table.len()).rev() {
            let scope = &self.table[idx];
            if let Some(sym) = scope.get(name) {
                return Some(sym);
            }
        }
        None
    }

    pub fn lookup_current(&self, name: &'static str) -> Option<&Symbol> {
        let scope = self.table.last()?;
        scope.get(name)
    }
}

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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_symbol_table_push_pop_scope() {
        let mut sym = SymbolTable::default();
        sym.push_scope();
        assert_eq!(1, sym.depth());

        sym.pop_scope();
        assert_eq!(0, sym.depth())
    }

    #[test]
    fn test_symbol_lookup_in_local_scope() {
        let mut sym = SymbolTable::default();
        // Push global scope
        sym.push_scope();
        // Push local scope
        sym.push_scope();
        let x = Symbol::new(Type::Number, "x");
        sym.define(x);

        let actual = sym.lookup("x");

        let x = Symbol::new(Type::Number, "x");
        assert_eq!(Some(&x), actual);

        sym.pop_scope();
        let actual = sym.lookup("x");
        assert_eq!(None, actual);
    }

    #[test]
    fn test_symbol_lookup_searches_in_higher_scopes() {
        let mut sym = SymbolTable::default();
        // Push global scope
        sym.push_scope();
        let x = Symbol::new(Type::Number, "x");
        sym.define(x);
        sym.push_scope();

        let actual = sym.lookup("x");
        let x = Symbol::new(Type::Number, "x");
        assert_eq!(Some(&x), actual);
    }

    #[test]
    fn test_symbol_lookup_current_only_searches_current_scope() {
        let mut sym = SymbolTable::default();
        // Push global scope
        sym.push_scope();
        let x = Symbol::new(Type::Number, "x");
        sym.define(x);
        sym.push_scope();

        let actual = sym.lookup_current("x");
        assert_eq!(None, actual);

        sym.pop_scope();

        let actual = sym.lookup_current("x");
        let x = Symbol::new(Type::Number, "x");
        assert_eq!(Some(&x), actual);
    }
}
