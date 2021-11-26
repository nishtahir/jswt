use std::collections::HashMap;

#[derive(Debug)]
pub struct SymbolTable<T> {
    table: Vec<HashMap<&'static str, T>>,
}

impl<T> SymbolTable<T> {
    pub fn new(table: Vec<HashMap<&'static str, T>>) -> Self {
        Self { table }
    }

    pub fn push_scope(&mut self) {
        self.table.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.table.pop();
    }

    pub fn depth(&self) -> usize {
        self.table.len()
    }

    pub fn define(&mut self, name: &'static str, symbol: T) {
        // TODO - make this safe
        let current_scope = self.table.last_mut().unwrap();
        current_scope.insert(name, symbol);
    }

    pub fn lookup(&self, name: &'static str) -> Option<&T> {
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

    pub fn lookup_current(&self, name: &'static str) -> Option<&T> {
        let scope = self.table.last()?;
        scope.get(name)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Default, Debug, PartialEq)]
    struct MockSymbol {
        ty: &'static str,
    }

    impl MockSymbol {
        fn new(ty: &'static str) -> Self {
            Self { ty }
        }
    }

    #[test]
    fn test_symbol_table_push_pop_scope() {
        let mut sym = SymbolTable::<MockSymbol>::new(vec![]);
        sym.push_scope();
        assert_eq!(1, sym.depth());

        sym.pop_scope();
        assert_eq!(0, sym.depth())
    }

    #[test]
    fn test_symbol_lookup_in_local_scope() {
        let mut sym = SymbolTable::<MockSymbol>::new(vec![]);
        // Push global scope
        sym.push_scope();
        // Push local scope
        sym.push_scope();
        let x = MockSymbol::new("x");
        sym.define("x", x);

        let actual = sym.lookup("x");
        let x = MockSymbol::new("x");
        assert_eq!(Some(&x), actual);

        sym.pop_scope();
        let actual = sym.lookup("x");
        assert_eq!(None, actual);
    }

    #[test]
    fn test_symbol_lookup_searches_in_higher_scopes() {
        let mut sym = SymbolTable::<MockSymbol>::new(vec![]);
        // Push global scope
        sym.push_scope();
        let x = MockSymbol::new("x");
        sym.define("x", x);
        sym.push_scope();

        let actual = sym.lookup("x");
        let x = MockSymbol::new("x");
        assert_eq!(Some(&x), actual);
    }

    #[test]
    fn test_symbol_lookup_current_only_searches_current_scope() {
        let mut sym = SymbolTable::new(vec![]);
        // Push global scope
        sym.push_scope();
        let x = MockSymbol::new("x");
        sym.define("x", x);
        sym.push_scope();

        let actual = sym.lookup_current("x");
        assert_eq!(None, actual);

        sym.pop_scope();

        let actual = sym.lookup_current("x");
        let x = MockSymbol::new("x");
        assert_eq!(Some(&x), actual);
    }
}
