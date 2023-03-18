use crate::{Scope, Symbol};
use core::hash::Hash;
use jswt_common::{Span, Type};
use std::{borrow::Cow, collections::BTreeMap};

#[derive(Debug)]
pub struct SimpleSymbolTable<K: Eq + Hash + Ord, V> {
    // BTree map to enforce order in tests
    table: Vec<BTreeMap<K, V>>,
}

impl<K: Eq + Hash + Ord, V> SimpleSymbolTable<K, V> {
    pub fn new(table: Vec<BTreeMap<K, V>>) -> Self {
        Self { table }
    }

    // TODO - we could capture the block span here
    // as a scope identifier
    pub fn push_scope(&mut self) {
        self.table.push(BTreeMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.table.pop();
    }

    pub fn depth(&self) -> usize {
        self.table.len()
    }

    pub fn define(&mut self, name: K, symbol: V) {
        let current_scope = self.table.last_mut().unwrap();
        current_scope.insert(name, symbol);
    }

    /// Looks up a gvien symbol in the table
    pub fn lookup(&self, name: &K) -> Option<&V> {
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

    /// Look for the symbol in the local scope on
    /// the top of the stack
    pub fn lookup_current(&self, name: &K) -> Option<&V> {
        let scope = self.table.last()?;
        scope.get(name)
    }

    /// Look for the symbol in the local scope on
    /// the bottom of the stack
    pub fn lookup_global(&self, name: &K) -> Option<&V> {
        let scope = self.table.first()?;
        scope.get(name)
    }

    /// Returns all the symbols available in
    /// the current local scope
    pub fn symbols_in_current_scope(&self) -> Vec<(&K, &V)> {
        self.table.last().unwrap().iter().collect()
    }
}

// Default instance will always have 1 scope on it
impl<K: Eq + Hash + Ord, V> Default for SimpleSymbolTable<K, V> {
    fn default() -> Self {
        Self {
            table: vec![BTreeMap::new()],
        }
    }
}

#[derive(Debug, Default)]
pub struct ScopedSymbolTable {
    scopes: Vec<Span>,
    table: BTreeMap<Span, Scope>,
}

impl ScopedSymbolTable {
    // TODO - We want to have entities within the parser have
    // their own spans. We're inserting a synthetic one for now
    // just to make this work
    pub fn push_global_scope(&mut self) {
        self.push_scope(Span::new("program".into(), "root".into(), 0, 0));
    }

    // Pushing a scope adds it to the scope stack
    // and defines a new key in our global symbol map
    pub fn push_scope(&mut self, key: Span) {
        self.scopes.push(key.clone());
        self.table
            .entry(key)
            .or_insert_with(|| Scope::new(None));
    }

    pub fn push_scope_with_return(&mut self, key: Span, returns: Option<Type>) {
        self.scopes.push(key.clone());
        self.table
            .entry(key.clone())
            .or_insert_with(|| Scope::new(returns));
    }

    // Poping a scope removes it from the scope stack
    // but keeps the table reference around.
    // This allows future passes to find a symbol based on the span key
    // such that they don't have to do a full pass to find the symbol again
    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    // Returns the current size of the scopes stack
    // This is used to determine if we are in the global scope or not
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    /// Define a symbol within the current active scope using an owned key
    // pub fn define_owned(&mut self, name: String, symbol: Symbol) -> Option<Symbol> {
    //     debug_assert!(self.scopes.len() > 0);
    //     let key = self.scopes.last().unwrap();
    //     let scope = self.table.get_mut(key).unwrap();
    //     scope.symbols.insert(Cow::Owned(name), symbol)
    // }

    // Define a symbol within the current active scope using a borrowed key
    pub fn define(&mut self, name: &Cow<'static, str>, symbol: Symbol) -> Option<Symbol> {
        debug_assert!(self.scopes.len() > 0);
        let key = self.scopes.last().unwrap();
        let scope = self.table.get_mut(key).unwrap();
        scope.symbols.insert(name.clone(), symbol)
    }

    // pub fn update_type(&mut self, name: &Cow<'static, str>, ty: Type) {
    //     debug_assert!(self.scopes.len() > 0);
    //     let key = self.scopes.last().unwrap();
    //     let scope = self.table.get_mut(key).unwrap();
    //     if let Some(symbol) = scope.symbols.get_mut(name) {
    //         symbol.ty = ty;
    //     }
    // }

    /// Lookup a Symbol based on symbols in the current active scope
    /// as well as scopes lower in the stack
    pub fn lookup(&mut self, name: &str) -> Option<&Symbol> {
        // Apparently descending ranges aren't a thing.
        // as of 1.53, trying to use one doesn't generate a warning
        // https://github.com/rust-lang/rust/issues/70925
        for idx in (0..self.scopes.len()).rev() {
            let key = &self.scopes[idx];
            let scope = self.table.get(key).unwrap();
            if let Some(sym) = scope.symbols.get(name) {
                return Some(sym);
            }
        }
        None
    }

    /// Look for the symbol in the local scope on
    /// the top of the stack
    pub fn lookup_current(&mut self, name: &str) -> Option<&Symbol> {
        debug_assert!(self.scopes.len() > 0);
        let key = &self.scopes.last().unwrap();
        let scope = self.table.get_mut(key).unwrap();
        scope.symbols.get(name)
    }

    /// Look for the symbol in the global scope on
    /// the top of the stack
    pub fn lookup_global(&mut self, name: &Cow<'static, str>) -> Option<&Symbol> {
        debug_assert!(self.scopes.len() > 0);
        let key = &self.scopes.first().unwrap();
        let scope = self.table.get_mut(key).unwrap();
        scope.symbols.get(name)
    }

    /// Returns all the symbols available in
    /// the current local scope
    pub fn get_scope(&mut self, key: Span) -> Option<&Scope> {
        self.table.get(&key)
    }

    pub fn scope_return_type(&mut self) -> Option<&Type> {
        debug_assert!(self.scopes.len() > 0);
        let key = &self.scopes.last().unwrap();
        let scope = self.table.get_mut(key).unwrap();
        scope.ret.as_ref()
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
        let mut sym: SimpleSymbolTable<&'static str, MockSymbol> = Default::default();
        sym.push_scope();
        assert_eq!(2, sym.depth());

        sym.pop_scope();
        assert_eq!(1, sym.depth())
    }

    #[test]
    fn test_symbol_lookup_in_local_scope() {
        let mut sym: SimpleSymbolTable<&'static str, MockSymbol> = Default::default();
        // Push local scope
        sym.push_scope();
        let x = MockSymbol::new("x");
        sym.define("x", x);

        let actual = sym.lookup(&"x");
        let x = MockSymbol::new("x");
        assert_eq!(Some(&x), actual);

        sym.pop_scope();
        let actual = sym.lookup(&"x");
        assert_eq!(None, actual);
    }

    #[test]
    fn test_symbol_lookup_searches_in_higher_scopes() {
        let mut sym: SimpleSymbolTable<&'static str, MockSymbol> = Default::default();
        let x = MockSymbol::new("x");
        sym.define("x", x);
        sym.push_scope();

        let actual = sym.lookup(&"x");
        let x = MockSymbol::new("x");
        assert_eq!(Some(&x), actual);
    }

    #[test]
    fn test_symbol_lookup_current_only_searches_current_scope() {
        let mut sym: SimpleSymbolTable<&'static str, MockSymbol> = Default::default();
        let x = MockSymbol::new("x");
        sym.define("x", x);
        sym.push_scope();

        let actual = sym.lookup_current(&"x");
        assert_eq!(None, actual);

        sym.pop_scope();

        let actual = sym.lookup_current(&"x");
        let x = MockSymbol::new("x");
        assert_eq!(Some(&x), actual);
    }
}
