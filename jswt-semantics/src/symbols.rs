use std::{borrow::Cow, collections::BTreeMap};

use jswt_common::Span;
use jswt_derive::FromEnumVariant;
use jswt_types::Type;

#[derive(Debug, Default)]
pub struct Scope {
    pub symbols: BTreeMap<Cow<'static, str>, Symbol>,
}

#[derive(Debug, PartialEq, FromEnumVariant)]
pub enum Symbol {
    Type(TypeBinding),
    Function(FunctionBinding),
    Class(ClassBinding),
}

impl Symbol {
    /// Returns `true` if the symbol is [`Function`].
    ///
    /// [`Function`]: Symbol::Function
    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function(..))
    }
}

#[derive(Debug, PartialEq)]
pub struct TypeBinding {
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub struct FunctionBinding {
    pub params: Vec<Type>,
    pub returns: Type,
}

#[derive(Debug, Default, PartialEq)]
pub struct ClassBinding {}

#[derive(Debug, Default)]
pub struct SymbolTable {
    scopes: Vec<Span>,
    table: BTreeMap<Span, Scope>,
}

impl SymbolTable {
    pub fn push_global_scope(&mut self) {
        // TODO - We want to have entities within the parser have
        // their own spans. We're inserting a synthetic one for now
        // just to make this work
        let global_span = Span::new("program".into(), "root".into(), 0, 0);
        self.push_scope(global_span);
    }

    // Pushing a scope adds it to the scope stack
    // and defines a new key in our global symbol map
    pub fn push_scope(&mut self, key: Span) {
        self.scopes.push(key.clone());
        self.table.entry(key).or_insert_with(Scope::default);
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

    /// Define a symbol within the current active scope
    pub fn define<T: Into<Cow<'static, str>>, U: Into<Symbol>>(&mut self, name: T, symbol: U) {
        debug_assert!(!self.scopes.is_empty());
        let key = self.scopes.last().unwrap();
        let scope = self.table.get_mut(key).unwrap();
        scope.symbols.insert(name.into(), symbol.into());
    }

    /// Lookup a Symbol based on symbols in the current active scope
    /// as well as scopes lower in the stack
    pub fn lookup<T: Into<Cow<'static, str>>>(&mut self, name: T) -> Option<&Symbol> {
        // Apparently descending ranges aren't a thing.
        // as of 1.53, trying to use one doesn't generate a warning
        // https://github.com/rust-lang/rust/issues/70925
        let name = &name.into();
        for idx in (0..self.scopes.len()).rev() {
            let key = &self.scopes[idx];
            if let Some(scope) = self.table.get(key) {
                if let Some(sym) = scope.symbols.get(name) {
                    return Some(sym);
                }
            }
        }
        None
    }

    /// Look for the symbol in the local scope on
    /// the top of the stack
    pub fn lookup_current(&mut self, name: &str) -> Option<&Symbol> {
        debug_assert!(!self.scopes.is_empty());
        let key = &self.scopes.last().unwrap();
        self.table
            .get_mut(key)
            .and_then(|scope| scope.symbols.get(name))
    }

    /// Look for the symbol in the global scope on
    /// the top of the stack
    pub fn lookup_global(&mut self, name: &Cow<'static, str>) -> Option<&Symbol> {
        debug_assert!(!self.scopes.is_empty());
        let key = &self.scopes.first().unwrap();
        self.table
            .get_mut(key)
            .and_then(|scope| scope.symbols.get(name))
    }

    /// Returns all the symbols available in
    /// the current local scope
    pub fn get_scope(&mut self, key: Span) -> Option<&Scope> {
        self.table.get(&key)
    }
}
