use crate::{FunctionSignature, TypeSignature};
use jswt_common::{Span, Type};
use std::{borrow::Cow, collections::BTreeMap};

#[derive(Debug)]
pub enum Symbol {
    Type(TypeSignature),
    Function(FunctionSignature),
    Class,
    Unknown,
}

impl Symbol {
    pub fn ty(ty: Type) -> Self {
        Symbol::Type(TypeSignature { ty })
    }

    pub fn function(params: Vec<Type>, returns: Type) -> Self {
        Symbol::Function(FunctionSignature { params, returns })
    }
}

#[derive(Debug)]
pub struct Scope {
    pub symbols: BTreeMap<Cow<'static, str>, Symbol>,
    pub ret: Option<Type>,
}

impl Scope {
    pub fn new(ret: Option<Type>) -> Self {
        Self {
            symbols: BTreeMap::new(),
            ret,
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
        self.push_scope(Span::new("program".into(), "root".into(), 0, 0), None);
    }

    // Pushing a scope adds it to the scope stack
    // and defines a new key in our global symbol map
    pub fn push_scope(&mut self, key: Span, returns: Option<Type>) {
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

    /// Define a symbol within the current active scope
    pub fn define<T: Into<Cow<'static, str>>>(&mut self, name: T, symbol: Symbol) {
        debug_assert!(self.scopes.len() > 0);
        let key = self.scopes.last().unwrap();
        let scope = self.table.get_mut(key).unwrap();
        scope.symbols.insert(name.into(), symbol);
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
    pub fn lookup<T: Into<Cow<'static, str>>>(&mut self, name: T) -> Option<&Symbol> {
        // Apparently descending ranges aren't a thing.
        // as of 1.53, trying to use one doesn't generate a warning
        // https://github.com/rust-lang/rust/issues/70925
        let name = &name.into();
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
