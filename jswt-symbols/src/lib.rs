mod bindings;
mod tables;
mod types;

use jswt_common::Span;
use jswt_common::Type;
use std::borrow::Cow;
use std::collections::BTreeMap;

pub use self::bindings::*;
pub use self::tables::*;
pub use self::types::*;

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
    pub declaration: Span,
    pub initialization: Option<Span>,
    pub name: Cow<'static, str>,
    pub ty: Type,
    pub is_mutable: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Parameter {
    pub name: Cow<'static, str>,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: Cow<'static, str>,
    pub params: Vec<Parameter>,
    pub ret: Type,
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Variable(Variable),
    Function(Function),
    // This means that it should be referenced in the bindings table
    Class,
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

pub trait Environment {
    type Symbols;
    type Bindings;
    type Types;

    fn bindings(&self) -> &Self::Bindings;
    fn bindings_mut(&mut self) -> &mut Self::Bindings;

    fn symbols(&self) -> &Self::Symbols;
    fn symbols_mut(&mut self) -> &mut Self::Symbols;

    fn types(&self) -> &Self::Types;
    fn types_mut(&mut self) -> &mut Self::Types;
}

#[derive(Debug, Default)]
pub struct SemanticEnvironment {
    bindings: SemanticBindingsTable,
    symbols: ScopedSymbolTable,
    types: TypeMap,
}

impl Environment for SemanticEnvironment {
    type Symbols = ScopedSymbolTable;
    type Bindings = SemanticBindingsTable;
    type Types = TypeMap;

    fn bindings(&self) -> &Self::Bindings {
        &self.bindings
    }

    fn bindings_mut(&mut self) -> &mut Self::Bindings {
        &mut self.bindings
    }

    fn symbols(&self) -> &Self::Symbols {
        &self.symbols
    }

    fn symbols_mut(&mut self) -> &mut Self::Symbols {
        &mut self.symbols
    }

    fn types(&self) -> &Self::Types {
        &self.types
    }

    fn types_mut(&mut self) -> &mut Self::Types {
        &mut self.types
    }
}

pub trait SymbolTable {
    fn get_symbol(&self, name: &str) -> Option<&Symbol>;
    fn insert_symbol(&mut self, name: &Cow<'static, str>, symbol: Symbol);
    fn push_global_symbol_scope(&mut self);
    fn push_symbol_scope(&mut self, span: Span);
    fn pop_symbol_scope(&mut self);
    fn symbol_scope_depth(&self) -> usize;
}

impl SymbolTable for SemanticEnvironment {
    fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbols.lookup(name)
    }

    fn insert_symbol(&mut self, name: &Cow<'static, str>, symbol: Symbol) {
        self.symbols.define(name, symbol);
    }

    fn push_symbol_scope(&mut self, span: Span) {
        self.symbols.push_scope(span);
    }

    fn pop_symbol_scope(&mut self) {
        self.symbols.pop_scope();
    }

    fn push_global_symbol_scope(&mut self) {
        self.symbols.push_global_scope();
    }

    fn symbol_scope_depth(&self) -> usize {
        self.symbols.depth()
    }
}

pub trait BindingsTable {
    fn get_binding(&self, name: &str) -> Option<&ClassBinding>;
    fn insert_binding(&mut self, name: Cow<'static, str>, binding: ClassBinding);
}

impl BindingsTable for SemanticEnvironment {
    fn get_binding(&self, name: &str) -> Option<&ClassBinding> {
        self.bindings.lookup(name)
    }

    fn insert_binding(&mut self, name: Cow<'static, str>, binding: ClassBinding) {
        self.bindings.insert(name, binding);
    }
}

pub trait TypesTable {
    fn get_type(&self, span: &Span) -> &Type;
    fn insert_type(&mut self, span: &Span, ty: &Type);
    fn insert_owned_type(&mut self, span: Span, ty: Type);
}

impl TypesTable for SemanticEnvironment {
    fn get_type(&self, span: &Span) -> &Type {
        self.types.get(span)
    }

    fn insert_type(&mut self, span: &Span, ty: &Type) {
        self.types.insert(span, ty);
    }

    fn insert_owned_type(&mut self, span: Span, ty: Type) {
        self.types.insert_owned(span, ty);
    }
}
