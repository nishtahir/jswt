mod bindings;
mod tables;

use std::borrow::Cow;
use std::collections::BTreeMap;

use jswt_common::Type;

pub use self::bindings::*;
pub use self::tables::*;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionSignature {
    pub params: Vec<Type>,
    pub returns: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeSignature {
    pub ty: Type,
}

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
