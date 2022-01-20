use crate::{
    symbols::{FunctionBinding, SymbolTable},
    SemanticError,
};
use std::vec;

use jswt_ast::*;
use jswt_types::Type;

#[derive(Debug)]
pub(crate) struct GlobalSymbolsResolver<'a> {
    pub symbols: &'a mut SymbolTable,
    pub errors: Vec<SemanticError>,
}

impl<'a> GlobalSymbolsResolver<'a> {
    pub fn new(symbols: &'a mut SymbolTable) -> Self {
        Self {
            symbols,
            errors: vec![],
        }
    }

    pub fn enter_program(&mut self, _: &Program) {
        self.symbols.push_global_scope();
    }

    pub fn exit_program(&mut self, _: &Program) {
        // we should only have the global scope
        debug_assert!(self.symbols.depth() == 1);
        self.symbols.pop_scope()
    }

    pub fn enter_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        // add to global symbol table
        let ident = &node.ident;
        let name = &ident.value;

        let returns = node
            .returns
            .as_ref()
            .map(|it| it.ty.clone())
            .unwrap_or(Type::Void);

        let params = node
            .params
            .parameters
            .iter()
            .map(|param| param.type_annotation.ty.clone())
            .collect();

        if self.symbols.lookup_current(&name).is_some() {
            let error = SemanticError::FunctionAlreadyDefined {
                name: name.clone(),
                span: ident.span.to_owned(),
            };
            self.errors.push(error);
        }

        self.symbols
            .define(name.clone(), FunctionBinding { params, returns });
    }

    pub fn exit_function_declaration(&mut self, _: &FunctionDeclarationElement) {
        // No-op
    }
}
