use crate::SemanticError;
use std::vec;

use jswt_ast::*;
use jswt_symbols::{ClassBinding, Field, FunctionBinding, SymbolTable};
use jswt_types::{PrimitiveType, Type};

#[derive(Debug)]
pub(crate) struct GlobalSymbolsResolver<'a> {
    pub symbols: &'a mut SymbolTable,
    pub stack: Vec<ClassBinding>,
    pub errors: Vec<SemanticError>,
}

impl<'a> GlobalSymbolsResolver<'a> {
    pub fn new(symbols: &'a mut SymbolTable) -> Self {
        Self {
            symbols,
            stack: vec![],
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

        if self.symbols.lookup_current(name).is_some() {
            let error = SemanticError::FunctionAlreadyDefined {
                name: name.clone(),
                span: ident.span.to_owned(),
            };
            self.errors.push(error);
        }

        self.symbols
            .define(name.clone(), FunctionBinding { params, returns });
    }

    pub(crate) fn exit_function_declaration(&mut self, _: &FunctionDeclarationElement) {
        // No-op
    }

    pub(crate) fn enter_constructor_declaration(&mut self, node: &ClassConstructorElement) {
        let binding = self.stack.last_mut().unwrap();
        for (i, param) in node.params.parameters.iter().enumerate() {
            binding.fields.push(Field {
                name: param.ident.value.clone(),
                index: i,
                size: 4,
            })
        }

        if self.symbols.lookup_current("constructor").is_some() {
            let error = SemanticError::FunctionAlreadyDefined {
                name: "constructor".into(),
                span: node.span.to_owned(),
            };
            self.errors.push(error);
        }

        // This should be the type binding but for now mark as a pointer
        let returns = Type::Primitive(PrimitiveType::I32);

        let params = node
            .params
            .parameters
            .iter()
            .map(|param| param.type_annotation.ty.clone())
            .collect();

        self.symbols
            .define("constructor", FunctionBinding { params, returns });
    }

    pub(crate) fn enter_class_declaration(&mut self, node: &ClassDeclarationElement) {
        let binding = ClassBinding {
            name: node.ident.value.clone(),
            fields: vec![],
        };
        self.stack.push(binding);
    }

    pub(crate) fn exit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        let current = self.stack.pop().unwrap();
        let name = &node.ident.value;
        self.symbols.define(name.clone(), current);
    }

    pub(crate) fn enter_method_declaration(&self, node: &ClassMethodElement) {}
}
