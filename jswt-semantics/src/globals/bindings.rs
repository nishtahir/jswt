use crate::{bindings::*, SemanticError};

use jswt_ast::*;

#[derive(Debug)]
pub struct GlobalBindingsResolver<'a> {
    pub stack: Vec<ClassBinding>,
    pub bindings: &'a mut BindingsTable,
    pub errors: Vec<SemanticError>,
}

impl<'a> GlobalBindingsResolver<'a> {
    pub fn new(bindings: &'a mut BindingsTable) -> Self {
        Self {
            stack: vec![],
            bindings,
            errors: vec![],
        }
    }

    pub fn enter_class_declaration(&mut self, _: &ClassDeclarationElement) {
        let binding = ClassBinding {};
        self.stack.push(binding);
    }

    pub fn exit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        let current = self.stack.pop().unwrap();
        let name = &node.ident.value;
        self.bindings.define(name.clone(), current);
    }
}
