use crate::SemanticError;

use jswt_ast::*;
use jswt_common::{BindingsTable, ClassBinding, Field};

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

    pub fn enter_class_declaration(&mut self, node: &ClassDeclarationElement) {
        let binding = ClassBinding {
            name: node.ident.value.clone(),
            fields: vec![],
        };
        self.stack.push(binding);
    }

    pub fn enter_constructor_declaration(&mut self, node: &ClassConstructorElement) {
        let binding = self.stack.last_mut().unwrap();
        for (i, param) in node.params.parameters.iter().enumerate() {
            binding.fields.push(Field {
                name: param.ident.value.clone(),
                index: i,
                size: 4,
            })
        }
    }

    pub fn exit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        let current = self.stack.pop().unwrap();
        let name = &node.ident.value;
        self.bindings.define(name.clone(), current);
    }
}
