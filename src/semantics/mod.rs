mod symbol;
use std::panic;

use self::symbol::{Symbol, SymbolTable, Type};
use crate::ast::visitor::Visitor;
use crate::ast::{program::*, Ast};

#[derive(Default)]
pub struct Resolver {
    symbols: SymbolTable,
}

impl Resolver {
    pub fn resolve(&mut self, ast: &Ast) {
        self.visit_program(&ast.program);
    }
}

impl Visitor for Resolver {
    fn visit_program(&mut self, node: &Program) {
        // Push global scope
        self.symbols.push_scope();
        self.visit_source_elements(&node.source_elements);
        self.symbols.pop_scope();
    }

    fn visit_source_elements(&mut self, node: &SourceElements) {
        for element in &node.source_elements {
            self.visit_source_element(element);
        }
    }

    fn visit_source_element(&mut self, node: &SourceElement) {
        match node {
            SourceElement::FunctionDeclaration(function) => {
                self.visit_function_declaration(function)
            }
            SourceElement::Statement(statement) => self.visit_statement_element(statement),
        }
    }

    fn visit_statement_element(&mut self, node: &StatementElement) {
        match node {
            StatementElement::Block(block) => self.visit_block_statement(block),
            StatementElement::Empty(empty) => self.visit_empty_statement(empty),
            StatementElement::Return(ret) => self.visit_return_statement(ret),
            StatementElement::Variable(variable) => self.visit_variable_statement(variable),
        }
    }

    fn visit_block_statement(&mut self, node: &BlockStatement) {
        self.symbols.push_scope();
        self.visit_statement_list(&node.statements);
        // TODO - before we pop the scope, determine if
        // there are any types we could not resolve
        self.symbols.pop_scope();
    }

    fn visit_empty_statement(&mut self, _: &EmptyStatement) {
        // No-op
    }

    fn visit_return_statement(&mut self, _: &ReturnStatement) {
        todo!();
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        let name = match &node.target {
            AssignableElement::Identifier(ident) => ident.value,
        };
        self.visit_single_expression(&node.expression);
        if self.symbols.lookup_current(name).is_some() {
            panic!("var with name '{}' is already defined in same scope", name);
        }
        self.symbols.define(Symbol::new(Type::Unknown, name));
    }

    fn visit_statement_list(&mut self, node: &StatementList) {
        for statement in &node.statements {
            self.visit_statement_element(statement);
        }
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        let name = node.ident.value;
        if self.symbols.lookup_current(name).is_some() {
            panic!(
                "function with name '{}' is already defined in same scope",
                name
            );
        }
        self.symbols.define(Symbol::new(Type::Function, name));
        self.visit_function_body(&node.body);
    }

    fn visit_function_body(&mut self, node: &FunctionBody) {
        self.symbols.push_scope();
        self.visit_source_elements(&node.source_elements);
        self.symbols.pop_scope();
    }

    fn visit_assignable_element(&mut self, _: &AssignableElement) {
        // No-op
    }

    fn visit_single_expression(&mut self, node: &SingleExpression) {
        match node {
            SingleExpression::Literal(lit) => self.visit_literal(lit),
            SingleExpression::Multiplicative(_) => todo!(),
            SingleExpression::Additive(_) => todo!(),
        }
    }

    fn visit_literal(&mut self, _: &Literal) {
        // No-op
    }
}
