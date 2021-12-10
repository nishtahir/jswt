use jswt_ast::*;
use jswt_common::SymbolTable;

use crate::{SemanticError, Symbol, Type};

pub struct GlobalResolver {
    pub symbols: SymbolTable<&'static str, Symbol>,
    pub errors: Vec<SemanticError>,
}

impl Default for GlobalResolver {
    fn default() -> Self {
        Self {
            symbols: SymbolTable::new(vec![]),
            errors: vec![],
        }
    }
}

impl GlobalResolver {
    pub fn resolve(&mut self, ast: &Ast) {
        self.visit_program(&ast.program);
    }
}

impl StatementVisitor for GlobalResolver {
    fn visit_program(&mut self, node: &Program) {
        // Push the global scope
        self.symbols.push_scope();
        self.visit_source_elements(&node.source_elements);
        // Don't pop it because the symbol table here is going to be
        // used in the next resolution pass
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
            StatementElement::Block(stmt) => self.visit_block_statement(stmt),
            StatementElement::Empty(_) => {}
            StatementElement::Return(_) => {}
            StatementElement::Variable(stmt) => self.visit_variable_statement(stmt),
            StatementElement::Expression(_) => {}
            StatementElement::If(_) => {}
            StatementElement::Iteration(_) => {}
        };
    }

    fn visit_block_statement(&mut self, _: &BlockStatement) {
        unreachable!()
    }

    fn visit_empty_statement(&mut self, _: &EmptyStatement) {
        unreachable!()
    }

    fn visit_if_statement(&mut self, _: &IfStatement) {
        unreachable!()
    }

    fn visit_iteration_statement(&mut self, _: &IterationStatement) {
        unreachable!()
    }

    fn visit_while_iteration_element(&mut self, _: &WhileIterationElement) {
        unreachable!()
    }

    fn visit_return_statement(&mut self, _: &ReturnStatement) {
        unreachable!()
    }

    fn visit_variable_statement(&mut self, _: &VariableStatement) {
        // add to global symbol table
    }

    fn visit_expression_statement(&mut self, _: &ExpressionStatement) {
        unreachable!()
    }

    fn visit_statement_list(&mut self, node: &StatementList) {
        for statement in &node.statements {
            self.visit_statement_element(statement);
        }
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        // add to global symbol table
        let ident = &node.ident;
        let name = ident.value;
        if self.symbols.lookup_current(&name).is_some() {
            let error = SemanticError::FunctionAlreadyDefined {
                name,
                span: ident.span.to_owned(),
            };
            self.errors.push(error);
        }
        self.symbols.define(name, Symbol::new(Type::Function, name));
    }

    fn visit_function_body(&mut self, _: &FunctionBody) {
        unreachable!()
    }
}
