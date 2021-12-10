use crate::error::SemanticError;
use crate::symbol::{Symbol, Type};
use std::borrow::Borrow;
use std::collections::HashMap;

use jswt_ast::*;
use jswt_common::SymbolTable;

impl Default for Resolver {
    fn default() -> Self {
        Self {
            symbols: SymbolTable::new(vec![HashMap::new()]),
            errors: Default::default(),
        }
    }
}

pub struct Resolver {
    pub symbols: SymbolTable<&'static str, Symbol>,
    pub errors: Vec<SemanticError>,
}

impl Resolver {
    pub fn new(symbols: SymbolTable<&'static str, Symbol>) -> Self {
        Self {
            symbols,
            errors: Default::default(),
        }
    }

    pub fn resolve(&mut self, ast: &Ast) {
        self.visit_program(&ast.program);
    }
}

impl StatementVisitor for Resolver {
    fn visit_program(&mut self, node: &Program) {
        // We expect that the symbol table has the
        // global scope on it
        debug_assert!(self.symbols.depth() == 1);

        self.visit_source_elements(&node.source_elements);

        // Local scopes should have popped whatever scopes they created
        debug_assert!(self.symbols.depth() == 1);
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
            StatementElement::Block(stmt) => self.visit_block_statement(stmt),
            StatementElement::Empty(stmt) => self.visit_empty_statement(stmt),
            StatementElement::Return(stmt) => self.visit_return_statement(stmt),
            StatementElement::Variable(stmt) => self.visit_variable_statement(stmt),
            StatementElement::Expression(stmt) => self.visit_expression_statement(stmt),
            StatementElement::If(stmt) => self.visit_if_statement(stmt),
            StatementElement::Iteration(stmt) => self.visit_iteration_statement(stmt),
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

    fn visit_if_statement(&mut self, node: &IfStatement) {
        match node.condition {
            SingleExpression::Equality(_)
            | SingleExpression::Relational(_)
            | SingleExpression::Literal(Literal::Boolean(_)) => {
                // Valid boolean expression
            }
            SingleExpression::Arguments(_) => todo!(), // TODO type check the function
            SingleExpression::Identifier(_) => {}      // TODO Type check the symbol
            _ => {
                let error = SemanticError::TypeError {
                    span: node.condition.span(),
                    offending_token: node.condition.span(),
                    expected: "Boolean",
                };
                self.errors.push(error);
            }
        }
    }

    fn visit_iteration_statement(&mut self, node: &IterationStatement) {
        match node {
            IterationStatement::While(elem) => self.visit_while_iteration_element(elem),
        }
    }

    fn visit_while_iteration_element(&mut self, _node: &WhileIterationElement) {
        // Check that exp is boolean
    }

    fn visit_return_statement(&mut self, node: &ReturnStatement) {
        self.visit_single_expression(&node.expression);
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        let name = match &node.target {
            AssignableElement::Identifier(ident) => ident.value,
        };
        self.visit_single_expression(&node.expression);
        if self.symbols.lookup_current(&name).is_some() {
            let error = SemanticError::VariableAlreadyDefined {
                name,
                span: node.target.span(),
            };
            self.errors.push(error);
        }
        self.symbols.define(name, Symbol::new(Type::Unknown, name));
    }

    fn visit_expression_statement(&mut self, node: &ExpressionStatement) {
        self.visit_single_expression(&node.expression)
    }

    fn visit_statement_list(&mut self, node: &StatementList) {
        for statement in &node.statements {
            self.visit_statement_element(statement);
        }
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        // Push a new local scope for the function body
        self.symbols.push_scope();
        // Add function parameters as variables in scope
        node.params.parameters.iter().for_each(|param| {
            // Resolve Type from Type Annotation
            let param_name = param.ident.value;
            self.symbols
                .define(param_name, Symbol::new(Type::Unknown, param_name));
        });

        self.visit_function_body(&node.body);
        self.symbols.pop_scope();
    }

    fn visit_function_body(&mut self, node: &FunctionBody) {
        self.visit_source_elements(&node.source_elements);
    }
}

impl ExpressionVisitor<()> for Resolver {
    fn visit_assignment_expression(&mut self, _node: &BinaryExpression) {
        // We should assert the the element on the left is assignable
    }

    fn visit_assignable_element(&mut self, _: &AssignableElement) {
        // No-op
    }

    fn visit_single_expression(&mut self, node: &SingleExpression) {
        match node {
            SingleExpression::Arguments(exp) => self.visit_argument_expression(exp),
            SingleExpression::Literal(lit) => self.visit_literal(lit),
            SingleExpression::Multiplicative(exp) => self.visit_binary_expression(exp),
            SingleExpression::Additive(exp) => self.visit_binary_expression(exp),
            SingleExpression::Identifier(ident) => self.visit_identifier_expression(ident),
            SingleExpression::Equality(exp) => self.visit_binary_expression(exp),
            SingleExpression::Bitwise(exp) => self.visit_binary_expression(exp),
            SingleExpression::Relational(exp) => self.visit_binary_expression(exp),
            SingleExpression::Assignment(exp) => self.visit_assignment_expression(exp),
            SingleExpression::Unary(exp) => self.visit_unary_expression(exp),
        }
    }

    fn visit_unary_expression(&mut self, _node: &UnaryExpression) {}

    fn visit_binary_expression(&mut self, node: &BinaryExpression) {
        self.visit_single_expression(&node.left);
        self.visit_single_expression(&node.right);
    }

    fn visit_identifier_expression(&mut self, node: &IdentifierExpression) {
        let ident = &node.ident;
        let name = ident.value;
        if self.symbols.lookup(&name).is_none() {
            let error = SemanticError::VariableNotDefined {
                name,
                span: ident.span.to_owned(),
            };
            self.errors.push(error);
        }
    }

    fn visit_argument_expression(&mut self, node: &ArgumentsExpression) {
        let expression = node.ident.borrow();
        match expression {
            // Function calls but be followed by an identifier for now
            SingleExpression::Identifier(exp) => {
                let name = exp.ident.value;

                if let Some(sym) = self.symbols.lookup(&name) {
                    if !sym.ty.is_function() {
                        self.errors.push(SemanticError::NotAFunctionError {
                            span: node.span(),
                            name_span: exp.span(),
                        });
                    }
                } else {
                    self.errors.push(SemanticError::FunctionNotDefined {
                        span: node.span(),
                        name_span: exp.ident.span.to_owned(),
                    });
                    // not defined error
                }

                // Check that the args are defined in this scope
                for arg in &node.arguments.arguments {
                    self.visit_single_expression(arg);
                }
            }
            exp => self.errors.push(SemanticError::NotAFunctionError {
                span: node.span(),
                name_span: exp.span(),
            }),
        }
    }

    fn visit_literal(&mut self, _: &Literal) {
        // No-op
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use jswt_assert::assert_eq;
    use jswt_parser::Parser;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_duplicate_variable_declaration_generates_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let x = 0; let x = 1;");
        let ast = Parser::new(&mut tokenizer).parse();
        let mut resolver = Resolver::default();
        resolver.resolve(&ast);

        let expected = vec![SemanticError::VariableAlreadyDefined {
            name: "x",
            span: Span::new("test.1", 15, 16),
        }];

        assert_eq!(expected, resolver.errors);
    }

    #[test]
    fn test_variable_not_defined_generates_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test() { return x; }");
        let ast = Parser::new(&mut tokenizer).parse();
        let mut resolver = Resolver::default();
        resolver.resolve(&ast);

        let expected = vec![SemanticError::VariableNotDefined {
            name: "x",
            span: Span::new("test.1", 25, 26),
        }];

        assert_eq!(expected, resolver.errors);
    }
}
