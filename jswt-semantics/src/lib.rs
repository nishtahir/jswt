mod error;
mod symbol;

pub use error::SemanticError;

use self::symbol::{Symbol, Type};
use std::borrow::Borrow;

use jswt_ast::*;
use jswt_common::SymbolTable;

impl Default for Resolver {
    fn default() -> Self {
        Self {
            symbols: SymbolTable::new(vec![]),
            errors: Default::default(),
        }
    }
}

pub struct Resolver {
    symbols: SymbolTable<&'static str, Symbol>,
    errors: Vec<SemanticError>,
}

impl Resolver {
    pub fn resolve(&mut self, ast: &Ast) {
        self.visit_program(&ast.program);
    }

    /// Get a reference to the resolver's errors.
    pub fn errors(&self) -> Vec<SemanticError> {
        self.errors.clone()
    }
}

impl Visitor for Resolver {
    fn visit_program(&mut self, node: &Program) {
        // Push global scope
        self.symbols.push_scope();
        // Add built-ins to global scope
        self.symbols
            .define("println", Symbol::new(Type::Function, "println"));

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
            StatementElement::Block(stmt) => self.visit_block_statement(stmt),
            StatementElement::Empty(stmt) => self.visit_empty_statement(stmt),
            StatementElement::Return(stmt) => self.visit_return_statement(stmt),
            StatementElement::Variable(stmt) => self.visit_variable_statement(stmt),
            StatementElement::Expression(stmt) => self.visit_expression_statement(stmt),
            StatementElement::If(stmt) => self.visit_if_statement(stmt),
            StatementElement::Iteration(_) => todo!(),
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

        // Add function parameters as variables in scope
        node.params.parameters.iter().for_each(|param| {
            // Resolve Type from Type Annotation
            let param_name = param.ident.value;
            self.symbols
                .define(param_name, Symbol::new(Type::Unknown, param_name));
        });

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
            SingleExpression::Arguments(exp) => self.visit_argument_expression(exp),
            SingleExpression::Literal(lit) => self.visit_literal(lit),
            SingleExpression::Multiplicative(exp) => self.visit_binary_expression(exp),
            SingleExpression::Additive(exp) => self.visit_binary_expression(exp),
            SingleExpression::Identifier(ident) => self.visit_identifier_expression(ident),
            SingleExpression::Equality(exp) => self.visit_binary_expression(exp),
            SingleExpression::Bitwise(exp) => self.visit_binary_expression(exp),
            SingleExpression::Relational(exp) => self.visit_binary_expression(exp),
        }
    }

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
            SingleExpression::Identifier(_) => {
                // We should check that the function is defined
                // but because we're doing a depth first pass we can't know if the function is
                // defined later.

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

        assert_eq!(expected, resolver.errors());
    }

    #[test]
    fn test_duplicate_function_declaration_generates_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function x() {} function x() {}");
        let ast = Parser::new(&mut tokenizer).parse();
        let mut resolver = Resolver::default();
        resolver.resolve(&ast);

        let expected = vec![SemanticError::FunctionAlreadyDefined {
            name: "x",
            span: Span::new("test.1", 25, 26),
        }];

        assert_eq!(expected, resolver.errors());
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

        assert_eq!(expected, resolver.errors());
    }
}
