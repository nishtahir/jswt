use crate::bindings::BindingsTable;
use crate::symbols::TypeBinding;
use crate::{error::SemanticError, symbols::SymbolTable};
use std::borrow::Borrow;

use jswt_ast::*;
use jswt_common::Spannable;
use jswt_types::Type;

pub struct Resolver<'a> {
    pub symbols: &'a mut SymbolTable,
    pub bindings: &'a mut BindingsTable,
    pub errors: Vec<SemanticError>,
}

impl<'a> Resolver<'a> {
    pub fn new(symbols: &'a mut SymbolTable, bindings: &'a mut BindingsTable) -> Self {
        Self {
            symbols,
            bindings,
            errors: vec![],
        }
    }

    pub fn resolve(&mut self, ast: &Ast) {
        self.visit_program(&ast.program);
    }
}

impl<'a> ProgramVisitor<()> for Resolver<'a> {
    fn visit_program(&mut self, node: &Program) {
        self.symbols.push_global_scope();

        for file in &node.files {
            self.visit_file(file)
        }

        // Local scopes should have popped whatever scopes they created
        debug_assert!(self.symbols.depth() == 1);
        self.symbols.pop_scope();
    }

    fn visit_file(&mut self, node: &File) {
        self.visit_source_elements(&node.source_elements);
    }

    fn visit_source_elements(&mut self, node: &SourceElements) {
        for element in &node.source_elements {
            self.visit_source_element(element);
        }
    }

    fn visit_source_element(&mut self, node: &SourceElement) {
        match node {
            SourceElement::FunctionDeclaration(elem) => self.visit_function_declaration(elem),
            SourceElement::Statement(elem) => self.visit_statement_element(elem),
            SourceElement::ClassDeclaration(elem) => self.visit_class_declaration(elem),
        }
    }
}

impl<'a> StatementVisitor<()> for Resolver<'a> {
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
        self.symbols.push_scope(node.span());
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
            SingleExpression::Arguments(_) => {
                // TODO type check the function
            }
            SingleExpression::Identifier(_) => {} // TODO Type check the symbol
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
            AssignableElement::Identifier(ident) => &ident.value,
        };
        self.visit_single_expression(&node.expression);
        if self.symbols.lookup_current(&name).is_some() {
            let error = SemanticError::VariableAlreadyDefined {
                name: name.clone(),
                span: node.target.span(),
            };
            self.errors.push(error);
        }

        let type_binding = match &node.type_annotation {
            Some(type_annotation) => TypeBinding {
                ty: type_annotation.ty.clone(),
            },
            None => TypeBinding { ty: Type::Unknown },
        };

        self.symbols.define(name.clone(), type_binding);
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
        // Scope definition should have been defined during the global pass
        self.symbols.push_scope(node.span());

        // Add function parameters as variables in scope
        node.params.parameters.iter().for_each(|param| {
            // Resolve Type from Type Annotation
            let param_name = &param.ident.value;
            self.symbols.define(
                param_name.clone(),
                TypeBinding {
                    ty: param.type_annotation.ty.clone(),
                },
            );
        });

        self.visit_function_body(&node.body);
        self.symbols.pop_scope();
    }

    fn visit_function_body(&mut self, node: &FunctionBody) {
        self.visit_source_elements(&node.source_elements);
    }

    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) {}

    fn visit_class_body(&mut self, node: &ClassBody) {}

    fn visit_class_constructor_declaration(&mut self, node: &ClassConstructorElement) {}

    fn visit_class_method_declaration(&mut self, node: &ClassMethodElement) {}
}

impl<'a> ExpressionVisitor<()> for Resolver<'a> {
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
            SingleExpression::MemberIndex(exp) => self.visit_member_index(exp),
            SingleExpression::This(exp) => self.visit_this_expression(exp),
            SingleExpression::MemberDot(exp) => self.visit_member_dot(exp),
        }
    }

    fn visit_assignable_element(&mut self, _: &AssignableElement) {
        // No-op
    }

    fn visit_member_index(&mut self, _: &MemberIndexExpression) {}

    fn visit_identifier_expression(&mut self, node: &IdentifierExpression) {
        let ident = &node.ident;
        let name = &ident.value;
        if self.symbols.lookup(name.clone()).is_none() {
            let error = SemanticError::VariableNotDefined {
                name: name.clone(),
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
                let name = &exp.ident.value;

                if let Some(sym) = self.symbols.lookup(name.clone()) {
                    if !sym.is_function() {
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

    fn visit_unary_expression(&mut self, _node: &UnaryExpression) {
        // TODO - Type check values
    }

    fn visit_assignment_expression(&mut self, _node: &BinaryExpression) {
        // We should assert the the element on the left is assignable
    }

    fn visit_binary_expression(&mut self, node: &BinaryExpression) {
        self.visit_single_expression(&node.left);
        self.visit_single_expression(&node.right);
    }

    fn visit_this_expression(&mut self, node: &ThisExpression) {}

    fn visit_literal(&mut self, _: &Literal) {
        // No-op
    }

    fn visit_member_dot(&mut self, node: &MemberDotExpression) {
        todo!()
    }
}
