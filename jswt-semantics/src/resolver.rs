use crate::error::SemanticError;
use crate::{convert::Convert, types::*};
use std::borrow::Borrow;

use jswt_ast::high_level::*;
use jswt_common::{PrimitiveType, SemanticSymbolTable, Symbol, Type, Typeable};

impl Default for Resolver {
    fn default() -> Self {
        Self {
            symbols: SemanticSymbolTable::default(),
            errors: Default::default(),
        }
    }
}

pub struct Resolver {
    pub symbols: SemanticSymbolTable,
    pub errors: Vec<SemanticError>,
}

impl Resolver {
    pub fn new(symbols: SemanticSymbolTable) -> Self {
        Self {
            symbols,
            errors: Default::default(),
        }
    }

    pub fn resolve(&mut self, ast: &mut Ast) {
        self.visit_program(&mut ast.program);
    }
}

impl MutStatementVisitor for Resolver {
    fn visit_program(&mut self, node: &mut Program) {
        // We expect that the symbol table has the
        // global scope on it
        debug_assert_eq!(self.symbols.depth(), 1);
        self.visit_source_elements(&mut node.source_elements);
        // Local scopes should have popped whatever scopes they created
        debug_assert_eq!(self.symbols.depth(), 1);
        self.symbols.pop_scope();
    }

    fn visit_source_elements(&mut self, node: &mut SourceElements) {
        for element in &mut node.source_elements {
            self.visit_source_element(element);
        }
    }

    fn visit_source_element(&mut self, node: &mut SourceElement) {
        match node {
            SourceElement::FunctionDeclaration(function) => {
                self.visit_function_declaration(function)
            }
            SourceElement::Statement(statement) => self.visit_statement_element(statement),
        }
    }

    fn visit_statement_element(&mut self, node: &mut StatementElement) {
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

    fn visit_block_statement(&mut self, node: &mut BlockStatement) {
        self.symbols.push_scope(node.span(), None);
        self.visit_statement_list(&mut node.statements);
        // TODO - before we pop the scope, determine if
        // there are any types we could not resolve
        self.symbols.pop_scope();
    }

    fn visit_empty_statement(&mut self, _: &mut EmptyStatement) {
        // No-op
    }

    fn visit_if_statement(&mut self, node: &mut IfStatement) {
        self.visit_single_expression(&mut node.condition);

        let condition_ty = &node.condition.defined_type();
        if !condition_ty.is_boolean() {
            self.errors.push(SemanticError::TypeAssignmentError {
                span: node.condition.span(),
                lhs: Type::Primitive(PrimitiveType::Boolean),
                rhs: condition_ty.clone(),
            });
        }
    }

    fn visit_iteration_statement(&mut self, node: &mut IterationStatement) {
        match node {
            IterationStatement::While(elem) => self.visit_while_iteration_element(elem),
        }
    }

    fn visit_while_iteration_element(&mut self, node: &mut WhileIterationElement) {
        self.visit_single_expression(&mut node.expression);

        let condition_ty = &node.expression.defined_type();
        if !condition_ty.is_boolean() {
            self.errors.push(SemanticError::TypeAssignmentError {
                span: node.expression.span(),
                lhs: Type::Primitive(PrimitiveType::Boolean),
                rhs: condition_ty.clone(),
            });
        }
    }

    fn visit_return_statement(&mut self, node: &mut ReturnStatement) {
        self.visit_single_expression(&mut node.expression);

        let value = &node.expression.defined_type();
        let expected = self.symbols.scope_return_type().unwrap_or(&Type::Void);
        if expected != value {
            self.errors.push(SemanticError::TypeMismatchError {
                span: node.expression.span(),
                expected: expected.clone(),
                actual: value.clone(),
            });
        }
    }

    fn visit_variable_statement(&mut self, node: &mut VariableStatement) {
        // Visit right hand side of expression
        self.visit_single_expression(&mut node.expression);

        let name = match &node.target {
            AssignableElement::Identifier(ident) => ident.value,
        };

        if self.symbols.lookup_current(&name.into()).is_some() {
            let error = SemanticError::VariableAlreadyDefined {
                name,
                span: node.target.span(),
            };
            self.errors.push(error);
        }

        // Try to determine defined type from the type annotation
        let lhs = node
            .type_annotation
            .as_ref()
            .map(Type::convert)
            .unwrap_or(Type::Unknown);

        // RHS should have been determined from by the visitor
        let rhs = node.expression.defined_type();
        let res = check_assignment(&lhs, &rhs);

        if res.is_unknown() {
            self.errors.push(SemanticError::TypeMismatchError {
                span: node.expression.span(),
                expected: lhs.clone(),
                actual: rhs.clone(),
            });
        }
        // Define the symbol with the correct type annotation in the symbol table
        self.symbols.define(name.into(), Symbol::new(res, name));
    }

    fn visit_expression_statement(&mut self, node: &mut ExpressionStatement) {
        self.visit_single_expression(&mut node.expression);
    }

    fn visit_statement_list(&mut self, node: &mut StatementList) {
        for statement in &mut node.statements {
            self.visit_statement_element(statement);
        }
    }

    fn visit_function_declaration(&mut self, node: &mut FunctionDeclarationElement) {
        // Push a new local scope for the function body
        self.symbols
            .push_scope(node.span(), node.returns.as_ref().map(Type::convert));
        // Add function parameters as variables in scope
        node.params.parameters.iter().for_each(|param| {
            // Resolve Type from Type Annotation
            let param_name = param.ident.value;
            let ty = Type::convert(&param.type_annotation);
            self.symbols
                .define(param_name.into(), Symbol::new(ty, param_name));
        });

        self.visit_function_body(&mut node.body);
        self.symbols.pop_scope();
    }

    fn visit_function_body(&mut self, node: &mut FunctionBody) {
        self.visit_source_elements(&mut node.source_elements);
    }
}

impl MutExpressionVisitor<()> for Resolver {
    fn visit_single_expression(&mut self, node: &mut SingleExpression) {
        match node {
            SingleExpression::Arguments(exp) => self.visit_argument_expression(exp),
            SingleExpression::Identifier(ident) => self.visit_identifier_expression(ident),
            SingleExpression::MemberIndex(exp) => self.visit_member_index(exp),
            SingleExpression::Assignment(exp) => self.visit_assignment_expression(exp),
            SingleExpression::Multiplicative(exp) => self.visit_binary_expression(exp),
            SingleExpression::Additive(exp) => self.visit_binary_expression(exp),
            SingleExpression::Equality(exp) => self.visit_binary_expression(exp),
            SingleExpression::Bitwise(exp) => self.visit_binary_expression(exp),
            SingleExpression::Relational(exp) => self.visit_binary_expression(exp),
            SingleExpression::Unary(exp) => self.visit_unary_expression(exp),
            SingleExpression::Literal(lit) => self.visit_literal(lit),
        }
    }

    fn visit_assignable_element(&mut self, node: &mut AssignableElement) {
        match node {
            AssignableElement::Identifier(ident) => {
                if let Some(sym) = self.symbols.lookup(ident.value.into()) {}
            }
        }
    }

    fn visit_member_index(&mut self, node: &mut MemberIndexExpression) {
        self.visit_single_expression(&mut *node.index);
        self.visit_single_expression(&mut *node.target);

        let index_type = &node.index.defined_type();
        if !&node.index.defined_type().is_real_number() {
            self.errors.push(SemanticError::TypeMismatchError {
                span: node.span(),
                expected: Type::Primitive(PrimitiveType::I32),
                actual: index_type.clone(),
            });
        }
    }

    fn visit_identifier_expression(&mut self, node: &mut IdentifierExpression) {
        let ident = &node.ident;
        let name = ident.value;

        // Update the node with the type defined in the symbol table if we
        // know what it is
        if let Some(sym) = self.symbols.lookup(name.into()) {
            node.ty = sym.ty.clone();
            return;
        };

        // We haven't encountered this symbol before
        // report error
        self.errors.push(SemanticError::VariableNotDefined {
            name,
            span: ident.span.to_owned(),
        });
    }

    fn visit_argument_expression(&mut self, node: &mut ArgumentsExpression) {
        let expression = node.ident.borrow();
        match expression {
            // Function calls but be followed by an identifier for now
            SingleExpression::Identifier(exp) => {
                // Check that the args are defined in this scope
                for arg in &mut node.arguments.arguments {
                    self.visit_single_expression(arg);
                }

                let name = exp.ident.value;
                if let Some(sym) = self.symbols.lookup(name.into()) {
                    match &sym.ty {
                        Type::Function(_, returns) => {
                            // The type of a function call node is the
                            // type that the function returns
                            node.ty = *returns.clone();
                        }
                        // Someone is trying to invoke something that isn't a function
                        _ => self.errors.push(SemanticError::NotAFunctionError {
                            span: node.span(),
                            name_span: exp.span(),
                        }),
                    }
                    return;
                }

                // We haven't encountered the symbol before
                // report diagnostic error
                self.errors.push(SemanticError::FunctionNotDefined {
                    span: node.span(),
                    name_span: exp.ident.span.to_owned(),
                });
            }

            // We don't have higher order functions yet. if the target of the call
            // is not an identifier report an error
            exp => self.errors.push(SemanticError::NotAFunctionError {
                span: node.span(),
                name_span: exp.span(),
            }),
        };
    }

    fn visit_unary_expression(&mut self, node: &mut UnaryExpression) {
        self.visit_single_expression(&mut node.expr);
        let exp = node.expr.defined_type();

        match node.op {
            UnaryOperator::Plus(_) | UnaryOperator::Minus(_) => {
                if !exp.is_signed_number() {
                    self.errors.push(SemanticError::TypeMismatchError {
                        span: node.expr.span(),
                        // TODO generify this and provide alternatives
                        expected: Type::i32(),
                        actual: exp.clone(),
                    });
                }
                node.ty = exp;
            }
            UnaryOperator::Not(_) => {
                if !exp.is_real_number() {
                    self.errors.push(SemanticError::TypeMismatchError {
                        span: node.expr.span(),
                        expected: Type::i32(),
                        actual: exp.clone(),
                    });
                }
                node.ty = exp;
            }
        }
    }

    fn visit_assignment_expression(&mut self, node: &mut BinaryExpression) {
        self.visit_single_expression(&mut node.left);
        self.visit_single_expression(&mut node.right);

        let lhs = &node.left.defined_type();
        let rhs = &node.right.defined_type();

        // Check that we can perform the assignment
        let res = check_assignment(lhs, rhs);
        if res.is_unknown() {
            self.errors.push(SemanticError::TypeAssignmentError {
                span: node.span(),
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            });
        }

        // If we don't know the type of the LHS update the symbol table
        if lhs.is_unknown() {
            if let SingleExpression::Identifier(exp) = node.left.borrow() {
                self.symbols
                    .update_type(&exp.ident.value.into(), rhs.clone())
            }
        }

        // The node itself is always void because you can't
        // operate on an assignment
        node.ty = Type::Void;
    }

    fn visit_binary_expression(&mut self, node: &mut BinaryExpression) {
        self.visit_single_expression(&mut node.left);
        self.visit_single_expression(&mut node.right);

        let lhs = &node.left.defined_type();
        let rhs = &node.right.defined_type();

        match node.op {
            BinaryOperator::Plus(_)
            | BinaryOperator::Minus(_)
            | BinaryOperator::Mult(_)
            | BinaryOperator::Div(_) => {
                let res = check_arithmetic(&lhs, &rhs);
                if res.is_unknown() {
                    self.errors.push(SemanticError::TypeBinaryOperationError {
                        span: node.span(),
                        op: node.op.clone(),
                        lhs: lhs.clone(),
                        rhs: rhs.clone(),
                    })
                }
                node.ty = res;
            }
            BinaryOperator::Equal(_)
            | BinaryOperator::NotEqual(_)
            | BinaryOperator::Greater(_)
            | BinaryOperator::GreaterEqual(_)
            | BinaryOperator::Less(_)
            | BinaryOperator::LessEqual(_) => {
                let res = check_comparison(&lhs, &rhs);
                if res.is_unknown() {
                    self.errors.push(SemanticError::TypeBinaryOperationError {
                        span: node.span(),
                        op: node.op.clone(),
                        lhs: lhs.clone(),
                        rhs: rhs.clone(),
                    })
                }
                node.ty = res;
            }
            BinaryOperator::And(_) | BinaryOperator::Or(_) => {
                let res = check_bitwise(&lhs, &rhs);
                if res.is_unknown() {
                    self.errors.push(SemanticError::TypeBinaryOperationError {
                        span: node.span(),
                        op: node.op.clone(),
                        lhs: lhs.clone(),
                        rhs: rhs.clone(),
                    });
                }
                node.ty = res;
            }
            BinaryOperator::Assign(_) => {
                let res = check_assignment(&lhs, &rhs);
                if res.is_unknown() {
                    self.errors.push(SemanticError::TypeBinaryOperationError {
                        span: node.span(),
                        op: node.op.clone(),
                        lhs: lhs.clone(),
                        rhs: rhs.clone(),
                    });
                }
                node.ty = Type::Void;
            }
        }
    }

    fn visit_literal(&mut self, literal: &mut Literal) {
        match literal {
            Literal::Array(a) => {
                let mut ty = Type::unknown();
                for e in a.elements.iter_mut() {
                    self.visit_single_expression(e);
                    // if we don't know the type at this point
                    // The type of the first element is our type
                    let defined_type = e.defined_type();
                    if ty.is_unknown() {
                        ty = defined_type;
                    } else if ty != defined_type {
                        ty = Type::unknown();
                        // error
                        break;
                    }
                }
                // Set the type of the array
                a.ty = Type::array(ty);
            }
            _ => {
                // Type literals have their types embedded
            }
        }
    }
}
