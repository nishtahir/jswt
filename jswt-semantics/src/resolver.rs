// use crate::error::SemanticError;
// use std::borrow::{Borrow, Cow};

use jswt_ast::visit::*;
use jswt_ast::*;
use jswt_common::Spannable;
use jswt_symbols::{ClassBinding, Symbol};
// use jswt_symbols::{SymbolTable, TypeBinding};
// use jswt_types::{ObjectType, Type};

pub struct Resolver<'a> {
    pub symbols: &'a mut SymbolTable,
    pub errors: Vec<SemanticError>,
    binding_context: Option<&'a ClassBinding>,
}

impl<'a> Resolver<'a> {
    pub fn new(symbols: &'a mut SymbolTable) -> Self {
        Self {
            symbols,
            errors: vec![],
            binding_context: None,
        }
    }

    pub fn resolve(&mut self, ast: &Ast) {
        self.visit_program(&ast.program);
        debug_assert!(self.symbols.depth() == 1);
    }
}

impl<'a> Visitor for Resolver<'a> {
    fn visit_block_statement(&mut self, node: &BlockStatement) {
        self.symbols.push_scope();
        walk_block_statement(self, node);
        self.symbols.pop_scope();
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        walk_variable_statement(self, node);

        let name = match &node.target {
            AssignableElement::Identifier(ident) => &ident.value,
        };

        if self.symbols.lookup_current(name).is_some() {
            let error = SemanticError::VariableAlreadyDefined {
                name: name.clone(),
                span: node.target.span(),
            };
            self.errors.push(error);
        }

        let declared_type = node
            .type_annotation
            .as_ref()
            .map(|t| Symbol::ty(t.ty.clone()))
            .unwrap_or(Symbol::Unknown);

        self.symbols.define(name.clone(), declared_type);
    }
}

// impl<'a> StatementVisitor<()> for Resolver<'a> {

//     fn visit_if_statement(&mut self, node: &IfStatement) {
//         match node.condition {
//             SingleExpression::Equality(_)
//             | SingleExpression::Relational(_)
//             | SingleExpression::Literal(Literal::Boolean(_)) => {
//                 // Valid boolean expression
//             }
//             SingleExpression::Arguments(_) => {
//                 // TODO type check the function
//             }
//             SingleExpression::Identifier(_) => {} // TODO Type check the symbol
//             _ => {
//                 let error = SemanticError::TypeError {
//                     span: node.condition.span(),
//                     offending_token: node.condition.span(),
//                     expected: "Boolean",
//                 };
//                 self.errors.push(error);
//             }
//         }
//     }

//     fn visit_expression_statement(&mut self, node: &ExpressionStatement) {
//         self.visit_single_expression(&node.expression);
//     }

//     fn visit_statement_list(&mut self, node: &StatementList) {
//         for statement in &node.statements {
//             self.visit_statement_element(statement);
//         }
//     }

//     fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
//         // Push a new local scope for the function body
//         // Scope definition should have been defined during the global pass
//         self.symbols.push_scope(node.span());

//         // Add function parameters as variables in scope
//         node.params.parameters.iter().for_each(|param| {
//             // Resolve Type from Type Annotation
//             let param_name = &param.ident.value;
//             self.symbols.define(
//                 param_name.clone(),
//                 TypeBinding {
//                     ty: param.type_annotation.ty.clone(),
//                 },
//             );
//         });

//         self.visit_block_statement(&node.body);
//         self.symbols.pop_scope();
//     }

//     fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) {
//         self.binding_context = Some(node.ident.value.clone());
//         self.visit_class_body(&node.body);
//         self.binding_context = None;
//     }

//     fn visit_class_body(&mut self, node: &ClassBody) {
//         for class_element in &node.class_elements {
//             match class_element {
//                 ClassElement::Constructor(elem) => self.visit_class_constructor_declaration(elem),
//                 ClassElement::Method(elem) => self.visit_class_method_declaration(elem),
//                 ClassElement::Field(elem) => self.visit_class_field_declaration(elem),
//             }
//         }
//     }

//     fn visit_class_constructor_declaration(&mut self, node: &ClassConstructorElement) {
//         self.symbols.push_scope(node.span());
//         // Add function parameters as variables in scope
//         node.params.parameters.iter().for_each(|param| {
//             // Resolve Type from Type Annotation
//             let param_name = &param.ident.value;
//             self.symbols.define(
//                 param_name.clone(),
//                 TypeBinding {
//                     ty: param.type_annotation.ty.clone(),
//                 },
//             );
//         });

//         self.visit_block_statement(&node.body);
//         self.symbols.pop_scope();
//     }

//     fn visit_class_method_declaration(&mut self, node: &ClassMethodElement) {
//         self.symbols.push_scope(node.span());
//         // Add function parameters as variables in scope
//         node.params.parameters.iter().for_each(|param| {
//             // Resolve Type from Type Annotation
//             let param_name = &param.ident.value;
//             self.symbols.define(
//                 param_name.clone(),
//                 TypeBinding {
//                     ty: param.type_annotation.ty.clone(),
//                 },
//             );
//         });

//         self.visit_block_statement(&node.body);
//         self.symbols.pop_scope();
//     }

//     fn visit_class_field_declaration(&mut self, node: &ClassFieldElement) {}
// }

// impl<'a> ExpressionVisitor<Type> for Resolver<'a> {
//     fn visit_single_expression(&mut self, node: &SingleExpression) -> Type {
//         match node {
//             SingleExpression::Arguments(exp) => self.visit_argument_expression(exp),
//             SingleExpression::Literal(lit) => self.visit_literal(lit),
//             SingleExpression::Multiplicative(exp) => self.visit_binary_expression(exp),
//             SingleExpression::Additive(exp) => self.visit_binary_expression(exp),
//             SingleExpression::Identifier(ident) => self.visit_identifier_expression(ident),
//             SingleExpression::Equality(exp) => self.visit_binary_expression(exp),
//             SingleExpression::Bitwise(exp) => self.visit_binary_expression(exp),
//             SingleExpression::Relational(exp) => self.visit_binary_expression(exp),
//             SingleExpression::Assignment(exp) => self.visit_assignment_expression(exp),
//             SingleExpression::Unary(exp) => self.visit_unary_expression(exp),
//             SingleExpression::MemberIndex(exp) => self.visit_member_index(exp),
//             SingleExpression::This(exp) => self.visit_this_expression(exp),
//             SingleExpression::MemberDot(exp) => self.visit_member_dot(exp),
//             SingleExpression::New(exp) => self.visit_new(exp),
//         }
//     }

//     fn visit_assignable_element(&mut self, node: &AssignableElement) -> Type {
//         match node {
//             AssignableElement::Identifier(ident) => self
//                 .symbols
//                 .lookup(ident.value.clone())
//                 .map(|s| s.as_type())
//                 .unwrap_or(Type::Unknown),
//         }
//     }

//     fn visit_member_index(&mut self, node: &MemberIndexExpression) -> Type {
//         let target_ty = self.visit_single_expression(&node.target);
//         if let Type::Object(ObjectType::Reference(exp)) = target_ty {
//             let binding = self.symbols.lookup(exp);
//             let index = self.visit_single_expression(&node.index);
//             // TODO Check for get method. return type is the return type of get
//         }

//         Type::Unknown
//     }

//     fn visit_identifier_expression(&mut self, node: &IdentifierExpression) -> Type {
//         let ident = &node.ident;
//         let name = &ident.value;
//         let symbol = self.symbols.lookup(name.clone());
//         match symbol {
//             Some(_) => {}
//             None => {
//                 let error = SemanticError::VariableNotDefined {
//                     name: name.clone(),
//                     span: ident.span.to_owned(),
//                 };
//                 self.errors.push(error);
//             }
//         };

//         symbol.map(|s| s.as_type()).unwrap_or(Type::Unknown)
//     }

//     fn visit_argument_expression(&mut self, node: &ArgumentsExpression) -> Type {
//         let expression = node.ident.borrow();
//         let mut ty = Type::Unknown;
//         match expression {
//             // Function calls must be an identifier for now
//             SingleExpression::Identifier(exp) => {
//                 let name = &exp.ident.value;

//                 if let Some(sym) = self.symbols.lookup(name.clone()) {
//                     if !sym.is_function() && !sym.is_class() {
//                         self.errors.push(SemanticError::NotAFunctionError {
//                             span: node.span(),
//                             name_span: exp.span(),
//                         });
//                     }
//                     ty = sym.as_type()
//                 } else {
//                     self.errors.push(SemanticError::FunctionNotDefined {
//                         span: node.span(),
//                         name_span: exp.ident.span.to_owned(),
//                     });
//                 }

//                 // Check that the args are defined in this scope
//                 for arg in &node.arguments.arguments {
//                     self.visit_single_expression(arg);
//                 }
//             }
//             _ => self.errors.push(SemanticError::NotAFunctionError {
//                 span: node.span(),
//                 name_span: expression.span(),
//             }),
//         };
//         return ty;
//     }

//     fn visit_unary_expression(&mut self, node: &UnaryExpression) -> Type {
//         self.visit_single_expression(&node.expr)
//     }

//     fn visit_assignment_expression(&mut self, node: &BinaryExpression) -> Type {
//         // We should assert the the element on the left is assignable
//         self.visit_single_expression(&node.left);
//         self.visit_single_expression(&node.right);

//         // Assignments are untyped
//         Type::Void
//     }

//     fn visit_binary_expression(&mut self, node: &BinaryExpression) -> Type {
//         self.visit_single_expression(&node.left);
//         self.visit_single_expression(&node.right);

//         // TODO
//         Type::Unknown
//     }

//     fn visit_this_expression(&mut self, _: &ThisExpression) -> Type {
//         // This is the type of the current class
//         self.symbols
//             .lookup(self.binding_context.as_ref().unwrap().clone())
//             .unwrap()
//             .as_type()
//     }

//     fn visit_literal(&mut self, lit: &Literal) -> Type {
//         lit.as_type()
//     }

//     fn visit_member_dot(&mut self, node: &MemberDotExpression) -> Type {
//         self.visit_single_expression(&node.target);
//         // Check the index/expression
//         self.visit_single_expression(&node.expression)
//     }

//     fn visit_new(&mut self, node: &NewExpression) -> Type {
//         self.visit_single_expression(&node.expression)
//     }
// }

use crate::{SemanticError, SymbolTable};
