mod class;
mod functions;
mod identifier;
mod new;
mod this;
mod variables;

use self::{
    class::ClassLocalContext, functions::FunctionsLocalContext,
    identifier::IdentifierExpressionLocalContext, new::NewExpressionLocalContext,
    this::ThisExpressionLocalContext, variables::VariableDeclarationLocalContext,
};
use crate::SemanticError;
use jswt_ast::{visit::*, *};
use jswt_common::Spannable;
use jswt_symbols::{BindingsTable, ScopedSymbolTable};

#[derive(Debug)]
pub struct LocalSemanticResolver<'a> {
    pub symbols: &'a mut ScopedSymbolTable,
    pub bindings: &'a mut BindingsTable,
    pub errors: Vec<SemanticError>,
}

impl<'a> LocalSemanticResolver<'a> {
    pub fn new(bindings: &'a mut BindingsTable, symbols: &'a mut ScopedSymbolTable) -> Self {
        Self {
            symbols,
            bindings,
            errors: vec![],
        }
    }

    pub fn resolve(&mut self, ast: &Ast) {
        debug_assert!(self.symbols.depth() == 1);
        self.visit_program(&ast.program);
        debug_assert!(self.symbols.depth() == 1);
    }

    pub fn errors(&mut self) -> &mut Vec<SemanticError> {
        &mut self.errors
    }
}

impl<'a> Visitor for LocalSemanticResolver<'a> {
    fn visit_block_statement(&mut self, node: &BlockStatement) {
        self.symbols.push_scope(node.span());
        walk_block_statement(self, node);
        self.symbols.pop_scope();
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        let mut ctx = VariableDeclarationLocalContext::new(self);
        ctx.visit_variable_statement(node);
        visit::walk_variable_statement(self, node);
    }

    fn visit_identifier_expression(&mut self, node: &IdentifierExpression) {
        let mut ctx = IdentifierExpressionLocalContext::new(self);
        ctx.visit_identifier_expression(node);
        walk_identifier_expression(self, node);
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        let mut ctx = FunctionsLocalContext::new(self);
        ctx.visit_function_declaration(node);
        walk_function_declaration(self, node);
    }

    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        let mut ctx = ClassLocalContext::new(self);
        ctx.visit_class_declaration(&node);
        walk_class_declaration(self, node);
    }

    fn visit_new(&mut self, node: &NewExpression) {
        let mut ctx = NewExpressionLocalContext::new(self);
        ctx.visit_new(node);
    }

    fn visit_this_expression(&mut self, node: &ThisExpression) {
        let mut ctx = ThisExpressionLocalContext::new(self);
        ctx.visit_this_expression(node);
    }

    // fn visit_new(&mut self, node: &NewExpression) {
    //     println!("{:#?}", node);
    //     // let mut ctx = NewExpressionLocalContext::new(self);
    //     // ctx.visit_new(node);
    // }

    // fn visit_if_statement(&mut self, node: &IfStatement) {
    //     match node.condition {
    //         SingleExpression::Equality(_)
    //         | SingleExpression::Relational(_)
    //         | SingleExpression::Literal(Literal::Boolean(_)) => {
    //             // Valid boolean expression
    //         }
    //         SingleExpression::Arguments(_) => {
    //             // TODO type check the function
    //         }
    //         SingleExpression::Identifier(_) => {} // TODO Type check the symbol
    //         _ => {
    //             let error = SemanticError::TypeError {
    //                 span: node.condition.span(),
    //                 offending_token: node.condition.span(),
    //                 expected: "Boolean",
    //             };
    //             self.errors.push(error);
    //         }
    //     }
    //     walk_if_statement(self, node);
    // }

    // fn visit_identifier_expression(&mut self, node: &IdentifierExpression) {
    //     let ident = &node.ident;
    //     let name = &ident.value;
    //     if self.in_this_expr {
    //         // check bindings table for the field
    //         let binding = self
    //             .bindings
    //             .lookup(&self.binding_context.as_ref().unwrap())
    //             .unwrap();

    //         if let None = binding.field(name) {
    //             let error = SemanticError::PropertyNotDefined {
    //                 name: name.clone(),
    //                 span: ident.span.to_owned(),
    //             };
    //             self.errors.push(error);
    //         }
    //     } else {
    //         let symbol = self.symbols.lookup(name);
    //         if let None = symbol {
    //             let error = SemanticError::VariableNotDefined {
    //                 name: name.clone(),
    //                 span: ident.span.to_owned(),
    //             };
    //             self.errors.push(error);
    //         };
    //     }
    //     walk_identifier_expression(self, node);
    // }

    // fn visit_member_dot(&mut self, node: &MemberDotExpression) {
    //     if let SingleExpression::This(_) = &*node.target {
    //         self.in_this_expr = true;
    //     }
    //     walk_member_dot(self, node);
    //     self.in_this_expr = false;
    // }
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

// impl<'a> ExpressionVisitor<Type> for Resolver<'a> {

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

// }
