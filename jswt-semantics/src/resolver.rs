use crate::error::SemanticError;
use std::{borrow::Cow, collections::BTreeMap};

use jswt_ast::{visit::Visitor, *};
use jswt_common::{Span, Spannable};
use jswt_symbols::{SymbolTable, TypeBinding};
use jswt_types::Type;

#[derive(Debug)]
pub struct Resolver<'a> {
    pub symbols: &'a mut SymbolTable,
    pub errors: Vec<SemanticError>,
    binding_context: Option<Cow<'static, str>>,
    types: BTreeMap<Span, Type>,
}

impl<'a> Resolver<'a> {
    pub fn new(symbols: &'a mut SymbolTable) -> Self {
        Self {
            symbols,
            errors: vec![],
            binding_context: None,
            types: BTreeMap::new(),
        }
    }

    pub fn resolve(&mut self, ast: &Ast) {
        self.visit_program(&ast.program);
    }

    fn type_of(&self, span: &Span) -> &Type {
        self.types.get(span).unwrap_or(&Type::Unknown)
    }
}

impl<'a> Visitor for Resolver<'a> {
    fn visit_program(&mut self, node: &Program) {
        self.symbols.push_global_scope();
        visit::walk_program(self, node);
        // Local scopes should have popped whatever scopes they created
        debug_assert!(self.symbols.depth() == 1);
        self.symbols.pop_scope();
    }

    fn visit_block_statement(&mut self, node: &BlockStatement) {
        self.symbols.push_scope(node.span());
        visit::walk_block_statement(self, node);
        // TODO - before we pop the scope, determine if
        // there are any types we could not resolve
        self.symbols.pop_scope();
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
        visit::walk_if_statement(self, node);
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        visit::walk_variable_statement(self, node);

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

        let derived_type = self.type_of(&node.expression.span()).clone();
        // Type check against derived type
        let declared_type = node
            .type_annotation
            .as_ref()
            .map(|t| t.ty.clone())
            .unwrap_or(Type::Unknown);

        self.symbols
            .define(name.clone(), TypeBinding { ty: derived_type });
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

        visit::walk_function_declaration(self, node);
        self.symbols.pop_scope();
    }

    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        self.binding_context = Some(node.ident.value.clone());
        visit::walk_class_declaration(self, node);
        self.binding_context = None;
    }

    fn visit_class_constructor_declaration(&mut self, node: &ClassConstructorElement) {
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
        visit::walk_class_constructor_declaration(self, node);
        self.symbols.pop_scope();
    }

    fn visit_class_method_declaration(&mut self, node: &ClassMethodElement) {
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
        visit::walk_class_method_declaration(self, node);
        self.symbols.pop_scope();
    }

    fn visit_single_expression(&mut self, node: &SingleExpression) {
        visit::walk_single_expression(self, node);

        let ty = self.type_of(&node.span()).clone();
        self.types.insert(node.span(), ty);
    }

    fn visit_assignable_element(&mut self, node: &AssignableElement) {
        visit::walk_assignable_element(self, node);
    }

    fn visit_member_dot(&mut self, node: &MemberDotExpression) {
        visit::walk_member_dot(self, node);
    }

    fn visit_member_index(&mut self, node: &MemberIndexExpression) {
        visit::walk_member_index(self, node);
    }

    fn visit_new(&mut self, node: &NewExpression) {
        visit::walk_new(self, node);
    }

    fn visit_identifier_expression(&mut self, node: &IdentifierExpression) {
        visit::walk_identifier_expression(self, node);
        let name = &node.ident.value;
        let ty = self.symbols.lookup(name.clone()).map(|sym| sym.as_type());
        let ty = match ty {
            Some(inner) => inner,
            None => {
                let error = SemanticError::VariableNotDefined {
                    name: name.clone(),
                    span: node.ident.span(),
                };
                self.errors.push(error);
                Type::Unknown
            }
        };
        self.types.insert(node.span(), ty);
    }

    fn visit_argument_expression(&mut self, node: &ArgumentsExpression) {
        visit::walk_argument_expression(self, node);

        let expression = &*node.ident;
        // function calls are idents for now
        let name = expression.as_identifier().unwrap().ident.value.clone();
        let ty = self
            .symbols
            .lookup(name.clone())
            .map(|sym| sym.as_type())
            .unwrap_or(Type::Unknown);

        let ty = match ty {
            // If it's a function the derived type of the expression is the return type
            Type::Function(binding) => *binding.returns.clone(),
            // If it's a class the binding is the derived type
            Type::Class(binding) => Type::Class(binding),
            _ => {
                self.errors.push(SemanticError::NotAFunctionError {
                    span: node.span(),
                    name_span: expression.span(),
                });
                Type::Unknown
            }
        };

        self.types.insert(node.span(), ty);
    }

    fn visit_unary_expression(&mut self, node: &UnaryExpression) {
        visit::walk_unary_expression(self, node);
    }

    fn visit_assignment_expression(&mut self, node: &BinaryExpression) {
        visit::walk_assignment_expression(self, node);
        let lhs = self.type_of(&node.left.span()).clone();
        let rhs = self.type_of(&node.right.span()).clone();

        if !lhs.is_assignable_to(&rhs) {
            panic!("cannot assign {:?} to {:?}", lhs, rhs);
        }

        self.types.insert(node.span(), lhs.coerce(&rhs));
    }

    fn visit_binary_expression(&mut self, node: &BinaryExpression) {
        visit::walk_binary_expression(self, node);
        let lhs = self.type_of(&node.left.span()).clone();
        let rhs = self.type_of(&node.right.span()).clone();

        match node.op {
            BinaryOperator::Plus(_)
            | BinaryOperator::Minus(_)
            | BinaryOperator::Mult(_)
            | BinaryOperator::Div(_) => {
                if lhs.is_same_as(&rhs) {
                    let error = SemanticError::TypeError {
                        span: node.span(),
                        offending_token: node.span(),
                        expected: "",
                    };
                    self.errors.push(error);
                };
            }
            BinaryOperator::Equal(_)
            | BinaryOperator::NotEqual(_)
            | BinaryOperator::Greater(_)
            | BinaryOperator::GreaterEqual(_)
            | BinaryOperator::Less(_)
            | BinaryOperator::LessEqual(_) => {
                if lhs.is_assignable_to(&rhs) {
                    let error = SemanticError::TypeError {
                        span: node.span(),
                        offending_token: node.span(),
                        expected: "",
                    };
                    self.errors.push(error);
                };
            }
            BinaryOperator::And(_) => {},
            BinaryOperator::Or(_) => {},
            BinaryOperator::Assign(_) => {}
        }

        self.types.insert(node.span(), lhs);
    }

    fn visit_this_expression(&mut self, node: &ThisExpression) {
        visit::walk_this_expression(self, node);
    }

    fn visit_literal(&mut self, node: &Literal) {
        self.types.insert(node.span(), node.as_type());
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::{globals::GlobalResolver, resolver::Resolver};

    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_symbols::SymbolTable;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_global_resolver_resolves_class_binding() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test.1",
            r"
        let global = 55;

        class Array {
            a: i32;
            b: i32;

            constructor(a: i32, b: f32) {}
            method() {}
        }

        function test() {
            let x = 99 + value();
        }
 
        function doSomething(): Array { }

        function value(): i32 {
            return 9;
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = SymbolTable::default();
        let mut resolver = GlobalResolver::new(&mut symbols);
        resolver.resolve(&ast);

        let mut resolver = Resolver::new(&mut symbols);
        resolver.resolve(&ast);

        assert_debug_snapshot!(resolver);
    }
}
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

//     fn visit_member_dot(&mut self, node: &MemberDotExpression) -> Type {
//         self.visit_single_expression(&node.target);
//         // Check the index/expression
//         self.visit_single_expression(&node.expression)
//     }

//     fn visit_new(&mut self, node: &NewExpression) -> Type {
//         self.visit_single_expression(&node.expression)
//     }
// }
