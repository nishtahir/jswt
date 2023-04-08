use crate::SemanticError;
use core::panic;
use jswt_ast::*;
use jswt_common::{Spannable, Type};
use jswt_symbols::{
    BindingsTable, ClassBinding, SemanticEnvironment, Symbol, SymbolTable, TypesTable, Variable,
};

#[derive(Debug)]
pub struct LocalSemanticResolver<'a> {
    environment: &'a mut SemanticEnvironment,
    errors: Vec<SemanticError>,
    class: Option<ClassBinding>,
}

impl<'a> LocalSemanticResolver<'a> {
    pub fn new(environment: &'a mut SemanticEnvironment) -> Self {
        Self {
            environment,
            errors: vec![],
            class: None,
        }
    }

    pub fn resolve(&mut self, ast: &Ast) {
        debug_assert!(self.environment.symbol_scope_depth() == 1);
        self.visit_program(&ast.program);
        debug_assert!(self.environment.symbol_scope_depth() == 1);
    }

    pub fn errors(&mut self) -> &mut Vec<SemanticError> {
        &mut self.errors
    }
}

impl<'a> Visitor for LocalSemanticResolver<'a> {
    fn visit_import_declaration_element(&mut self, _node: &ImportDeclarationElement) {
        // self.symbols_mut().merge_scope(node.span());
    }

    fn visit_variable_declaration_element(&mut self, node: &VariableDeclarationElement) {
        ast::walk_variable_declaration_element(self, node);
        let name = &node.name.value;
        // Redefition errors are handled during the global pass

        let is_mutable = match node.modifier {
            VariableModifier::Let(_) => true,
            VariableModifier::Const(_) => false,
        };

        let initialized_type = self.environment.get_type(&node.expression.span()).clone();
        if let Some(annotation) = node.type_annotation.as_ref() {
            let declared_type = annotation.ty.clone();
            if declared_type != initialized_type {
                panic!(
                    "Type mismatch: {:?} != {:?}",
                    declared_type, initialized_type
                );
                // let error = SemanticError::TypeMismatch {
                //     lhs,
                //     rhs,
                //     span: node.span(),
                // };
                // self.errors.push(error);
            }
        }

        self.environment.insert_symbol(
            name,
            Symbol::Variable(Variable {
                ty: initialized_type.clone(),
                declaration: node.span(),
                initialization: Some(node.expression.span()),
                name: name.clone(),
                is_mutable,
            }),
        );

        self.environment
            .insert_owned_type(node.span(), initialized_type);
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        ast::walk_variable_statement(self, node);
        // Check if we're in a local scope
        if self.environment.symbol_scope_depth() > 1 {
            let name = match &node.target {
                AssignableElement::Identifier(ident) => &ident.value,
            };

            // Error if a variable name collides with an already defined variable
            if let Some(_) = self.environment.get_symbol(name) {
                let error = SemanticError::VariableAlreadyDefined {
                    name: name.clone(),
                    span: node.target.span(),
                };
                self.errors.push(error);
            }

            // Figure out if the variable is mutable or not
            let is_mutable = match node.modifier {
                VariableModifier::Let(_) => true,
                VariableModifier::Const(_) => false,
            };

            let initialized_type = self.environment.get_type(&node.expression.span()).clone();
            if let Some(annotation) = node.type_annotation.as_ref() {
                let declared_type = annotation.ty.clone();
                if declared_type != initialized_type {
                    panic!(
                        "Type mismatch: {:?} != {:?}",
                        declared_type, initialized_type
                    );
                    // let error = SemanticError::TypeMismatch {
                    //     lhs,
                    //     rhs,
                    //     span: node.span(),
                    // };
                    // self.errors.push(error);
                }
            }

            // Add the variable to the symbol table
            self.environment.insert_symbol(
                name,
                Symbol::Variable(Variable {
                    declaration: node.span(),
                    initialization: Some(node.expression.span()),
                    name: name.clone(),
                    ty: initialized_type.clone(),
                    is_mutable,
                }),
            );
            // Add the type to the types table
            self.environment
                .insert_owned_type(node.span(), initialized_type.clone());
        }
    }

    fn visit_identifier_expression(&mut self, node: &IdentifierExpression) {
        ast::walk_identifier_expression(self, node);
        let ident = &node.ident;
        let name = &ident.value;
        match self.environment.get_symbol(name) {
            Some(symbol) => {
                let ty = match symbol {
                    Symbol::Variable(var) => var.ty.clone(),
                    Symbol::Function(func) => func.ret.clone(),
                    Symbol::Class => Type::Binding(name.clone()),
                };
                self.environment.insert_owned_type(node.span(), ty);
            }
            None => {
                let error = SemanticError::SymbolNotDefined {
                    name: name.clone(),
                    span: ident.span(),
                };
                self.errors.push(error);
            }
        }
    }

    fn visit_function_declaration_element(&mut self, node: &FunctionDeclarationElement) {
        // Redefinition errors are handled by the GlobalSemanticResolver
        // during clobal symbol resolution
        self.environment.push_symbol_scope(node.body.span());
        ast::walk_function_declaration_element(self, node);
        self.environment.pop_symbol_scope();
    }

    fn visit_class_declaration_element(&mut self, node: &ClassDeclarationElement) {
        self.class = self.environment.get_binding(&node.ident.value).cloned();
        ast::walk_class_declaration_element(self, node);
        self.class = None;
    }

    fn visit_class_body(&mut self, node: &ClassBody) {
        self.environment.push_symbol_scope(node.span());
        ast::walk_class_body(self, node);
        self.environment.pop_symbol_scope();
    }

    fn visit_class_constructor_element(&mut self, node: &ClassConstructorElement) {
        self.environment.push_symbol_scope(node.body.span());
        ast::walk_class_constructor_element(self, node);
        self.environment.pop_symbol_scope();
    }

    fn visit_class_method_element(&mut self, node: &ClassMethodElement) {
        self.environment.push_symbol_scope(node.body.span());
        ast::walk_class_method_element(self, node);
        self.environment.pop_symbol_scope();
    }

    fn visit_new_expression(&mut self, node: &NewExpression) {
        match &*node.expression {
            SingleExpression::Arguments(args) => {
                // The inner expression should be an identifier expression
                match &*args.ident {
                    SingleExpression::Identifier(exp) => {
                        let name = &exp.ident.value;

                        // We should know what type this identifier is at this point
                        match self.environment.get_symbol(name) {
                            Some(Symbol::Class) => {
                                // The new expression is valid
                                self.environment
                                    .insert_owned_type(node.span(), Type::Binding(name.clone()));

                                // TODO - Check that the constructor exists
                                // TODO - walk the arguments
                            }
                            Some(_) => {
                                // They are calling new on something that isn't a class
                                let error = SemanticError::InvalidNewCall { span: node.span() };
                                self.errors.push(error);
                            }
                            None => {
                                // We haven't seen this class before
                                let error = SemanticError::ClassNotDefined {
                                    // TODO - this should be name
                                    ident: name.clone(),
                                    span: exp.span(),
                                };
                                self.errors.push(error);
                            }
                        }
                    }
                    _ => {
                        // The inner expression is not an identifier expression
                        let error = SemanticError::InvalidNewCall { span: node.span() };
                        self.errors.push(error);
                    }
                }

                for arg in &args.arguments.arguments {
                    self.visit_single_expression(arg);
                }
            }
            _ => {
                let error = SemanticError::InvalidNewCall { span: node.span() };
                self.errors.push(error);
            }
        }
    }

    fn visit_this_expression(&mut self, node: &ThisExpression) {
        // Here we assume that we're not in class declaration context.
        // That's handled by class::ClassDeclarationGlobalContext. So
        // we can just emit an error here.
        if let None = self.class {
            self.errors
                .push(SemanticError::ThisOutsideClass { span: node.span() })
        }
    }

    fn visit_arguments_expression(&mut self, node: &ArgumentsExpression) {
        match &*node.ident {
            SingleExpression::Arguments(exp) => {
                // This is a nested function call a()()
                self.visit_arguments_expression(exp);
                // check the type of args and validate the params
            }
            SingleExpression::Identifier(exp) => {
                // This is a direct function call a()
                let name = &exp.ident.value;
                // Look up the identifier in the symbol table
                match &self.environment.get_symbol(name) {
                    Some(Symbol::Function(func)) => {
                        // Check the number of arguments
                        // if func.params.len() != node.args.len() {
                        //     self.errors.push(SemanticError::InvalidNumberOfArguments {
                        //         name: name.clone(),
                        //         span: node.span(),
                        //     });
                        // }
                        // // Check the types of the arguments
                        // for (i, arg) in node.args.iter().enumerate() {
                        //     self.visit_single_expression(arg);
                        //     let arg_ty = self.environment.get_type(&arg.span());
                        //     if arg_ty != func.params[i].ty {
                        //         self.errors.push(SemanticError::InvalidArgumentType {
                        //             name: name.clone(),
                        //             span: arg.span(),
                        //         });
                        //     }
                        // }

                        // Set the type of the expression to the return type of the function
                        self.environment
                            .insert_owned_type(node.span(), func.ret.clone());
                    }
                    _ => {
                        panic!("Invalid function call")
                    }
                }
            }
            SingleExpression::MemberDot(exp) => {
                // This is a function call on a member dot expression a.b()

                // Visit the left hand side of the member
                // expression to derive the type
                let lhs = &exp.target;
                self.visit_single_expression(&lhs);

                match &*exp.expression {
                    // the right side of the member expression should always be an identifier
                    SingleExpression::Identifier(ident_exp) => {
                        let name = &ident_exp.ident.value;
                        // Lookup the inferred type. The right hand side of the member expression
                        // should be a method of the type of the left hand side.
                        match self.environment.get_type(&lhs.span()) {
                            Type::Binding(class_name) => {
                                //
                                let class = match self.environment.get_symbol(class_name) {
                                    Some(Symbol::Class) => {
                                        self.environment.get_binding(class_name).unwrap()
                                    }
                                    _ => {
                                        panic!("Invalid member expression");
                                    }
                                };

                                // Get the method from the class
                                let method = match class.method(name) {
                                    Some(method) => method,
                                    None => {
                                        panic!("Invalid member expression - method not found {} on class {}", name, class.name);
                                    }
                                };

                                // TODO - check the arguments

                                // The inferred type of the member expression is the
                                // return type of the method
                                let method_ty = method.ret.clone();
                                self.environment.insert_owned_type(node.span(), method_ty);
                            }
                            _ => {
                                panic!("Invalid member expression - unable to infer type of left hand side");
                            }
                        }
                    }
                    _ => {
                        panic!(
                            "Invalid member expression - right hand side should be an identifier"
                        );
                    }
                }
            }
            _ => {
                panic!("Invalid function call");
                // let error = SemanticError::InvalidFunctionCall {
                //     span: node.span(),
                // };
                // self.errors.push(error);
            }
        }

        // Visit and resolve the arguments
        for arg in &node.arguments.arguments {
            self.visit_single_expression(arg);
        }
    }
    fn visit_formal_parameter_arg(&mut self, node: &FormalParameterArg) {
        ast::walk_formal_parameter_arg(self, node);
        let name = &node.ident.value;
        let ty = node.type_annotation.ty.clone();

        // Check if the variable has already been defined
        match self.environment.get_symbol(name) {
            Some(_) => {
                let error = SemanticError::VariableAlreadyDefined {
                    name: name.clone(),
                    span: node.span(),
                };
                self.errors.push(error);
            }
            None => {
                self.environment.insert_symbol(
                    name,
                    Symbol::Variable(Variable {
                        declaration: node.span(),
                        initialization: None,
                        name: name.clone(),
                        ty: ty.clone(),
                        is_mutable: true,
                    }),
                );
                self.environment.insert_owned_type(node.span(), ty)
            }
        }
    }

    fn visit_member_dot_expression(&mut self, node: &MemberDotExpression) {
        // TODO - check that the member expression is valid
    }

    fn visit_literal(&mut self, node: &Literal) {
        ast::walk_literal(self, node);
        let ty = match node {
            Literal::Array(_) => todo!(),
            Literal::String(_) => Type::STRING,
            Literal::Integer(_) => Type::I32,
            Literal::Float(_) => Type::F32,
            Literal::Boolean(_) => Type::BOOLEAN,
        };
        self.environment.insert_owned_type(node.span(), ty);
    }

    fn visit_assignment_expression(&mut self, node: &AssignmentExpression) {
        ast::walk_assignment_expression(self, node);
        let lhs = &node.target;
        let rhs = &node.expression;

        // Visit the left hand side of the assignment
        self.visit_single_expression(lhs);

        // Visit the right hand side of the assignment
        self.visit_single_expression(rhs);

        // Check if the target of the assignment is mutable
        match &**lhs {
            SingleExpression::Identifier(ident) => {
                let name = &ident.ident.value;
                match self.environment.get_symbol(name) {
                    Some(Symbol::Variable(var)) => {
                        if !var.is_mutable {
                            panic!(
                                "Cannot assign to immutable variable {} at {}",
                                name,
                                node.span().lexme()
                            );
                            // let error = SemanticError::AssignmentToImmutableVariable {
                            //     name: name.clone(),
                            //     span: node.span(),
                            // };
                            // self.errors.push(error);
                        }
                    }
                    _ => {
                        panic!(
                            "Cannot assign to non variable {} at {}",
                            name,
                            node.span().lexme()
                        );
                        // let error = SemanticError::AssignmentToNonVariable {
                        //     name: name.clone(),
                        //     span: node.span(),
                        // };
                        // self.errors.push(error);
                    }
                }
            }
            _ => {
                // TODO - this should be assignable elements

                // let error = SemanticError::AssignmentToNonVariable {
                //     name: "unknown".to_string(),
                //     span: node.span(),
                // };
                // self.errors.push(error);
            }
        }

        // Get the inferred type of the left hand side
        let lhs_ty = self.environment.get_type(&node.target.span());

        // Get the inferred type of the right hand side
        let _rhs_ty = self.environment.get_type(&node.expression.span());

        // Check if the types are compatible
        // if !lhs_ty.is_compatible(&rhs_ty) {
        //     let error = SemanticError::TypeError {
        //         span: node.span(),
        //         offending_token: node.span(),
        //         expected: lhs_ty.to_string(),
        //     };
        //     self.errors.push(error);
        // }

        // The inferred type of the assignment expression is the type of the
        // left hand side
        self.environment
            .insert_owned_type(node.span(), lhs_ty.clone());
    }
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

//     fn visit_argument_expression(&mut self, node: &ArgumentsExpression) -> Type {
//         let expression = node.ident.borrow();
//         let mut ty = Type::Unknown;
//         match expression {
//             // Function calls must be an identifier for now
//             SingleExpression::Identifier(exp) => {
//                 let name = &exp.ident.value;

//                 if let Some(sym) = self.environment.get_symbol(name.clone()) {
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

// }

#[cfg(test)]
mod test {
    use crate::{GlobalSemanticResolver, LocalSemanticResolver};
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_symbols::SemanticEnvironment;
    use jswt_tokenizer::Tokenizer;
    use std::default::Default;

    #[test]
    fn test_function_parameter_redefinition_error_in_constructors() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_function_parameter_redefinition_error_in_constructors",
            r"
        class Test { 
            constructor(a: i32, a: i32) {
                
            }
        }

        function main() {
            let test = new Test(1, 2);
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut environment = SemanticEnvironment::default();
        let mut global = GlobalSemanticResolver::new(&mut environment);
        global.resolve(&ast);
        let mut local = LocalSemanticResolver::new(&mut environment);
        local.resolve(&ast);

        assert_debug_snapshot!(local);
    }

    #[test]
    fn test_context_errors_on_this_outside_class() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_context_errors_on_this_outside_class",
            r"
            function foo() {
                this;
            }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut environment = SemanticEnvironment::default();
        let mut global = GlobalSemanticResolver::new(&mut environment);
        global.resolve(&ast);
        let mut local = LocalSemanticResolver::new(&mut environment);
        local.resolve(&ast);
        assert_debug_snapshot!(local);
    }

    #[test]
    fn test_local_context_resolves_local_variables() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_local_context_resolves_local_variables",
            r"
        const PI = 3.14;

        function test(a: i32, b: i32) {
            let x = 99;
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut environment = SemanticEnvironment::default();
        let mut global = GlobalSemanticResolver::new(&mut environment);
        global.resolve(&ast);
        let mut local = LocalSemanticResolver::new(&mut environment);
        local.resolve(&ast);
        assert_debug_snapshot!(local);
        assert!(local.errors.is_empty(), "{:?}", local.errors);
    }

    #[test]
    fn test_local_context_resolves_duplicate_variable_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_local_context_resolves_duplicate_variable_error",
            r"
        function test(a: i32, b: i32) {
            let a = 99;
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut environment = SemanticEnvironment::default();
        let mut global = GlobalSemanticResolver::new(&mut environment);
        global.resolve(&ast);
        let mut local = LocalSemanticResolver::new(&mut environment);
        local.resolve(&ast);
        assert_debug_snapshot!(local);
    }

    #[test]
    fn test_local_context_error_on_undefined_variable() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_local_context_error_on_undefined_variable",
            r"
        function test() {
            x;
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut environment = SemanticEnvironment::default();
        let mut global = GlobalSemanticResolver::new(&mut environment);
        global.resolve(&ast);
        let mut local = LocalSemanticResolver::new(&mut environment);
        local.resolve(&ast);
        assert_debug_snapshot!(local);
    }

    #[test]
    fn test_function_parameters_are_in_scope() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_function_parameters_are_in_scope",
            r"
        function test(x: i32, y: i32) {
            x;
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut environment = SemanticEnvironment::default();
        let mut global = GlobalSemanticResolver::new(&mut environment);
        global.resolve(&ast);
        let mut local = LocalSemanticResolver::new(&mut environment);
        local.resolve(&ast);
        assert_debug_snapshot!(local);
    }

    #[test]
    fn test_function_parameter_redefinition() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_function_parameter_redefinition",
            r"
        function test(x: i32, x: i32) {
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut environment = SemanticEnvironment::default();
        let mut global = GlobalSemanticResolver::new(&mut environment);
        global.resolve(&ast);
        let mut local = LocalSemanticResolver::new(&mut environment);
        local.resolve(&ast);
        assert_debug_snapshot!(local);
    }
}
