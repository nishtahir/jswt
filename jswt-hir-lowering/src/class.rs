use std::borrow::Cow;

use jswt_ast::{transform::TransformVisitor, *};
use jswt_common::Spannable;
use jswt_symbols::{BindingsTable, ClassBinding};
use jswt_synthetic::*;

pub struct HirClassLoweringContext<'a> {
    class_name: Cow<'static, str>,
    class_binding: &'a ClassBinding,
}

impl<'a> HirClassLoweringContext<'a> {
    pub fn new(class: &'a ClassDeclarationElement, bindings: &'a BindingsTable) -> Self {
        let class_name = class.ident.value.clone();
        let class_binding = bindings.lookup(&class_name).expect(&format!(
            "class binding '{}' missing from bindings table",
            class_name
        ));
        Self {
            class_name,
            class_binding,
        }
    }
}

/// Minimum tree walker
impl<'a> TransformVisitor for HirClassLoweringContext<'a> {
    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) -> SourceElements {
        transform::walk_class_declaration(self, node)
    }

    fn visit_class_body(&mut self, node: &ClassBody) -> SourceElements {
        transform::walk_class_body(self, node)
    }

    fn visit_class_constructor_declaration(
        &mut self,
        node: &ClassConstructorElement,
    ) -> SourceElements {
        // Size to allocate for the class instance
        let class_size = self.class_binding.size();

        // Walk the constructor body
        let mut body = self.visit_block_statement(&node.body);

        // Allocate the class instance
        let this = variable_decl_stmt("this".into(), malloc(class_size));
        let returns = return_stmt(ident_exp("this".into()));

        // Insert the class allocation at the beginning of the constructor body
        body.statements.insert(0, this);
        // Insert the return statement at the end of the constructor body
        body.statements.push(returns);

        // Build the synthetic function declaration representing the constructor
        let synthetic_constructor_name = format!("{}#constructor", self.class_name);
        SourceElements {
            span: node.span(),
            source_elements: vec![SourceElement::FunctionDeclaration(
                FunctionDeclarationElement {
                    span: node.span(),
                    decorators: FunctionDecorators {
                        annotations: vec![],
                        export: false,
                    },
                    ident: Identifier {
                        span: node.span(),
                        value: synthetic_constructor_name.into(),
                    },
                    params: node.params.clone(),
                    // Class constructors always return a pointer
                    returns: Some(TypeAnnotation {
                        span: node.span(),
                        ty: type_ptr(),
                    }),
                    body: BlockStatement {
                        span: node.body.span(),
                        statements: body,
                    },
                },
            )],
        }
    }

    fn visit_class_method_declaration(&mut self, node: &ClassMethodElement) -> SourceElements {
        let method_name = &node.ident.value;
        // Generate synthetic function name ClassName#methodName
        let ident = Identifier {
            span: node.ident.span(),
            value: format!("{}#{}", self.class_name, method_name).into(),
        };

        // Generate function parameters
        // The first parameter is the 'this' instance
        let mut params = node.params.clone();
        params.parameters.insert(
            0,
            FormalParameterArg {
                span: node.span(),
                ident: Identifier {
                    span: node.span(),
                    value: "this".into(),
                },
                type_annotation: TypeAnnotation {
                    span: node.span(),
                    ty: type_ptr(),
                },
            },
        );

        // Generate the function body
        let block = BlockStatement {
            span: node.span(),
            statements: self.visit_block_statement(&node.body),
        };

        SourceElements {
            span: node.span(),
            source_elements: vec![SourceElement::FunctionDeclaration(
                FunctionDeclarationElement {
                    span: node.span(),
                    decorators: FunctionDecorators {
                        annotations: node.annotations.clone(),
                        export: false,
                    },
                    ident,
                    params,
                    returns: node.returns.clone(),
                    body: block,
                },
            )],
        }
    }

    fn visit_assignment_expression(&mut self, node: &BinaryExpression) -> SingleExpression {
        if let SingleExpression::MemberDot(dot) = &*node.left {
            if let SingleExpression::This(_) = &*dot.target {
                // Handle this.field = value assignments inside of class methods
                // The member dot target should always be an identifier
                let lhs = dot.expression.as_identifier().unwrap();
                let rhs = &*node.right;
                let field_name = &lhs.ident.value;

                let field = self.class_binding.field(field_name).expect(&format!(
                    "missing field '{}' in class '{}'.", // This should never happen
                    field_name, self.class_name
                ));
                // Assignment is
                return i32_store("this", field.index as i32 * 4, rhs.clone());
            }
        }
        transform::walk_assignment_expression(self, node)
    }

    fn visit_class_field_declaration(&mut self, node: &ClassFieldElement) -> SourceElements {
        // Class fields are not desugared
        SourceElements {
            span: node.span(),
            source_elements: vec![],
        }
    }

    fn visit_argument_expression(&mut self, node: &ArgumentsExpression) -> SingleExpression {
        // Resolve the expression arguments
        let argument_list = &node.arguments;
        let mut arguments = vec![];
        for arg in &argument_list.arguments {
            arguments.push(self.visit_single_expression(arg));
        }

        // If the ident portion of the arguments is a memberdot expression
        if let SingleExpression::MemberDot(member_dot) = &*node.ident {
            // and the target is a this expression. We want to desurar the call
            // into a call to the class member function.
            let target = &*member_dot.target;
            if let SingleExpression::This(_) = target {
                // The rhs should be an identifier targetting the member function
                let expr = member_dot
                    .expression
                    .as_identifier()
                    .expect("expected identifier expression on member dot expression");

                let method_name = &expr.ident.value;
                let method = self.class_binding.method(&method_name).expect(&format!(
                    "missing method '{}' in class '{}'.", // This should never happen
                    method_name, self.class_name
                ));

                // Generate a function that the class member function with
                // this as the first argument and the node args as the rest.
                let function_name = format!("{}#{}", self.class_name, method_name);
                // insert this as the first argument of the arguments list
                arguments.insert(0, ident_exp("this".into()));
                return function_call(
                    function_name.into(),
                    arguments,
                    method.signature.returns.clone(),
                );
                // Call the function associated with the field
            }
        }
        transform::walk_argument_expression(self, node)
    }

    fn visit_member_dot(&mut self, node: &MemberDotExpression) -> SingleExpression {
        if let SingleExpression::This(_) = &*node.target {
            // Handle this.field access inside of class methods
            // Determine if the target is an identifier or arguments expression
            match &*node.expression {
                SingleExpression::Identifier(ident_exp) => {
                    let field_name = &ident_exp.ident.value;
                    let field = self.class_binding.field(field_name).expect(&format!(
                        "missing field '{}' in class '{}'.", // This should never happen
                        field_name, self.class_name
                    ));
                    // Load the value associated with the field
                    return i32_load("this", field.index as i32 * 4);
                }
                _ => {
                    // We only care about lowering this expressions at this stage
                    // so we can just continue to walk the tree as is.
                }
            };
        }
        transform::walk_member_dot(self, node)
    }
}

#[cfg(test)]
mod test {

    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_semantics::GlobalSemanticResolver;
    use jswt_symbols::ScopedSymbolTable;
    use jswt_tokenizer::Tokenizer;

    use super::*;
    use crate::HirLoweringContext;

    #[test]
    fn test_class_declaration_lowers_class_with_empty_constructor() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_class_declaration_lowers_class_with_empty_constructor",
            r"
        class Array {
            len: i32;
            capacity: i32;

            constructor() {
                this.len = 0;
                this.capacity = 0;
            }
        }
    ",
        );

        let ast = Parser::new(&mut tokenizer).parse();

        let mut symbol_table = ScopedSymbolTable::default();
        let mut bindings_table = BindingsTable::default();
        let mut global_resolver =
            GlobalSemanticResolver::new(&mut bindings_table, &mut symbol_table);
        global_resolver.resolve(&ast);

        // No errors in global resolver
        assert!(global_resolver.errors().len() == 0);

        let mut lowering = HirLoweringContext::new(&mut bindings_table, &symbol_table);
        let lowered = lowering.lower(&ast);
        assert_debug_snapshot!(lowered);
    }

    #[test]
    fn test_class_declaration_ignores_class_fields() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_class_declaration_ignores_class_fields",
            r"
        class Array {
            len: i32;
            capacity: i32;
        }
    ",
        );

        let ast = Parser::new(&mut tokenizer).parse();

        let mut symbol_table = ScopedSymbolTable::default();
        let mut bindings_table = BindingsTable::default();
        let mut global_resolver =
            GlobalSemanticResolver::new(&mut bindings_table, &mut symbol_table);
        global_resolver.resolve(&ast);

        // No errors in global resolver
        assert!(global_resolver.errors().len() == 0);

        let mut lowering = HirLoweringContext::new(&bindings_table, &symbol_table);
        let lowered = lowering.lower(&ast);
        assert_debug_snapshot!(lowered);
    }

    #[test]
    fn test_class_declaration_lowers_methods_into_functions() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_class_declaration_lowers_methods_into_functions",
            r"
        class Array {
            len(): i32 {
                return 0;
            }
        }
    ",
        );

        let ast = Parser::new(&mut tokenizer).parse();

        let mut symbol_table = ScopedSymbolTable::default();
        let mut bindings_table = BindingsTable::default();
        let mut global_resolver =
            GlobalSemanticResolver::new(&mut bindings_table, &mut symbol_table);
        global_resolver.resolve(&ast);

        // No errors in global resolver
        assert!(global_resolver.errors().len() == 0);

        let mut lowering = HirLoweringContext::new(&bindings_table, &symbol_table);
        let lowered = lowering.lower(&ast);
        assert_debug_snapshot!(lowered);
    }

    #[test]
    fn test_class_declaration_lowers_class_this_reference() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_class_declaration_lowers_class_this_reference",
            r"
        class Array {
            len(): i32 {
                return 0;
            }

            len2(): i32 {
                return this.len();
            }
        }
    ",
        );

        let ast = Parser::new(&mut tokenizer).parse();

        let mut symbol_table = ScopedSymbolTable::default();
        let mut bindings_table = BindingsTable::default();
        let mut global_resolver =
            GlobalSemanticResolver::new(&mut bindings_table, &mut symbol_table);
        global_resolver.resolve(&ast);

        // No errors in global resolver
        assert!(global_resolver.errors().len() == 0);

        let mut lowering = HirLoweringContext::new(&bindings_table, &symbol_table);
        let lowered = lowering.lower(&ast);
        assert_debug_snapshot!(lowered);
    }
}
