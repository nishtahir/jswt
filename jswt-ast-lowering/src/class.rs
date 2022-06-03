use crate::{gen::*, AstLowering};

use jswt_ast::{transform::TransformVisitor, *};
use jswt_common::Spannable;

// At a high level the goal here is to remove class declarations from the AST by rewriting them
// into a series of functions
//
// ```
// class Array {
//   constructor() {}
//   get(index: i32): i32 {}
// }
// ```
//
// should be rewriten as
//
// ```
// function Array#constructor(): i32 {}
// function Array#get(this: i32, index: i32): i32 {}
// ```
impl<'a> AstLowering<'a> {
    pub(crate) fn enter_class_declaration(&mut self, node: &ClassDeclarationElement) {
        let name = node.ident.value.clone();
        self.binding_context = Some(name);
    }

    pub(crate) fn exit_class_declaration(&mut self) {
        self.binding_context = None;
    }

    pub(crate) fn enter_class_method(&mut self, node: &ClassMethodElement) -> SourceElement {
        let method_name = &node.ident.value;
        let binding = self
            .bindings
            .lookup(&self.binding_context.as_ref().unwrap())
            .unwrap();

        // Generate synthetic function name
        // The function name is the ClassName#methodName
        let ident = Identifier {
            span: node.ident.span(),
            value: format!("{}#{}", binding.name, method_name).into(),
        };

        // Generate function parameters
        // The first parameter should be the pointer to the instance binding
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

        let block = BlockStatement {
            span: node.span(),
            statements: self.visit_block_statement(&node.body),
        };

        SourceElement::FunctionDeclaration(FunctionDeclarationElement {
            span: node.span(),
            decorators: FunctionDecorators {
                annotations: node.annotations.clone(),
                export: false,
            },
            ident,
            params,
            returns: node.returns.clone(),
            body: block,
        })
    }

    pub(crate) fn enter_class_constructor(
        &mut self,
        node: &ClassConstructorElement,
    ) -> SourceElement {
        let binding = self
            .bindings
            .lookup(&self.binding_context.as_ref().unwrap())
            .unwrap();

        let class_name = binding.name.clone();
        let class_size = binding.size();
        let mut body = self.visit_block_statement(&node.body);

        let this = variable_decl_stmt("this".into(), malloc(class_size));
        let returns = return_stmt(ident_exp("this".into()));

        // Allocate the struct before anything else
        body.statements.insert(0, this);
        // finally return the struct
        body.statements.push(returns);

        SourceElement::FunctionDeclaration(FunctionDeclarationElement {
            span: node.span(),
            decorators: FunctionDecorators {
                annotations: vec![],
                export: false,
            },
            ident: Identifier {
                span: node.span(),
                value: format!("{}#constructor", class_name).into(),
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
        })
    }

    pub(crate) fn class_this_field_assignment(
        &mut self,
        target: &IdentifierExpression,
        value: &SingleExpression,
    ) -> SingleExpression {
        let binding = self
            .bindings
            .lookup(&self.binding_context.as_ref().unwrap())
            .unwrap();

        // lhs is this - so we are in a class
        let field = binding.field(&target.ident.value).unwrap();
        i32_store("this", field.index as i32 * 4, value.clone())
    }

    pub(crate) fn class_this_access(&mut self, target: &IdentifierExpression) -> SingleExpression {
        let binding = self
            .bindings
            .lookup(&self.binding_context.as_ref().unwrap())
            .unwrap();

        // lhs is this - so we are in a class
        let field = binding.field(&target.ident.value).unwrap();
        i32_load("this", field.index as i32 * 4)
    }
}
