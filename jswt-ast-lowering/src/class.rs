use crate::{gen::*, AstLowering};

use jswt_ast::{transform::TransformVisitor, *};
use jswt_common::Spannable;
use jswt_types::Type;

impl<'a> AstLowering<'a> {
    pub(crate) fn enter_class_declaration(&mut self, node: &ClassDeclarationElement) {
        let name = node.ident.value.clone();
        self.binding_context = Some(name);
    }

    pub(crate) fn exit_class_declaration(&mut self) {
        self.binding_context = None;
    }

    pub(crate) fn enter_class_method(&self, node: &ClassMethodElement) -> SourceElement {
        let name = &node.ident.value;
        let binding_context = self
            .symbols
            .lookup(self.binding_context.as_ref().unwrap().clone())
            .and_then(|b| b.as_class());
        let class_name = binding_context.as_ref().unwrap().name.clone();

        // Generate synthetic function name
        // The function name is the ClassName#methodName
        let ident = Identifier {
            span: node.ident.span(),
            value: format!("{}#{}", class_name, name).into(),
        };

        // Generate function parameters
        // The first parameter should be the pointer to the instance binding
        let mut params = node.params.clone();
        params.parameters.insert(
            0,
            FormalParameterArg {
                ident: Identifier {
                    span: node.span(),
                    value: "this".into(),
                },
                type_annotation: TypeAnnotation {
                    span: node.span(),
                    ty: Type::Class("i32".into()),
                },
            },
        );

        SourceElement::FunctionDeclaration(FunctionDeclarationElement {
            span: node.span(),
            decorators: FunctionDecorators {
                annotations: vec![],
                export: false,
            },
            ident,
            params,
            returns: node.returns.clone(),
            body: node.body.clone(),
        })
    }

    pub(crate) fn enter_class_constructor(
        &mut self,
        node: &ClassConstructorElement,
    ) -> SourceElement {
        let binding = self
            .symbols
            .lookup(self.binding_context.as_ref().unwrap().clone())
            .and_then(|b| b.as_class())
            .unwrap();

        let class_name = binding.name.clone();
        let class_size = binding.size();
        let mut body = self.visit_block_statement(&node.body);

        let this = parse_statement(format!("let self = malloc({class_size});"));
        let returns = parse_statement(format!("return self;"));

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
                ty: Type::Class("i32".into()),
            }),
            body: BlockStatement {
                span: node.body.span(),
                statements: body,
            },
        })
    }

    pub(crate) fn class_this_field_assignment(
        &self,
        target: &IdentifierExpression,
        value: &SingleExpression,
    ) -> SingleExpression {
        let binding = self
            .symbols
            .lookup(self.binding_context.as_ref().unwrap().clone())
            .and_then(|b| b.as_class())
            .unwrap();

        // lhs is this - so we are in a class
        let field = binding.field(&target.ident.value).unwrap();
        i32_store("self", field.index as i32 * 4, value.clone())
    }
}
