use std::borrow::Cow;

use jswt_ast::*;
use jswt_common::Spannable;
use jswt_types::{PrimitiveType, Type};

use crate::symbols::ClassBinding;

#[derive(Debug, Default)]
pub(crate) struct ClassDesugaring {
    binding: ClassBinding,
    class_context: Option<Cow<'static, str>>,
}

impl ClassDesugaring {
    pub(crate) fn enter_class_declaration(&mut self, node: &ClassDeclarationElement) {
        let name = node.ident.value.clone();
        self.class_context = Some(name);
    }

    pub(crate) fn exit_class_declaration(&mut self) {
        self.class_context = None;
    }

    pub(crate) fn enter_class_method(&self, node: &ClassMethodElement) -> SourceElement {
        let name = &node.ident.value;

        // Generate synthetic function name
        // The function name is the ClassName#methodName
        let ident = Identifier {
            span: node.ident.span(),
            value: format!("{}#{}", self.class_context.as_ref().unwrap(), name).into(),
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
                    ty: Type::Primitive(PrimitiveType::I32),
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

    pub(crate) fn enter_class_constructor(&self, node: &ClassConstructorElement) -> SourceElement {
        SourceElement::FunctionDeclaration(FunctionDeclarationElement {
            span: node.span(),
            decorators: FunctionDecorators {
                annotations: vec![],
                export: false,
            },
            ident: Identifier {
                span: node.span(),
                value: format!("{}#constructor", self.class_context.as_ref().unwrap()).into(),
            },
            params: node.params.clone(),
            // Class constructors always return a pointer
            returns: Some(TypeAnnotation {
                span: node.span(),
                ty: Type::Primitive(PrimitiveType::I32),
            }),
            body: node.body.clone(),
        })
    }
}
