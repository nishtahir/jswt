use std::borrow::Cow;

use jswt_ast::*;
use jswt_common::Spannable;
use jswt_types::{PrimitiveType, Type};

#[derive(Debug, Default)]
pub(crate) struct ClassDesugaring {
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
        SourceElement::FunctionDeclaration(FunctionDeclarationElement {
            span: node.span(),
            decorators: FunctionDecorators {
                annotations: vec![],
                export: false,
            },
            ident: Identifier {
                span: node.ident.span(),
                value: format!("{}#{}", self.class_context.as_ref().unwrap(), name).into(),
            },
            params: node.params.clone(),
            returns: node.returns.clone(),
            body: FunctionBody {
                span: node.body.span(),
                source_elements: SourceElements {
                    source_elements: vec![],
                },
            },
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
            returns: Some(TypeAnnotation {
                span: node.span(),
                ty: Type::Primitive(PrimitiveType::I32),
            }),
            body: FunctionBody {
                span: node.span(),
                source_elements: SourceElements {
                    source_elements: vec![],
                },
            },
        })
    }
}
