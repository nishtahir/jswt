use std::borrow::Cow;

use jswt_ast::*;
use jswt_common::Spannable;

#[derive(Debug, Default)]
pub(crate) struct ClassDesugaring {
    class_context: Option<Cow<'static, str>>,
    source_elements: Vec<SourceElement>,
}

impl ClassDesugaring {
    pub(crate) fn enter_class_declaration(&mut self, node: &mut ClassDeclarationElement) {
        let name = node.ident.value.clone();
        self.class_context = Some(name);
    }

    pub(crate) fn exit_class_declaration(&mut self) {
        self.class_context = None;
    }

    pub(crate) fn enter_class_method_declaration(&mut self, node: &mut ClassMethodElement) {
        let name = &node.ident.value;
        let elem = SourceElement::FunctionDeclaration(FunctionDeclarationElement {
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
        });
        self.source_elements.push(elem);
    }

    pub(crate) fn enter_source_elements(&mut self, node: &mut SourceElements) {
        // self.source_elements = ;
    }

    pub(crate) fn exit_source_elements(&mut self, node: &mut SourceElements) {
        node.source_elements.append(&mut self.source_elements);
    }
}
