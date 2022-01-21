use jswt_ast::*;
use jswt_common::{BindingsTable, ClassBinding, Span, Spannable};
use jswt_types::{PrimitiveType, Type};

#[derive(Debug)]
pub(crate) struct ClassLowering<'a> {
    bindings: &'a BindingsTable,
    binding_context: Option<&'a ClassBinding>,
}

impl<'a> ClassLowering<'a> {
    pub(crate) fn new(bindings: &'a BindingsTable) -> Self {
        Self {
            bindings,
            binding_context: None,
        }
    }

    pub(crate) fn enter_class_declaration(&mut self, node: &ClassDeclarationElement) {
        let name = node.ident.value.clone();
        self.binding_context = self.bindings.lookup(&name).and_then(|b| b.as_class());
    }

    pub(crate) fn exit_class_declaration(&mut self) {
        self.binding_context = None;
    }

    pub(crate) fn enter_class_method(&self, node: &ClassMethodElement) -> SourceElement {
        let name = &node.ident.value;
        let class_name = self.binding_context.as_ref().unwrap().name.clone();

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
        let binding = self.binding_context.unwrap();
        let class_name = binding.name.clone();
        let class_size = binding.size();

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
                ty: Type::Primitive(PrimitiveType::I32),
            }),
            body: node.body.clone(),
        })
    }
}

fn function_call(span: Span, name: &'static str, arg: i32) -> SingleExpression {
    SingleExpression::Arguments(ArgumentsExpression {
        span: span.clone(),
        ident: Box::new(SingleExpression::Identifier(IdentifierExpression {
            span: span.clone(),
            ident: Identifier::new(name, span.clone()),
        })),
        arguments: ArgumentsList {
            span: span.clone(),
            arguments: vec![SingleExpression::Literal(Literal::Integer(
                IntegerLiteral { span, value: arg },
            ))],
        },
    })
}
