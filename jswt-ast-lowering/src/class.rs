use jswt_ast::*;
use jswt_common::{BindingsTable, ClassBinding, Spannable};
use jswt_types::{PrimitiveType, Type};

use crate::gen::*;

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

        let mut body = node.body.clone();

        let this = variable("this", malloc(class_size as i32));
        let returns = returns(identifier("this"));

        body.statements.statements.push(this);

        binding.fields.iter().for_each(|f| {
            let offset = f.index * 4;
            body.statements.statements.push(to_statement(i32_store(
                "this",
                offset as i32,
                node.params.parameters[f.index].ident.clone(),
            )));
        });

        body.statements.statements.push(returns);
        // let elements = generate_expression(format!("const ptr:i32 = malloc({});", class_size));
        // println!("{:#?}", this);

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
            body,
        })
    }
}
