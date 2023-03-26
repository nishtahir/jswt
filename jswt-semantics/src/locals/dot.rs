use jswt_ast::{visit::Visitor, MemberDotExpression};
use jswt_common::{Spannable, Type, Typeable};
use jswt_symbols::BindingsTable;

use crate::{LocalSemanticResolver, SemanticError};

pub struct MemberDotExpressionLocalContext<'a> {
    bindings: &'a BindingsTable,
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> MemberDotExpressionLocalContext<'a> {
    pub fn new(resolver: &'a mut LocalSemanticResolver) -> Self {
        Self {
            bindings: resolver.bindings,
            errors: &mut resolver.errors,
        }
    }

    pub(crate) fn visit_member_dot_arguments_call(&mut self, node: &MemberDotExpression) {
        // Target is currently always an identifier
        if let Some(ident_exp) = node.expression.as_identifier() {
            let target_type = node.target.ty();
            let method_name = ident_exp.ident.value.clone();

            // We need to know the type of the target to know what methods are available
            match &target_type {
                Type::Binding(binding_name) => {
                    println!("Binding name: {}", binding_name);
                    if let None = self.bindings.lookup(&binding_name) {
                        self.errors.push(SemanticError::VariableNotDefined {
                            name: ident_exp.ident.value.clone(),
                            span: ident_exp.span(),
                        });
                    }
                }
                Type::Unknown => {
                    self.errors.push(SemanticError::UnknownType {
                        span: node.span(),
                        ident: method_name,
                    });
                }
            }
        }
    }
}

impl<'a> Visitor for MemberDotExpressionLocalContext<'a> {}
