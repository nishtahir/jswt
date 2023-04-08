mod index;
mod operators;

use index::HirMemberIndexContext;
use jswt_ast::{
    transform::TransformVisitor, Ast, BinaryExpression, ClassDeclarationElement,
    MemberIndexExpression, SingleExpression, SourceElement, SourceElements, AssignmentExpression,
};
use jswt_common::Spannable;
use jswt_symbols::SemanticEnvironment;
use operators::HirOperatorsLoweringContext;

/// High level desugarings are performed here such as
/// Lowering operators into function calls. This simplifies the process
/// of type checking and symbol resolution in later passes.
#[derive(Debug)]
pub struct HirLoweringContext<'a> {
    _environment: &'a SemanticEnvironment,
}

impl<'a> HirLoweringContext<'a> {
    pub fn new(environment: &'a SemanticEnvironment) -> Self {
        Self {
            _environment: environment,
        }
    }

    pub fn lower(&mut self, ast: &Ast) -> Ast {
        let program = self.visit_program(&ast.program);
        Ast { program }
    }
}

impl<'a> TransformVisitor for HirLoweringContext<'a> {
    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) -> SourceElements {
        // TODO - fix this
        // Looks like this has come back to be a problem.
        // We need to visit the class declaration element in order to desugar correctly
        // but we also need to return a SourceElements type.
        // The solution here is probably to have a class specific rewrite visitor that we
        // use to lower the class in later passes.
        SourceElements {
            span: node.span(),
            source_elements: vec![SourceElement::ClassDeclaration(node.clone())],
        }
    }
    fn visit_member_index(&mut self, node: &MemberIndexExpression) -> SingleExpression {
        let mut ctx = HirMemberIndexContext::new();
        ctx.visit_member_index(node)
    }

    fn visit_assignment_expression(&mut self, node: &AssignmentExpression) -> SingleExpression {
        // TODO - Move this into an assignment context visitor
        let mut ctx = HirMemberIndexContext::new();
        ctx.visit_assignment_expression(node)
    }

    fn visit_binary_expression(&mut self, node: &BinaryExpression) -> SingleExpression {
        let mut ctx = HirOperatorsLoweringContext::new();
        ctx.visit_binary_expression(node)
    }
}
