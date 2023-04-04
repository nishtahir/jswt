mod arguments;
mod class;
mod new;

use arguments::MirArgumentsLoweringContext;
use class::MirClassLoweringContext;
use jswt_ast::{transform::*, *};
use jswt_symbols::SemanticEnvironment;
use new::MirNewLoweringContext;

/// Mir lowering focuses on reducing high level calls and constructs into
/// simpler forms that can be more easily transformed at later stages.
/// At this stage we expect global bindings to be resolved and basic type checking
/// to have been performed.
#[derive(Debug)]
pub struct MirLoweringContext<'a> {
    environment: &'a SemanticEnvironment,
}

impl<'a> MirLoweringContext<'a> {
    pub fn new(environment: &'a SemanticEnvironment) -> Self {
        Self { environment }
    }

    pub fn lower(&mut self, ast: &Ast) -> Ast {
        let program = self.visit_program(&ast.program);
        Ast { program }
    }
}

impl<'a> TransformVisitor for MirLoweringContext<'a> {
    fn visit_program(&mut self, node: &Program) -> Program {
        transform::walk_program(self, node)
    }

    /// Lower class declarations into a series of functions
    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) -> SourceElements {
        let mut lowering = MirClassLoweringContext::new(node, &self.environment);
        lowering.visit_class_declaration(node)
    }

    fn visit_new(&mut self, node: &NewExpression) -> SingleExpression {
        let mut lowering = MirNewLoweringContext::new(&self.environment);
        lowering.visit_new(node)
    }

    fn visit_argument_expression(&mut self, node: &ArgumentsExpression) -> SingleExpression {
        if let SingleExpression::MemberDot(_) = &*node.ident {
            let mut ctx = MirArgumentsLoweringContext::new(&self.environment);
            return ctx.visit_argument_expression(node);
        }

        walk_argument_expression(self, node)
    }
}
