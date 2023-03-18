mod class;
mod new;

use class::HirClassLoweringContext;
use jswt_ast::{transform::*, *};
use jswt_symbols::{BindingsTable, ScopedSymbolTable};
use new::HirNewLoweringContext;

/// HIR lowering focuses on reducing high level calls and constructs into
/// simpler forms that can be more easily transformed at later stages.
/// At this stage we expect global bindings to be resolved and basic type checking
/// to have been performed.
#[derive(Debug)]
pub struct HirLoweringContext<'a> {
    bindings: &'a BindingsTable,
    symbols: &'a ScopedSymbolTable,
}

impl<'a> HirLoweringContext<'a> {
    pub fn new(bindings: &'a BindingsTable, symbols: &'a ScopedSymbolTable) -> Self {
        Self { bindings, symbols }
    }

    pub fn lower(&mut self, ast: &Ast) -> Ast {
        let program = self.visit_program(&ast.program);
        Ast { program }
    }
}

impl<'a> TransformVisitor for HirLoweringContext<'a> {
    fn visit_program(&mut self, node: &Program) -> Program {
        transform::walk_program(self, node)
    }

    /// Lower class declarations into a series of functions
    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) -> SourceElements {
        let mut lowering = HirClassLoweringContext::new(node, &self.bindings);
        lowering.visit_class_declaration(node)
    }

    fn visit_new(&mut self, node: &NewExpression) -> SingleExpression {
        let mut lowering = HirNewLoweringContext::new(&self.bindings);
        lowering.visit_new(node)
    }
}
