use std::borrow::Cow;

use jswt_ast::{transform::TransformVisitor, *};
use jswt_symbols::{BindingsTable, Symbol};
use jswt_synthetic::ident_exp;

type SymbolTable = jswt_symbols::SymbolTable<Cow<'static, str>, Symbol>;

pub struct MirLoweringContext<'a> {
    bindings: &'a BindingsTable,
    symbols: &'a SymbolTable,
}

impl<'a> MirLoweringContext<'a> {
    pub fn new(bindings: &'a BindingsTable, symbols: &'a SymbolTable) -> Self {
        Self { bindings, symbols }
    }

    pub fn lower(&mut self, ast: &Ast) -> Ast {
        let program = self.visit_program(&ast.program);
        Ast { program }
    }
}

impl<'a> TransformVisitor for MirLoweringContext<'a> {
    fn visit_this_expression(&mut self, _: &ThisExpression) -> SingleExpression {
        ident_exp("this".into())
    }
}
