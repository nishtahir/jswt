use jswt_ast::{mut_visit::MutVisitor, Ast, Literal};
use jswt_symbols::{BindingsTable, ScopedSymbolTable};
use jswt_common::Type;

use crate::SemanticError;

#[derive(Debug)]
pub struct TypeResolver<'a> {
    pub symbols: &'a mut ScopedSymbolTable,
    pub bindings: &'a mut BindingsTable,
    pub errors: Vec<SemanticError>,
}
impl<'a> TypeResolver<'a> {
    pub fn new(symbols: &'a mut ScopedSymbolTable, bindings: &'a mut BindingsTable) -> Self {
        Self {
            symbols,
            bindings,
            errors: vec![],
        }
    }
    pub fn resolve(&mut self, ast: &mut Ast) {
        // We require the global scope to be defined
        debug_assert!(self.symbols.depth() == 1);

        self.visit_program(&mut ast.program);

        // We should have popped down to the global scope
        debug_assert!(self.symbols.depth() == 1);
    }
}

impl<'a> MutVisitor for TypeResolver<'a> {
    fn visit_literal(&mut self, node: &mut jswt_ast::Literal) {
        // Annotate the node on the tree with the type
        match node {
            Literal::Array(_) => todo!(),
            Literal::String(s) => s.ty = Type::Binding("string".into()),
            Literal::Integer(i) => i.ty = Type::Binding("i32".into()),
            Literal::Float(f) => f.ty = Type::Binding("f32".into()),
            Literal::Boolean(b) => b.ty = Type::Binding("boolean".into()),
        }
    }
}
