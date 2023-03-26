use jswt_ast::{
    mut_visit::{self, MutVisitor},
    Ast, Literal, IdentifierExpression,
};
use jswt_common::{Spannable, Type};
use jswt_symbols::{BindingsTable, ScopedSymbolTable};

use crate::SemanticError;

/// Type inference resolution pass. The goal is to walk the AST and annotate each node with the type
/// of the expression. A later pass will then use this information to perform type checking.
#[derive(Debug)]
pub struct TypeInferenceResolver<'a> {
    pub symbols: &'a mut ScopedSymbolTable,
    pub bindings: &'a mut BindingsTable,
    pub errors: Vec<SemanticError>,
}
impl<'a> TypeInferenceResolver<'a> {
    pub fn new(bindings: &'a mut BindingsTable, symbols: &'a mut ScopedSymbolTable) -> Self {
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

impl<'a> MutVisitor for TypeInferenceResolver<'a> {
    fn visit_block_statement(&mut self, node: &mut jswt_ast::BlockStatement) {
        self.symbols.push_scope(node.span());
        mut_visit::walk_block_statement(self, node);
        self.symbols.pop_scope();
    }

    fn visit_identifier_expression(&mut self, node: &mut IdentifierExpression) {
        
    }

    fn visit_literal(&mut self, node: &mut jswt_ast::Literal) {
        // Annotate the node on the tree with the type
        match node {
            Literal::Array(_) => todo!(),
            Literal::String(s) => s.ty = Type::STRING,
            Literal::Integer(i) => i.ty = Type::I32,
            Literal::Float(f) => f.ty = Type::F32,
            Literal::Boolean(b) => b.ty = Type::BOOLEAN,
        }
    }
}
