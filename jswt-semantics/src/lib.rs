mod error;
mod globals;
mod resolver;

use std::borrow::Cow;

pub use error::*;
use globals::*;
use resolver::*;

use jswt_ast::Ast;
use jswt_symbols::{BindingsTable, Symbol};

type SymbolTable = jswt_symbols::SymbolTable<Cow<'static, str>, Symbol>;

#[derive(Debug, Default)]
pub struct SemanticAnalyzer {
    pub symbol_table: SymbolTable,
    pub bindings_table: BindingsTable,
}

impl SemanticAnalyzer {
    pub fn analyze(&mut self, ast: &mut Ast) -> Vec<SemanticError> {
        let mut errors = vec![];

        // This is the first semantic pass to resolve global variables
        let mut global_resolver =
            GlobalResolver::new(&mut self.bindings_table, &mut self.symbol_table);
        global_resolver.resolve(ast);
        errors.append(&mut global_resolver.errors);

        // // This is the second semantic pass to inspect function content
        let mut resolver = Resolver::new(&mut self.symbol_table);
        resolver.resolve(ast);
        errors.append(&mut resolver.errors);

        errors
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_duplicate_function_declaration_generates_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_duplicate_function_declaration_generates_error",
            r"
        class Array {

        }

        function test(a: i32, b: i32) {
            let x = 99;
        }
 
        function test(): Array {

        }
        ",
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let errors = SemanticAnalyzer::default().analyze(&mut ast);
        assert_debug_snapshot!(errors);
    }

    #[test]
    fn test_duplicate_global_variable_declaration_generates_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_duplicate_global_variable_declaration_generates_error",
            r"
            let x = 99;
            let x = 55;
            ",
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let errors = SemanticAnalyzer::default().analyze(&mut ast);
        assert_debug_snapshot!(errors);
    }

    #[test]
    #[ignore]
    fn test_duplicate_variable_declaration_generates_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_duplicate_variable_declaration_generates_error",
            r"
        function test(a: i32, b: i32) {
            let x = 99;
            let x = 55;
        }
        ",
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let errors = SemanticAnalyzer::default().analyze(&mut ast);
        assert_debug_snapshot!(errors);
    }

    #[test]
    fn test_duplicate_class_declaration_generates_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_duplicate_class_declaration_generates_error",
            r"
            class Array {

            }

            class Array {
                
            }
        ",
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let errors = SemanticAnalyzer::default().analyze(&mut ast);
        assert_debug_snapshot!(errors);
    }

    #[test]
    #[ignore]
    fn test_variable_not_defined_generates_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_variable_not_defined_generates_error",
            "function test() { return x; }",
        );

        let mut ast = Parser::new(&mut tokenizer).parse();
        let errors = SemanticAnalyzer::default().analyze(&mut ast);
        assert_debug_snapshot!(errors);
    }
}
