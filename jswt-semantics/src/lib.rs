mod error;
mod globals;
mod resolver;
mod types;

use std::borrow::Cow;

pub use error::*;
pub use globals::*;
pub use resolver::*;

use jswt_ast::Ast;
use jswt_symbols::{BindingsTable, Symbol};
use types::TypeChecker;

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
        let mut global = GlobalResolver::new(&mut self.bindings_table, &mut self.symbol_table);
        global.resolve(ast);
        errors.append(&mut global.errors);

        // // This is the second semantic pass to inspect function content
        let mut resolver = Resolver::new(&mut self.symbol_table, &mut self.bindings_table);
        resolver.resolve(ast);
        errors.append(&mut resolver.errors);

        // Perform type checks and annotate AST
        let mut types = TypeChecker::new(&mut self.symbol_table, &mut self.bindings_table);
        types.resolve(ast);
        errors.append(&mut types.errors);

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
    fn test_semantic_analyzer_analyzes_correctly() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_semantic_analyzer_analyzes_correctly",
            r"
        class Array {

        }

        function test(a: i32, b: i32) {
            let x = 99;
        }
        ",
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let errors = SemanticAnalyzer::default().analyze(&mut ast);
        assert_debug_snapshot!(errors);
    }

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

    #[test]
    fn test_if_statement_condition_must_be_boolean_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_if_statement_condition_must_be_boolean_expression",
            "function test() { if(10) {} }",
        );

        let mut ast = Parser::new(&mut tokenizer).parse();
        let errors = SemanticAnalyzer::default().analyze(&mut ast);
        assert_debug_snapshot!(errors);
    }

    // #[test]
    // fn test_if_statement_condition_must_be_boolean_expression() {
    //     let mut tokenizer = Tokenizer::default();
    //     tokenizer.enqueue_source_str(
    //         "test_if_statement_condition_must_be_boolean_expression",
    //         "function test() { if(10) {} }",
    //     );

    //     let mut ast = Parser::new(&mut tokenizer).parse();
    //     let errors = SemanticAnalyzer::default().analyze(&mut ast);
    //     assert_debug_snapshot!(errors);
    // }
}
