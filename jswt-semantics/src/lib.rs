mod convert;
mod error;
mod globals;
mod resolver;
mod types;

use jswt_ast::high_level::Ast;

pub use error::*;
use globals::*;
use jswt_common::SemanticSymbolTable;
use resolver::*;

pub struct SemanticAnalyzer;

pub struct SemanticData {
    pub errors: Vec<SemanticError>,
    pub symbols: SemanticSymbolTable,
}

impl SemanticAnalyzer {
    pub fn analyze(ast: &mut Ast) -> SemanticData {
        let mut global_resolver = GlobalResolver::default();
        global_resolver.resolve(ast);

        let symbols = global_resolver.symbols;
        let mut resolver = Resolver::new(symbols);
        resolver.resolve(ast);

        let mut errors = vec![];
        errors.append(&mut global_resolver.errors);
        errors.append(&mut resolver.errors);

        SemanticData {
            errors,
            symbols: resolver.symbols,
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_duplicate_variable_declaration_generates_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test.1", "function test() { let x = 0; let x = 1; }");
        let mut ast = Parser::new(&mut tokenizer).parse();
        let data = SemanticAnalyzer::analyze(&mut ast);
        assert_debug_snapshot!(data.errors);
    }

    #[test]
    fn test_variable_not_defined_generates_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test.1", "function test() { return x; }");
        let mut ast = Parser::new(&mut tokenizer).parse();
        let data = SemanticAnalyzer::analyze(&mut ast);
        assert_debug_snapshot!(data.errors);
    }

    #[test]
    fn test_function_return_type_is_checked_when_invoked_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test.1",
            r#"
            function main():i32 { 
                return test(); 
            } 
            
            function test(): boolean { 
                return x; 
            }
        "#,
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let data = SemanticAnalyzer::analyze(&mut ast);
        assert_debug_snapshot!(data.errors);
    }

    #[test]
    fn test_if_statement_expression_must_be_boolean() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test.1",
            r#"
            function main():i32 { 
                if(test()) {

                }
            } 
            
            function test(): i32 { 
                return 0; 
            }
        "#,
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let data = SemanticAnalyzer::analyze(&mut ast);
        assert_debug_snapshot!(data.errors);
    }
}
