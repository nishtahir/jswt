mod class;
mod gen;

use std::borrow::Cow;

use gen::ident_exp;
use jswt_ast::{transform::*, *};
use jswt_common::Spannable;
use jswt_symbols::{BindingsTable, Symbol};

type SymbolTable = jswt_symbols::SymbolTable<Cow<'static, str>, Symbol>;

#[derive(Debug)]
pub struct AstLowering<'a> {
    bindings: &'a mut BindingsTable,
    symbols: &'a mut SymbolTable,
    binding_context: Option<Cow<'static, str>>,
}

impl<'a> AstLowering<'a> {
    pub fn new(bindings: &'a mut BindingsTable, symbols: &'a mut SymbolTable) -> Self {
        Self {
            bindings,
            symbols,
            binding_context: None,
        }
    }

    pub fn desugar(&mut self, ast: &mut Ast) {
        let program = self.visit_program(&mut ast.program);
        ast.program = program;
    }
}

impl<'a> TransformVisitor for AstLowering<'a> {
    fn visit_program(&mut self, node: &Program) -> Program {
        transform::walk_program(self, node)
    }

    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) -> SourceElements {
        self.enter_class_declaration(node);
        let elements = transform::walk_class_declaration(self, node);
        self.exit_class_declaration();
        elements
    }

    fn visit_class_constructor_declaration(
        &mut self,
        node: &ClassConstructorElement,
    ) -> SourceElements {
        SourceElements {
            span: node.span(),
            source_elements: vec![self.enter_class_constructor(node)],
        }
    }

    fn visit_class_method_declaration(&mut self, node: &ClassMethodElement) -> SourceElements {
        SourceElements {
            span: node.span(),
            source_elements: vec![self.enter_class_method(node)],
        }
    }

    fn visit_class_field_declaration(&mut self, node: &ClassFieldElement) -> SourceElements {
        SourceElements {
            span: node.span(),
            // Fields don't show up in the lowered AST
            // They are only indicators for the compiler to align class structures
            source_elements: vec![],
        }
    }

    fn visit_new(&mut self, node: &NewExpression) -> SingleExpression {
        // rewrite new as a function call invoking the lowered synthetic
        // constructor declaration of the class
        let mut args = node.expression.as_arguments().unwrap().clone();
        let mut ident = args.ident.as_identifier_mut().unwrap();
        ident.ident.value = format!("{}#constructor", ident.ident.value).into();
        SingleExpression::Arguments(args)
    }

    fn visit_assignment_expression(&mut self, node: &BinaryExpression) -> SingleExpression {
        if let SingleExpression::MemberDot(dot) = &*node.left {
            if let SingleExpression::This(_) = &*dot.target {
                // This is always an identifier
                let target = dot.expression.as_identifier().unwrap();
                let value = &*node.right;
                return self.class_this_field_assignment(target, value);
            }
        }

        SingleExpression::Assignment(BinaryExpression {
            span: node.span(),
            left: Box::new(self.visit_single_expression(&node.left)),
            op: node.op.clone(),
            right: Box::new(self.visit_single_expression(&node.right)),
        })
    }

    fn visit_member_dot(&mut self, node: &MemberDotExpression) -> SingleExpression {
        if let SingleExpression::This(_) = &*node.target {
            // This is always an identifier
            let target = node.expression.as_identifier().unwrap();
            return self.class_this_access(target);
        }
        return transform::walk_member_dot(self, node);
    }

    fn visit_this_expression(&mut self, _: &ThisExpression) -> SingleExpression {
        ident_exp("this".into())
    }

    // fn visit_single_expression(&mut self, node: &SingleExpression) -> SingleExpression {
    //     match node {
    //         SingleExpression::Arguments(exp) => self.visit_argument_expression(exp),
    //         SingleExpression::Literal(lit) => self.visit_literal(lit),
    //         SingleExpression::Multiplicative(exp) => self.visit_binary_expression(exp),
    //         SingleExpression::Additive(exp) => self.lower_additive_expression(node),
    //         SingleExpression::Identifier(ident) => self.visit_identifier_expression(ident),
    //         SingleExpression::Equality(exp) => self.visit_binary_expression(exp),
    //         SingleExpression::Bitwise(exp) => self.visit_binary_expression(exp),
    //         SingleExpression::Relational(exp) => self.visit_binary_expression(exp),
    //         SingleExpression::Assignment(exp) => self.visit_assignment_expression(exp),
    //         SingleExpression::Unary(exp) => self.visit_unary_expression(exp),
    //         SingleExpression::MemberIndex(exp) => self.visit_member_index(exp),
    //         SingleExpression::This(exp) => self.visit_this_expression(exp),
    //         SingleExpression::MemberDot(exp) => self.visit_member_dot(exp),
    //         SingleExpression::New(exp) => self.visit_new(exp),
    //     }
    // }
}

// impl<'a> AstLowering<'a> {
//     fn lower_additive_expression(&self, node: &SingleExpression) -> SingleExpression {
//         todo!()
//     }
// }
#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_semantics::SemanticAnalyzer;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_class_declaration_lowers_class_fields() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_class_declaration_lowers_class_fields",
            r"
        class Array {
            len: i32;
            capacity: i32;
        }
    ",
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let mut analyzer = SemanticAnalyzer::default();
        analyzer.analyze(&mut ast);

        let mut lowering =
            AstLowering::new(&mut analyzer.bindings_table, &mut analyzer.symbol_table);
        lowering.desugar(&mut ast);

        assert_debug_snapshot!(ast);
    }

    #[test]
    fn test_class_declaration_lowers_methods_into_functions() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_class_declaration_lowers_methods_into_functions",
            r"
        class Array {
            len(): i32 {
                return 0;
            }
        }
    ",
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let mut analyzer = SemanticAnalyzer::default();
        analyzer.analyze(&mut ast);

        let mut lowering =
            AstLowering::new(&mut analyzer.bindings_table, &mut analyzer.symbol_table);
        lowering.desugar(&mut ast);

        assert_debug_snapshot!(ast);
    }

    #[test]
    fn test_class_declaration_lowers_new_expression_into_constructor_invocation() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_class_declaration_lowers_new_expression_into_constructor_invocation",
            r"
        class Array {
            constructor() {}
        }

        function main(): i32 {
            let x = new Array();
            return 0;
        }
    ",
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let mut analyzer = SemanticAnalyzer::default();
        analyzer.analyze(&mut ast);

        let mut lowering =
            AstLowering::new(&mut analyzer.bindings_table, &mut analyzer.symbol_table);
        lowering.desugar(&mut ast);

        assert_debug_snapshot!(ast);
    }

    #[test]
    fn test_class_declaration_lowers_this_binding() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_class_declaration_lowers_this_binding",
            r"
        class Array {
            len: i32;
            capacity: i32;

            constructor(len: i32, capacity: i32) {
                this.len = len;
                this.capacity = capacity;
            }
        }
    ",
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let mut analyzer = SemanticAnalyzer::default();
        analyzer.analyze(&mut ast);

        let mut lowering =
            AstLowering::new(&mut analyzer.bindings_table, &mut analyzer.symbol_table);
        lowering.desugar(&mut ast);

        assert_debug_snapshot!(ast);
    }
}
