use crate::{consume, ParseError, ParseResult, Parser};
use jswt_ast::{
    BlockStatement, EmptyStatement, ExpressionStatement, ForIterationElement, IfStatement,
    IterationStatement, ReturnStatement, StatementElement, VariableStatement,
    WhileIterationElement,
};
use jswt_common::{Span, Spannable};
use jswt_tokenizer::TokenType;

impl<'a> Parser<'a> {
    ////
    //// Statement
    ////   :  BlockStatement
    ////   |  EmptyStatement
    ////   |  IfStatement
    ////   |  IterationStatement
    ////   |  ReturnStatement
    ////   |  VariableStatement
    ////   |  ExpressionStatement
    ////   ;
    ////
    pub(crate) fn statement(&mut self) -> ParseResult<StatementElement> {
        let elem = match self.lookahead_type() {
            Some(TokenType::LeftBrace) => self.block_statement()?.into(),
            Some(TokenType::Semi) => self.empty_statement()?.into(),
            Some(TokenType::If) => self.if_statement()?.into(),
            Some(TokenType::While) | Some(TokenType::For) => self.iteration_statement()?.into(),
            Some(TokenType::Return) => self.return_statement()?.into(),
            Some(TokenType::Let) | Some(TokenType::Const) => self.variable_statement()?.into(),
            _ => self.expression_statement()?.into(),
        };
        Ok(elem)
    }

    ////
    //// BlockStatement
    ////   :  '{' statementList? '}'
    ////   ;
    ////
    pub(crate) fn block_statement(&mut self) -> ParseResult<BlockStatement> {
        let start = consume!(self, TokenType::LeftBrace)?;
        let statements = self.statement_list(Some(TokenType::RightBrace))?;
        let end = consume!(self, TokenType::RightBrace)?;
        Ok(BlockStatement {
            span: start + end,
            statements,
        })
    }

    ////
    //// EmptyStatement
    ////   : ';'
    ////   ;
    ////
    fn empty_statement(&mut self) -> ParseResult<EmptyStatement> {
        let span = consume!(self, TokenType::Semi)?;
        Ok(EmptyStatement { span })
    }

    ////
    //// IfStatement
    ////   :'if' '(' SingleExpression ')' Statement
    ////   | if' '(' SingleExpression ')' Statement 'else' Statement
    ////   ;
    ////
    fn if_statement(&mut self) -> ParseResult<IfStatement> {
        let start = consume!(self, TokenType::If)?;
        consume!(self, TokenType::LeftParen)?;
        let condition = self.single_expression()?;
        consume!(self, TokenType::RightParen)?;
        let consequence = Box::new(self.statement()?);

        let mut alternative = None;
        if self.lookahead_is(TokenType::Else) {
            consume!(self, TokenType::Else)?;
            alternative = Some(Box::new(self.statement()?));
        }

        let end = alternative
            .as_ref()
            .map(|alt| alt.span())
            .unwrap_or_else(|| consequence.span());

        Ok(IfStatement {
            span: start + end,
            condition,
            consequence,
            alternative,
        })
    }

    //// IterationStatement
    ////   :  WhileStatement
    ///    |  ForStatement
    ////   ;
    fn iteration_statement(&mut self) -> ParseResult<IterationStatement> {
        let elem = match self.lookahead_type() {
            Some(TokenType::While) => self.while_statement()?,
            Some(TokenType::For) => self.for_statement()?,
            _ => todo!(),
        };

        Ok(elem)
    }

    //// WhileStatement
    ////   : 'while' '(' SingleExpression ')' Statement
    ////   ;
    fn while_statement(&mut self) -> ParseResult<IterationStatement> {
        let start = consume!(self, TokenType::While)?;
        consume!(self, TokenType::LeftParen)?;
        let expression = self.single_expression()?;
        consume!(self, TokenType::RightParen)?;
        let block = self.block_statement()?;

        Ok(WhileIterationElement {
            span: start + block.span(),
            expression,
            block,
        }
        .into())
    }

    //// ForStatement
    ///    : 'for' '(' SingleExpression? ';' SingleExpression? ';' SingleExpression? ')' BlockStatement
    ///    ;
    fn for_statement(&mut self) -> ParseResult<IterationStatement> {
        let start = consume!(self, TokenType::For)?;
        consume!(self, TokenType::LeftParen)?;
        // TODO - the spec says that the initializer can be a
        // variable declaration, but we don't support that yet.
        let initializer = self.single_expression()?;
        consume!(self, TokenType::Semi)?;
        let condition = self.single_expression()?;
        consume!(self, TokenType::Semi)?;
        let update = self.single_expression()?;
        consume!(self, TokenType::RightParen)?;
        let block = self.block_statement()?;
        Ok(ForIterationElement {
            span: start + block.span(),
            initializer,
            condition,
            update,
            block,
        }
        .into())
    }

    ////
    //// ReturnStatement
    ////   : 'return' SingleExpression ';'
    ////   ;
    ////
    fn return_statement(&mut self) -> ParseResult<ReturnStatement> {
        let start = consume!(self, TokenType::Return)?;
        let expression = self.single_expression()?;
        let end = consume!(self, TokenType::Semi)?;

        Ok(ReturnStatement {
            span: start + end,
            expression,
        })
    }

    ////
    //// VariableStatement
    ////   :  VariableModifier Assignable '=' SingleExpression ';'
    ////   ;
    ////
    fn variable_statement(&mut self) -> ParseResult<VariableStatement> {
        let modifier = self.variable_modifier()?;
        let target = self.assignable()?;

        let mut type_annotation = None;
        if self.lookahead_is(TokenType::Colon) {
            type_annotation = Some(self.type_annotation()?);
        }

        consume!(self, TokenType::Equal)?;
        let expression = self.single_expression()?;
        let end = consume!(self, TokenType::Semi)?;

        Ok(VariableStatement {
            span: modifier.span() + end,
            modifier,
            target,
            expression,
            type_annotation,
        })
    }

    ////
    //// ExpressionStatement
    ////   : SingleExpression';'
    ////   ;
    ////
    fn expression_statement(&mut self) -> ParseResult<ExpressionStatement> {
        let expression = self.single_expression()?;
        let end = consume!(self, TokenType::Semi)?;

        Ok(ExpressionStatement {
            span: expression.span() + end,
            expression,
        })
    }
}

#[cfg(test)]
mod test {
    use crate::Parser;
    use jswt_assert::assert_debug_snapshot;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_parse_if_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_if_statement",
            "function main() { if(x == y) { return 0; } else { return 1; } }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_if_else_if_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_if_else_if_statement",
            "function main() { if(x == y) { return 0; } else if (x > y) { return 1; } else { return -1; } }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_return_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_return_statement",
            "function main() { return 0; }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_variable_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_variable_statement",
            "function main() { let x = 0; }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_variable_statement_with_type_annotation() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_variable_statement_with_type_annotation",
            "let x: i32 = 99;",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_expression_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_expression_statement",
            "function main() { x = 0; }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_while_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_while_statement",
            "function main() { while(x == y) { return 0; } }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_for_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_for_statement",
            "function main() { for(i = 0; i < 10; i = i + 1) { return 0; } }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }
}
