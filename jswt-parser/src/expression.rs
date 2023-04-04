use crate::{consume, consume_unchecked, ident, ParseError, ParseResult, Parser};
use jswt_ast::*;
use jswt_common::{Span, Spannable};
use jswt_tokenizer::TokenType;

macro_rules! binary_expression {
    ($name:ident, next: $next:ident, $([exp => $exp:ident, op => $op:ident, token => $token:ident]),*) => {
        pub(crate) fn $name(&mut self) -> ParseResult<SingleExpression>{
            let mut left = self.$next()?;
            while let Some(token) = self.lookahead_type() {
                match token {
                    $(
                        TokenType::$token => {
                            let op_span = consume_unchecked!(self);
                            let right = self.$next()?;
                            left = SingleExpression::$exp(BinaryExpression {
                                span: left.span() + right.span(),
                                left: Box::new(left),
                                op: BinaryOperator::$op(op_span),
                                right: Box::new(right),
                            });
                        }
                    )*
                    _ => break,
                }
            }

            Ok(left)
        }
    };
}

impl<'a> Parser<'a> {
    ////
    //// SingleExpression
    ////   : 'new' SingleExpression
    ////   | SingleExpression '=' SingleExpression
    ////   | SingleExpression '|' SingleExpression
    ////   | SingleExpression '&' SingleExpression
    ////   | SingleExpression ('==' | '!=') SingleExpression
    ////   | SingleExpression ('<' | '>' | '<=' | '>=') SingleExpression
    ////   | SingleExpression ('+' | '-') SingleExpression
    ////   | SingleExpression ('*' | '/') SingleExpression
    ///    | SingleExpression (++ | --)
    ////   | (+ | - | ! ) SingleExpression
    ////   | SingleExpression '(' Arguments ')'
    ////   | SingleExpression '[' SingleExpression ']'
    ////   | SingleExpression '.' Identifier
    ////   | ThisExpression
    ////   | IdentifierExpression
    ////   | ArrayLiteralExpression
    ////   | Literal
    ////   ;
    ////
    pub(crate) fn single_expression(&mut self) -> ParseResult<SingleExpression> {
        self.new_expression()
    }

    ////
    //// NewExpression
    ////   : 'new' SingleExpression
    ////   ;
    ////
    pub(crate) fn new_expression(&mut self) -> ParseResult<SingleExpression> {
        if self.lookahead_is(TokenType::New) {
            // Consume new token
            let start = consume_unchecked!(self);
            let expression = self.arguments_expression()?;
            return Ok(SingleExpression::New(NewExpression {
                span: start + expression.span(),
                expression: Box::new(expression),
            }));
        }

        self.assignment_expression()
    }

    ////
    //// AssignmentExpression
    ////   : ArgumentsExpression '=' SingleExpression
    ////   ;
    ////
    pub(crate) fn assignment_expression(&mut self) -> ParseResult<SingleExpression> {
        let left = self.bitwise_or_expression()?;
        if self.lookahead_is(TokenType::Equal) {
            let op_span = consume_unchecked!(self);
            let right = self.single_expression()?;
            return Ok(SingleExpression::Assignment(BinaryExpression {
                span: left.span() + right.span(),
                left: Box::new(left),
                op: BinaryOperator::Assign(op_span),
                right: Box::new(right),
            }));
        }
        Ok(left)
    }

    ////
    ////  BitwiseOrExpression
    ////    : BitwiseAndExpression '|' BitwiseAndExpression
    ////    ;
    ////
    binary_expression!(
        bitwise_or_expression,
        next: bitwise_and_expression,
        [exp => Bitwise, op => Or, token => Or]
    );

    ////
    ////  BitwiseAndExpression
    ////    : EqualityExpression '&' EqualityExpression
    ////    ;
    ////
    binary_expression!(
        bitwise_and_expression,
        next: equality_expression,
        [exp => Bitwise, op => And, token => And]
    );

    ////
    //// EqualityExpression
    ////    : RelationalExpression ('==' | '!=') RelationalExpression
    ////    ;
    ////
    binary_expression!(
        equality_expression,
        next: relational_expression,
        [exp => Equality, op => Equal, token => EqualEqual],
        [exp => Equality, op => NotEqual, token => BangEqual]
    );

    ////
    ////  RelationalExpression
    ////    : AdditiveExpression ('<' | '>' | '<=' | '>=') AdditiveExpression
    ////    ;
    ////
    binary_expression!(
        relational_expression,
        next: additive_expression,
        [exp => Relational, op => Greater, token => Greater],
        [exp => Relational, op => GreaterEqual, token => GreaterEqual],
        [exp => Relational, op => Less, token => Less],
        [exp => Relational, op => LessEqual, token => LessEqual]
    );

    ////
    //// AdditiveExpression
    ////    : MultiplicativeExpression ('+' | '-') MultiplicativeExpression
    ////    ;
    ////
    binary_expression!(
        additive_expression,
        next: multipicative_expression,
        [exp => Additive, op => Plus, token => Plus],
        [exp => Additive, op => Minus, token => Minus]
    );

    ////
    //// MultiplicativeExpression
    ////    : PostfixUnaryExpression ('*' | '/' | '%') PostfixUnaryExpression
    ////    ;
    ////
    binary_expression!(
        multipicative_expression,
        next: postfix_unary_expression,
        [exp => Multiplicative, op => Mult, token => Star],
        [exp => Multiplicative, op => Div, token => Slash]
    );

    ////
    //// PostfixUnaryExpression
    ////    : PrefixUnaryExpression ('++' | '--')
    ////    ;
    ////
    fn postfix_unary_expression(&mut self) -> ParseResult<SingleExpression> {
        let expr = self.prefix_unary_expression()?;
        if self.lookahead_is(TokenType::PlusPlus) {
            let op = consume_unchecked!(self);
            return Ok(SingleExpression::Unary(UnaryExpression {
                span: op.to_owned() + expr.span(),
                op: UnaryOperator::PostIncrement(op),
                expr: Box::new(expr),
            }));
        }
        if self.lookahead_is(TokenType::MinusMinus) {
            let op = consume_unchecked!(self);
            return Ok(SingleExpression::Unary(UnaryExpression {
                span: op.to_owned() + expr.span(),
                op: UnaryOperator::PostDecrement(op),
                expr: Box::new(expr),
            }));
        }
        Ok(expr)
    }

    ////
    //// PrefixUnaryExpression
    ////   : '+' SingleExpression
    ////   | '-' SingleExpression
    ////   | '!' SingleExpression
    ////   ;
    ////
    pub(crate) fn prefix_unary_expression(&mut self) -> ParseResult<SingleExpression> {
        if self.lookahead_is(TokenType::Plus) {
            let op = consume_unchecked!(self);
            let expr = self.single_expression()?;
            return Ok(SingleExpression::Unary(UnaryExpression {
                span: op.to_owned() + expr.span(),
                op: UnaryOperator::Plus(op),
                expr: Box::new(expr),
            }));
        }

        if self.lookahead_is(TokenType::Minus) {
            let op = consume_unchecked!(self);
            let expr = self.single_expression()?;
            return Ok(SingleExpression::Unary(UnaryExpression {
                span: op.to_owned() + expr.span(),
                op: UnaryOperator::Minus(op),
                expr: Box::new(expr),
            }));
        }

        if self.lookahead_is(TokenType::Not) {
            let op = consume_unchecked!(self);
            let expr = self.single_expression()?;
            return Ok(SingleExpression::Unary(UnaryExpression {
                span: op.to_owned() + expr.span(),
                op: UnaryOperator::Not(op),
                expr: Box::new(expr),
            }));
        }

        self.arguments_expression()
    }

    ////
    //// ArgumentsExpression
    ////   :  MemberIndexExpression '(' ArgumentList ')'
    ////   ;
    ////
    pub(crate) fn arguments_expression(&mut self) -> ParseResult<SingleExpression> {
        let mut expr = self.member_index_expression()?;
        while self.lookahead_is(TokenType::LeftParen) {
            let args = self.argument_list()?;
            expr = SingleExpression::Arguments(ArgumentsExpression {
                span: expr.span() + args.span(),
                ident: Box::new(expr),
                arguments: args,
            });
        }
        Ok(expr)
    }

    ////
    //// ArgumentList
    ////   :  '(' SingleExpression (',' SingleExpression)* ')'
    ////   ;
    ////
    fn argument_list(&mut self) -> ParseResult<ArgumentsList> {
        let mut arguments = vec![];
        let start = consume!(self, TokenType::LeftParen)?;
        while !self.lookahead_is(TokenType::RightParen) {
            let param = self.single_expression()?;
            arguments.push(param);
            if !self.lookahead_is(TokenType::Comma) {
                break;
            }
            // Eat the comma token
            consume_unchecked!(self);
        }
        let end = consume!(self, TokenType::RightParen)?;
        Ok(ArgumentsList {
            span: start + end,
            arguments,
        })
    }

    ////
    //// MemberIndexExpression
    ////   : MemberDotExpression '[' SingleExpression ']'
    ////   ;
    ////
    pub(crate) fn member_index_expression(&mut self) -> ParseResult<SingleExpression> {
        let mut target = self.member_dot_expression()?;
        while self.lookahead_is(TokenType::LeftBracket) {
            consume_unchecked!(self);
            let expression = self.single_expression()?;
            let end = consume!(self, TokenType::RightBracket)?;
            target = SingleExpression::MemberIndex(MemberIndexExpression {
                span: target.span() + end,
                index: Box::new(expression),
                target: Box::new(target),
            });
        }
        Ok(target)
    }

    ////
    //// MemberDotExpression
    ////   : BitwiseOrExpression '.' Identifier
    ////   ;
    ////
    pub(crate) fn member_dot_expression(&mut self) -> ParseResult<SingleExpression> {
        let target = self.this_expression()?;
        if self.lookahead_is(TokenType::Dot) {
            consume_unchecked!(self);
            let expression = self.identifier_expression()?;
            return Ok(SingleExpression::MemberDot(MemberDotExpression {
                span: target.span() + expression.span(),
                expression: Box::new(expression),
                target: Box::new(target),
            }));
        }
        Ok(target)
    }

    ////
    //// ThisExpression
    ////   :  'this'
    ////   ;
    ////
    pub(crate) fn this_expression(&mut self) -> ParseResult<SingleExpression> {
        if self.lookahead_is(TokenType::This) {
            let span = consume!(self, TokenType::This)?;
            return Ok(SingleExpression::This(ThisExpression { span }));
        }
        self.identifier_expression()
    }

    ////
    //// IdentifierExpression
    ////   : Identifier
    ////   ;
    ////
    pub(crate) fn identifier_expression(&mut self) -> ParseResult<SingleExpression> {
        if self.lookahead_is(TokenType::Identifier) {
            let ident = ident!(self)?;
            return Ok(SingleExpression::Identifier(IdentifierExpression {
                span: ident.span.to_owned(),
                ident,
            }));
        }
        // If we can't find an ident, continue by
        // trying to resolve an array literal
        self.array_literal_expression()
    }

    //// ArrayLiteralExpression
    ////   :  '[' (SingleExpression ,)* ']'
    ////   ;
    pub(crate) fn array_literal_expression(&mut self) -> ParseResult<SingleExpression> {
        if self.lookahead_is(TokenType::LeftBracket) {
            let start = consume_unchecked!(self);

            let mut elements = vec![];
            while !self.lookahead_is(TokenType::RightBracket) {
                let element = self.single_expression()?;
                elements.push(element);
                if !self.lookahead_is(TokenType::Comma) {
                    break;
                }
                // Eat the comma
                consume_unchecked!(self);
            }

            let end = consume!(self, TokenType::RightBracket)?;

            return Ok(SingleExpression::Literal(Literal::Array(ArrayLiteral {
                span: start + end,
                elements,
            })));
        };

        self.literal()
    }
}

#[cfg(test)]
mod test {
    use crate::Parser;
    use jswt_assert::assert_debug_snapshot;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_parse_array_literal() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_array_literal",
            "function test() { let x = [-1, 10, -5, \"test\"]; }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_member_dot_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_member_dot_expression",
            "function test() { a.b = 10; }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_new_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_new_expression",
            "function test() { new Class(); }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_this_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_parse_this_expression", "function main() { this; }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_member_dot_arguments_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_member_dot_arguments_expression",
            "function test() { this.set(10); }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_nested_arguments_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_nested_arguments_expression",
            "function test() { a()(); }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_nested_member_index_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_nested_member_index_expression",
            "function test() { a[0][1]; }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_argument_expression_with_expression_parameter() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_argument_expression_with_expression_parameter",
            "function main() { test(1 + 2); }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_arguments_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_arguments_expression",
            " function main() { print(6); }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_postfix_unary_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_postfix_unary_expression",
            "function test() { let x: i32 = 99--; let y = i++; }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_member_dot_is_left_associative() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_member_dot_is_left_associative",
            "function test() { this.data + 1; }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_parse_unary_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_unary_expression",
            "function test() { let x = -1 * 10 + -5; }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }
}
