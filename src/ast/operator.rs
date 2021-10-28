use super::span::Span;

#[derive(Debug, PartialEq)]

pub enum Operator {
    Plus(Span),
}
