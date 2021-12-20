use jswt_common::{Span, Spannable};

#[derive(Debug, PartialEq)]
pub enum VariableModifier {
    Let(Span),
    Const(Span),
}

impl Spannable for VariableModifier {
    fn span(&self) -> Span {
        match self {
            VariableModifier::Let(span) => span.to_owned(),
            VariableModifier::Const(span) => span.to_owned(),
        }
    }
}
