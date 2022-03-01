use std::borrow::Cow;

#[derive(Debug, Clone)]
pub enum TokenizerError {
    UnreconizedToken {
        file: String,
        // TODO - replace this with Span
        token: Cow<'static, str>,
        offset: usize,
    },
    UnexpectedEof,
}
