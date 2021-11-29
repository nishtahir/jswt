#[derive(Debug, Clone)]
pub enum TokenizerError {
    UnreconizedToken {
        file: String,
        token: &'static str,
        offset: usize,
    },
    UnexpectedEof,
}
