mod error;
mod globals;
mod locals;
mod types;

pub use error::SemanticError;
pub use globals::GlobalSemanticResolver;
pub use locals::LocalSemanticResolver;
pub use types::TypeResolver;