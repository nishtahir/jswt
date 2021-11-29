pub mod codegen;
pub mod common;
pub mod errors;
mod semantics;
pub mod wast;

pub use semantics::Resolver;
