#[macro_use]
extern crate lazy_static;

pub mod assert;
pub mod codegen;
pub mod common;
pub mod errors;
mod parser;
mod semantics;
mod tokenizer;
pub mod wast;

pub use parser::Parser;
pub use semantics::Resolver;
pub use tokenizer::Tokenizer;
