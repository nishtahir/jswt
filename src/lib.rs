#[macro_use]
extern crate lazy_static;

mod ast;
mod errors;
mod parser;
mod tokenizer;

// pub mod wasm;

pub use parser::Parser;
pub use tokenizer::Tokenizer;
pub use errors::*;