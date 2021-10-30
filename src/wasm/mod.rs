mod convert;
mod encoding;
mod module;
mod section;

pub use module::Module;
pub use section::Section;

use std::error::Error;

pub trait Serialize {
    fn serialize(&self) -> Result<Vec<u8>, Box<dyn Error>>;
}
