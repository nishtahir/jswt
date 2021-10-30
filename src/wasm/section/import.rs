use crate::wasm::Serialize;
use std::error::Error;
pub struct ImportSection {}

impl ImportSection {
    pub fn new() -> Self {
        Self {}
    }
}

impl<'a> Serialize for ImportSection {
    fn serialize(&self) -> Result<Vec<u8>, Box<dyn Error>> {
        let test = vec![0x0a, 0x07, 0x01, 0x05, 0x00, 0x41, 0x2a, 0x0f, 0x0b];
        Ok(test)
    }
}
