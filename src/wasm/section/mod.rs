mod code;
mod export;
mod function;
mod import;
mod type_;

use std::{error::Error, io::Write};
use self::{
    code::CodeSection, export::ExportSection, function::FunctionSection, type_::TypeSection,
};

use super::Serialize;

pub enum Section<'a> {
    Type(TypeSection<'a>),
    Function(FunctionSection<'a>),
    Export(ExportSection<'a>),
    Code(CodeSection),
}

impl<'a> Section<'a> {
    pub fn type_section(functions: Vec<FunctionType<'a>>) -> Self {
        Section::Type(TypeSection::new(functions))
    }

    pub fn function_section(functions: Vec<FunctionType<'a>>) -> Self {
        Section::Function(FunctionSection::new(functions))
    }

    pub fn export_section(functions: Vec<FunctionType<'a>>) -> Self {
        Section::Export(ExportSection::new(functions))
    }

    pub fn code_section() -> Self {
        Section::Code(CodeSection::new())
    }
}

impl<'a> Serialize for Section<'a> {
    fn serialize(&self) -> Result<Vec<u8>, Box<dyn Error>> {
        match self {
            Section::Type(inner) => inner.serialize(),
            Section::Function(inner) => inner.serialize(),
            Section::Export(inner) => inner.serialize(),
            Section::Code(inner) => inner.serialize(),
        }
    }
}

pub enum ValType {
    I32,
    F32,
    Void,
}

impl From<&ValType> for u8 {
    fn from(ty: &ValType) -> Self {
        match ty {
            ValType::I32 => 0x7fu8,
            ValType::F32 => 0x7du8,
            ValType::Void => 0x00u8,
        }
    }
}

impl From<ValType> for u8 {
    fn from(ty: ValType) -> Self {
        match ty {
            ValType::I32 => 0x7fu8,
            ValType::F32 => 0x7du8,
            ValType::Void => 0x00u8,
        }
    }
}

pub struct FunctionType<'a> {
    pub name: &'a str,
    pub export: bool,
    pub params: Vec<ValType>,
    pub ret: ValType,
}

impl<'a> Serialize for FunctionType<'a> {
    fn serialize(&self) -> Result<Vec<u8>, Box<dyn Error>> {
        let mut res = vec![
            0x60u8, // Function type ID
        ];

        let params = self
            .params
            .iter()
            .map(|param| param.into())
            .collect::<Vec<u8>>();

        // Param count
        res.push(self.params.len() as u8);
        // number of return values
        res.write_all(&params)?;

        // Return value count
        if let ValType::Void = self.ret {
            // Void functions do not count towards
            // return value count
            res.push(0x00);
        } else {
            // only single return values are supported for now
            res.push(0x01);
            res.push((&self.ret).into());
        };

        Ok(res)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_serialize_type_section() {
        let section = TypeSection {
            functions: vec![FunctionType {
                name: "",
                export: false,
                params: vec![ValType::I32, ValType::I32],
                ret: ValType::I32,
            }],
        };

        let actual = section.serialize().unwrap();
        assert_eq!(
            actual,
            [0x01, 0x07, 0x01, 0x60, 0x02, 0x7F, 0x7F, 0x01, 0x7F]
        );
    }

    #[test]
    fn test_serialize_type_section_with_2_functions() {
        let section = TypeSection {
            functions: vec![
                FunctionType {
                    name: "",
                    export: false,
                    params: vec![ValType::I32, ValType::I32],
                    ret: ValType::I32,
                },
                FunctionType {
                    name: "",
                    export: false,
                    params: vec![],
                    ret: ValType::Void,
                },
            ],
        };

        let actual = section.serialize().unwrap();
        assert_eq!(
            actual,
            [0x01, 0x0a, 0x02, 0x60, 0x02, 0x7F, 0x7F, 0x01, 0x7F, 0x60, 0x00, 0x00]
        );
    }

    #[test]
    fn test_serialize_function_type_with_2_i32_params() {
        let function = FunctionType {
            name: "",
            export: false,
            params: vec![ValType::I32, ValType::I32],
            ret: ValType::I32,
        };
        let actual = function.serialize().unwrap();
        assert_eq!(actual, [0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f]);
    }

    #[test]
    fn test_serialize_void_function_type_with_2_i32_params() {
        let function = FunctionType {
            name: "",
            export: false,
            params: vec![ValType::I32, ValType::I32],
            ret: ValType::Void,
        };
        let actual = function.serialize().unwrap();
        assert_eq!(actual, [0x60, 0x02, 0x7f, 0x7f, 0x00]);
    }

    #[test]
    fn test_serialize_void_function_type_with_0_params() {
        let function = FunctionType {
            name: "",
            export: false,
            params: vec![],
            ret: ValType::Void,
        };
        let actual = function.serialize().unwrap();
        assert_eq!(actual, [0x60, 0x00, 0x00]);
    }

    #[test]
    fn test_serialize_function_section_with_no_functions() {
        let function = FunctionSection { functions: vec![] };
        let actual = function.serialize().unwrap();
        assert_eq!(actual, [0x03, 0x01, 0x00]);
    }

    #[test]
    fn test_serialize_function_section_with_1_void_function() {
        let function = FunctionSection {
            functions: vec![FunctionType {
                name: "",
                export: false,
                params: vec![],
                ret: ValType::Void,
            }],
        };
        let actual = function.serialize().unwrap();
        assert_eq!(actual, [0x03, 0x02, 0x01, 0x00]);
    }

    #[test]
    fn test_serialize_function_section_with_2_functions() {
        let function = FunctionSection {
            functions: vec![
                FunctionType {
                    name: "",
                    export: false,
                    params: vec![ValType::I32, ValType::I32],
                    ret: ValType::I32,
                },
                FunctionType {
                    name: "",
                    export: false,
                    params: vec![],
                    ret: ValType::Void,
                },
            ],
        };
        let actual = function.serialize().unwrap();
        assert_eq!(actual, [0x03, 0x03, 0x2, 0x00, 0x01]);
    }

    #[test]
    fn test_serialize_export_section_with_1_function() {
        let export = ExportSection {
            functions: vec![FunctionType {
                name: "main",
                export: true,
                params: vec![ValType::I32, ValType::I32],
                ret: ValType::I32,
            }],
        };
        let actual = export.serialize().unwrap();
        assert_eq!(
            actual,
            [0x07, 0x08, 0x01, 0x04, 0x6d, 0x61, 0x69, 0x6e, 0x00, 0x00]
        );
    }

    #[test]
    fn test_serialize_export_section_with_2_functions() {
        let export = ExportSection {
            functions: vec![
                FunctionType {
                    name: "main",
                    export: true,
                    params: vec![ValType::I32, ValType::I32],
                    ret: ValType::I32,
                },
                FunctionType {
                    name: "test",
                    export: true,
                    params: vec![ValType::I32, ValType::I32],
                    ret: ValType::I32,
                },
            ],
        };
        let actual = export.serialize().unwrap();
        assert_eq!(
            actual,
            [
                0x07, 0x0F, 0x02, // Section meta
                0x04, 0x6d, 0x61, 0x69, 0x6e, 0x00, 0x00, // First function
                0x04, 0x74, 0x65, 0x73, 0x74, 0x00, 0x01 // Second function
            ]
        );
    }
}
