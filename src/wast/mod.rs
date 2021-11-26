#[derive(Debug, Default, PartialEq)]
pub struct Module {
    pub types: Vec<Type>,
    pub functions: Vec<Function>,
    pub exports: Vec<Export>,
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Function(FunctionType),
}

#[derive(Debug, PartialEq)]
pub struct FunctionType {
    pub params: Vec<ValueType>,
    pub ret: Option<ValueType>,
}

#[derive(Debug, Default, PartialEq)]
pub struct Function {
    pub type_idx: usize,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, PartialEq)]
pub struct Parameter {
    name: &'static str,
    ty: ValueType,
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    I32Const(i32),
    I32Add,
    Return,
}

#[derive(Debug, PartialEq)]
pub enum ValueType {
    I32,
}

impl From<&Instruction> for String {
    fn from(isr: &Instruction) -> Self {
        match isr {
            Instruction::I32Const(value) => format!("i32.const {}", value),
            Instruction::I32Add => "i32.add".into(),
            Instruction::Return => "return".into(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum WastSymbol {
    Function,
    Param(usize),
    Local,
    Global,
}

#[derive(Debug, PartialEq)]
pub enum Export {
    Global,
    Function(FunctionExport),
}

impl Export {
    pub fn as_function(&self) -> Option<&FunctionExport> {
        if let Self::Function(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct FunctionExport {
    pub function_idx: usize,
    pub name: &'static str,
}

impl Module {
    pub fn as_wat(&self) -> String {
        let mut wat = "(module".to_string();

        for (i, function) in self.functions.iter().enumerate() {
            wat += "(func ";
            let type_def = &self.types[function.type_idx];
            match type_def {
                Type::Function(ty) => {
                    // Generate Export Definition
                    // (export "addTwo")
                    let maybe_export = self
                        .exports
                        .iter()
                        .filter_map(|e| e.as_function())
                        .find(|e| e.function_idx == i);

                    if let Some(export) = maybe_export {
                        wat += &format!("(export \"{}\")", export.name);
                    }

                    // Generate Type Definition
                    // function(p1: i32, p2: i32) : i32
                    // (param $p1 i32) (param $p2 i32) (result i32)
                    for _ in &ty.params {
                        // TODO - support more than just i32
                        wat += "(param i32)";
                    }

                    if ty.ret.is_some() {
                        wat += "(result i32)";
                    }
                }
            }

            for isr in &function.instructions {
                wat += "(";
                wat += &String::from(isr);
                wat += ")";
            }
            wat += ")";
        }

        wat += ")";
        wat
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::assert_str_eq;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_wat_generation_for_simple_function() {
        let module = Module {
            exports: vec![],
            types: vec![Type::Function(FunctionType {
                params: vec![],
                ret: None,
            })],
            functions: vec![Function {
                type_idx: 0,
                instructions: vec![
                    Instruction::I32Const(1),
                    Instruction::I32Const(2),
                    Instruction::I32Add,
                    Instruction::Return,
                ],
            }],
        };

        assert_str_eq!(
            "(module(func (i32.const 1)(i32.const 2)(i32.add)(return)))",
            &module.as_wat()
        );
    }
}
