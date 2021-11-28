#[derive(Debug, Default, PartialEq)]
pub struct Module {
    pub imports: Vec<Import>,
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
    pub name: &'static str,
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
    Local(&'static str, ValueType),
    LocalGet(&'static str),
    LocalSet(&'static str, Vec<Instruction>),
    GlobalSet(&'static str, Vec<Instruction>),
    I32Const(i32),
    I32Add,
    I32Sub,
    I32Mul,
    I32Eq,
    I32Neq,
    Return,
    If(Vec<Instruction>, Vec<Instruction>),
    Call(&'static str, Vec<Instruction>),

    // A meta instruction not part of the wasm specification but
    // an instruction to the code generator to inline raw instructions
    // exactly as they are provided.
    // Warning: No checks are performed on the input before they are inlined
    RawWast(&'static str),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ValueType {
    I32,
}

impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::I32 => f.write_str("i32"),
        }
    }
}

impl From<&Instruction> for String {
    fn from(isr: &Instruction) -> Self {
        match isr {
            Instruction::I32Const(value) => format!("(i32.const {})", value),
            Instruction::I32Add => "(i32.add)".into(),
            Instruction::I32Sub => "(i32.sub)".into(),
            Instruction::I32Mul => "(i32.mul)".into(),
            Instruction::Return => "(return)".into(),
            Instruction::Call(name, args) => {
                format!("(call ${} {})", name, instructions_to_string(args))
            }
            Instruction::RawWast(text) => format!("{}", text),
            Instruction::LocalGet(name) => format!("(local.get ${})", name),
            Instruction::LocalSet(name, args) => {
                format!("(local.set ${} {})", name, instructions_to_string(args))
            }
            Instruction::GlobalSet(name, args) => {
                format!("(global.set ${} {})", name, instructions_to_string(args))
            }
            Instruction::Local(name, ty) => format!("(local ${} {})", name, ty),
            Instruction::I32Eq => "(i32.eq)".into(),
            Instruction::I32Neq => "(i32.ne)".into(),
            Instruction::If(cons, alt) => format!(
                "(if (then {}) (else {}))",
                instructions_to_string(cons),
                instructions_to_string(alt)
            ),
        }
    }
}

fn instructions_to_string(instructions: &Vec<Instruction>) -> String {
    instructions
        .iter()
        .map(String::from)
        .collect::<Vec<String>>()
        .join(" ")
}
// impl From<Vec<Instruction>> for String {
//     fn from(instructions: Vec<Instruction>) -> Self {
//         instructions.iter()
//         .map(String::from)
//         .collect::<Vec<String>>()
//         .join(" ")
//     }
// }

#[derive(Debug, PartialEq)]
pub enum WastSymbol {
    Function(&'static str),
    Param(&'static str, ValueType),
    Local(&'static str, ValueType),
    Global(&'static str, ValueType),
}

#[derive(Debug, PartialEq)]
pub enum Import {
    Function(FunctionImport),
}

impl Import {
    pub fn as_function(&self) -> Option<&FunctionImport> {
        // TODO - add other import variants
        let Self::Function(v) = self;
        Some(v)
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionImport {
    pub type_idx: usize,
    pub name: &'static str,
    pub module: &'static str,
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

        // Add Imports
        // (import "console" "log" (func $log (param i32) (param i32)))
        self.imports
            .iter()
            .filter_map(|e| e.as_function())
            .for_each(|e| {
                let signature = self.function_type_signature(e.type_idx, e.name);
                wat += &format!("(import \"{}\" \"{}\" ({}))", e.module, e.name, signature);
            });

        // TODO - make this configurable as part of the WAST IR
        // Add built in memory
        wat += " (memory $0 1)";

        for function in self.functions.iter() {
            wat += "(";
            wat += &self.function_type_signature(function.type_idx, function.name);
            for isr in &function.instructions {
                wat += &String::from(isr);
            }
            wat += ")";
        }

        // Generate Export Definitions
        // export built in memory
        wat += "(export \"memory\" (memory $0))";
        // Export functions
        // (export "addTwo" (func $addTwo))
        self.exports
            .iter()
            .filter_map(|e| e.as_function())
            .for_each(|e| {
                wat += &format!("(export \"{name}\" (func ${name}))", name = e.name);
            });

        wat += ")";
        wat
    }

    fn function_type_signature(&self, type_idx: usize, name: &str) -> String {
        let mut wat = format!("func ${} ", name);
        let type_def = &self.types[type_idx];
        match type_def {
            Type::Function(ty) => {
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
            imports: vec![],
            exports: vec![],
            types: vec![Type::Function(FunctionType {
                params: vec![],
                ret: None,
            })],
            functions: vec![Function {
                name: "test",
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
            "(module(func $test(i32.const 1)(i32.const 2)(i32.add)(return)))",
            &module.as_wat()
        );
    }
}
