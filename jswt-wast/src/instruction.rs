use crate::ValueType;

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Local(&'static str, ValueType),
    LocalGet(&'static str),
    LocalSet(&'static str, Vec<Instruction>),
    GlobalSet(&'static str, Vec<Instruction>),
    GlobalGet(&'static str),
    I32Const(i32),
    I32Add,
    I32Sub,
    I32Mul,
    I32Eq,
    I32Neq,
    I32And,
    I32Or,
    I32Gt,
    I32Ge,
    I32Lt,
    I32Le,
    Return,
    If(Vec<Instruction>, Vec<Instruction>),
    Call(&'static str, Vec<Instruction>),
    // A meta instruction not part of the wasm specification but
    // an instruction to the code generator to inline raw instructions
    // exactly as they are provided.
    // Warning: No checks are performed on the input before they are inlined
    RawWast(&'static str),
}

impl From<&Instruction> for String {
    fn from(isr: &Instruction) -> Self {
        match isr {
            Instruction::I32Const(value) => format!("(i32.const {})", value),
            Instruction::I32Add => "(i32.add)".into(),
            Instruction::I32Sub => "(i32.sub)".into(),
            Instruction::I32Mul => "(i32.mul)".into(),
            Instruction::I32Eq => "(i32.eq)".into(),
            Instruction::I32Neq => "(i32.ne)".into(),
            Instruction::I32Gt => "(i32.gt_s)".into(),
            Instruction::I32Ge => "(i32.ge_s)".into(),
            Instruction::I32Lt => "(i32.lt_s)".into(),
            Instruction::I32Le => "(i32.le_s)".into(),
            Instruction::Return => "(return)".into(),
            Instruction::Call(name, args) => {
                format!("(call ${} {})", name, args.to_string())
            }
            Instruction::RawWast(text) => format!("{}", text),
            Instruction::LocalGet(name) => format!("(local.get ${})", name),
            Instruction::LocalSet(name, args) => {
                format!("(local.set ${} {})", name, args.to_string())
            }
            Instruction::GlobalSet(name, args) => {
                format!("(global.set ${} {})", name, args.to_string())
            }
            Instruction::Local(name, ty) => format!("(local ${} {})", name, ty),
            Instruction::If(cons, alt) => {
                // https://github.com/WebAssembly/wabt/issues/1075
                // The wat format requires that you annotate any blocks that return values with their signature. 
                // If no signature is provided, it is assumed that the block has no parameters and no results.
                if cons.contains(&Instruction::Return) && alt.contains(&Instruction::Return) {
                    format!(
                        // TODO type check here and annotate appropriately
                        "(if (result i32) (then {}) (else {}))",
                        cons.to_string(),
                        alt.to_string()
                    )
                } else {
                    format!(
                        "(if (then {}) (else {}))",
                        cons.to_string(),
                        alt.to_string()
                    )
                }
            }
            Instruction::I32And => "(i32.and)".into(),
            Instruction::I32Or => "(i32.or)".into(),
            Instruction::GlobalGet(name) => format!("(global.get ${})", name),
        }
    }
}

impl Stringify for Vec<Instruction> {
    fn to_string(&self) -> String {
        self.iter()
            .map(String::from)
            .collect::<Vec<String>>()
            .join(" ")
    }
}

pub trait Stringify {
    fn to_string(&self) -> String;
}
