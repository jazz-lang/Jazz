#[derive(Clone,Debug,PartialEq,Eq,Copy,PartialOrd,Ord,Hash)]
pub enum Type {
    Int(u8),
    F32,
    F64,
    Bool(u8),
    Void,
}

use jazz_jit::MachineMode;

impl Type {
    pub fn to_machine(&self) -> MachineMode {
        match self {
            Type::Int(size) => match size {
                x if *x <= 8 => MachineMode::Int8,
                x if *x <= 32 => MachineMode::Int32,
                x if *x <= 64 => MachineMode::Int64,
                x => panic!("Unsupported Int<{}>",x),
            }
            Type::Bool(size) => match size {
                x if *x <= 8 => MachineMode::Int8,
                x if *x <= 32 => MachineMode::Int32,
                x if *x <= 64 => MachineMode::Int64,
                x => panic!("Unsupported Bool<{}>",x),
            }
            Type::Void => MachineMode::Int32,
            Type::F32 => MachineMode::Float32,
            Type::F64 => MachineMode::Float64,
            
        }
    }

    pub fn x64(&self) -> u8 {
        match self {
            Type::Int(size) => (*size > 32) as u8,
            Type::F64 => 1,
            _ => 0,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::F32 | Type::F64 => true,
            _ => false,
        }
    }
}

pub use self::Type::*;