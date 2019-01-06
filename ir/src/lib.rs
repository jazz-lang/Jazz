#[warn(rust_2018_idioms)]

use jazz_jit::constants_x64::*;

pub mod ir;
pub mod ty;
pub mod module;
pub mod dylib;
pub type Imm    = i64;
pub type Ieee32 = u32;
pub type Ieee64 = u64;
pub type Bool   = i8;

#[derive(Clone,Debug,PartialEq,Eq)]
pub enum Linkage {
    Extern(*const u8),
    Dylib(&'static str),
    Local,
}

pub const GPR_REGS: [Register;9] = [RCX,R8,R9,R10,R11,R12,R13,R14,R15];
pub const CALEE_PUSH: [Register;8] = [R8,R9,R10,R11,R12,R13,R14,R15];
pub const FPU_REGS: [XMMRegister;6] = [XMM10,XMM11,XMM12,XMM13,XMM14,XMM15];
pub const TMP1: Register = R10;
pub const TMP2: Register = R11;
pub const TMP3: Register = RAX;