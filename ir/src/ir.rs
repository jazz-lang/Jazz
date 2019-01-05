use crate::ty::Type;
use crate::*;

#[derive(Clone,Debug,PartialEq,Eq,PartialOrd,Ord,Hash,Copy)]
pub enum Store {
    Gpr(Register),
    Fpu(XMMRegister),
    Stack(i32),
}

impl Store {
    pub fn is_reg(&self) -> bool {
        match self {
            Store::Gpr(_) => true,
            _ => false
        }
    }

    pub fn is_freg(&self) -> bool {
        match self {
            Store::Fpu(_) => true,
            _ => false,
        }
    }
    pub fn is_stack(&self) -> bool {
        match self {
            Store::Stack(_) => true,
            _ => false,
        }
    }
    pub fn reg(&self) -> Register {
        match self {
            Store::Gpr(reg) => *reg,
            _ => panic!("Called `reg` on non-gpr store")
        }
    }

    pub fn freg(&self) -> XMMRegister {
        match self {
            Store::Fpu(fpu) => *fpu,
            _ => panic!("Called `freg` on non-fpu store")
        }
    }
    pub fn offset(&self) -> i32 {
        match self {
            Store::Stack(off) => *off,
            _ => panic!(""),
        }
    }
}

pub type Value = u32;

#[derive(Clone,Debug,Eq,Hash,PartialEq)]
pub enum Instruction
{
    Iconst(Type,Imm),
    F32Const(Ieee32),
    F64Const(Ieee64),

    Iadd(Value,Value),
    Isub(Value,Value),
    Imul(Value,Value),
    Idiv(Value,Value),

    Fadd(Value,Value),
    Fsub(Value,Value),
    Fdiv(Value,Value),
    Fmul(Value,Value),

    UseVar(usize),
    DefVar(Type),
    DeclareVar(usize,Value),

    CallDirect(String,Vec<Value>),
    CallIndirect(Value,Vec<Value>),
}

use jazz_jit::assembler::*;
use jazz_jit::assembler_x64::*;
use jazz_jit::constants_x64::*;
use jazz_jit::utils::align;
use std::collections::HashSet;
use crate::Linkage;

#[derive(Clone,Debug)]
pub struct Module {

}
use std::collections::HashMap;

#[derive(Clone,Debug)]
pub struct Function {
    pub name: String,
    asm: Assembler,
    vstore: HashMap<Value,Store>,
    used: HashSet<Store>,
    variables: HashSet<Value>,
    pub linkage: Linkage,
    pub ret_ty: Type,
    pub args: Vec<Type>,
    value_typs: HashMap<Value,Type>,
    pub fixups: Vec<CallFixup>,  
    align: i32,  
    vidx: u32,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
pub struct CallFixup {
    pub name: String,
    pub pos: usize,
}


impl Function {
    pub fn new(name: String,linkage: Linkage) -> Function {
        Function {
            name,
            linkage,
            ret_ty: Type::Void,
            vstore: HashMap::new(),
            args: Vec::new(),
            value_typs: HashMap::new(),
            used: HashSet::new(),
            asm: Assembler::new(),
            fixups: Vec::new(),
            align: 0,
            variables: HashSet::new(),
            vidx: 0,
        }
    }

    pub fn allocate_in_stack(&mut self,ty: Type) -> i32 {
        let size = ty.to_machine().size();
        let offset = align(self.align + size as i32, size as i32);
        self.align = offset;
        offset
    }

    /// Get first available register or place in stack
    fn first_available_place(&mut self,ty: Type,is_var: bool) -> Store {
        if !ty.is_float() {
            for gpr in GPR_REGS.iter() {
                if !self.used.contains(&Store::Gpr(*gpr)) {
                    self.used.insert(Store::Gpr(*gpr));

                    return Store::Gpr(*gpr);
                }
            }

            let off = self.allocate_in_stack(ty);
            // temp values stored in [rbp - offset], variables in [rbp + offset]
            if is_var {
                return Store::Stack(off);
            } else {
                return Store::Stack(-off);
            }
        } else {
            for fpu in FPU_REGS.iter() {
                if !self.used.contains(&Store::Fpu(*fpu)) {
                    self.used.insert(Store::Fpu(*fpu));
                    return Store::Fpu(*fpu);
                }
            }

            let off = self.allocate_in_stack(ty);
            if is_var {
                return Store::Stack(off);
            } else {
                return Store::Stack(-off);
            }
        }
    }


    pub fn iconst(&mut self,ty: Type,imm: impl Into<Imm>) -> Value {
        assert!(!ty.is_float(),"Called iconst on float type");
        let place = self.first_available_place(ty, false);
        let value = self.vidx;
        self.vidx += 1;
        self.vstore.insert(value, place);
        self.value_typs.insert(value,ty);
        if place.is_reg() {
            self.asm.load_int_const(ty.to_machine(),place.reg(),imm.into());
        } else {
            if self.used.contains(&Store::Gpr(R11)) {
                emit_pushq_reg(&mut self.asm,R11);
            }
            self.asm.load_int_const(ty.to_machine(),R11,imm.into());
            self.asm.store_mem(ty.to_machine(),Mem::Local(place.offset()),Reg::Gpr(R11));
            if self.used.contains(&Store::Gpr(R11)) {
                emit_popq_reg(&mut self.asm, R11);
            }
        }
        value
    }

    pub fn free(&mut self,v: Value) {
        let place: &Store = &self.vstore.get(&v).unwrap().clone();

        if place.is_reg() || place.is_freg() {
            self.used.remove(place);
            self.vstore.remove(&v);
        } else {
            self.align -= place.offset();
            self.vstore.remove(&v);
            self.used.remove(place);
        }

    }

    pub fn asm<'r>(&'r self) -> &'r Assembler {
        &self.asm
    }

    pub fn is_used_gpr(&self,gpr: Register) -> bool {
        self.used.contains(&Store::Gpr(gpr))
    }

    pub fn is_used_freg(&self,f: XMMRegister) -> bool {
        self.used.contains(&Store::Fpu(f))
    }
    
    pub fn iadd(&mut self,x: Value,y: Value) -> Value 
    {
        let value = self.vidx;
        self.vidx += 1;
        let x_place = self.vstore.get(&x).unwrap().clone();
        let y_place = self.vstore.get(&y).unwrap().clone();
        let x_ty = self.value_typs.get(&x).unwrap().clone();
        let x_is_reg = x_place.is_reg();
        let y_is_reg = y_place.is_reg();
        self.free(x);
        self.free(y);
        let new_place = self.first_available_place(x_ty, false);
        self.vstore.insert(value,new_place);
        self.value_typs.insert(value,x_ty);
        if x_is_reg && y_is_reg {
            if new_place.is_reg() {
                self.asm.int_add(x_ty.to_machine(),new_place.reg(),x_place.reg(),y_place.reg());
            } else {
                if self.is_used_gpr(R11) {
                    emit_pushq_reg(&mut self.asm, R11);
                }

                self.asm.int_add(x_ty.to_machine(),R11,x_place.reg(),y_place.reg());
                self.asm.store_mem(x_ty.to_machine(),Mem::Local(new_place.offset()),Reg::Gpr(R11));
                if self.is_used_gpr(R11) {
                    emit_popq_reg(&mut self.asm,R11);
                }
            }
        } else {
            if self.is_used_gpr(R11) {
                emit_pushq_reg(&mut self.asm, R11);
            }
            if self.is_used_gpr(R10) && !x_is_reg {
                emit_pushq_reg(&mut self.asm, R10);
            }
            if x_is_reg {
                self.asm.load_mem(x_ty.to_machine(),Reg::Gpr(R11),Mem::Local(y_place.offset()));
                self.asm.int_add(x_ty.to_machine(),R11,x_place.reg(),R11);
                if new_place.is_reg() {
                    emit_mov_reg_reg(&mut self.asm, x_ty.x64(), R11, new_place.reg());
                } else {
                    self.asm.store_mem(x_ty.to_machine(),Mem::Local(new_place.offset()),Reg::Gpr(R11));
                }
            } else {
                self.asm.load_mem(x_ty.to_machine(),Reg::Gpr(R10),Mem::Local(y_place.offset()));
                self.asm.load_mem(x_ty.to_machine(),Reg::Gpr(R11),Mem::Local(y_place.offset()));
                self.asm.int_add(x_ty.to_machine(),R11,R10,R11);
                if new_place.is_reg() {
                    emit_mov_reg_reg(&mut self.asm, x_ty.x64(), R11, new_place.reg());
                } else {
                    self.asm.store_mem(x_ty.to_machine(),Mem::Local(new_place.offset()),Reg::Gpr(R11));
                }
            }
            if self.is_used_gpr(R10) && !x_is_reg {
                emit_popq_reg(&mut self.asm,R10);
            }
            if self.is_used_gpr(R11) {
                emit_popq_reg(&mut self.asm,R11);
            }   
        }
        value
    }

    


}