use crate::ty::Type;
use crate::*;
use jazz_jit::MachineMode;

#[cfg(target_family="windows")]
const GPR_ARGS: [Register;4] = [RCX,RDX,R8,R9];
#[cfg(target_family="windows")]
const FPU_ARGS: [Register;4] = [XMM0,XMM1,XMM2,XMM3];
#[cfg(target_family="unix")]
const GPR_ARGS: [Register;6] = [RDI,RSI,RDX,RCX,R8,R9];
#[cfg(target_family="unix")]
const FPU_ARGS: [XMMRegister;7] = [XMM0,XMM1,XMM3,XMM4,XMM5,XMM6,XMM7];

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
use jazz_jit::utils::align;
use std::collections::HashSet;
use crate::Linkage;


use std::collections::HashMap;

#[derive(Clone,Debug)]
pub struct Function {
    pub name: String,
    asm: Assembler,
    vstore: HashMap<Value,Store>,
    used: HashSet<Store>,
    variables: HashMap<u32,i32>,
    vars_ty: HashMap<u32,Type>,
    pub linkage: Linkage,
    pub ret_ty: Type,
    pub args: Vec<Type>,
    value_typs: HashMap<Value,Type>,
    pub fixups: Vec<Fixup>,  
    align: i32,  
    vidx: u32,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
pub struct Fixup {
    pub global_name: String,
    pub pos: usize,
}


impl Function {
    pub fn new(name: String,linkage: Linkage) -> Function {
        let mut asm = Assembler::new();
        emit_pushq_reg(&mut asm, RBP);
        emit_mov_reg_reg(&mut asm, 1,RSP,RBP);


        Function {
            name,
            linkage,
            ret_ty: Type::Void,
            vstore: HashMap::new(),
            args: Vec::new(),
            value_typs: HashMap::new(),
            used: HashSet::new(),
            asm: asm,
            fixups: Vec::new(),
            align: 0,
            vars_ty: HashMap::new(),
            variables: HashMap::new(),
            vidx: 0,
        }
    }

    pub fn allocate_in_stack(&mut self,ty: Type) -> i32 {
        let size = ty.to_machine().size();
        let offset = align(self.align + size as i32, size as i32);
        self.align = offset;
        offset
    }

    pub fn declare_var(&mut self,v_id /* variable id */: u32,vtype: Type /* variable type */) -> u32 {
        let offset = -self.allocate_in_stack(vtype);

        self.variables.insert(v_id,offset);
        self.vars_ty.insert(v_id,vtype);
        v_id
    }
    /// Initialize variable
    /// 
    /// type of value and variable must be same
    pub fn def_var(&mut self,v_id: u32,v: Value) {
        let v_typ: Type = self.vars_ty.get(&v_id).unwrap().clone();
        let value_typ: Type = self.value_typs.get(&v).unwrap().clone();
        assert_eq!(v_typ,value_typ, "Expected `{:?}`,found {:?}",v_typ,value_typ);
        let value: Store = self.vstore.get(&v).unwrap().clone();
        let var_off = self.variables.get(&v_id).expect("Variable doesn't exists").clone();
        if value.is_reg() {
           self.asm.store_mem(value_typ.to_machine(),Mem::Local(var_off),Reg::Gpr(value.reg()));
        } else if value.is_freg() {
            self.asm.store_mem(value_typ.to_machine(),Mem::Local(var_off),Reg::Float(value.freg()));
        } else {
            if value_typ.is_float() {
                self.asm.load_mem(value_typ.to_machine(),Reg::Float(XMM0),Mem::Local(value.offset()));
                self.asm.store_mem(value_typ.to_machine(),Mem::Local(var_off),Reg::Float(XMM0));
            } else {
                self.asm.load_mem(value_typ.to_machine(),Reg::Gpr(RAX),Mem::Local(value.offset()));
                self.asm.store_mem(value_typ.to_machine(),Mem::Local(var_off),Reg::Gpr(RAX));
            }
        }
        self.free(v);
    }

    pub fn use_var(&mut self,v_id: u32) -> Value {
        let value = self.vidx;
        self.vidx += 1;
        let variable_ty: Type = self.vars_ty.get(&v_id).expect("Variable not defined").clone();
        let variable_off = self.variables.get(&v_id).unwrap().clone();

        let place = self.first_available_place(variable_ty, false);
        self.value_typs.insert(value, variable_ty);
        self.vstore.insert(value,place);
        if variable_ty.is_float() {
            self.asm.load_mem(variable_ty.to_machine(),Reg::Float(XMM0),Mem::Local(variable_off));
            if place.is_freg() {
                if variable_ty.x64() > 0 {
                    movsd(&mut self.asm, place.freg(), XMM0);
                } else {
                    movss(&mut self.asm,place.freg(),  XMM0);
                }
            } else {
                self.asm.store_mem(variable_ty.to_machine(),Mem::Local(place.offset()),Reg::Float(XMM0));
            }
        } else {
            self.asm.load_mem(variable_ty.to_machine(),Reg::Gpr(RAX),Mem::Local(variable_off));
            if place.is_reg() {
                emit_mov_reg_reg(&mut self.asm, variable_ty.x64(), RAX, place.reg());
            } else {
                self.asm.store_mem(variable_ty.to_machine(),Mem::Local(place.offset()),Reg::Gpr(RAX));
            }
        }


        value
        
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

    
    /// Create integer constant
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


    /// Free some value, e.g remove from stack or clear register
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

    pub fn asm_mut<'r>(&'r mut self) -> &'r mut Assembler {
        &mut self.asm
    }

    pub fn is_used_gpr(&self,gpr: Register) -> bool {
        self.used.contains(&Store::Gpr(gpr))
    }

    pub fn is_used_freg(&self,f: XMMRegister) -> bool {
        self.used.contains(&Store::Fpu(f))
    }
    

    fn bin_int(&mut self,x: Value,y: Value,f: &Fn(&mut Assembler,MachineMode,Register,Register,Register)) -> Value {
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
                f(&mut self.asm,x_ty.to_machine(),new_place.reg(),x_place.reg(),y_place.reg());
                //self.asm.int_add(x_ty.to_machine(),new_place.reg(),x_place.reg(),y_place.reg());
            } else {
                if self.is_used_gpr(R11) {
                    emit_pushq_reg(&mut self.asm, R11);
                }
                f(&mut self.asm,x_ty.to_machine(),R11,x_place.reg(),y_place.reg());
                //self.asm.int_add(x_ty.to_machine(),R11,x_place.reg(),y_place.reg());
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
                f(&mut self.asm,x_ty.to_machine(),R11,x_place.reg(),R11);
                //self.asm.int_add(x_ty.to_machine(),R11,x_place.reg(),R11);
                if new_place.is_reg() {
                    emit_mov_reg_reg(&mut self.asm, x_ty.x64(), R11, new_place.reg());
                } else {
                    self.asm.store_mem(x_ty.to_machine(),Mem::Local(new_place.offset()),Reg::Gpr(R11));
                }
            } else {
                self.asm.load_mem(x_ty.to_machine(),Reg::Gpr(R10),Mem::Local(y_place.offset()));
                self.asm.load_mem(x_ty.to_machine(),Reg::Gpr(R11),Mem::Local(y_place.offset()));
                f(&mut self.asm,x_ty.to_machine(),R11,R10,R11);
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



    /// Integer addition
    pub fn iadd(&mut self,x: Value,y: Value) -> Value 
    {
        self.bin_int(x, y, &Assembler::int_add)
    }
    /// Integer multiplication
    pub fn imul(&mut self,x: Value,y: Value) -> Value {
        self.bin_int(x,y, &Assembler::int_mul)
    }
    /// Integer substraction
    pub fn isub(&mut self,x: Value,y: Value) -> Value {
        self.bin_int(x,y,&Assembler::int_sub)
    }
    /// Integer division
    pub fn idiv(&mut self,x: Value,y: Value) -> Value {
        self.bin_int(x, y,&Assembler::int_div)
    }
    
    pub fn imod(&mut self,x: Value,y: Value) -> Value {
        self.bin_int(x, y,&Assembler::int_mod)
    }

    /// Currently only loading Int<64> or pointer supported
    pub fn load_global_data(&mut self,global_name: &str) -> Value {
        let place = self.first_available_place(Type::Int(64),false);
        let value = self.vidx;
        self.vidx += 1;
        self.asm.load_int_const(MachineMode::Ptr,RAX,0);
        self.fixups.push(Fixup {
            global_name: global_name.to_string(),
            pos: self.asm.pos() - 8,
        });
        self.value_typs.insert(value,Type::Int(64));
        self.vstore.insert(value,place);

        self.store_value(Store::Gpr(RAX), place, Type::Int(64));   

        value
    }

    pub fn call_direct(&mut self,f: Value,args: &[Value],ret: Type) -> Value {
        let value = self.vidx;
        self.vidx += 1;


        let mut fpc = 0;
        let mut pc = 0;
        let mut used_params = vec![];
        /*for reg in CALEE_PUSH.iter() {
            emit_pushq_reg(&mut self.asm, *reg);
        }*/

        self.load_value(f, Store::Gpr(RAX));
        for (idx,value) in args.iter().enumerate() {
            let vtype: Type = self.value_typs.get(value).unwrap().clone();

            if !vtype.is_float() && pc < GPR_ARGS.len() {
                self.load_value(*value, Store::Gpr(GPR_ARGS[pc]));

                if pc != GPR_ARGS.len() {
                    pc += 1;
                }
                used_params.push(idx);
                self.free(*value);
                continue;
            } else {
                self.load_value(*value, Store::Fpu(FPU_ARGS[fpc]));
                if fpc != FPU_ARGS.len() {
                    fpc += 1;
                }
                used_params.push(idx);
                self.free(*value);
                continue;
            }
            
        }

        let mut _size = 0i32;
        for (idx,value) in args.iter().enumerate() {
            if used_params.contains(&idx) {
                continue;
            } 

            let value_typ: Type = self.value_typs.get(value).unwrap().clone();
            _size += value_typ.to_machine().size() as i32;
            if !value_typ.is_float() {
                self.load_value(*value, Store::Gpr(R11));
                self.asm.store_mem(value_typ.to_machine(),Mem::Local(_size),Reg::Gpr(R11));
            } else {
                self.load_value(*value, Store::Fpu(XMM8));
                self.asm.store_mem(value_typ.to_machine(),Mem::Local(_size),Reg::Float(XMM8));
            }

            self.free(*value);
        }

        
        emit_callq_reg(&mut self.asm, RAX);

        /*for reg in CALEE_PUSH.iter().rev() {
            emit_popq_reg(&mut self.asm, *reg);
        }*/

        self.value_typs.insert(value,ret);

        self.free(f);

        let place = self.first_available_place(ret,false);
        self.vstore.insert(value,place);
        if ret.is_float() {
            self.store_value(Store::Fpu(XMM0), place, ret);
        }  else {
            self.store_value(Store::Gpr(RAX),place, ret);
        }
       

        value
    }

    pub fn call_indirect(&mut self,f: &str,args: &[Value],ret: Type) -> Value {
        let value = self.vidx;
        self.vidx += 1;


        let mut fpc = 0;
        let mut pc = 0;
        let mut used_params = vec![];
        /*for reg in CALEE_PUSH.iter() {
            emit_pushq_reg(&mut self.asm, *reg);
        }*/


        for (idx,value) in args.iter().enumerate() {
            let vtype: Type = self.value_typs.get(value).unwrap().clone();

            if !vtype.is_float() && pc < GPR_ARGS.len() {
                self.load_value(*value, Store::Gpr(GPR_ARGS[pc]));

                if pc != GPR_ARGS.len() {
                    pc += 1;
                }
                used_params.push(idx);
                self.free(*value);
                continue;
            } else {
                self.load_value(*value, Store::Fpu(FPU_ARGS[fpc]));
                if fpc != FPU_ARGS.len() {
                    fpc += 1;
                }
                used_params.push(idx);
                self.free(*value);
                continue;
            }
            
        }

        let mut _size = 0i32;
        for (idx,value) in args.iter().enumerate() {
            if used_params.contains(&idx) {
                continue;
            } 

            let value_typ: Type = self.value_typs.get(value).unwrap().clone();
            _size += value_typ.to_machine().size() as i32;
            if !value_typ.is_float() {
                self.load_value(*value, Store::Gpr(R11));
                self.asm.store_mem(value_typ.to_machine(),Mem::Local(_size),Reg::Gpr(R11));
            } else {
                self.load_value(*value, Store::Fpu(XMM8));
                self.asm.store_mem(value_typ.to_machine(),Mem::Local(_size),Reg::Float(XMM8));
            }

            self.free(*value);
        }

        self.asm.load_int_const(MachineMode::Ptr,RAX,0);
        self.fixups.push(Fixup {
            global_name: f.to_string(),
            pos: self.asm.pos() - 8,
        });

        emit_callq_reg(&mut self.asm, RAX);

        /*for reg in CALEE_PUSH.iter().rev() {
            emit_popq_reg(&mut self.asm, *reg);
        }*/

        self.value_typs.insert(value,ret);

        let place = self.first_available_place(ret,false);
        self.vstore.insert(value,place);
        if ret.is_float() {
            self.store_value(Store::Fpu(XMM0), place, ret);
        }  else {
            self.store_value(Store::Gpr(RAX),place, ret);
        }
       

        value
    }


    fn store_value(&mut self,from: Store,to: Store,ty: Type) {
        match from {
            Store::Gpr(reg) => {
                if to.is_reg() {
                    emit_mov_reg_reg(&mut self.asm, ty.x64(), reg, to.reg());
                } else if to.is_freg() {
                    self.asm.store_mem(ty.to_machine(),Mem::Local(to.offset()),Reg::Gpr(reg));
                } else {
                    self.asm.store_mem(ty.to_machine(),Mem::Local(to.offset()),Reg::Gpr(reg));
                }
            }

            Store::Stack(off) => {
                if to.is_reg() {
                    self.asm.load_mem(ty.to_machine(),Reg::Gpr(RDI),Mem::Local(off));
                    emit_mov_reg_reg(&mut self.asm,ty.x64(), RDI, to.reg());
                } else if to.is_freg() {
                    self.asm.load_mem(ty.to_machine(),Reg::Float(XMM8),Mem::Local(off));
                   if ty.x64() == 1 {
                       movsd(&mut self.asm,to.freg(),XMM8);
                   } else {
                       movss(&mut self.asm,to.freg(),XMM8);
                   }
                } else {
                    if !ty.is_float() {
                        self.asm.load_mem(ty.to_machine(),Reg::Gpr(RDI),Mem::Local(off));
                        self.asm.store_mem(ty.to_machine(),Mem::Local(to.offset()),Reg::Gpr(RDI));
                    } else {
                        self.asm.load_mem(ty.to_machine(),Reg::Float(XMM8),Mem::Local(off));
                        self.asm.store_mem(ty.to_machine(),Mem::Local(to.offset()),Reg::Float(XMM8));
                    }
                }
            }
            Store::Fpu(freg) => {
                if to.is_freg() {
                    if ty.x64() == 1 {
                        movsd(&mut self.asm,to.freg(),freg);
                    } else {
                        movss(&mut self.asm,to.freg(),freg);
                    }
                } else {
                    self.asm.store_mem(ty.to_machine(),Mem::Local(to.offset()),Reg::Float(freg));
                }
            }
        }
    }

    fn load_value(&mut self,x: Value,to: Store) {
        let x_place = self.vstore.get(&x).unwrap().clone();
        let x_ty = self.value_typs.get(&x).unwrap().clone();

        match to {
            Store::Gpr(reg) => {
                if x_place.is_reg() {
                    emit_mov_reg_reg(&mut self.asm,x_ty.x64() ,x_place.reg(), reg);
                    return;
                } else {
                    if self.is_used_gpr(R10) {
                        emit_pushq_reg(&mut self.asm,R10);
                    }

                    self.asm.load_mem(x_ty.to_machine(),Reg::Gpr(R10),Mem::Local(x_place.offset()));

                    if self.is_used_gpr(R10) {
                        emit_popq_reg(&mut self.asm,R10);
                    }
                    return;
                }
            }
            Store::Stack(off) => {
                if x_ty.is_float() {
                    if x_place.is_freg() {
                        self.asm.store_mem(x_ty.to_machine(),Mem::Local(off),Reg::Float(x_place.freg()));
                    } else {
                        self.asm.load_mem(x_ty.to_machine(),Reg::Float(XMM8),Mem::Local(x_place.offset()));
                        self.asm.store_mem(x_ty.to_machine(),Mem::Local(off),Reg::Float(XMM8));
                    }
                } else {
                    if x_place.is_reg() {
                        self.asm.store_mem(x_ty.to_machine(),Mem::Local(off),Reg::Gpr(x_place.reg()));
                    } else {
                        if self.is_used_gpr(R10) {
                            emit_pushq_reg(&mut self.asm,R10);
                        }
                        self.asm.load_mem(x_ty.to_machine(),Reg::Gpr(R10),Mem::Local(x_place.offset()));
                        self.asm.store_mem(x_ty.to_machine(),Mem::Local(off),Reg::Gpr(R10));
                        if self.is_used_gpr(R10) {
                            emit_popq_reg(&mut self.asm,R10);
                        }
                    }
                }
            }
            Store::Fpu(_) => {
                if x_place.is_freg() {
                    if x_ty.x64() == 0 {
                        movss(&mut self.asm,to.freg(),x_place.freg());
                        return
                    } else {
                        movsd(&mut self.asm,to.freg(),x_place.freg());
                        return
                    }
                } else {
                    self.asm.load_mem(x_ty.to_machine(),Reg::Float(to.freg()),Mem::Local(x_place.offset()));
                }
            }
        }
    }
    /// Return some value and emit function epilog
    pub fn ret(&mut self,x: Value) {
        let x_ty = self.value_typs.get(&x).unwrap().clone();
        if x_ty.is_float() {
            self.load_value(x, Store::Fpu(XMM0));
        } else {
            self.load_value(x, Store::Gpr(RAX));
        }

        emit_popq_reg(&mut self.asm, RBP);
        emit_retq(&mut self.asm);
    }

}