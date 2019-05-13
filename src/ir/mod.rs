use crate::{intern, Position};

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct IRValue
{
    pub ty: Box<IRType>,
    pub data: ValueData,
    pub id: usize,
}

impl IRValue
{
    pub fn empty() -> IRValue
    {
        IRValue {
            ty: Box::new(IRType {
                kind: IRTypeKind::None,
            }),
            data: ValueData::Null,
            id: 0,
        }
    }

    pub fn is_empty(&self) -> bool
    {
        self.ty.kind == IRTypeKind::None && self.data == ValueData::Null
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ValueData
{
    Null,
    Result(usize, usize),
    Array(Vec<Box<IRValue>>, usize),
    Struct(Vec<Box<IRValue>>),
    String(String),
    StructConstruction(Vec<Box<IRValue>>),
    Int(i64),
    Float32(u32),
    Float64(u64),
}

use crate::syntax::interner::Name;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct IRInstr
{
    pub id: NodeId,
    pub result_ty: Box<IRType>,
    pub op: IROp,
}

impl IRInstr
{
    pub fn new() -> IRInstr
    {
        IRInstr {
            id: ir_gen_id(),
            result_ty: Box::new(IRType {
                kind: IRTypeKind::None,
            }),
            op: IROp::Nop,
        }
    }
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum CastType
{
    FuncAddr,
    BitCast,
    Zext,
    Trunc,
    Fext,
    FTrunc,
    IntToPtr,
    PtrToInt,
    FpToUi,
    FpToSi,
    UiToFp,
    SiToFp,
    Reinterpret,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum IROp
{
    Nop,
    Math(String, Box<IRValue>, Box<IRValue>),
    Unary(String, Box<IRValue>),
    Ret(Box<IRValue>),
    Call(NodeId, Vec<Box<IRValue>>),
    CallAddr(Box<IRValue>, Vec<Box<IRValue>>),
    Alloc(Box<IRType>, usize),
    /// allocate memory on stack,always return *u8 type
    Alloca(Box<IRValue>),
    Malloc(Box<IRType>, Box<IRValue>),
    Free(Box<IRValue>),
    Store(Box<IRValue>, Box<IRValue>),
    Load(Box<IRValue>),
    VarPtr(usize),
    Break(usize),
    Member(Box<IRValue>, usize),
    Array(Box<IRValue>, Box<IRValue>),
    FuncAddr(String, NodeId),
    Cast(CastType, Box<IRValue>),
    BitCast(Box<IRValue>),
    SizeOf(Box<IRType>),
    OffsetOf(Box<IRType>, usize),
    Memcpy(Box<IRValue>, Box<IRValue>, Box<IRValue>, bool),
    Cond(Box<IRValue>, usize, usize),
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BasicBlock
{
    pub instructions: Vec<IRInstr>,
}

pub struct IrFunc
{
    pub static_: bool,
    pub inline: bool,
    pub external: bool,
    pub name: String,
    pub return_type: Box<IRType>,
    pub arguments: Vec<Box<IRType>>,
    pub basic_blocks: Vec<BasicBlock>,
}

use crate::NodeIdGenerator;
use parking_lot::{Mutex, RwLock};
lazy_static::lazy_static! {
    pub static ref IR_IDGEN: Mutex<RwLock< NodeIdGenerator>> = Mutex::new(RwLock::new(NodeIdGenerator::new()));
}

pub fn ir_gen_id() -> NodeId
{
    let lock = IR_IDGEN.lock();
    let read = lock.read();
    read.next()
}

use crate::NodeId;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct IRType
{
    pub kind: IRTypeKind,
}

impl IRType
{
    pub fn dereference(&self) -> Option<&IRType>
    {
        if let IRTypeKind::Ptr(subty) = &self.kind
        {
            return Some(subty);
        }
        else
        {
            None
        }
    }

    pub const fn pointer_to(ty: Box<IRType>) -> IRType
    {
        IRType {
            kind: IRTypeKind::Ptr(ty),
        }
    }

    pub fn is_ptr(&self) -> bool
    {
        match self.kind
        {
            IRTypeKind::Ptr(_) => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool
    {
        match self.kind
        {
            IRTypeKind::Structure(_) => true,
            _ => false,
        }
    }

    pub fn identical(&self, b: &IRType) -> bool
    {
        if self != b
        {
            return false;
        }
        if self.is_ptr()
        {
            return self
                .dereference()
                .unwrap()
                .identical(&Box::new(b.dereference().unwrap().clone()));
        }
        else if self.is_struct()
        {
            let struct_ = if let IRTypeKind::Structure(fields) = &self.kind
            {
                fields.clone()
            }
            else
            {
                panic!()
            };
            let struct2_ = if let IRTypeKind::Structure(fields) = &b.kind
            {
                fields.clone()
            }
            else
            {
                panic!()
            };

            if struct_.len() != struct2_.len()
            {
                return false;
            }
            let mut matches = false;
            for (s1, s2) in struct_.iter().zip(&struct2_)
            {
                if !s1.identical(s2)
                {
                    return false;
                }
                matches = true;
            }
            return matches;
        }

        true
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum IRTypeKind
{
    S8,
    S16,
    S32,
    S64,
    U8,
    U16,
    U32,
    U64,
    Float,
    Double,
    Bool,
    None,
    Ptr(Box<IRType>),
    Structure(Vec<Box<IRType>>),
    Function(Vec<Box<IRType>>, Box<IRType>, bool),
    FuncPtr,
    Void,
    FixedArray(Box<IRType>, u32),
}

use crate::syntax::ast::Type;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct BridgeVar
{
    pub name: Name,
    pub ast_type: Box<Type>,
    pub ir_type: Box<IRType>,
    pub id: usize,
}

impl BridgeVar
{
    pub fn new() -> BridgeVar
    {
        BridgeVar {
            name: intern(""),
            ast_type: Box::new(Type::Void(Position::new(intern(""), 0, 0))),
            ir_type: Box::new(IRType {
                //id: 0,
                kind: IRTypeKind::None,
            }),
            id: 0,
        }
    }
}

impl Default for BridgeVar
{
    fn default() -> Self { Self::new() }
}

use std::cell::RefCell;
pub struct IrBuilder
{
    pub basickblocks: Vec<RefCell<BasicBlock>>,
    pub current: RefCell<BasicBlock>,
    pub current_id: usize,
    pub break_id: usize,
    pub continue_id: usize,
    pub stack_labels: Vec<String>,
}

impl IrBuilder
{
    pub fn build_using_bb(&mut self, id: usize)
    {
        self.current = self.basickblocks[id].clone();
        self.current_id = id;
    }

    pub fn build_value_from_prev_ins(&mut self) -> Box<IRValue>
    {
        let block_id = self.current_id;
        let ins_id = self.current.borrow().instructions.len() - 1;

        let value = IRValue {
            data: ValueData::Result(block_id, ins_id),
            ty: self.current.borrow().instructions[ins_id].result_ty.clone(),
            id: 0,
        };

        Box::new(value)
    }

    pub fn load(&mut self, value: Box<IRValue>) -> Box<IRValue>
    {
        let ty = value.ty.dereference();

        if ty.is_none()
        {
            return unsafe { Box::from_raw(std::ptr::null_mut()) };
        }
        let load = IRInstr {
            id: ir_gen_id(),
            result_ty: Box::new(ty.unwrap().clone()),
            op: IROp::Load(value),
        };

        self.current.borrow_mut().instructions.push(load);

        self.build_value_from_prev_ins()
    }

    pub fn store(&mut self, value: Box<IRValue>, dest: Box<IRValue>)
    {
        let store = IRInstr {
            id: ir_gen_id(),
            op: IROp::Store(value, dest),
            result_ty: Box::new(IRType {
                kind: IRTypeKind::None,
            }),
        };
        self.current.borrow_mut().instructions.push(store);
    }

    pub fn bool_type(&self) -> Box<IRType>
    {
        Box::new(IRType {
            kind: IRTypeKind::Bool,
        })
    }

    pub fn int_type(&self, size: usize) -> Box<IRType>
    {
        Box::new(IRType {
            kind: match size
            {
                size if size <= 8 => IRTypeKind::S8,
                size if size <= 16 => IRTypeKind::S16,
                size if size <= 32 => IRTypeKind::S32,
                size if size <= 64 => IRTypeKind::S64,
                _ => unreachable!(),
            },
        })
    }

    pub fn uint_type(&self, size: usize) -> Box<IRType>
    {
        Box::new(IRType {
            kind: match size
            {
                size if size <= 8 => IRTypeKind::U8,
                size if size <= 16 => IRTypeKind::U16,
                size if size <= 32 => IRTypeKind::U32,
                size if size <= 64 => IRTypeKind::U64,
                _ => unreachable!(),
            },
        })
    }

    pub fn float_type(&self) -> Box<IRType>
    {
        Box::new(IRType {
            kind: IRTypeKind::Float,
        })
    }
    pub fn double_type(&self) -> Box<IRType>
    {
        Box::new(IRType {
            kind: IRTypeKind::Double,
        })
    }
    pub fn build_break(&mut self, bb: usize)
    {
        let instr = IRInstr {
            id: ir_gen_id(),
            op: IROp::Break(bb),
            result_ty: Box::new(IRType {
                kind: IRTypeKind::None,
            }),
        };
        self.current.borrow_mut().instructions.push(instr);
    }
    pub fn eq(&mut self, a: Box<IRValue>, b: Box<IRValue>) -> Box<IRValue>
    {
        let math = IRInstr {
            id: ir_gen_id(),
            op: IROp::Math("==".into(), a, b),
            result_ty: self.bool_type(),
        };

        self.current.borrow_mut().instructions.push(math);

        self.build_value_from_prev_ins()
    }

    pub fn struct_construction(
        &mut self,
        ty: Box<IRType>,
        values: Vec<Box<IRValue>>,
    ) -> Box<IRValue>
    {
        let mut value = IRValue::empty();

        value.data = ValueData::StructConstruction(values);
        value.ty = ty;

        Box::new(value)
    }

    pub fn func_ptr(&self) -> Box<IRType>
    {
        Box::new(IRType {
            kind: IRTypeKind::FuncPtr,
        })
    }

    pub fn build_int(&mut self, value_: i64) -> Box<IRValue>
    {
        let mut value = IRValue::empty();
        value.data = ValueData::Int(value_);
        value.ty = self.int_type(32);

        Box::new(value)
    }

    pub fn build_usize(&mut self, value_: usize) -> Box<IRValue>
    {
        let mut value = IRValue::empty();
        value.data = ValueData::Int(value_ as i64);
        value.ty = self.uint_type(64);

        Box::new(value)
    }

    pub fn build_str(&mut self, value_: String) -> Box<IRValue>
    {
        let mut value = IRValue::empty();
        value.data = ValueData::String(value_);
        value.ty = Box::new(IRType::pointer_to(self.uint_type(8)));

        Box::new(value)
    }

    pub fn bitcast(&mut self, from: Box<IRValue>, to: Box<IRType>) -> Box<IRValue>
    {
        let mut instr = IRInstr::new();
        instr.result_ty = to;
        instr.op = IROp::BitCast(from);
        self.current.borrow_mut().instructions.push(instr);
        self.build_value_from_prev_ins()
    }

    pub fn zext(&mut self, from: Box<IRValue>, to: Box<IRType>) -> Box<IRValue>
    {
        let mut instr = IRInstr::new();

        instr.result_ty = to;
        instr.op = IROp::Cast(CastType::Zext, from);

        self.current.borrow_mut().instructions.push(instr);
        self.build_value_from_prev_ins()
    }

    pub fn trunc(&mut self, from: Box<IRValue>, to: Box<IRType>) -> Box<IRValue>
    {
        let mut instr = IRInstr::new();

        instr.result_ty = to;
        instr.op = IROp::Cast(CastType::Trunc, from);

        self.current.borrow_mut().instructions.push(instr);
        self.build_value_from_prev_ins()
    }

    pub fn fext(&mut self, from: Box<IRValue>, to: Box<IRType>) -> Box<IRValue>
    {
        let mut instr = IRInstr::new();

        instr.result_ty = to;
        instr.op = IROp::Cast(CastType::Fext, from);

        self.current.borrow_mut().instructions.push(instr);
        self.build_value_from_prev_ins()
    }

    pub fn int_to_ptr(&mut self, from: Box<IRValue>, to: Box<IRType>) -> Box<IRValue>
    {
        let mut instr = IRInstr::new();

        instr.result_ty = to;
        instr.op = IROp::Cast(CastType::IntToPtr, from);

        self.current.borrow_mut().instructions.push(instr);
        self.build_value_from_prev_ins()
    }

    pub fn ptr_to_int(&mut self, from: Box<IRValue>, to: Box<IRType>) -> Box<IRValue>
    {
        let mut instr = IRInstr::new();

        instr.result_ty = to;
        instr.op = IROp::Cast(CastType::PtrToInt, from);

        self.current.borrow_mut().instructions.push(instr);
        self.build_value_from_prev_ins()
    }
}
