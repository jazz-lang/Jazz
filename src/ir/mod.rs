use std::fmt;

#[macro_use]
pub mod macros;
pub mod builder;

use crate::syntax::interner::Name;

pub struct Function
{
    pub name: Name,
    pub id: FuncionId,
    pub cfg: CFG,
    pub params: Vec<IrType>,
    pub ret: IrType,
    pub external: bool,
    pub value_types: HashMap<Value, IrType>,
}

impl Function
{
    pub fn new(id: FuncionId, name: &str) -> Function
    {
        Function {
            name: crate::intern(name),
            id,
            cfg: CFG::new(),
            params: vec![],
            ret: IrType::Void,
            external: false,
            value_types: HashMap::new(),
        }
    }
}

impl fmt::Display for Function
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "function {}(", crate::str(self.name))?;
        for (i, param) in self.params.iter().enumerate()
        {
            write!(f, "{}", param)?;
            if i != self.params.len() - 1
            {
                write!(f, ",")?;
            }
        }

        write!(f, ") {} {{\n", self.ret)?;
        for (id, block) in self.cfg.blocks.iter()
        {
            write!(f, "{}:\n", id)?;
            let block: &BlockData = block;

            for (id, ins) in block.insts.iter()
            {
                if ins.without_result()
                {
                    write!(f, "\t{}\n", ins)?;
                }
                else
                {
                    let val = self.cfg.results.get(id).unwrap().clone();
                    write!(f, "\t{} = {}\n", val, ins)?;
                }
            }
        }
        write!(f, "}}")
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Value(u32);

impl fmt::Display for Value
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "v{}", self.0) }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Inst(u32);
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Block(pub u32);
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FuncionId(pub u32);

impl fmt::Display for Block
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "ebb{}", self.0) }
}
impl_entity!(Value);
impl_entity!(Block);
impl_entity!(Inst);
impl_entity!(FuncionId);

pub trait Entity: Copy + PartialEq
{
    fn new(_: impl Into<usize>) -> Self;
    fn idx(self) -> usize;
}

#[derive(Clone, PartialEq, Debug)]
pub enum IrType
{
    Int(u8),
    UInt(u8),
    F32,
    F64,
    Usize,
    Isize,
    Bool,
    Void,
    Ptr(Box<IrType>),
    Struct(String, Vec<IrType>),
    Func(Vec<IrType>, Box<IrType>),
}

impl fmt::Display for IrType
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            IrType::Void => write!(f, "void"),
            IrType::Int(i) => write!(f, "int:{}", i),
            IrType::UInt(u) => write!(f, "uint:{}", u),
            IrType::Usize => write!(f, "usize"),
            IrType::Bool => write!(f, "bool"),
            IrType::F32 => write!(f, "float:32"),
            IrType::F64 => write!(f, "float:64"),
            IrType::Isize => write!(f, "isize"),
            IrType::Ptr(subtype) => write!(f, "*{}", subtype),
            IrType::Struct(name, fields) =>
            {
                write!(f, "{}(", name)?;
                for (i, field) in fields.iter().enumerate()
                {
                    write!(f, "{}", field)?;
                    if i != fields.len() - 1
                    {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            IrType::Func(params, return_type) =>
            {
                write!(f, "(")?;
                for (i, param) in params.iter().enumerate()
                {
                    write!(f, "{}", param)?;
                    if i != params.len() - 1
                    {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") {}", return_type)
            }
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum CondCode
{
    GreaterThan,
    GreaterThanEquals,
    LessThan,
    LessThanEquals,

    Equals,
    NotEquals,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Opcode
{
    Iconst,
    PtrConst,
    F32Const,
    F64Const,
    StrConst,
    ZeroInit,

    Iadd,
    Isub,
    Idiv,
    Imul,
    Imod,
    Shr,
    Shl,
    LoadParam,
    Icmp(CondCode),
    Fcmp(CondCode),
    Fadd,
    Fsub,
    Fdiv,
    Fmul,
    Fmod,

    BrNz,
    BrZ,
    Br,
    Load,
    Store,
    Call,
    CallValue,

    Cast,
    Alloca,
    Return,
}

impl fmt::Display for Opcode
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            Opcode::Br => write!(f, "br"),
            Opcode::BrNz => write!(f, "brnz"),
            Opcode::BrZ => write!(f, "brz"),
            Opcode::Iconst => write!(f, "iconst"),
            Opcode::F32Const => write!(f, "f32const"),
            Opcode::F64Const => write!(f, "f64const"),
            Opcode::StrConst => write!(f, "strconst"),
            Opcode::PtrConst => write!(f, "ptrconst"),
            Opcode::ZeroInit => write!(f, "zeroinit"),
            Opcode::Iadd => write!(f, "iadd"),
            Opcode::Isub => write!(f, "isub"),
            Opcode::Idiv => write!(f, "idiv"),
            Opcode::Imul => write!(f, "imul"),
            Opcode::Imod => write!(f, "imod"),
            Opcode::Shr => write!(f, "shr"),
            Opcode::Shl => write!(f, "shl"),
            Opcode::Fadd => write!(f, "fadd"),
            Opcode::Fsub => write!(f, "fsub"),
            Opcode::Fdiv => write!(f, "fdiv"),
            Opcode::Fmul => write!(f, "fmul"),
            Opcode::Fmod => write!(f, "fmod"),
            Opcode::Call | Opcode::CallValue => write!(f, "call"),
            Opcode::Alloca => write!(f, "alloca"),
            Opcode::Cast => write!(f, "cast"),
            Opcode::Store => write!(f, "store"),
            Opcode::Load => write!(f, "load"),
            Opcode::Icmp(_cc) => write!(f, "icmp"),
            Opcode::Fcmp(_cc) => write!(f, "fcmp"),
            Opcode::LoadParam => write!(f, "loadparam"),
            Opcode::Return => write!(f, "return"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum InstructionData
{
    Unary(Opcode, Value),
    LoadParam(Opcode, Box<IrType>, u16),
    UnaryImm(Opcode, i64),
    UnaryIeee32(Opcode, u32),
    UnaryIeee64(Opcode, u64),
    UnaryStr(Opcode, Name),

    ZeroInit(Opcode, Box<IrType>),

    CallValue(Opcode, Value, Vec<Value>),
    Call(Opcode, FuncionId, Vec<Value>),
    Branch(
        Opcode,
        Block,
        Option<Value>, /* Some if opcode is BrZ or BrNz*/
    ),

    IntBinary(Opcode, Value, Value),
    FloatBinary(Opcode, Value, Value),
    Cast(Opcode, Value, Box<IrType>),
    Alloca(Opcode, Box<IrType>),
    AllocaSize(Opcode, usize),
    Move(Opcode, Value),
    Return(Opcode, Value),
    VoidReturn(Opcode),
    /// Load value at v0[v1]
    Load(Opcode, Value, Value),
    /// Store v2 at v0[v1]
    Store(Opcode, Value, Value),
}

impl InstructionData
{
    pub fn without_result(&self) -> bool
    {
        match self
        {
            InstructionData::Store(_, _, _) => true,
            InstructionData::Branch(_, _, _) => true,
            InstructionData::Return(_, _) => true,
            _ => false,
        }
    }
}

impl fmt::Display for InstructionData
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            InstructionData::Alloca(_opcode, ty) => write!(f, "alloca.{}", ty),
            InstructionData::Move(_, value) => write!(f, "{}", value),
            InstructionData::Branch(opcode, to, value) =>
            {
                write!(f, "{} {} ", opcode, to)?;
                if value.is_some()
                {
                    write!(f, "{}", value.as_ref().unwrap())
                }
                else
                {
                    write!(f, "")
                }
            }
            InstructionData::Cast(opcode, val, ty) => write!(f, "{}.{} {}", opcode, val, ty),
            InstructionData::Return(opcode, val) => write!(f, "{} {}", opcode, val),
            InstructionData::VoidReturn(opcode) => write!(f, "{}", opcode),
            InstructionData::FloatBinary(op, x, y) => write!(f, "{} {} {}", op, x, y),
            InstructionData::IntBinary(op, x, y) => write!(f, "{} {} {}", op, x, y),
            InstructionData::Load(op, val, index) => write!(f, "{} {}, {}", op, val, index),
            InstructionData::Store(op, val, value) => write!(f, "{} {}, {}", op, val, value),
            InstructionData::ZeroInit(_, ty) => write!(f, "zeroinit {}", ty),
            InstructionData::UnaryIeee32(op, iee) => write!(f, "{} {}", op, f32::from_bits(*iee)),
            InstructionData::UnaryIeee64(op, iee) => write!(f, "{} {}", op, f64::from_bits(*iee)),
            InstructionData::UnaryImm(op, imm) => write!(f, "{} {}", op, imm),
            InstructionData::UnaryStr(op, string) => write!(f, "{} {}", op, crate::str(*string)),
            InstructionData::Unary(op, val) => write!(f, "{} {}", op, val),
            InstructionData::LoadParam(_, ty, num) => write!(f, "loadparam.{} {}", ty, num),
            InstructionData::Call(_, func, params) =>
            {
                write!(f, "call function:{} ", func.0)?;
                for param in params.iter()
                {
                    write!(f, "{} ", param)?
                }
                write!(f, "")
            }
            InstructionData::CallValue(_, func, params) =>
            {
                write!(f, "call {} ", func)?;
                for param in params.iter()
                {
                    write!(f, "{} ", param)?
                }
                write!(f, "")
            }
            InstructionData::AllocaSize(opcode, size) => write!(f, "{} {}", opcode, size),
        }
    }
}

use linked_hash_map::LinkedHashMap as HashMap;

#[derive(Clone, PartialEq)]
pub struct BlockData
{
    pub insts: HashMap<Inst, InstructionData>,
    pub terminated: bool,
}

impl BlockData
{
    pub fn new() -> BlockData
    {
        BlockData {
            insts: HashMap::new(),
            terminated: false,
        }
    }
}

pub struct CFG
{
    results: HashMap<Inst, Value>,
    blocks: HashMap<Block, BlockData>,
    cur_block: Option<Block>,
}

impl CFG
{
    #[inline]
    pub fn new() -> CFG
    {
        CFG {
            results: HashMap::new(),
            blocks: HashMap::new(),
            cur_block: None,
        }
    }

    pub fn new_block(&mut self) -> Block
    {
        let block = Block::new(self.blocks.len());
        let block_data = BlockData::new();
        self.blocks.insert(block, block_data);

        block
    }

    pub fn switch_to_block(&mut self, block: Block) { self.cur_block = Some(block); }

    pub fn make_inst(&mut self, data: InstructionData) -> Value
    {
        let cur_block: Block = self.cur_block.unwrap();
        let cur_block = self.blocks.get_mut(&cur_block).unwrap();
        let inst = Inst::new(cur_block.insts.len());
        let val = if !data.without_result()
        {
            let id = self.results.len();
            let val = Value::new(id);
            self.results.insert(inst, val);
            val
        }
        else
        {
            Value::new(0usize)
        };
        cur_block.insts.insert(inst, data);

        val
    }
}
