use super::{Block, Entity, Function, InstructionData, IrType, Opcode, Value, CFG};
use linked_hash_map::LinkedHashMap as HashMap;

pub struct FunctionBuilder<'a>
{
    pub func: &'a mut Function,
    variables: HashMap<u32, Value>,
    types: HashMap<u32, IrType>,
}

impl<'a> FunctionBuilder<'a>
{
    pub fn new(func: &'a mut Function) -> FunctionBuilder<'a>
    {
        FunctionBuilder {
            func,
            variables: HashMap::new(),
            types: HashMap::new(),
        }
    }

    fn cfg<'b>(&'b mut self) -> &'b mut CFG { &mut self.func.cfg }

    fn get_value_type(&self, val: &Value) -> &IrType { self.func.value_types.get(val).unwrap() }

    pub fn switch_to_block(&mut self, block: Block) { self.cfg().switch_to_block(block); }

    pub fn new_block(&mut self) -> Block { self.cfg().new_block() }

    pub fn stack_alloc(&mut self, size: usize) -> Value
    {
        let value = self
            .cfg()
            .make_inst(InstructionData::AllocaSize(Opcode::Alloca, size));
        self.func
            .value_types
            .insert(value, IrType::Ptr(box IrType::UInt(8)));
        value
    }
    pub fn stack_alloc_ty(&mut self, ty: IrType) -> Value
    {
        let value = self
            .cfg()
            .make_inst(InstructionData::Alloca(Opcode::Alloca, box ty));
        self.func
            .value_types
            .insert(value, IrType::Ptr(box IrType::UInt(8)));
        value
    }

    pub fn iconst(&mut self, type_: IrType, imm: impl Into<i64>) -> Value
    {
        let value = self
            .cfg()
            .make_inst(InstructionData::UnaryImm(Opcode::Iconst, imm.into()));
        self.func.value_types.insert(value, type_);
        value
    }

    pub fn iadd(&mut self, lhs: Value, rhs: Value) -> Value
    {
        let value = self
            .cfg()
            .make_inst(InstructionData::IntBinary(Opcode::Iadd, lhs, rhs));
        let ty = self.get_value_type(&lhs).clone();
        self.func.value_types.insert(value, ty);
        value
    }

    pub fn isub(&mut self, lhs: Value, rhs: Value) -> Value
    {
        let value = self
            .cfg()
            .make_inst(InstructionData::IntBinary(Opcode::Isub, lhs, rhs));
        let ty = self.get_value_type(&lhs).clone();
        self.func.value_types.insert(value, ty);
        value
    }

    pub fn store(&mut self, val: Value, elem: Value)
    {
        self.cfg()
            .make_inst(InstructionData::Store(Opcode::Store, val, elem));
    }

    pub fn jump(&mut self, block: Block)
    {
        self.cfg()
            .make_inst(InstructionData::Branch(Opcode::Br, block, None));
    }

    pub fn jump_if(&mut self, block: Block, val: Value)
    {
        self.cfg()
            .make_inst(InstructionData::Branch(Opcode::BrNz, block, Some(val)));
    }

    pub fn jump_ifnot(&mut self, block: Block, val: Value)
    {
        self.cfg()
            .make_inst(InstructionData::Branch(Opcode::BrZ, block, Some(val)));
    }

    fn zero_val(&self) -> Value { Value::new(0xffffusize) }

    pub fn declare_variable(&mut self, ty: IrType, var: u32) -> u32
    {
        self.variables.insert(var, self.zero_val());
        self.types.insert(var, ty);
        var
    }

    pub fn def_var(&mut self, var: u32, val: Value)
    {
        let ty = self.get_value_type(&val).clone();
        let var_ty = self.types.get(&var).unwrap().clone();
        if ty != var_ty
        {
            panic!("Mismatched types");
        }

        self.variables.insert(var, val);
    }

    pub fn use_var(&mut self, var: u32) -> Value
    {
        self.variables.get(&var).expect("Undefined var").clone()
    }

    pub fn return_(&mut self, val: Value)
    {
        self.cfg()
            .make_inst(InstructionData::Return(Opcode::Return, val));
    }
}
