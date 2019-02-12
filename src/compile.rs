use fxhash::FxHashMap;

use waffle::builtins::init_builtins;
use waffle::opcodes::Opcode;
use waffle::VirtualMachine;

#[derive(Clone, Debug)]
pub enum UOP {
    Op(Opcode),
    Goto(String),
}


pub struct Compiler<'a> {
    /// This field is required for `break` in `while` (Check `check_labels` documentation)
    /// 
    end_labels: Vec<String>,
    /// This field is required for `continue` in `while`
    /// # Example
    /// ```
    /// while i != 100 { // push to check_labels location of this check
    ///     while i < 50 { // push to check_labels location of this check
    ///         i = i + 2        
    ///         continue // use last label from check_labels
    ///     } // pop check_labels
    ///     i = i + 1
    /// }
    /// ```
    check_labels: Vec<String>,

    pub ins: Vec<UOP>,
    pub vm: &'a mut VirtualMachine,
    pub labels: FxHashMap<String,Option<usize>>,
    pub fdefs: FxHashMap<&'static str,usize>,
}

impl<'a> Compiler<'a> {
    pub fn new(vm: &'a mut VirtualMachine) -> Compiler<'a> {
        Self {
            end_labels: vec![],
            check_labels: vec![],
            fdefs: init_builtins(vm),
            ins: vec![],
            vm,
            labels: FxHashMap::default(),
        }
    }
}