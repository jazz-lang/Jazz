#[macro_use]
extern crate waffle;

#[macro_use]
pub mod macros;
pub mod ast;
pub mod lexer;
pub mod msg;
pub mod parser;
pub mod reader;
pub mod token;
pub mod codegen;
pub mod stdlib;

/*pub macro rust_function($vm: expr,fn $name:ident ($($arg:ident),*) $b:block) {
    pub fn $name (vm: &mut VirtualMachine,args: Vec<Value>) -> Value {
        let mut idx = 0;
        $(
            let $arg = &args[idx];
            idx += 1;
        )*

        $b
    }

    $vm.pool.add_func()
}*/
