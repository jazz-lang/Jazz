extern crate jazz;

use jazz::reader::Reader;
use jazz::parser::Parser;
use waffle::vm::VirtualMachine;
use jazz::codegen::Compiler;
use gc::{Gc,GcCell};
use waffle::value::Value;

fn main() {
    let reader = Reader::from_string("
        function factorial(x) {
            if x < 2 {
                return 1
            } else {
                return factorial(x - 1) * x
            }
        }

        function main() {
            return factorial(5)
        }
        ");
    let mut ast = vec![];
    let mut parser = Parser::new(reader,&mut ast);
    parser.parse().unwrap();
    let vm = VirtualMachine::new();

    let mut compiler = Compiler::new(vm);
    compiler.compile_ast(ast);

    let f = compiler.globals.get("main").unwrap();
    let result = compiler.vm.run_function(*f,vec![]);
    println!("{:?}",result);
}