extern crate jazz;

use jazz::reader::Reader;
use jazz::parser::Parser;
use waffle::vm::VirtualMachine;
use jazz::codegen::Compiler;
use time::PreciseTime;

fn main() {
    let reader = Reader::from_string("

        class Point {
            function __init__(x,y) {
                self.x = x
                self.y = y
            }
        }
        function main() {
            var p = new Point(2,3)
            print(p.x) 
        }
        ");
    let mut ast = vec![];
    let mut parser = Parser::new(reader,&mut ast);
    parser.parse().unwrap();
    let vm = VirtualMachine::new();

    let mut compiler = Compiler::new(vm);
    compiler.compile_ast(ast);

    let f = compiler.globals.get("main").unwrap();
    let start = PreciseTime::now();
    let result = compiler.vm.run_function(*f,vec![]);
    let end = PreciseTime::now();
    
    println!("RESULT: {:?} in {} ms",result,start.to(end).num_milliseconds());
}