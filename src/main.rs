extern crate jazzc;

use jazzc::compile::compile_ast;
use jazzc::parser::Parser;
use jazzc::reader::Reader;
use jazzvm::module::Module;
use jazzvm::vm::VM;
use jazzvm::P;
fn main() {
    let reader = Reader::from_string(
        "
        var a = 2 + 3 
        var b = a + 2
    return b
    ",
    );
    let mut ast = vec![];
    let mut parser = Parser::new(reader, &mut ast);
    parser.parse().unwrap();
    let ctx = compile_ast(ast);

    for op in ctx.ops.iter() {
        println!("{:?}", op);
    }

    let mut m = Module::new("main");
    m.code = ctx.ops.clone();

    let mut m = P(m);
    let mut vm = VM::new();
    vm.code = ctx.ops.clone();
    println!("{:?}", vm.interp(&mut m));
}
