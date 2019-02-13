extern crate jazz;

use jazz::compile::Compiler;
use jazz::msg::MsgWithPos;
use jazz::parser::Parser;
use jazz::reader::Reader;
use jazz::runtime::init;
use waffle::value::{FuncKind, Function};
use waffle::VirtualMachine;
fn main() -> Result<(), MsgWithPos>
{
    use std::env::args;
    let fname = args().nth(1).expect("You must specify file name!");

    let reader = Reader::from_file(&fname).unwrap();
    let mut ast = vec![];
    let mut parser = Parser::new(reader, &mut ast);

    parser.parse()?;
    let mut vm = VirtualMachine::new();
    let builtins = init(&mut vm);
    let mut cmpl = Compiler::new(&mut vm, builtins);
    cmpl.compile(ast, vec![]);
    let opcodes = cmpl.finish();
    let fun = Function { nargs: 0,
                         is_native: false,
                         addr: FuncKind::Interpret(opcodes) };

    let id = vm.pool.add_func(fun);

    println!("{:?}",vm.run_func(id));

    Ok(())
}
