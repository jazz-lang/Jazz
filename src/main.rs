extern crate jazz;

use jazz::module::Module;
use jazz::opcode::Opcode;
use jazz::value::*;
use jazz::vm::VM;
use jazz::P;

fn main() {
    let mut m = Module::new("main");
    m.code = vec![
        Opcode::AccInt(3),
        Opcode::Push,
        Opcode::AccInt(5),
        Opcode::Add,
        Opcode::Last,
    ];
    let mut module = P(m);
    let func = Function {
        module: module.clone(),
        var: FuncVar::Offset(1),
        nargs: 1,
        env: P(Value::Array(P(vec![]))),
    };

    let m = module.borrow_mut();
    m.globals.push(P(Value::Func(P(func))));

    let mut vm = VM::new();

    vm.code = m.code.clone();

    println!("{:?}", vm.interp(&mut module));
}
