extern crate jazz;

use jazz::ast::*;
use jazz::compile::Compiler;
use jazz::msg::MsgWithPos;
use jazz::parser::Parser;
use jazz::reader::Reader;
use jazz::runtime::init;
use waffle::value::{FuncKind, Function};
use waffle::VirtualMachine;

use walkdir::WalkDir;

fn proceed_opens(ast: &mut Vec<Box<Expr>>)
{
    for (id, expr) in ast.clone().iter().enumerate()
    {
        if let ExprKind::Open(file) = &expr.expr
        {
            let file = WalkDir::new(file).into_iter()
                                         .nth(0)
                                         .expect("File not found");
            let file = file.unwrap();
            let file: &std::path::Path = file.path();
            let reader = Reader::from_file(file.to_str().unwrap()).unwrap();
            let mut ast1 = vec![];

            let mut parser = Parser::new(reader, &mut ast1);
            parser.parse().unwrap();

            ast.remove(id);
            ast1.append(ast);

            *ast = ast1;
            proceed_opens(ast);
        }
    }
}

fn main() -> Result<(), MsgWithPos>
{
    use std::env::args;
    let fname = args().nth(1).expect("You must specify file name!");

    let reader = Reader::from_file(&fname).unwrap();
    let mut ast = vec![];
    let mut parser = Parser::new(reader, &mut ast);

    parser.parse()?;
    proceed_opens(&mut ast);
    let mut vm = VirtualMachine::new();
    let builtins = init(&mut vm);
    let mut cmpl = Compiler::new(&mut vm, builtins);
    cmpl.in_global_scope = true;
    cmpl.compile(ast, vec![]);
    let opcodes = cmpl.finish();
    let fun = Function { nargs: 0,
                         is_native: false,
                         addr: FuncKind::Interpret(opcodes),nlocals: cmpl.locals.len() };

    let id = vm.pool.add_func(fun);

    println!("{:?}", vm.run_func(id));

    Ok(())
}
