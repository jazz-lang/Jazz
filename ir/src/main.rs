extern crate jazz_ir;

use jazz_ir::*;
use self::ir::*;
use self::ty::*;
use capstone::prelude::*;

fn main() {
    let mut func = Function::new("main".into(), Linkage::Local);


    let v1 = func.iconst(Int(32), 4);
    let v2 = func.iconst(Int(32), 2);
    let v3 = func.imul(v1,v2);
    func.ret(v3);
    let mut cs = Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Att)
        .detail(true)
        .build().unwrap();

    let ins = cs.disasm_all(&func.asm().data(),0).unwrap();

    for i in ins.iter() {
        println!("{}",i);
    }

    use jazz_jit::get_executable_memory;
    let mem = get_executable_memory(&func.asm());

    let f: fn() -> i32 = unsafe {::std::mem::transmute(mem.ptr())};

    println!("{}",f());

}
