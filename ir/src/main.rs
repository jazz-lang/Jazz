extern crate regalloc;

use regalloc::*;
use self::ir::*;
use self::ty::*;
use capstone::prelude::*;

fn main() {
    let mut func = Function::new("main".into(), Linkage::Local);


    let x = func.iconst(Int(32), 4);
    let y = func.iconst(Int(32), 2);
    let v1 = func.iadd(x,y);
    let v2 = func.iconst(Int(32),4);
    let v3 = func.iadd(v1,v2);
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


}
