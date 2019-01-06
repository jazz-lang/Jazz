extern crate jazz_ir;

use jazz_ir::*;
use self::ty::*;
use self::module::*;
use capstone::prelude::*;

fn main() {
    
    let mut module = Module::new();
    module.declare_function("main".into(), Linkage::Local);
    //module.declare_function("puts".into(), Linkage::Extern(puts as *const u8));
    module.declare_function("init".into(), Linkage::Local);    
    module.declare_function("puts".into(), Linkage::Dylib("/usr/lib64/libc++.so.1".into()));
    let func = module.get_function(&"main".to_string());

    let int = Int(32);

    let v1 = func.iconst(int,4);
    let v2 = func.iconst(int,2);
    let v3 = func.imul(v1,v2);
    let v4 = func.iconst(int,4);
    let v5 = func.iadd(v4,v3);
    func.ret(v5);

    module.finish();
    let (data,size) = module.get_finalized_data(&"main".to_owned());
    let mut cs = Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Att)
        .detail(true)
        .build().unwrap();


    let ins = cs.disasm_all(unsafe {::std::slice::from_raw_parts(data,size)},0).unwrap();
    for i in ins.iter() {
        println!("{}",i);
    }

    let fdata = module.get_finalized_function(&"main".to_string());

    let func: fn() -> i32 = unsafe {::std::mem::transmute(fdata)};

    println!("{}",func());

}
