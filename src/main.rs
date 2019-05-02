extern crate jazz;

use jazz::err::MsgWithPos;
use jazz::semantic::*;
use jazz::syntax::ast::*;
use jazz::syntax::lexer::reader::Reader;
use jazz::syntax::parser::*;
use jazz::Context;

fn main() -> Result<(), MsgWithPos> {
    pretty_backtrace::force_setup();
    let mut file = File {
        root: String::new(),
        src: String::new(),
        path: "<>".to_owned(),
        elems: vec![],
    };

    use std::env::args;

    let reader = Reader::from_file(&args().nth(1).unwrap()).unwrap();

    let mut parser = Parser::new(reader, &mut file);

    let err = parser.parse();
    if err.is_err() {
        println!("{}", err.clone().err().unwrap());
    }

    /*if !err.is_err() {
        println!("{:#?}", file.elems);
    }*/

    let mut ctx = Context::new(file);
    //ctx.imports();
    let mut semantic = SemCheck::new(&mut ctx);

    semantic.run();
    Ok(())
}
