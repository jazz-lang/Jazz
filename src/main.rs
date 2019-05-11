extern crate jazz;
extern crate structopt;

use jazz::err::MsgWithPos;
use jazz::gccjit::Codegen;
use jazz::semantic::*;
use jazz::syntax::ast::*;
use jazz::syntax::lexer::reader::Reader;
use jazz::syntax::parser::*;
use jazz::Context;
use structopt::StructOpt;

use std::path::PathBuf;

#[derive(StructOpt, Debug)]
#[structopt(name = "jazz", about = "Jazz language compiler")]
pub struct Options {
    #[structopt(parse(from_os_str))]
    pub file: PathBuf,
    #[structopt(short = "O", long = "opt-level", default_value = "2")]
    pub opt_level: u8,
    #[structopt(long = "jit")]
    pub jit: bool,
    #[structopt(long = "emit-obj")]
    pub emit_obj: bool,
    #[structopt(long = "emit-asm")]
    pub emit_asm: bool,
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    pub output: Option<PathBuf>,
    #[structopt(long = "shared")]
    pub shared: bool,
}

fn main() -> Result<(), MsgWithPos> {
    let opts: Options = Options::from_args();
    let mut file = File {
        root: String::new(),
        src: String::new(),
        path: opts.file.file_stem().unwrap().to_str().unwrap().to_owned(),
        elems: vec![],
    };

    let reader = Reader::from_file(opts.file.to_str().unwrap()).unwrap();

    let mut parser = Parser::new(reader, &mut file);

    let err = parser.parse();
    if err.is_err() {
        println!("{}", err.clone().err().unwrap());
    }

    let mut ctx = Context::new(file);
    ctx.shared = opts.shared;
    ctx.emit_asm = opts.emit_asm;
    ctx.emit_obj = opts.emit_obj;
    ctx.jit = opts.jit;
    ctx.output = opts
        .output
        .map_or(String::new(), |e: PathBuf| e.to_str().unwrap().to_owned());
    ctx.opt = opts.opt_level;
    let mut semantic = SemCheck::new(&mut ctx);

    semantic.run();
    let mut cgen = Codegen::new(&ctx, "module");
    cgen.compile();
    Ok(())
}
