extern crate jazz;

use jazz::lexer::reader::Reader;
use jazz::parser::Parser;
use jazz::semcheck::Semchecker;


extern crate structopt;


use structopt::StructOpt;
use std::path::PathBuf;
#[derive(Clone,StructOpt)]
#[structopt(name = "jazz", about = "")]
struct Opt {
    #[structopt(parse(from_os_str))]
    file: PathBuf,
}

fn main() {
    let opt: Opt = Opt::from_args();
    let reader = Reader::from_file(opt.file.to_str().unwrap()).unwrap();
    let mut ast = vec![];
    let mut parser = Parser::new(reader, &mut ast);
    parser.parse().unwrap();
    let mut semck = Semchecker::new(&ast);
    let result = semck.check();
    match result {
        Ok(_) => {},
        Err(e) => {
            println!("{}",e);
        }
    }
}
