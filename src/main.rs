extern crate jazz;

use jazz::lexer::reader::Reader;
use jazz::parser::Parser;
use jazz::semcheck::Semchecker;


fn main() {
    let reader = Reader::from_string(
        "
        func main() {
            var a: int = 2 * 2;
            var b: float = 2.0 * 2.5;
            a = b;
        }
    ",
    );

    let mut ast = vec![];
    let mut parser = Parser::new(reader, &mut ast);
    parser.parse().unwrap();
    let mut semck = Semchecker::new(&ast);
    let result = semck.check();
    println!("{}",result.unwrap_err());
}
