extern crate jazz;

use jazz::lexer::reader::Reader;
use jazz::parser::Parser;
use jazz::semcheck::Semchecker;


fn main() {
    let reader = Reader::from_string(
        "
        func main() -> int {
            var a: int = 2;
            var b: float = 2.5;
            a = b;
            return 2;
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
