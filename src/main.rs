extern crate jazz;

use jazz::reader::Reader;
use jazz::parser::Parser;
use jazz::msg::MsgWithPos;
fn main() -> Result<(),MsgWithPos>
{
    let reader = Reader::from_string("
    let add = |x,y| {}

    add()
    "
    );
    let mut ast = vec![];
    let mut parser = Parser::new(reader,&mut ast);

    parser.parse()?;

    for elem in ast.iter() {
        println!("{:#?}",elem.expr);
    }

    Ok(())
}
