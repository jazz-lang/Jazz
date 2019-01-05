pub mod lexer;
pub mod error;

#[macro_export]
macro_rules! unwrap_err {
    ($e:expr) => {
        match $e {
            Ok(result) => result,
            Err(e) => {
                eprintln!("{}",e);
                ::std::process::exit(-1);
            }
        }
    };
}