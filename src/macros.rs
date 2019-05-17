#[macro_export]
macro_rules! warn {
    ($fmt: expr,$pos: expr) => {{
        eprintln!("{} {}: {}", "WARNING".yellow(), $pos, $fmt);
    }};
}

#[macro_export]
macro_rules! error {
    ($fmt: expr,$pos: expr) => {{
        eprintln!("{} {}: {}", "ERROR".red(), $pos, $fmt);
        std::process::exit(-1);
    }};

    ($EXIT: expr,$fmt: expr,$pos: expr) => {{
        eprintln!("{} {}: {}", "ERROR".red(), $pos, $fmt);
        if $EXIT
        {
            std::process::exit(-1);
        }
    }};
}
