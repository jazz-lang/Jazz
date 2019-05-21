use std::process::Command;

use lang_tester::LangTester;

#[test]
fn run_tests()
{
    LangTester::new()
        .test_dir("tests/jazz_tests")
        .test_file_filter(|p| p.extension().unwrap().to_str().unwrap() == "jazz")
        .test_extract(|s| {
            Some(
                s.lines()
                    // Skip non-commented lines at the start of the file.
                    .skip_while(|l| !l.starts_with("//"))
                    // Extract consecutive commented lines.
                    .take_while(|l| l.starts_with("//"))
                    .map(|l| &l[2..])
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
        })
        .test_cmds(move |p| {
            let mut compiler = Command::new("jazz");
            compiler.args(&["--jit", p.to_str().unwrap()]);
            vec![("Compiler", compiler)]
        })
        .run();
}
