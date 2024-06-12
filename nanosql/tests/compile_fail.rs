use std::path::PathBuf;
use compiletest_rs::{Config, common::Mode};


#[test]
fn compile_fail() {
    let mut config = Config::default();

    config.mode = Mode::CompileFail;
    config.src_base = PathBuf::from("tests/compile_fail");
    config.target_rustcflags = Some([
        "-L ../target/debug",
        "-L ../target/debug/deps",
        "--extern nanosql",
        "--extern nanosql_macros",
        "--edition 2021",
    ].join(" "));
    config.clean_rmeta();

    compiletest_rs::run_tests(&config);
}
