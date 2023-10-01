use std::path::PathBuf;

use inkwell::{context::Context, execution_engine::JitFunction};
use luminary::run_on;

type MainFunc = unsafe extern "C" fn();

fn main() {
    let context = Context::create();
    let module = run_on(
        &context,
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("examples")
            .join("early_example.lua"),
    );
    let jit = module
        .create_jit_execution_engine(inkwell::OptimizationLevel::Aggressive)
        .unwrap();
    unsafe {
        let main: JitFunction<MainFunc> = jit.get_function("main").expect("main function");
        main.call();
    };
}
