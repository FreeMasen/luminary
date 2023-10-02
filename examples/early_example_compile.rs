use std::{
    io::Write,
    path::PathBuf,
    process::{Child, Command, Stdio},
};

use inkwell::context::Context;
use luminary::run_on;

fn main() {
    let context = Context::create();
    let base_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let module = run_on(
        &context,
        base_path.join("examples").join("early_example.lua"),
    );

    std::fs::write("early_example.ll", module.to_string()).unwrap();

    let bc = module.write_bitcode_to_memory();
    std::fs::write("early_example.bc", bc.as_slice()).unwrap();
    let mut cmd = Command::new("llc");

    cmd.arg("-o").arg("early_example.s").stdin(Stdio::piped());

    let mut cmp_task = cmd.spawn().expect("llc");
    let stdin = cmp_task.stdin.as_mut().expect("stdin");
    stdin.write_all(bc.as_slice()).unwrap();
    do_command(cmp_task);
    let mut cmd = Command::new("clang");
    cmd.arg("-o")
        .arg("early_example")
        .arg("-no-pie")
        .arg("-lm")
        .arg("early_example.s");
    do_command(cmd.spawn().expect("clang"));
}

fn do_command(cmd: Child) {
    let output = cmd.wait_with_output().unwrap();

    if !output.status.success() {
        println!("{}", String::from_utf8_lossy(&output.stdout));
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
    }
}
