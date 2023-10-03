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
    let bc = module.write_bitcode_to_memory();
    let mut cmd = Command::new("llc");
    let tmp_s_file = tempfile::Builder::new().suffix(".s").tempfile().unwrap();

    cmd.arg("-o").arg(tmp_s_file.path()).stdin(Stdio::piped());

    let mut cmp_task = cmd.spawn().expect("llc");
    let stdin = cmp_task.stdin.as_mut().expect("stdin");
    stdin.write_all(bc.as_slice()).unwrap();
    do_command(cmp_task);
    let mut cmd = Command::new("clang");
    cmd.arg("-o")
        .arg("early_example")
        .arg("-no-pie")
        .arg("-lm")
        .arg(tmp_s_file.path());
    do_command(cmd.spawn().expect("clang"));
}

fn do_command(cmd: Child) {
    let output = cmd.wait_with_output().unwrap();

    if !output.status.success() {
        println!("{}", String::from_utf8_lossy(&output.stdout));
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
    }
}
