use std::process::Command;

mod common;

#[test]
fn static_linking_works() {
    let config = common::setup("static_linking_works");
    let bin_path = config.build_static("");
    let exec = Command::new(bin_path)
        .spawn()
        .unwrap()
        .wait_with_output()
        .unwrap();
    assert_eq!(exec.status.code(), Some(0));
}

#[test]
fn dynamic_linking_works() {
    let config = common::setup("dynamic_linking_works");
    let bin_path = config.build_dynamic("");
    let exec = config.run_dynamic(&bin_path);
    assert_eq!(exec.status.code(), Some(0));
}
