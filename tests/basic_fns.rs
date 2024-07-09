use std::process::Command;

mod common;

#[test]
fn assert_positive_static() {
    let lua = r#"assert(true)"#;
    let test = common::setup("assert_positive_static");
    let bin_path = test.build_static(lua);
    println!("Executing assert binary");
    let exec = Command::new(bin_path)
        .spawn()
        .unwrap()
        .wait_with_output()
        .unwrap();
    assert_eq!(exec.status.code(), Some(0));
}

#[test]
fn assert_positive_dynamic() {
    let lua = r#"assert(true)"#;
    let test = common::setup("assert_positive_dynamic");
    let bin_path = test.build_dynamic(lua);
    println!("Executing assert binary");
    let exec = test.run_dynamic(bin_path);
    assert_eq!(exec.status.code(), Some(0));
}

#[test]
fn assert_negative_static() {
    let lua = r#"assert(false)"#;
    let test = common::setup("assert_negative_static");
    let bin_path = test.build_static(lua);
    println!("Executing assert binary");
    let exec = Command::new(bin_path)
        .spawn()
        .unwrap()
        .wait_with_output()
        .unwrap();
    assert_eq!(exec.status.code(), None);
}

#[test]
fn assert_negative_dynamic() {
    let lua = r#"assert(false)"#;
    let test = common::setup("assert_negative_dynamic");
    let bin_path = test.build_dynamic(lua);
    println!("Executing assert binary");
    let exec = test.run_dynamic(bin_path);
    assert_eq!(exec.status.code(), None);
}
