use common::{check_return_code, check_test, check_test_err};
use std::{fmt, process::Output};
mod common;


#[test]
fn assert_positive() {
    let lua = r#"assert(true)"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let (d, s) = test.run_lua(lua);
    check_test(&d);
    check_test(&s);
}

#[test]
fn assert_negative() {
    let lua = r#"assert(false)"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let (d, s) = test.run_lua(lua);
    check_test_err(&d);
    check_test_err(&s);
}

#[test]
fn assert_to_string_true() {
    let lua = r#"assert(tostring(true) == 'true')"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let (d, s) = test.run_lua(lua);
    check_test(&d);
    check_test(&s);
}

#[test]
fn assert_to_string_false() {
    let lua = r#"assert(tostring(false) == 'false')"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let (d, s) = test.run_lua(lua);
    check_test(&d);
    check_test(&s);
}

#[test]
fn assert_to_string_int_one() {
    let lua = r#"assert(tostring(1) == '1')"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let (d, s) = test.run_lua(lua);
    check_test(&d);
    check_test(&s);
}

#[test]
fn assert_to_string_float_one_dot_one() {
    let lua = r#"assert(tostring(1.1) == '1.1')"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let (d, s) = test.run_lua(lua);
    check_test(&d);
    check_test(&s);
}

#[test]
fn assert_to_string_string() {
    let lua = r#"assert(tostring("string") == 'string')"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let (d, s) = test.run_lua(lua);
    check_test(&d);
    check_test(&s);
}

#[test]
fn error_nil() {
    let lua = r#"error()"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let (d, s) = test.run_lua(lua);
    check_test_err(&d);
    check_test_err(&s);
}

#[test]
fn error_str() {
    let lua = r#"error("error message")"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let (d, s) = test.run_lua(lua);
    check_test_err(&d);
    check_test_err(&s);
}
