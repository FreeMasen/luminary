use common::{check_test, check_test_err};

mod common;

#[test]
fn assert_positive() {
    let lua = r#"assert(true)"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let res = test.run_lua(lua);
    res.check_success();
}

#[test]
fn assert_negative() {
    let lua = r#"assert(false)"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let res = test.run_lua(lua);
    res.check_non_success();
}

#[test]
fn assert_to_string_true() {
    let lua = r#"assert(tostring(true) == 'true')"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let res = test.run_lua(lua);
    res.check_return_code(0);
}

#[test]
fn assert_to_string_false() {
    let lua = r#"assert(tostring(false) == 'false')"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let res = test.run_lua(lua);
    res.check_return_code(0);
}

#[test]
fn assert_to_string_int_one() {
    let lua = r#"assert(tostring(1) == '1')"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let res = test.run_lua(lua);
    res.check_return_code(0);
}

#[test]
fn assert_to_string_float_one_dot_one() {
    let lua = r#"assert(tostring(1.1) == '1.1')"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let res = test.run_lua(lua);
    res.check_return_code(0);
}

#[test]
fn assert_to_string_string() {
    let lua = r#"assert(tostring("string") == 'string')"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let res = test.run_lua(lua);
    res.check_return_code(0);
}

#[test]
fn error_nil() {
    let lua = r#"error()"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let res = test.run_lua(lua);
    res.check_non_success();
}

#[test]
fn error_str() {
    let lua = r#"error("error message")"#;
    let test = common::setup(std::thread::current().name().unwrap());
    let res = test.run_lua(lua);
    res.check_non_success();
}
