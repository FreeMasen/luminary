use common::{check_test, check_test_err};
use std::process::Command;
mod common;

#[test]
fn linking_works() {
    let config = common::setup(std::thread::current().name().unwrap());
    config.run_lua("");
}
