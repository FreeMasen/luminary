mod common;

#[test]
fn linking_works() {
    let config = common::setup(std::thread::current().name().unwrap());
    config.run_lua("").check_return_code(0);
}
