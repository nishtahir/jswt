pub fn println(arg: i32) {
    println!("{}", arg);
}

pub fn exit(arg: i32) {
    std::process::exit(arg);
}

pub fn assert_equal(expected: i32, actual: i32) {
    assert!(expected == actual);
}
