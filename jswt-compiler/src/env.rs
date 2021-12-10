pub fn println(arg: i32) {
    println!("{}", arg);
}

pub fn exit(arg: i32) {
    std::process::exit(arg);
}
