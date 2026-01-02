pub mod ecs;

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

enum Value {
    Int(i32),
    Float(f32),
    String(String),
}

const STACK_SIZE: usize = 1 << 15;

pub struct VM {
    stack: Box<[Value; STACK_SIZE]>,
}