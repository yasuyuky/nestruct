#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
pub struct Qux {
    pub quux: Option<(usize, usize)>,
    pub quuz: Option<String>,
}
pub struct Baz {
    pub qux: Vec<Qux>,
    pub corge: String,
}
pub enum Grault {
    Garply,
    Waldo { wubble: String },
    Wubble(Option<usize>, Option<usize>),
}
pub struct FooBar {
    /// foo
    pub foo: Option<String>,
    /// bar
    pub bar: Option<Vec<Option<Vec<usize>>>>,
    pub baz: Baz,
    pub grault: Option<Grault>,
}
#[rustc_main]
pub fn main() -> () {
    extern crate test;
    test::test_main_static(&[])
}
