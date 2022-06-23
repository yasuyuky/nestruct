mod foo_bar {
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
}
