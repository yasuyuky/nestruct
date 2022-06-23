pub mod foo_bar {
    pub mod baz {
        pub mod qux {
            pub struct Qux {
                pub quux: Option<(usize, usize)>,
                pub quuz: Option<String>,
            }
        }
        pub struct Baz {
            pub qux: Vec<qux::Qux>,
            pub corge: String,
        }
    }
    pub mod grault {
        pub enum Grault {
            Garply,
            Waldo { wubble: String },
            Wubble(Option<usize>, Option<usize>),
        }
    }
    pub struct FooBar {
        /// foo
        pub foo: Option<String>,
        /// bar
        pub bar: Option<Vec<Option<Vec<usize>>>>,
        pub baz: baz::Baz,
        pub grault: Option<grault::Grault>,
    }
}
