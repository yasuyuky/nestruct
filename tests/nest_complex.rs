nestruct::nest! {
    FooBar {
        /// foo
        foo: String?,
        /// bar
        bar: [[usize]?]?,
        baz: {
            qux: [{
                quux: (usize, usize)?,
                quuz: String?,
            }],
            corge: String,
        },
        grault: {
            garply,
            waldo { wubble: String },
            wubble(usize?, usize?)
        }?,
    }
}
