nestruct::flatten! {
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
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

#[test]
fn test_flatten_deserialize() {
    let jsonstr = r#"
    {
        "foo": "foo",
        "bar": [null, [1, 2, 3]],
        "baz": {
            "qux": [
                {
                    "quux": [1, 2],
                    "quuz": "quuz"
                },
                {
                    "quux": [3, 4],
                    "quuz": "quuz2"
                }
            ],
            "corge": "corge"
        },
        "grault": { "wubble": [0, null] }
    }
    "#;
    let foobar = serde_json::from_str::<FooBar>(jsonstr).unwrap();
    println!("{:#?}", foobar);
}
