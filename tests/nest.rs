nestruct::nest! {
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    FooBar {
        foo: String?,
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
        }?,
    }
}

#[test]
fn test_nest_deserialize() {
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
        "grault": "garply"
    }
    "#;
    let foobar = serde_json::from_str::<foo_bar::FooBar>(jsonstr).unwrap();
    println!("{:#?}", foobar);
}
