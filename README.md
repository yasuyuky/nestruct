# Nestruct: A Rust Library for Flattening and Nesting Structs

Nestruct is a Rust library that provides macros to easily flatten or nest structs and enums in your code. It can help simplify the representation of complex data structures, such as web API response types. Nestruct allows you to write concise code and automatically generates the equivalent standard Rust structs and enums.

[![build](https://github.com/yasuyuky/nestruct/workflows/build/badge.svg)](https://github.com/yasuyuky/nestruct/actions)

## Features

- `nestruct::flatten!`: Flatten nested structs and enums into a single namespace.
- `nestruct::nest!`: Nest flattened structs and enums into their original hierarchical structure.

## Example
Using the `flatten!` macro, you can transform the following nested structure:

```rust
mod foo_bar {
    #[derive(serde::Deserialize)]
    nestruct::flatten! {
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
}
```

Into the following flattened structure:

```rust
mod foo_bar {
    #[derive(serde::Deserialize)]
    pub struct Qux {
        pub quux: Option<(usize, usize)>,
        pub quuz: Option<String>,
    }
    #[derive(serde::Deserialize)]
    pub struct Baz {
        pub qux: Vec<Qux>,
        pub corge: String,
    }
    #[derive(serde::Deserialize)]
    pub enum Grault {
        Garply,
        Waldo { wubble: String },
        Wubble(Option<usize>, Option<usize>),
    }
    #[derive(serde::Deserialize)]
    pub struct FooBar {
        /// foo
        pub foo: Option<String>,
        /// bar
        pub bar: Option<Vec<Option<Vec<usize>>>>,
        pub baz: Baz,
        pub grault: Option<Grault>,
    }
}
```

Similarly, using the `nest!` macro, you can nest a flattened structure like this:

```rust
nestruct::nest! {
    #[derive(serde::Deserialize)]
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
```

Into the following nested structure:

```rust
pub mod foo_bar {
    pub mod baz {
        pub mod qux {
            #[derive(serde::Deserialize)]
            pub struct Qux {
                pub quux: Option<(usize, usize)>,
                pub quuz: Option<String>,
            }
        }
        #[derive(serde::Deserialize)]
        pub struct Baz {
            pub qux: Vec<qux::Qux>,
            pub corge: String,
        }
    }
    pub mod grault {
        #[derive(serde::Deserialize)]
        pub enum Grault {
            Garply,
            Waldo { wubble: String },
            Wubble(Option<usize>, Option<usize>),
        }
    }
    #[derive(serde::Deserialize)]
    pub struct FooBar {
        /// foo
        pub foo: Option<String>,
        /// bar
        pub bar: Option<Vec<Option<Vec<usize>>>>,
        pub baz: baz::Baz,
        pub grault: Option<grault::Grault>,
    }
}
```

## Installation
Add the following dependency to your Cargo.toml:

```toml
[dependencies]
nestruct = "0.1.0"
```

## License
Nestruct is distributed under the MIT License. See the [LICENSE file](README.md) for more information.
