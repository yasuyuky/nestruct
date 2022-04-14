use proc_macro::TokenStream;
use syn::{braced, parse::Parse, punctuated::Punctuated, token, Ident, Token, Type};

struct Nestruct {
    ident: Ident,
    brace_token: token::Brace,
    fields: Punctuated<NestableField, Token![,]>,
}

struct NestableField {
    name: Ident,
    colon_token: Token![:],
    ty: FieldType,
}

enum FieldType {
    Struct(Nestruct),
    Type(Type),
}


#[proc_macro]
pub fn nestruct(input: TokenStream) -> TokenStream {
    return TokenStream::new();
}
