use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{braced, parse::Parse, punctuated::Punctuated, token, Ident, Token, Type};

#[derive(Clone)]
struct Nestruct {
    ident: Ident,
    brace_token: token::Brace,
    fields: Punctuated<NestableField, Token![,]>,
}

#[derive(Clone)]
struct NestableField {
    name: Ident,
    colon_token: Token![:],
    ty: FieldType,
}

#[derive(Clone)]
enum FieldType {
    Struct(Nestruct),
    Type(Type),
}

impl Parse for Nestruct {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        let content;
        let brace_token = braced!(content in input);
        let fields = content.parse_terminated(NestableField::parse)?;
        Ok(Nestruct {
            ident,
            brace_token,
            fields,
        })
    }
}

impl Parse for NestableField {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        let colon_token = input.parse()?;
        let ty = FieldType::parse_with_context(input, name.clone())?;
        Ok(NestableField {
            name,
            colon_token,
            ty,
        })
    }
}

impl FieldType {
    fn parse_with_context(input: syn::parse::ParseStream, ident: Ident) -> syn::Result<Self> {
        if input.peek(token::Brace) {
            let content;
            let brace_token = braced!(content in input);
            let fields = content.parse_terminated(NestableField::parse)?;
            Ok(FieldType::Struct(Nestruct {
                ident,
                brace_token,
                fields,
            }))
        } else {
            Ok(FieldType::Type(input.parse()?))
        }
    }
}

fn generate_structs(nestruct: Nestruct) -> Vec<TokenStream> {
    let mut tokens = Vec::new();
    let mut fields = Vec::new();
    for NestableField {
        name,
        colon_token,
        ty,
    } in nestruct.fields
    {
        let ty: Ident = match ty {
            FieldType::Struct(nestruct) => {
                tokens.extend(generate_structs(nestruct.clone()));
                nestruct.ident
            }
            FieldType::Type(ty) => format_ident!("{}", ty.to_token_stream().to_string()),
        };
        fields.push(quote! { #name #colon_token #ty });
    }
    let ident = nestruct.ident;
    tokens.push(
        quote! {
            pub struct #ident {
                #(#fields),*
            }
        }
        .into(),
    );
    tokens
}

#[proc_macro]
pub fn nestruct(input: TokenStream) -> TokenStream {
    return TokenStream::new();
}
