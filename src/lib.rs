use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{braced, parse::Parse, punctuated::Punctuated, token, Ident, Token, Type};

#[derive(Clone)]
struct Nestruct {
    ident: Ident,
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
        braced!(content in input);
        let fields = content.parse_terminated(NestableField::parse)?;
        Ok(Nestruct { ident, fields })
    }
}

impl Parse for NestableField {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        let colon_token = input.parse()?;
        let ident = format_ident!("{}", name.to_string().to_case(Case::Pascal));
        let ty = FieldType::parse_with_context(input, ident)?;
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
            braced!(content in input);
            let fields = content.parse_terminated(NestableField::parse)?;
            Ok(FieldType::Struct(Nestruct { ident, fields }))
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
        match ty {
            FieldType::Struct(nestruct) => {
                let ident = nestruct.ident.clone();
                tokens.extend(generate_structs(nestruct));
                fields.push(quote! { #name #colon_token #ident });
            }
            FieldType::Type(ty) => fields.push(quote! { #name #colon_token #ty }),
        };
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
    let nestruct = syn::parse_macro_input!(input as Nestruct);
    let tokens = generate_structs(nestruct);
    let out_token = tokens.into_iter().collect::<TokenStream>();
    return out_token.into();
}
