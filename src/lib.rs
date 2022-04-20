use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{braced, bracketed, parse::Parse, punctuated::Punctuated, token, Ident, Token, Type};

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
        let ty = if input.peek(token::Bracket) {
            let content;
            bracketed!(content in input);
            FieldType::parse_with_context(&content, ident)?
        } else {
            FieldType::parse_with_context(input, ident)?
        };
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
        let ty_token = match ty {
            FieldType::Struct(nestruct) => {
                let ident = nestruct.ident.clone();
                tokens.extend(generate_structs(nestruct));
                quote! { #ident }
            }
            FieldType::Type(ty) => quote! { #ty },
        };
        fields.push(quote! { #name #colon_token #ty_token });
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
