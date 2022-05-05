use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{braced, bracketed, punctuated::Punctuated, token, Attribute, Ident, Token, Type};

struct Nestruct {
    attrs: Vec<Attribute>,
    ident: Ident,
    fields: Punctuated<NestableField, Token![,]>,
}

struct NestableField {
    field_attrs: Vec<Attribute>,
    name: Ident,
    optional: bool,
    collection: Option<Ident>,
    ty: Option<FieldType>,
}

enum FieldType {
    Struct(Nestruct),
    Type(Type),
}

impl Parse for Nestruct {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let ident = input.parse()?;
        let content;
        braced!(content in input);
        let fields = content.parse_terminated(NestableField::parse)?;
        Ok(Nestruct { attrs, ident, fields })
    }
}

impl Parse for NestableField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let field_attrs = input.call(Attribute::parse_outer)?;
        let name: Ident = input.parse()?;
        if input.peek(token::Colon) {
            input.parse::<token::Colon>()?;
            let ident = format_ident!("{}", name.to_string().to_case(Case::Pascal));
            let buffer;
            let (collection, tyinput) = if input.peek(token::Bracket) {
                bracketed!(buffer in input);
                (Some(format_ident!("Vec")), &buffer)
            } else {
                (None, input)
            };
            let ctxattrs = tyinput.call(Attribute::parse_outer)?;
            let ty = Some(FieldType::parse_with_context(tyinput, ctxattrs, ident)?);
            let optional = input.parse::<Option<Token![?]>>()?.is_some();
            Ok(NestableField { field_attrs, name, optional, collection, ty })
        } else {
            Ok(NestableField { field_attrs, name, optional: false, collection: None, ty: None })
        }
    }
}

impl FieldType {
    fn parse_with_context(
        input: ParseStream,
        attrs: Vec<Attribute>,
        ident: Ident,
    ) -> syn::Result<Self> {
        if input.peek(token::Brace) {
            let content;
            braced!(content in input);
            let fields = content.parse_terminated(NestableField::parse)?;
            Ok(FieldType::Struct(Nestruct { attrs, ident, fields }))
        } else {
            Ok(FieldType::Type(input.parse()?))
        }
    }
}

fn generate_structs(nest: bool, nestruct: Nestruct, parent_attrs: &[Attribute]) -> TokenStream2 {
    let mut tokens = Vec::new();
    let mut fields = Vec::new();
    let mut variants = Vec::new();
    let mut attrs = Vec::from(parent_attrs);
    attrs.extend(nestruct.attrs.iter().cloned());
    for field in nestruct.fields {
        let field_attrs = field.field_attrs;
        let name = field.name;
        match field.ty {
            Some(ty) => {
                let ty_token = match ty {
                    FieldType::Struct(nestruct) => {
                        let ident = nestruct.ident.clone();
                        tokens.push(generate_structs(nest, nestruct, &attrs));
                        if nest {
                            let ns = format_ident!("{}", ident.to_string().to_case(Case::Snake));
                            quote! { #ns::#ident }
                        } else {
                            quote! { #ident }
                        }
                    }
                    FieldType::Type(ty) => quote! { #ty },
                };
                match field.collection {
                    Some(c) => fields.push(quote! { #(#field_attrs)* #name : #c<#ty_token> }),
                    None => fields.push(quote! { #(#field_attrs)* #name : #ty_token }),
                }
            }
            None => {
                let variant_name = format_ident!("{}", name.to_string().to_case(Case::Pascal));
                variants.push(quote! { #(#field_attrs)* #variant_name })
            }
        }
    }
    let ident = nestruct.ident;
    if variants.len() > 0 {
        if fields.len() > 0 {
            panic!("Cannot have both variants and fields in a brace");
        } else {
            tokens.push(quote! { #(#attrs)* pub enum #ident { #(#variants),* } });
        }
    } else {
        tokens.push(quote! { #(#attrs)* pub struct #ident { #(#fields),* } });
    }
    let token = tokens.into_iter().collect::<TokenStream2>();
    if nest {
        let ns = format_ident!("{}", ident.to_string().to_case(Case::Snake));
        quote! { pub mod #ns { #token } }
    } else {
        token
    }
}

#[proc_macro]
pub fn nest(input: TokenStream) -> TokenStream {
    let nestruct = syn::parse_macro_input!(input as Nestruct);
    let attrs = vec![];
    generate_structs(true, nestruct, &attrs).into()
}

#[proc_macro]
pub fn flatten(input: TokenStream) -> TokenStream {
    let nestruct = syn::parse_macro_input!(input as Nestruct);
    let attrs = vec![];
    generate_structs(false, nestruct, &attrs).into()
}
