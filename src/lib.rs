use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{
    braced, bracketed, parse::Parse, punctuated::Punctuated, token, Attribute, Ident, Token, Type,
};

#[derive(Clone)]
struct Nestruct {
    attrs: Vec<Attribute>,
    ident: Ident,
    fields: Punctuated<NestableField, Token![,]>,
}

#[derive(Clone)]
struct NestableField {
    field_attrs: Vec<Attribute>,
    name: Ident,
    collection: Option<Ident>,
    ty: FieldType,
}

#[derive(Clone)]
enum FieldType {
    Struct(Nestruct),
    Type(Type),
}

impl Parse for Nestruct {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let ident = input.parse()?;
        let content;
        braced!(content in input);
        let fields = content.parse_terminated(NestableField::parse)?;
        Ok(Nestruct {
            attrs,
            ident,
            fields,
        })
    }
}

impl Parse for NestableField {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let field_attrs = input.call(Attribute::parse_outer)?;
        let name: Ident = input.parse()?;
        input.parse::<token::Colon>()?;
        let ident = format_ident!("{}", name.to_string().to_case(Case::Pascal));
        let buffer;
        let (collection, input) = if input.peek(token::Bracket) {
            bracketed!(buffer in input);
            (Some(format_ident!("Vec")), &buffer)
        } else {
            (None, input)
        };
        let ctxattrs = input.call(Attribute::parse_outer)?;
        let ty = FieldType::parse_with_context(input, ctxattrs, ident)?;
        Ok(NestableField {
            field_attrs,
            name,
            collection,
            ty,
        })
    }
}

impl FieldType {
    fn parse_with_context(
        input: syn::parse::ParseStream,
        attrs: Vec<Attribute>,
        ident: Ident,
    ) -> syn::Result<Self> {
        if input.peek(token::Brace) {
            let content;
            braced!(content in input);
            let fields = content.parse_terminated(NestableField::parse)?;
            Ok(FieldType::Struct(Nestruct {
                attrs,
                ident,
                fields,
            }))
        } else {
            Ok(FieldType::Type(input.parse()?))
        }
    }
}

fn generate_structs(nest: bool, nestruct: Nestruct, parent_attrs: &[Attribute]) -> TokenStream2 {
    let mut tokens = Vec::new();
    let mut fields = Vec::new();
    let mut attrs = nestruct.attrs.clone();
    attrs.extend(parent_attrs.iter().map(|a| a.clone()));
    for field in nestruct.fields {
        let ty_token = match field.ty {
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
        let field_attrs = field.field_attrs;
        let name = field.name;
        match field.collection {
            Some(c) => fields.push(quote! { #(#field_attrs)* #name : #c<#ty_token> }),
            None => fields.push(quote! { #(#field_attrs)* #name : #ty_token }),
        }
    }
    let ident = nestruct.ident;
    tokens.push(quote! {
        #(#attrs)* pub struct #ident {
            #(#fields),*
        }
    });
    let token = tokens.into_iter().collect::<TokenStream2>();
    if nest {
        let ns = format_ident!("{}", ident.to_string().to_case(Case::Snake));
        quote! { pub mod #ns { #token } }
    } else {
        token
    }
}

#[proc_macro]
pub fn nestruct(input: TokenStream) -> TokenStream {
    let nestruct = syn::parse_macro_input!(input as Nestruct);
    let attrs = vec![];
    generate_structs(true, nestruct, &attrs).into()
}
