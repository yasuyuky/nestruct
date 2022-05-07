use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{braced, bracketed, parse_str, token, Attribute, Ident, Token, Type, TypePath};

struct Nestruct {
    attrs: Vec<Attribute>,
    ident: Ident,
    fields: Punctuated<NestableField, Token![,]>,
}

struct NestableField {
    field_attrs: Vec<Attribute>,
    name: Ident,
    meta_types: Vec<TypePath>,
    ty: FieldType,
    variant: bool,
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

fn parse_nest_types(input: ParseStream, ident: Ident) -> syn::Result<(Vec<TypePath>, FieldType)> {
    let mut outer_types = Vec::new();
    let buffer;
    let (mut inner_types, ty) = if input.peek(token::Bracket) {
        bracketed!(buffer in input);
        outer_types.push(parse_str::<TypePath>("Vec")?);
        parse_nest_types(&buffer, ident)?
    } else {
        let attrs = input.call(Attribute::parse_outer)?;
        (Vec::new(), FieldType::parse_with_context(input, attrs, ident)?)
    };
    if input.parse::<Option<Token![?]>>()?.is_some() {
        outer_types.push(parse_str::<TypePath>("Option")?);
    }
    inner_types.extend(outer_types);
    Ok((inner_types, ty))
}

impl Parse for NestableField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let field_attrs = input.call(Attribute::parse_outer)?;
        let name: Ident = input.parse()?;
        let meta_types = Vec::new();
        if input.peek(token::Colon) {
            input.parse::<token::Colon>()?;
            let ident = format_ident!("{}", name.to_string().to_case(Case::Pascal));
            let (meta_types, ty) = parse_nest_types(input, ident)?;
            Ok(NestableField { field_attrs, name, meta_types, ty, variant: false })
        } else {
            let ty = FieldType::Type(parse_str("()")?);
            Ok(NestableField { field_attrs, name, meta_types, ty, variant: true })
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
        match field.variant {
            false => {
                let mut ty_token = match field.ty {
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
                for meta_type in field.meta_types {
                    ty_token = quote! { #meta_type<#ty_token> };
                }
                fields.push(quote! { #(#field_attrs)* #name : #ty_token });
            }
            true => {
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
