use convert_case::{Case, Casing};
use proc_macro::TokenStream;
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
    attrs: Vec<Attribute>,
    name: Ident,
    is_array: bool,
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
        let attrs = input.call(Attribute::parse_outer)?;
        let name: Ident = input.parse()?;
        input.parse::<token::Colon>()?;
        let ctxattrs = input.call(Attribute::parse_outer)?;
        let ident = format_ident!("{}", name.to_string().to_case(Case::Pascal));
        let (is_array, ty) = if input.peek(token::Bracket) {
            let content;
            bracketed!(content in input);
            (
                true,
                FieldType::parse_with_context(&content, ctxattrs, ident)?,
            )
        } else {
            (
                false,
                FieldType::parse_with_context(input, ctxattrs, ident)?,
            )
        };
        Ok(NestableField {
            attrs,
            name,
            is_array,
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

fn generate_structs(nestruct: Nestruct, rootattrs: &[Attribute]) -> Vec<TokenStream> {
    let mut tokens = Vec::new();
    let mut fields = Vec::new();
    let mut structattrs = nestruct.attrs.clone();
    structattrs.extend(rootattrs.iter().map(|a| a.clone()));
    for NestableField {
        attrs,
        name,
        is_array,
        ty,
    } in nestruct.fields
    {
        let ty_token = match ty {
            FieldType::Struct(nestruct) => {
                let ident = nestruct.ident.clone();
                tokens.extend(generate_structs(nestruct, &structattrs));
                quote! { #ident }
            }
            FieldType::Type(ty) => quote! { #ty },
        };
        if is_array {
            fields.push(quote! { #(#attrs)* #name : Vec<#ty_token> });
        } else {
            fields.push(quote! { #(#attrs)* #name : #ty_token });
        }
    }
    let ident = nestruct.ident;
    tokens.push(
        quote! {
            #(#structattrs)*
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
    let attrs = vec![];
    let tokens = generate_structs(nestruct, &attrs);
    let out_token = tokens.into_iter().collect::<TokenStream>();
    return out_token.into();
}
