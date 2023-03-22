use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{
    braced, bracketed, parenthesized, parse_str, token, Attribute, Ident, Token, Type, TypePath,
};

struct Nestruct {
    attrs: Vec<Attribute>,
    ident: Ident,
    fields: Punctuated<NestableField, Token![,]>,
}

struct NestableField {
    field_attrs: Vec<Attribute>,
    name: Ident,
    fvtype: FVType,
}

enum NestableType {
    Nestruct(Nestruct),
    Type(Type),
}

enum FVType {
    Field { meta_types: Vec<TypePath>, ty: NestableType },
    UnitVariant,
    TupleVariant { types: Punctuated<(Vec<TypePath>, Type), Token![,]> },
    StructVariant { nestruct: Nestruct },
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

fn parse_nest_types(
    input: ParseStream,
    ident: Ident,
) -> syn::Result<(Vec<TypePath>, NestableType)> {
    let parse_nestable_type = |input: ParseStream| {
        let attrs = input.call(Attribute::parse_outer)?;
        NestableType::parse_with_context(input, attrs, ident.clone())
    };
    parse_shorthand_types(input, parse_nestable_type)
}

fn parse_simple_types(input: ParseStream) -> syn::Result<(Vec<TypePath>, Type)> {
    let parse_simple_type = |input: ParseStream| input.parse::<Type>();
    parse_shorthand_types(input, parse_simple_type)
}

fn parse_shorthand_types<T>(
    input: ParseStream,
    parser: impl Fn(ParseStream) -> syn::Result<T>,
) -> syn::Result<(Vec<TypePath>, T)> {
    let mut outer_types = Vec::new();
    let buffer;
    let (mut inner_types, ty) = if input.peek(token::Bracket) {
        bracketed!(buffer in input);
        outer_types.push(parse_str::<TypePath>("Vec")?);
        parse_shorthand_types(&buffer, parser)?
    } else {
        (Vec::new(), parser(input)?)
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
        let ident = format_ident!("{}", name.to_string().to_case(Case::Pascal));
        if input.peek(token::Colon) {
            input.parse::<token::Colon>()?;
            let (meta_types, ty) = parse_nest_types(input, ident)?;
            Ok(NestableField { field_attrs, name, fvtype: FVType::Field { meta_types, ty } })
        } else if input.peek(token::Brace) {
            let content;
            braced!(content in input);
            let fields = content.parse_terminated(NestableField::parse)?;
            let nestruct = Nestruct { attrs: vec![], ident, fields };
            Ok(NestableField { field_attrs, name, fvtype: FVType::StructVariant { nestruct } })
        } else if input.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            let types = content.parse_terminated(parse_simple_types)?;
            let fvtype = FVType::TupleVariant { types };
            Ok(NestableField { field_attrs, name, fvtype })
        } else {
            Ok(NestableField { field_attrs, name, fvtype: FVType::UnitVariant })
        }
    }
}

impl NestableType {
    fn parse_with_context(
        input: ParseStream,
        attrs: Vec<Attribute>,
        ident: Ident,
    ) -> syn::Result<Self> {
        if input.peek(token::Brace) {
            let content;
            braced!(content in input);
            let fields = content.parse_terminated(NestableField::parse)?;
            Ok(Self::Nestruct(Nestruct { attrs, ident, fields }))
        } else {
            Ok(Self::Type(input.parse()?))
        }
    }
}

fn generate_field_type<'a>(
    ty: &'a NestableType,
    meta_types: &[TypePath],
    nest: bool,
) -> (TokenStream2, Option<&'a Nestruct>) {
    let (mut ty_token, nestruct) = match ty {
        NestableType::Nestruct(nestruct) => {
            let ident = nestruct.ident.clone();
            if nest {
                let ns = format_ident!("{}", ident.to_string().to_case(Case::Snake));
                (quote! { #ns::#ident }, Some(nestruct))
            } else {
                (quote! { #ident }, Some(nestruct))
            }
        }
        NestableType::Type(ty) => (quote! { #ty }, None),
    };
    for meta_type in meta_types {
        ty_token = quote! { #meta_type<#ty_token> };
    }
    (ty_token, nestruct)
}

fn generate_fields<'a>(
    nestablefields: &[&'a NestableField],
    nest: bool,
    pubtoken: TokenStream2,
) -> (Vec<&'a Nestruct>, bool, Vec<TokenStream2>) {
    let mut fields = Vec::new();
    let mut variants = Vec::new();
    let mut children = Vec::new();
    for field in nestablefields {
        let field_attrs = &field.field_attrs;
        let name = &field.name;
        let vname = format_ident!("{}", name.to_string().to_case(Case::Pascal));
        match &field.fvtype {
            FVType::Field { meta_types, ty } => {
                let (ty_token, nestruct) = generate_field_type(ty, meta_types, nest);
                fields.push(quote! { #(#field_attrs)* #pubtoken #name : #ty_token });
                if let Some(nestruct) = nestruct {
                    children.push(nestruct);
                }
            }
            FVType::UnitVariant => {
                variants.push(quote! { #(#field_attrs)* #vname });
            }
            FVType::TupleVariant { types } => {
                let mut ty_tokens = Vec::new();
                for (meta_types, ty) in types {
                    let (ty_token, _) =
                        generate_field_type(&NestableType::Type(ty.clone()), meta_types, nest);
                    ty_tokens.push(ty_token);
                }
                variants.push(quote! { #(#field_attrs)* #vname(#(#ty_tokens),*) });
            }
            FVType::StructVariant { nestruct } => {
                let fields: Vec<&NestableField> = nestruct.fields.iter().collect();
                let (grandchildren, is_variant, fields) = generate_fields(&fields, nest, quote! {});
                if is_variant {
                    panic!("Children of struct variants shoukd not be variants");
                } else {
                    variants.push(quote! { #(#field_attrs)* #vname{#(#fields),*} });
                }
                children.extend(grandchildren);
            }
        }
    }
    if !variants.is_empty() {
        if !fields.is_empty() {
            panic!("Cannot have both variants and fields in a brace");
        } else {
            (children, true, variants)
        }
    } else {
        (children, false, fields)
    }
}

fn is_reset_attr(attr: &Attribute) -> bool {
    if let Some(ident) = attr.path().get_ident() {
        if ident == "nestruct" {
            if let Ok(arg) = attr.parse_args_with(Ident::parse_any) {
                return arg == "reset";
            }
        }
    }
    false
}

fn generate_structs(nest: bool, nestruct: &Nestruct, parent_attrs: &[Attribute]) -> TokenStream2 {
    let mut tokens = Vec::new();
    let fields: Vec<&NestableField> = nestruct.fields.iter().collect();
    let mut attrs = Vec::from(parent_attrs);
    for attr in nestruct.attrs.clone() {
        if is_reset_attr(&attr) {
            attrs.clear()
        } else {
            attrs.push(attr)
        }
    }
    let (children, is_variant, fvs) = generate_fields(&fields, nest, quote! { pub });
    for child in children {
        tokens.push(generate_structs(nest, child, &attrs))
    }
    let ident = nestruct.ident.clone();
    tokens.push(if is_variant {
        quote! { #(#attrs)* pub enum #ident { #(#fvs),* } }
    } else {
        quote! { #(#attrs)* pub struct #ident { #(#fvs),* } }
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
pub fn nest(input: TokenStream) -> TokenStream {
    let nestruct = syn::parse_macro_input!(input as Nestruct);
    let attrs = vec![];
    generate_structs(true, &nestruct, &attrs).into()
}

#[proc_macro]
pub fn flatten(input: TokenStream) -> TokenStream {
    let nestruct = syn::parse_macro_input!(input as Nestruct);
    let attrs = vec![];
    generate_structs(false, &nestruct, &attrs).into()
}
