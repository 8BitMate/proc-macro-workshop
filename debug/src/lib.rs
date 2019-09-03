extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, parse_quote, Attribute, Data, DeriveInput, Error, Field, Fields,
    GenericArgument, Generics, Lit, Meta, MetaList, MetaNameValue, NestedMeta, Path, PathArguments,
    PathSegment, Result, Type, TypeParam, TypePath, WhereClause, WherePredicate,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let name_string = name.to_string();
    let bounds = parse_bounds(&input.attrs).unwrap_or_else(|err| {
        err.to_compile_error();
        Vec::default()
    });

    let generics;
    let where_clause: WhereClause;

    let (impl_generics, ty_generics, where_clause) = if bounds.is_empty() {
        let phantom_only_types = phantom_only_types(&input.data, &input.generics);
        generics = set_generic_trait_bounds(&input.generics, &input.data, phantom_only_types);
        generics.split_for_impl()
    } else {
        let (impl_generics, ty_generics, _) = input.generics.split_for_impl();
        where_clause = parse_quote! {
        where
        #(
            #bounds
        )*};
        (impl_generics, ty_generics, Some(&where_clause))
    };

    let debug_struct_fields = match input.data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => fields.named.iter().map(|field| {
                let field_name = &field.ident;
                let field_name_string = field_name.as_ref().unwrap().to_string();

                if field.attrs.len() == 1 {
                    let attr = field.attrs.iter().next().unwrap();
                    meta_attr_field(&field, &attr).unwrap_or_else(|err| err.to_compile_error())
                } else {
                    quote_spanned! {field.span()=>
                        .field(#field_name_string, &self.#field_name)
                    }
                }
            }),
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },

        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };

    let expanded = quote! {
        impl #impl_generics ::std::fmt::Debug for #name #ty_generics #where_clause{
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                fmt.debug_struct(#name_string)
                   #(
                       #debug_struct_fields
                   )*
                   .finish()
            }
        }
    };

    expanded.into()
}

fn meta_attr_field(field: &Field, attr: &Attribute) -> Result<proc_macro2::TokenStream> {
    let field_name = &field.ident;
    let field_name_string = field_name.as_ref().unwrap().to_string();
    let meta = Attribute::parse_meta(attr)?;

    if let Meta::NameValue(MetaNameValue {
        path,
        lit: Lit::Str(lit),
        ..
    }) = &meta
    {
        if path.is_ident("debug") {
            return Ok(quote_spanned! {attr.span()=>
                .field(#field_name_string, &format_args!(#lit, &self.#field_name))
            });
        }
    }
    Err(Error::new_spanned(&meta, "expected `debug = \"...\"`"))
}

fn set_generic_trait_bounds<'a>(
    generics: &Generics,
    data: &Data,
    phantom_only_types: Vec<Type>,
) -> Generics {
    let mut generics_clone = generics.clone();
    let where_clause = generics_clone.make_where_clause();
    let type_params = generics.type_params();

    match data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let associated_types: Vec<_> = fields
                    .named
                    .iter()
                    .flat_map(|field| {
                        associated_types(generics, &field.ty)
                            .into_iter()
                            .filter(|(_, ty)| !phantom_only_types.contains(ty))
                    })
                    .collect();

                for type_param in type_params.filter(|type_param| {
                    let ty = &type_param.ident;
                    let ty = &parse_quote!(#ty);
                    !phantom_only_types.contains(ty)
                        && !associated_types
                            .iter()
                            .any(|(type_param2, _)| type_param == type_param2)
                }) {
                    let gen_type = &type_param.ident;
                    where_clause
                        .predicates
                        .push(parse_quote!(#gen_type: ::std::fmt::Debug));
                }
                for (_, ty) in associated_types {
                    where_clause
                        .predicates
                        .push(parse_quote!(#ty: ::std::fmt::Debug));
                }
            }
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },

        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
    generics_clone
}

fn phantom_only_types(data: &Data, generics: &Generics) -> Vec<Type> {
    let mut phantom_only_types: Vec<Type> = Vec::default();

    match data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let type_params: Vec<_> = generics.type_params().collect();
                for type_param in type_params {
                    let filtered: Vec<_> = fields
                        .named
                        .iter()
                        .filter(|field| {
                            let gen_type = &type_param.ident;
                            field.ty != parse_quote! {PhantomData<#gen_type>}
                        })
                        .collect();

                    if filtered.len() != fields.named.len() {
                        let gen_type = &type_param.ident;
                        let phantom_type: Type = parse_quote! {#gen_type};

                        if !filtered
                            .iter()
                            .any(|field| inner_most_types(&field.ty).contains(&&phantom_type))
                        {
                            phantom_only_types.push(parse_quote!(#gen_type));
                        }
                    }
                }
            }
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },

        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };
    phantom_only_types
}

fn inner_most_types(ty: &Type) -> Vec<&Type> {
    fn inner_most_types<'a>(ty: &'a Type, types: &mut Vec<&'a Type>) {
        if let Type::Path(ref type_path) = ty {
            if let Some(PathSegment { arguments, .. }) = type_path.path.segments.last() {
                if arguments.is_empty() {
                    types.push(ty);
                } else if let PathArguments::AngleBracketed(arguments) = arguments {
                    for argument in arguments.args.iter() {
                        if let GenericArgument::Type(ty) = argument {
                            inner_most_types(ty, types);
                        }
                    }
                }
            }
        }
    }
    let mut types = Vec::default();
    inner_most_types(ty, &mut types);
    types
}

fn associated_types<'a>(generics: &'a Generics, ty: &'a Type) -> Vec<(&'a TypeParam, &'a Type)> {
    let type_params: Vec<_> = generics.type_params().collect();

    inner_most_types(ty)
        .into_iter()
        .filter_map(|ty| {
            if let Type::Path(TypePath {
                qself: None,
                path:
                    Path {
                        leading_colon: None,
                        segments,
                    },
            }) = ty
            {
                if segments.len() > 1 {
                    let segment = segments.first().unwrap();

                    return Some(type_params.iter().filter_map(move |type_param| {
                        let param_ty = &type_param.ident;

                        if segment == &parse_quote!(#param_ty) {
                            Some((*type_param, ty))
                        } else {
                            None
                        }
                    }));
                }
            }
            None
        })
        .flatten()
        .collect()
}

fn parse_bounds(attrs: &Vec<Attribute>) -> Result<Vec<proc_macro2::TokenStream>> {
    attrs
        .iter()
        .map(|attr| {
            let meta = attr.parse_meta()?;

            if let Meta::List(MetaList { path, nested, .. }) = &meta {
                if nested.len() == 1 && path.is_ident(&"debug") {
                    if let NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                        path,
                        lit: Lit::Str(lit),
                        ..
                    })) = nested.first().unwrap()
                    {
                        let lit: WherePredicate = lit.parse()?;

                        if path.is_ident("bound") {
                            return Ok(quote_spanned! {attr.span()=>
                                #lit,
                            });
                        }
                    }
                }
            }
            Err(Error::new_spanned(
                &meta,
                "expected `debug(bound = \"...\"`)",
            ))
        })
        .collect()
}
