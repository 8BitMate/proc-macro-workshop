extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::GenericArgument;
use syn::PathArguments::AngleBracketed;
use syn::{
    parse_macro_input, Data, DeriveInput, Error, Fields, FieldsNamed, Lit, LitStr, Meta, Path,
    PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive_builder(input: TokenStream) -> TokenStream {
    let parsed: DeriveInput = parse_macro_input!(input as DeriveInput);

    let name = &parsed.ident;

    let name_builder = Ident::new(&format!("{}Builder", parsed.ident), Span::call_site());

    let expanded = match &parsed.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields_named) => {
                let builder_fn_impl = impl_builder_fn(&name, &name_builder, fields_named);
                let build_fn_impl = impl_build_fn(&name, &name_builder, fields_named);
                let name_builder_struct = make_name_builder(&name_builder, fields_named);
                let name_builder_impl = impl_name_builder_fns(&name_builder, fields_named)
                    .unwrap_or_else(|err| err.to_compile_error());

                quote! {
                    #builder_fn_impl

                    #build_fn_impl

                    #name_builder_struct

                    #name_builder_impl
                }
            }

            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    expanded.into()
}

fn get_inner_type<'a>(ty: &'a Type, outer_type: &str) -> Option<&'a Type> {
    if let Type::Path(TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments,
        },
    }) = ty
    {
        if let Some(PathSegment {
            ident,
            arguments: AngleBracketed(bracketed),
        }) = segments.first()
        {
            if ident == outer_type {
                if let Some(GenericArgument::Type(ty)) = bracketed.args.first() {
                    return Some(ty);
                }
            }
        }
    }
    None
}

fn get_inner_optional_type(ty: &Type) -> Option<&Type> {
    get_inner_type(ty, "Option")
}

fn get_inner_vec_type(ty: &Type) -> Option<&Type> {
    get_inner_type(ty, "Vec")
}

fn make_name_builder(name_builder: &Ident, fields_named: &FieldsNamed) -> proc_macro2::TokenStream {
    let fields = &fields_named.named;
    let new_fields = fields.iter().map(|field| {
        let name = &field.ident;
        if let Some(inner_ty) = get_inner_optional_type(&field.ty) {
            quote! {#name: std::option::Option<#inner_ty>}
        } else if let Some(inner_ty) = get_inner_vec_type(&field.ty) {
            quote! {#name: std::vec::Vec<#inner_ty>}
        } else {
            let ty = &field.ty;
            quote! {#name: std::option::Option<#ty>}
        }
    });

    quote! {
        pub struct #name_builder {
            #( #new_fields ),*
        }
    }
}

struct EachArgMeta {
    fn_name: LitStr,
}

impl Parse for EachArgMeta {
    fn parse(input: ParseStream) -> Result<Self> {
        let meta: Meta = input.parse()?;
        let err = Err(Error::new_spanned(
            &meta,
            "expected `builder(each = \"...\")`",
        ));
        match meta {
            Meta::NameValue(meta_name_value) => {
                if let Some(path_segment) = meta_name_value.path.segments.first() {
                    if path_segment.ident == "each" {
                        if let Lit::Str(lit_str) = meta_name_value.lit {
                            if lit_str.value() != "" {
                                return Ok(EachArgMeta { fn_name: lit_str });
                            }
                        }
                    }
                }
                err
            }
            _ => err,
        }
    }
}

fn impl_name_builder_fns(
    name_builder: &Ident,
    fields_named: &FieldsNamed,
) -> Result<proc_macro2::TokenStream> {
    let fields = &fields_named.named;
    let mut builder_functions = quote!();

    for field in fields.iter() {
        let name = &field.ident;

        if field.attrs.len() > 0 {
            for attr in &field.attrs {
                let builder_each: EachArgMeta = attr.parse_args()?;
                let fn_name =
                    Ident::new(&builder_each.fn_name.value(), builder_each.fn_name.span());
                let _fn_name = Ident::new(&format!("_{}", &fn_name), fn_name.span());

                let ty = if let Some(inner_ty) = get_inner_vec_type(&field.ty) {
                    inner_ty
                } else {
                    &field.ty
                };
                builder_functions = quote! {
                    #builder_functions

                    pub fn #fn_name(&mut self, #_fn_name: #ty) -> &mut Self {
                        self.#name.push(#_fn_name);
                        self
                    }
                };
            }
        } else {
            let builder_fn = if let Some(inner_ty) = get_inner_optional_type(&field.ty) {
                quote! {
                    pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            } else if let Some(_) = get_inner_vec_type(&field.ty) {
                let ty = &field.ty;
                quote! {
                      pub fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = #name;
                        self
                    }
                }
            } else {
                let ty = &field.ty;
                quote! {
                       pub fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            };
            builder_functions = quote! {
                #builder_functions

                #builder_fn
            }
        }
    }

    Ok(quote! {
        impl #name_builder {
            #builder_functions
        }
    })
}

fn impl_builder_fn(
    name: &Ident,
    name_builder: &Ident,
    fields_named: &FieldsNamed,
) -> proc_macro2::TokenStream {
    let fields = &fields_named.named;
    let mut default_fields = quote!();

    for field in fields.iter() {
        let name = &field.ident;

        default_fields = quote! {
            #default_fields
            #name: std::default::Default::default(),
        };
    }

    quote! {
        impl #name {
            pub fn builder() -> #name_builder {
                #name_builder {
                    #default_fields
                }
            }
        }
    }
}

fn impl_build_fn(
    name: &Ident,
    name_builder: &Ident,
    fields_named: &FieldsNamed,
) -> proc_macro2::TokenStream {
    let fields = &fields_named.named;
    let mut name_fields = quote!();

    for field in fields.iter() {
        let name = &field.ident;
        let name_string = name.as_ref().unwrap().to_string();
        if let Some(_) = get_inner_optional_type(&field.ty) {
            name_fields = quote! {
                #name_fields
                #name: self.#name.take(),
            }
        } else if let Some(_) = get_inner_vec_type(&field.ty) {
            name_fields = quote! {
                #name_fields
                #name: std::mem::replace(&mut self.#name, std::default::Default::default()),
            }
        } else {
            let error_message = format!("{} field is missing", &name_string);
            name_fields = quote! {
                #name_fields
                #name: self.#name.take().ok_or_else(|| #error_message)?,
            }
        }
    }

    quote! {
        impl #name_builder {

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#name {
                    #name_fields
                })
            }
        }
    }
}
