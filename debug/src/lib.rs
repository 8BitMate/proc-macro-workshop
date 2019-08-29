extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, Attribute, Data, DeriveInput, Error, Field, Fields, Lit, Meta,
    MetaNameValue, Result,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ref input = parse_macro_input!(input as DeriveInput);
    let ref name = input.ident;
    let ref name_string = name.to_string();

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
        impl std::fmt::Debug for #name {
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
    if let Ok(Meta::NameValue(MetaNameValue { path, lit, .. })) = Attribute::parse_meta(attr) {
        if path.is_ident("debug") {
            if let Lit::Str(str_lit) = lit {
                return Ok(quote_spanned! {attr.span()=>
                    .field(#field_name_string, &format_args!(#str_lit, &self.#field_name))
                });
            }
        }
    }
    Err(Error::new_spanned(&attr, "expected `debug = \"...\"`"))
}
