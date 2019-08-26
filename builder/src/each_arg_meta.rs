use syn::parse::{Parse, ParseStream, Result};
use syn::{Error, Lit, LitStr, Meta};

pub(crate) struct EachArgMeta {
    pub(crate) fn_name: LitStr,
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
