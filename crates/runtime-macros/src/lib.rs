extern crate proc_macro;
use std::fmt::Debug;

use proc_macro::TokenStream;

use heck::ToShoutySnakeCase;
use proc_macro2::Span;
use syn::parse::Parse;

#[proc_macro_attribute]
pub fn std_tvalue_export(input: TokenStream, annotated_item: TokenStream) -> TokenStream {
    let attr: ExportArguments = syn::parse_macro_input!(input);
    let f: syn::ItemFn = syn::parse(annotated_item).unwrap();
    let name = f.sig.ident.to_string();
    let export_name = format!("std::{}::{name}", attr.module_name());
    let const_ident_name = name.to_shouty_snake_case();
    let const_ident = syn::Ident::new(&const_ident_name, Span::call_site());
    let value = syn::LitStr::new(&export_name, Span::call_site());
    (quote::quote!(
        #[cfg(feature = "names")]
        pub const #const_ident: &str = #value;
        #[cfg(feature = "runtime")]
        #[export_name = #value]
        #f
    ))
    .into()
}

struct ExportArguments {
    module_name: syn::LitStr,
}

impl ExportArguments {
    fn module_name(&self) -> String {
        self.module_name
            .token()
            .to_string()
            .trim_start_matches('"')
            .trim_end_matches('"')
            .to_string()
    }
}

impl Debug for ExportArguments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExportArguments")
            .field("module_name", &self.module_name())
            .finish()
    }
}

impl Default for ExportArguments {
    fn default() -> Self {
        Self {
            module_name: syn::LitStr::new("tvalue", Span::call_site()),
        }
    }
}

impl Parse for ExportArguments {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let next = input.lookahead1();
        if !next.peek(syn::Ident) {
            return Ok(Default::default());
        }
        let ident: syn::Ident = input.parse()?;
        if ident != "module" {
            return Err(syn::Error::new(
                Span::call_site(),
                format!("Unexpected attribute field, expected `module` found {ident}"),
            ));
        }
        let _eq: syn::token::Eq = input.parse()?;
        let module_name: syn::LitStr = input.parse()?;
        Ok(Self { module_name })
    }
}
