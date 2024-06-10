use proc_macro2::{TokenStream, Span};
use syn::Error;
use syn::DeriveInput;
use quote::quote;
use crate::util::ContainerAttributes;


pub fn expand(ts: TokenStream) -> Result<TokenStream, Error> {
    let input: DeriveInput = syn::parse2(ts)?;
    let attrs: ContainerAttributes = deluxe::parse_attributes(&input)?;
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();
    let ty_name = input.ident;
    let table = attrs.table.ok_or_else(|| {
        Error::new(
            Span::call_site(),
            "missing attribute `#[nanosql(table = ...)]` for InsertInput::Table associated type",
        )
    })?;
    let insert_input_lt = attrs.insert_input_lt;

    Ok(quote!{
        impl #impl_gen ::nanosql::table::InsertInput<#insert_input_lt>
            for #ty_name #ty_gen #where_clause
        {
            type Table = #table;
        }
    })
}
