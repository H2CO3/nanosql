use proc_macro2::{TokenStream, TokenTree, Literal, Ident};
use syn::Error;
use syn::{DeriveInput, Data, DataStruct, DataEnum, Fields};
use syn::parse_quote;
use quote::quote;
use crate::util::{add_bounds, ContainerAttributes};


pub fn expand(ts: TokenStream) -> Result<TokenStream, Error> {
    let input: DeriveInput = syn::parse2(ts)?;
    let attrs: ContainerAttributes = deluxe::parse_attributes(&input)?;

    match &input.data {
        Data::Struct(data) => expand_struct(&input, attrs, data),
        Data::Enum(data) => expand_enum(&input, attrs, data),
        Data::Union(_) => {
            return Err(Error::new_spanned(&input, "#[derive(ToSql)] is not supported for unions"));
        }
    }
}

/// TODO(H2CO3): respect a field-level `ignore` or `transparent` attribute,
/// for selecting exactly one of >1 fields to base the impl on (either by
/// ignoring all but one of them, or by marking exactly one as transparent).
fn expand_struct(
    input: &DeriveInput,
    _attrs: ContainerAttributes,
    data: &DataStruct,
) -> Result<TokenStream, Error> {
    let fields = match &data.fields {
        Fields::Named(fields) => &fields.named,
        Fields::Unnamed(fields) => &fields.unnamed,
        Fields::Unit => {
            return Err(Error::new_spanned(
                &input,
                "#[derive(ToSql)] is not supported for unit structs"
            ));
        }
    };

    let mut iter = fields.iter();

    let (Some(field), None) = (iter.next(), iter.next()) else {
        return Err(Error::new_spanned(
            &fields,
            "deriving `ToSql` on a struct is only allowed for a newtype with exactly one field"
        ));
    };
    let ty_name = &input.ident;
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();
    let bounds = parse_quote!(::nanosql::ToSql);
    let where_clause = add_bounds(&data.fields, where_clause, bounds)?;
    let field_name = field.ident.clone().map_or(
        TokenTree::Literal(Literal::usize_unsuffixed(0)),
        TokenTree::Ident,
    );

    Ok(quote!{
        impl #impl_gen ::nanosql::ToSql for #ty_name #ty_gen #where_clause {
            fn to_sql(&self) -> ::nanosql::rusqlite::Result<::nanosql::ToSqlOutput> {
                ::nanosql::ToSql::to_sql(&self.#field_name)
            }
        }
    })
}

fn expand_enum(
    input: &DeriveInput,
    _attrs: ContainerAttributes,
    data: &DataEnum,
) -> Result<TokenStream, Error> {
    let variant_names: Vec<_> = data.variants
        .iter()
        .map(|v| {
            if matches!(v.fields, Fields::Unit) {
                Ok(&v.ident)
            } else {
                Err(Error::new_spanned(
                    v,
                    "ToSql can only be derived on enums with all unit variants"
                ))
            }
        })
        .collect::<Result<_, _>>()?;

    // TODO(H2CO3): respect renaming attributes here
    let variant_strings: Vec<_> = variant_names.iter().copied().map(Ident::to_string).collect();
    let ty_name = &input.ident;

    // we don't need to handle generics from fields of variants,
    // because we only accept unit variants anyway
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();

    Ok(quote!{
        impl #impl_gen ::nanosql::ToSql for #ty_name #ty_gen #where_clause {
            fn to_sql(&self) -> ::nanosql::rusqlite::Result<::nanosql::ToSqlOutput> {
                let variant_name = match *self {
                    #(#ty_name::#variant_names => #variant_strings,)*
                };
                ::nanosql::ToSql::to_sql(variant_name)
            }
        }
    })
}
