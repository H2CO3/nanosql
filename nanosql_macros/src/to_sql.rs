use proc_macro2::{TokenStream, TokenTree, Literal};
use syn::Error;
use syn::{DeriveInput, Data, DataStruct, DataEnum, Fields};
use syn::parse_quote;
use syn::ext::IdentExt;
use quote::quote;
use crate::util::{add_bounds, ContainerAttributes, FieldAttributes};


pub fn expand(ts: TokenStream) -> Result<TokenStream, Error> {
    let input: DeriveInput = syn::parse2(ts)?;
    let attrs: ContainerAttributes = deluxe::parse_attributes(&input)?;

    match &input.data {
        Data::Struct(data) => expand_struct(&input, attrs, data),
        Data::Enum(data) => expand_enum(&input, attrs, data),
        Data::Union(_) => {
            Err(Error::new_spanned(&input, "#[derive(ToSql)] is not supported for unions"))
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
                input,
                "#[derive(ToSql)] is not supported for unit structs"
            ));
        }
    };

    let mut iter = fields.iter();

    let (Some(field), None) = (iter.next(), iter.next()) else {
        return Err(Error::new_spanned(
            fields,
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
    attrs: ContainerAttributes,
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

    let variant_strings: Vec<_> = data.variants
        .iter()
        .map(|v| {
            let var_attrs: FieldAttributes = deluxe::parse_attributes(v)?;
            let var_name = var_attrs.rename
                .as_ref()
                .map_or_else(
                    || attrs.rename_all.display(v.ident.unraw()).to_string(),
                    <_>::to_string,
                );

            Ok(var_name)
        })
        .collect::<Result<_, Error>>()?;

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
