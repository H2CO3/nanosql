use proc_macro2::TokenStream;
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
            Err(Error::new_spanned(&input, "#[derive(FromSql)] is not supported for unions"))
        }
    }
}

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
                "#[derive(FromSql)] is not supported for unit structs"
            ));
        }
    };

    let mut iter = fields
        .iter()
        .enumerate()
        .map(|(idx, field)| -> Result<_, Error> {
            let attrs: FieldAttributes = deluxe::parse_attributes(field)?;
            Ok(if attrs.ignore { None } else { Some((idx, field)) })
        })
        .filter_map(Result::transpose);

    let (Some(field_spec), None) = (iter.next(), iter.next()) else {
        return Err(Error::new_spanned(
            fields,
            "deriving `FromSql` on a struct is only allowed for a newtype with exactly one field"
        ));
    };
    let (payload_index, field) = field_spec?;
    let ty_name = &input.ident;
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();
    let bounds = parse_quote!(::nanosql::FromSql);
    let bounds_for_ignored = parse_quote!(::core::default::Default);
    let where_clause = add_bounds(&data.fields, where_clause, bounds, Some(bounds_for_ignored))?;

    let body = if let Some(payload_name) = field.ident.as_ref() {
        let field_ctors = fields
            .iter()
            .map(|f| {
                let name = f.ident.as_ref().expect("named field is unnamed?!");
                if name == payload_name {
                    quote!(#name: ::nanosql::FromSql::column_result(value)?)
                } else {
                    quote!(#name: ::core::default::Default::default())
                }
            });

        quote!{
            ::nanosql::FromSqlResult::Ok(#ty_name { #(#field_ctors,)* })
        }
    } else {
        let field_ctors = (0..fields.len()).map(|i| {
            if i == payload_index {
                quote!(::nanosql::FromSql::column_result(value)?)
            } else {
                quote!(::core::default::Default::default())
            }
        });

        quote!{
            ::nanosql::FromSqlResult::Ok(#ty_name(#(#field_ctors,)*))
        }
    };

    Ok(quote!{
        impl #impl_gen ::nanosql::FromSql for #ty_name #ty_gen #where_clause {
            fn column_result(value: ::nanosql::ValueRef<'_>) -> ::nanosql::FromSqlResult<Self> {
                #body
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
                    "FromSql can only be derived on enums with all unit variants"
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
    let ty_name_str = ty_name.unraw().to_string();

    // we don't need to handle generics from fields of variants,
    // because we only accept unit variants anyway
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();

    Ok(quote!{
        impl #impl_gen ::nanosql::FromSql for #ty_name #ty_gen #where_clause {
            fn column_result(value: ::nanosql::ValueRef<'_>) -> ::nanosql::FromSqlResult<Self> {
                ::nanosql::FromSqlResult::Ok(match value.as_str()? {
                    #(#variant_strings => #ty_name::#variant_names,)*
                    other @ _ => {
                        let ty_name = #ty_name_str;
                        return ::nanosql::FromSqlResult::Err(
                            ::nanosql::rusqlite::types::FromSqlError::Other(
                                <::std::boxed::Box<
                                    dyn ::std::error::Error
                                      + ::std::marker::Send
                                      + ::std::marker::Sync
                                      + 'static
                                > as ::std::convert::From<_>>::from(
                                    ::std::format!("invalid variant `{other}` for enum `{ty_name}`")
                                )
                            )
                        )
                    }
                })
            }
        }
    })
}
