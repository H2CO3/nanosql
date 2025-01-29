use proc_macro2::TokenStream;
use syn::{Error, WhereClause};
use syn::{DeriveInput, Data, DataStruct, DataEnum, Fields, FieldsNamed, FieldsUnnamed};
use syn::parse_quote;
use quote::quote;
use crate::util::{add_bounds, ContainerAttributes, FieldAttributes};


pub fn expand(ts: TokenStream) -> Result<TokenStream, Error> {
    let input: DeriveInput = syn::parse2(ts)?;
    let attrs: ContainerAttributes = deluxe::parse_attributes(&input)?;

    match &input.data {
        Data::Struct(data) => expand_struct(&input, attrs, data),
        Data::Enum(data) => expand_enum(&input, attrs, data),
        Data::Union(_) => {
            Err(Error::new_spanned(&input, "#[derive(ResultRecord)] is not supported for unions"))
        }
    }
}

/// Implements the bulk of the logic for a struct (either named fields or tuple).
fn expand_struct(
    input: &DeriveInput,
    attrs: ContainerAttributes,
    data: &DataStruct,
) -> Result<TokenStream, Error> {
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();
    let bounds = parse_quote!(::nanosql::FromSql);
    let bounds_for_ignored = parse_quote!(::core::default::Default);
    let where_clause = add_bounds(&data.fields, where_clause, bounds, Some(bounds_for_ignored))?;

    let body = match &data.fields {
        Fields::Named(fields) => expand_named_fields(fields, &attrs)?,
        Fields::Unnamed(fields) => expand_unnamed_fields(fields, &attrs),
        Fields::Unit => quote!(Self),
    };

    let ty_name = &input.ident;
    let num_fields = data.fields.len();

    Ok(quote!{
        impl #impl_gen ::nanosql::ResultRecord for #ty_name #ty_gen #where_clause {
            fn from_row(row: &::nanosql::Row<'_>) -> ::nanosql::Result<Self> {
                let statement: &::nanosql::Statement = row.as_ref();
                let actual = statement.column_count();
                let expected = #num_fields;

                if actual != expected {
                    return ::nanosql::Result::Err(
                        ::nanosql::Error::ColumnCountMismatch { expected, actual }
                    );
                }

                ::nanosql::Result::Ok(#body)
            }
        }
    })
}

fn expand_named_fields(
    fields: &FieldsNamed,
    attrs: &ContainerAttributes,
) -> Result<TokenStream, Error> {
    let mut field_names = Vec::with_capacity(fields.named.len());
    let mut column_names = Vec::with_capacity(fields.named.len());

    for field in &fields.named {
        let field_attrs: FieldAttributes = deluxe::parse_attributes(field)?;
        let field_name = field.ident.as_ref().ok_or_else(|| {
            Error::new_spanned(field, "named field has no name")
        })?;

        // If the field name is a raw identifier, still only use the non-raw
        // part for naming the column, because that's what people expect.
        // However, still use the original field name in the field access
        // expression, otherwise raw identifiers would cause a syntax error.
        let column_name = field_attrs.rename
            .as_ref()
            .map_or_else(
                || attrs.rename_all.display(field_name).to_string(),
                <_>::to_string,
            );

        field_names.push(field_name);
        column_names.push(column_name);
    }

    Ok(quote!{
        Self {
            #(#field_names: row.get(#column_names)?,)*
        }
    })
}

fn expand_unnamed_fields(
    fields: &FieldsUnnamed,
    _attrs: &ContainerAttributes,
) -> TokenStream {
    let field_ctors = (0..fields.unnamed.len()).map(|idx| quote!{ row.get(#idx)? });

    quote!{
        Self(#(#field_ctors,)*)
    }
}

/// Implements the bulk of the logic for an `enum` with all unit-like variants.
fn expand_enum(
    input: &DeriveInput,
    _attrs: ContainerAttributes,
    data: &DataEnum,
) -> Result<TokenStream, Error> {
    let ty_name = &input.ident;

    // add `where Self: ToSql` bound for clearer error message
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();
    let mut where_clause = where_clause.cloned().unwrap_or_else(|| {
        WhereClause {
            where_token: Default::default(),
            predicates: Default::default(),
        }
    });
    where_clause.predicates.push(parse_quote!(Self: ::nanosql::FromSql));

    // ensure that all variants are unit-like
    for variant in &data.variants {
        let Fields::Unit = variant.fields else {
            return Err(Error::new_spanned(variant, "only unit-like variants are allowed"));
        };
    }

    Ok(quote!{
        impl #impl_gen ::nanosql::ResultRecord for #ty_name #ty_gen #where_clause {
            fn from_row(row: &::nanosql::Row<'_>) -> ::nanosql::Result<Self> {
                let statement: &nanosql::Statement = row.as_ref();
                let actual = statement.column_count();
                let expected = 1;

                if actual != expected {
                    return ::nanosql::Result::Err(
                        ::nanosql::Error::ColumnCountMismatch { expected, actual }
                    );
                }

                // columns are 0-indexed
                row.get(0).map_err(<::nanosql::Error as ::core::convert::From<_>>::from)
            }
        }
    })
}
