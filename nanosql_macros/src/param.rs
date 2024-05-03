use proc_macro2::TokenStream;
use syn::{Error, WhereClause};
use syn::{DeriveInput, Data, DataStruct, DataEnum, Fields, FieldsNamed, FieldsUnnamed};
use syn::parse_quote;
use syn::ext::IdentExt;
use syn::spanned::Spanned;
use quote::quote;
use crate::util::{add_bounds, ContainerAttributes, ParamPrefix};


pub fn expand(ts: TokenStream) -> Result<TokenStream, Error> {
    let input: DeriveInput = syn::parse2(ts)?;
    let attrs: ContainerAttributes = deluxe::parse_attributes(&input)?;

    match &input.data {
        Data::Struct(data) => expand_struct(&input, attrs, data),
        Data::Enum(data) => expand_enum(&input, attrs, data),
        Data::Union(_) => {
            Err(Error::new_spanned(&input, "#[derive(Params)] is not supported for unions"))
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
    let bounds = parse_quote!(::nanosql::ToSql);
    let where_clause = add_bounds(&data.fields, where_clause, bounds)?;

    let (body, prefix) = match &data.fields {
        Fields::Named(fields) => expand_named_fields(fields, &attrs)?,
        Fields::Unnamed(fields) => expand_unnamed_fields(fields, &attrs)?,
        Fields::Unit => (TokenStream::new(), ParamPrefix::Question),
    };

    let ty_name = &input.ident;
    let num_fields = data.fields.len();

    Ok(quote!{
        impl #impl_gen ::nanosql::Param for #ty_name #ty_gen #where_clause {
            const PREFIX: ::nanosql::ParamPrefix = ::nanosql::ParamPrefix::#prefix;

            fn bind(&self, statement: &mut ::nanosql::Statement<'_>) -> ::nanosql::Result<()> {
                let expected = statement.parameter_count();
                let actual = #num_fields;

                if actual != expected {
                    return ::nanosql::Result::Err(
                        ::nanosql::Error::ParamCountMismatch { expected, actual }
                    );
                }

                #body

                ::nanosql::Result::Ok(())
            }
        }
    })
}

fn expand_named_fields(
    fields: &FieldsNamed,
    attrs: &ContainerAttributes,
) -> Result<(TokenStream, ParamPrefix), Error> {
    let prefix = attrs.param_prefix.unwrap_or(ParamPrefix::Dollar);

    match prefix {
        ParamPrefix::Dollar | ParamPrefix::At | ParamPrefix::Colon => {}
        ParamPrefix::Question => {
            return Err(Error::new_spanned(
                fields,
                format_args!("parameter prefix `{prefix}` is not allowed with named fields")
            ));
        }
    }

    let body = fields.named
        .iter()
        .map(|field| {
            let field_name = field.ident.as_ref().ok_or_else(|| {
                Error::new_spanned(field, "named field has no name")
            })?;

            // If the field name is a raw identifier, still use just the non-raw
            // part for naming the parameter, because that's what people expect.
            // However, still use the original field name in the field access
            // expression, otherwise raw identifiers would cause a syntax error.
            let literal_field_name = field_name.unraw();
            let param_name = format!("{prefix}{literal_field_name}");

            Ok(quote!{
                let index = statement.parameter_index(#param_name)?;
                let index = index.ok_or(::nanosql::Error::unknown_param(#param_name))?;
                statement.raw_bind_parameter(index, &self.#field_name)?;
            })
        })
        .collect::<Result<TokenStream, Error>>()?;

    Ok((body, prefix))
}

fn expand_unnamed_fields(
    fields: &FieldsUnnamed,
    attrs: &ContainerAttributes,
) -> Result<(TokenStream, ParamPrefix), Error> {
    let prefix = attrs.param_prefix.unwrap_or(ParamPrefix::Question);

    match prefix {
        ParamPrefix::Question => {}
        ParamPrefix::Dollar | ParamPrefix::At | ParamPrefix::Colon => {
            return Err(Error::new_spanned(
                fields,
                format_args!("parameter prefix `{prefix}` is not allowed for tuple structs")
            ));
        }
    }

    let body = fields.unnamed
        .iter()
        .enumerate()
        .map(|(idx, field)| {
            let field_name = syn::Index {
                index: idx as u32,
                span: field.span(),
            };

            Ok(quote!{
                // unlike columns, parameter indexes are one-based
                statement.raw_bind_parameter(#idx + 1, &self.#field_name)?;
            })
        })
        .collect::<Result<TokenStream, Error>>()?;

    Ok((body, prefix))
}

/// Implements the bulk of the logic for an `enum` with all unit-like variants.
fn expand_enum(
    input: &DeriveInput,
    attrs: ContainerAttributes,
    data: &DataEnum,
) -> Result<TokenStream, Error> {
    let prefix = attrs.param_prefix.unwrap_or(ParamPrefix::Question);
    let ty_name = &input.ident;

    match prefix {
        ParamPrefix::Question => {}
        ParamPrefix::Dollar | ParamPrefix::At | ParamPrefix::Colon => {
            return Err(Error::new_spanned(
                input,
                format_args!("parameter prefix `{prefix}` is not allowed for enums")
            ));
        }
    }

    // add `where Self: ToSql` bound for clearer error message
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();
    let mut where_clause = where_clause.cloned().unwrap_or_else(|| {
        WhereClause {
            where_token: Default::default(),
            predicates: Default::default(),
        }
    });
    where_clause.predicates.push(parse_quote!(Self: ::nanosql::ToSql));

    // ensure that all variants are unit-like
    for variant in &data.variants {
        let Fields::Unit = variant.fields else {
            return Err(Error::new_spanned(variant, "only unit-like variants are allowed"));
        };
    }

    Ok(quote!{
        impl #impl_gen ::nanosql::Param for #ty_name #ty_gen #where_clause {
            const PREFIX: ::nanosql::ParamPrefix = ::nanosql::ParamPrefix::#prefix;

            fn bind(&self, statement: &mut ::nanosql::Statement<'_>) -> ::nanosql::Result<()> {
                let expected = statement.parameter_count();
                let actual = 1;

                if actual != expected {
                    return ::nanosql::Result::Err(
                        ::nanosql::Error::ParamCountMismatch { expected, actual }
                    );
                }

                statement.raw_bind_parameter(1, self)?;

                ::nanosql::Result::Ok(())
            }
        }
    })
}
