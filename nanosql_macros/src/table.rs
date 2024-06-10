use proc_macro2::TokenStream;
use syn::Error;
use syn::{DeriveInput, Data, Fields, FieldsNamed};
use syn::ext::IdentExt;
use quote::quote;
use crate::util::{ContainerAttributes, FieldAttributes};


/// TODO(H2CO3): handle generics?
/// TODO(H2CO3): handle various field-level and table-level attributes such as:
/// rename(_all), skip, PK, FK, unique, check, default, generated, etc.
pub fn expand(ts: TokenStream) -> Result<TokenStream, Error> {
    let input: DeriveInput = syn::parse2(ts)?;
    let attrs: ContainerAttributes = deluxe::parse_attributes(&input)?;

    match &input.data {
        Data::Struct(data) => {
            match &data.fields {
                Fields::Named(fields) => expand_struct(&input, attrs, fields),
                Fields::Unnamed(_) => {
                    Err(Error::new_spanned(
                        &input,
                        "#[derive(Table)] is not supported for tuple structs"
                    ))
                }
                Fields::Unit => {
                    Err(Error::new_spanned(
                        &input,
                        "#[derive(Table)] is not supported for unit structs"
                    ))
                }
            }
        }
        Data::Enum(_) => {
            Err(Error::new_spanned(&input, "#[derive(Table)] is not supported for enums"))
        }
        Data::Union(_) => {
            Err(Error::new_spanned(&input, "#[derive(Table)] is not supported for unions"))
        }
    }
}

fn expand_struct(
    input: &DeriveInput,
    attrs: ContainerAttributes,
    fields: &FieldsNamed,
) -> Result<TokenStream, Error> {
    let ty_name = &input.ident;
    let table_name = attrs.rename.unwrap_or_else(|| ty_name.unraw().to_string());
    let insert_input_ty = attrs.insert_input_ty;
    let insert_input_lt = attrs.insert_input_lt;

    let attrs_for_each_field: Vec<FieldAttributes> = fields.named
        .iter()
        .map(deluxe::parse_attributes)
        .collect::<Result<_, _>>()?;

    let col_name_str = fields.named
        .iter()
        .zip(&attrs_for_each_field)
        .map(|(f, field_attrs)| {
            let field_name = f.ident.as_ref().expect("named field is unnamed?");

            field_attrs.rename.clone().unwrap_or_else(|| {
                attrs.rename_all.display(field_name.unraw()).to_string()
            })
        });

    let col_ty = fields.named
        .iter()
        .zip(&attrs_for_each_field)
        .map(|(f, field_attrs)| {
            field_attrs.sql_ty.clone().unwrap_or_else(|| f.ty.clone())
        });

    let uniq_constraint = attrs_for_each_field
        .iter()
        .map(|field_attrs| {
            field_attrs.unique.then_some(quote!(.unique()))
        });

    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();

    Ok(quote!{
        impl<#impl_gen> ::nanosql::Table<#ty_gen> for #ty_name #where_clause {
            type InsertInput<#insert_input_lt> = #insert_input_ty;

            fn description() -> ::nanosql::TableDesc {
                ::nanosql::TableDesc::new(#table_name) #(
                    .column(
                        ::nanosql::Column::new(#col_name_str)
                            .ty(<#col_ty as ::nanosql::AsSqlTy>::SQL_TY)
                            .check(
                                ::std::string::ToString::to_string(
                                    &::nanosql::table::ColumnConstraintFormatter::<#col_ty>::new(
                                        #col_name_str
                                    )
                                )
                            )
                            #uniq_constraint
                    )
                )*
            }
        }
    })
}
