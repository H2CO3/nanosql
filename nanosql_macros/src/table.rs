use proc_macro2::TokenStream;
use syn::Error;
use syn::{DeriveInput, Data, Fields, FieldsNamed};
use syn::ext::IdentExt;
use quote::quote;
use crate::util::{ContainerAttributes, FieldAttributes, IteratorExt};


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

    let (col_name_str, col_ty): (Vec<_>,  Vec<_>) = fields.named
        .iter()
        .map(|f| -> Result<_, Error> {
            let field_attrs: FieldAttributes = deluxe::parse_attributes(f)?;
            let field_name = f.ident.as_ref().expect("named field is unnamed?");

            let col_name = field_attrs.rename.unwrap_or_else(|| {
                attrs.rename_all.display(field_name.unraw()).to_string()
            });

            let col_ty = field_attrs.sql_ty.unwrap_or(f.ty.clone());

            Ok((col_name, col_ty))
        })
        .try_unzip()?;

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
                    )
                )*
            }
        }
    })
}
