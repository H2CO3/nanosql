use proc_macro2::TokenStream;
use syn::Error;
use syn::{DeriveInput, Data, Fields, FieldsNamed};
use syn::ext::IdentExt;
use quote::quote;
use crate::util::ContainerAttributes;


/// TODO(H2CO3): handle generics?
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

pub fn expand_struct(
    input: &DeriveInput,
    _attrs: ContainerAttributes,
    fields: &FieldsNamed,
) -> Result<TokenStream, Error> {
    let ty_name = &input.ident;
    let ty_name_str = ty_name.unraw().to_string();

    let col_name_str = fields.named.iter().map(|f| {
        f.ident.as_ref().expect("named field is unnamed?").unraw().to_string()
    });
    let col_ty = fields.named.iter().map(|f| &f.ty);

    Ok(quote!{
        impl ::nanosql::Table for #ty_name {
            type InsertInput<'p> = Self;

            fn description() -> ::nanosql::TableDesc {
                ::nanosql::TableDesc::new(#ty_name_str)
                    #(
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
