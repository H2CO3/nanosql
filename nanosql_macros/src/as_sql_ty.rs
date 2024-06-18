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
            Err(Error::new_spanned(&input, "#[derive(AsSqlTy)] is not supported for unions"))
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
                "#[derive(AsSqlTy)] is not supported for unit structs"
            ));
        }
    };

    let mut iter = fields.iter();

    let (Some(field), None) = (iter.next(), iter.next()) else {
        return Err(Error::new_spanned(
            fields,
            "deriving `AsSqlTy` on a struct is only allowed for a newtype with exactly one field"
        ));
    };
    let ty_name = &input.ident;
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();
    let bounds = parse_quote!(::nanosql::AsSqlTy);
    let where_clause = add_bounds(&data.fields, where_clause, bounds)?;
    let field_ty = &field.ty;

    Ok(quote!{
        impl #impl_gen ::nanosql::AsSqlTy for #ty_name #ty_gen #where_clause {
            const SQL_TY: ::nanosql::SqlTy = <#field_ty as ::nanosql::AsSqlTy>::SQL_TY;

            fn format_check_constraint(
                column: &dyn ::std::fmt::Display,
                formatter: &mut ::std::fmt::Formatter<'_>,
            ) -> ::std::fmt::Result {
                <#field_ty as ::nanosql::AsSqlTy>::format_check_constraint(column, formatter)
            }
        }
    })
}

fn expand_enum(
    input: &DeriveInput,
    attrs: ContainerAttributes,
    data: &DataEnum,
) -> Result<TokenStream, Error> {
    let variant_list = data.variants
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let var_attrs: FieldAttributes = deluxe::parse_attributes(v)?;

            if matches!(v.fields, Fields::Unit) {
                let sep = if i == 0 { "" } else { ", " };
                let var = var_attrs.rename
                    .as_ref()
                    .map_or_else(
                        || attrs.rename_all.display(v.ident.unraw()).to_string(),
                        <_>::to_string,
                    );

                Ok(format!("{sep}'{var}'"))
            } else {
                Err(Error::new_spanned(
                    v,
                    "AsSqlTy can only be derived on enums with all unit variants"
                ))
            }
        })
        .collect::<Result<String, _>>()?;

    let ty_name = &input.ident;

    // we don't need to handle generics from fields of variants,
    // because we only accept unit variants anyway
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();

    Ok(quote!{
        impl #impl_gen ::nanosql::AsSqlTy for #ty_name #ty_gen #where_clause {
            const SQL_TY: ::nanosql::SqlTy = ::nanosql::SqlTy::new(::nanosql::TyPrim::Text);

            fn format_check_constraint(
                column: &dyn ::std::fmt::Display,
                formatter: &mut ::std::fmt::Formatter<'_>,
            ) -> ::std::fmt::Result {
                use ::std::fmt::Write;

                ::std::write!(formatter, "{column} IN ({list})", list = #variant_list)
            }
        }
    })
}
