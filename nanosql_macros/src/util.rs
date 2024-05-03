use std::collections::HashSet;
use proc_macro::TokenStream as TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::{Error, Token, Fields, WhereClause, WherePredicate, TypeParamBound};
use syn::punctuated::Punctuated;
use syn::parse_quote;


pub fn expand<F>(ts: TokenStream, f: F) -> TokenStream
where
    F: FnOnce(TokenStream2) -> Result<TokenStream2, Error>
{
    f(ts.into()).unwrap_or_else(Error::into_compile_error).into()
}

pub fn add_bounds(
    fields: &Fields,
    where_clause: Option<&WhereClause>,
    bounds: Punctuated<TypeParamBound, Token![+]>,
) -> Result<WhereClause, Error> {
    let unique_types: HashSet<_> = match fields {
        Fields::Unit => HashSet::new(),
        Fields::Named(fields) => {
            fields.named
                .iter()
                .map(|field| &field.ty)
                .collect()
        }
        Fields::Unnamed(fields) => {
            fields.unnamed
                .iter()
                .map(|field| &field.ty)
                .collect()
        }
    };

    let mut where_clause = where_clause.cloned().unwrap_or_else(|| {
        WhereClause {
            where_token: Default::default(),
            predicates: Default::default(),
        }
    });

    where_clause.predicates.extend(
        unique_types.iter().map(|ty| -> WherePredicate {
            parse_quote!{
                #ty: #bounds
            }
        })
    );

    Ok(where_clause)
}
