#![forbid(unsafe_code)]

use proc_macro::TokenStream;

mod param;
mod result_record;
mod table;
mod insert_input;
mod to_sql;
mod from_sql;
mod as_sql_ty;
mod util;


/// See the documentation of the `Param` trait for details.
#[proc_macro_derive(Param, attributes(nanosql))]
pub fn derive_param(ts: TokenStream) -> TokenStream {
    util::expand(ts, param::expand)
}

/// See the documentation of the `ResultRecord` trait for details.
#[proc_macro_derive(ResultRecord, attributes(nanosql))]
pub fn derive_result_record(ts: TokenStream) -> TokenStream {
    util::expand(ts, result_record::expand)
}

/// See the documentation of the `Table` trait for details.
#[proc_macro_derive(Table, attributes(nanosql))]
pub fn derive_table(ts: TokenStream) -> TokenStream {
    util::expand(ts, table::expand)
}

/// See the documentation of the `InsertInput` trait for details.
#[proc_macro_derive(InsertInput, attributes(nanosql))]
pub fn derive_insert_input(ts: TokenStream) -> TokenStream {
    util::expand(ts, insert_input::expand)
}

/// The purpose of this derive macro is to implement the `rusqlite::ToSql`
/// trait for `enum`s (with unit variants only) and newtype wrapper `struct`s.
/// Multi-field `struct`s can use the `#[nanosql(ignore)]` attribute on all
/// except one field, in order to delegate the impl to that one field.
///
/// When derived on an `enum`, the `rename_all` (type-level) and `rename`
/// (variant-level) attributes work in the same manner as those of `Table`.
/// See the documentation of `Table` for details.
#[proc_macro_derive(ToSql, attributes(nanosql))]
pub fn derive_to_sql(ts: TokenStream) -> TokenStream {
    util::expand(ts, to_sql::expand)
}

/// The purpose of this derive macro is to implement the `rusqlite::FromSql`
/// trait for `enum`s (with unit variants only) and newtype wrapper `struct`s.
/// Multi-field `struct`s can use the `#[nanosql(ignore)]` attribute on all
/// except one field, in order to delegate the impl to that one field.
///
/// When derived on an `enum`, the `rename_all` (type-level) and `rename`
/// (variant-level) attributes work in the same manner as those of `Table`.
/// See the documentation of `Table` for details.
#[proc_macro_derive(FromSql, attributes(nanosql))]
pub fn derive_from_sql(ts: TokenStream) -> TokenStream {
    util::expand(ts, from_sql::expand)
}

/// See the documentation of the `AsSqlTy` trait for details.
#[proc_macro_derive(AsSqlTy, attributes(nanosql))]
pub fn derive_as_sql_ty(ts: TokenStream) -> TokenStream {
    util::expand(ts, as_sql_ty::expand)
}
