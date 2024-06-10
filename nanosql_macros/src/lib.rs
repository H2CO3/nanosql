use proc_macro::TokenStream;

mod param;
mod result_record;
mod table;
mod insert_input;
mod to_sql;
mod from_sql;
mod as_sql_ty;
mod util;


#[proc_macro_derive(Param, attributes(nanosql))]
pub fn derive_param(ts: TokenStream) -> TokenStream {
    util::expand(ts, param::expand)
}

#[proc_macro_derive(ResultRecord, attributes(nanosql))]
pub fn derive_result_record(ts: TokenStream) -> TokenStream {
    util::expand(ts, result_record::expand)
}

#[proc_macro_derive(Table, attributes(nanosql))]
pub fn derive_table(ts: TokenStream) -> TokenStream {
    util::expand(ts, table::expand)
}

#[proc_macro_derive(InsertInput, attributes(nanosql))]
pub fn derive_insert_input(ts: TokenStream) -> TokenStream {
    util::expand(ts, insert_input::expand)
}

/// The purpose of this derive macro is to implement the `rusqlite::ToSql`
/// trait for `enum`s (with unit variants only) and newtype wrapper `struct`s.
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
///
/// When derived on an `enum`, the `rename_all` (type-level) and `rename`
/// (variant-level) attributes work in the same manner as those of `Table`.
/// See the documentation of `Table` for details.
#[proc_macro_derive(FromSql, attributes(nanosql))]
pub fn derive_from_sql(ts: TokenStream) -> TokenStream {
    util::expand(ts, from_sql::expand)
}

#[proc_macro_derive(AsSqlTy, attributes(nanosql))]
pub fn derive_as_sql_ty(ts: TokenStream) -> TokenStream {
    util::expand(ts, as_sql_ty::expand)
}
