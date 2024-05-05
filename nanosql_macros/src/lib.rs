use proc_macro::TokenStream;

mod param;
mod result_record;
mod table;
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
