use proc_macro::TokenStream;


mod param;
mod util;

#[proc_macro_derive(Param, attributes(nanosql))]
pub fn derive_param(ts: TokenStream) -> TokenStream {
    util::expand(ts, param::expand)
}
