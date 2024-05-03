use proc_macro::TokenStream as TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::Error;


pub fn expand<F>(ts: TokenStream, f: F) -> TokenStream
where
    F: FnOnce(TokenStream2) -> Result<TokenStream2, Error>
{
    f(ts.into()).unwrap_or_else(Error::into_compile_error).into()
}
