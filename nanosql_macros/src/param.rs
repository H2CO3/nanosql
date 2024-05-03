use core::fmt::{self, Display, Formatter, Write};
use proc_macro2::{TokenStream, Span, Ident};
use syn::{Error, Lit, WhereClause};
use syn::{DeriveInput, Data, DataStruct, DataEnum, Fields, FieldsNamed, FieldsUnnamed};
use syn::parse::{Parse, ParseStream};
use syn::parse_quote;
use syn::ext::IdentExt;
use syn::spanned::Spanned;
use quote::{quote, ToTokens};
use deluxe::{ParseMetaItem, ParseAttributes};
use crate::util::add_bounds;


#[derive(Clone, Debug, ParseAttributes)]
#[deluxe(attributes(nanosql))]
struct ParamAttributes {
    #[deluxe(alias = prefix, alias = param_pfx)]
    param_prefix: Option<ParamPrefix>,
}

pub fn expand(ts: TokenStream) -> Result<TokenStream, Error> {
    let input: DeriveInput = syn::parse2(ts)?;
    let attrs: ParamAttributes = deluxe::parse_attributes(&input)?;

    match &input.data {
        Data::Struct(data) => expand_struct(&input, attrs, data),
        Data::Enum(data) => expand_enum(&input, attrs, data),
        Data::Union(_) => {
            Err(Error::new_spanned(&input, "#[derive(Params)] is not supported for unions"))
        }
    }
}

/// Implements the bulk of the logic for a struct (either named fields or tuple).
fn expand_struct(
    input: &DeriveInput,
    attrs: ParamAttributes,
    data: &DataStruct,
) -> Result<TokenStream, Error> {
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();
    let bounds = parse_quote!(::nanosql::ToSql);
    let where_clause = add_bounds(&data.fields, where_clause, bounds)?;

    let (body, prefix) = match &data.fields {
        Fields::Named(fields) => expand_named_fields(fields, &attrs)?,
        Fields::Unnamed(fields) => expand_unnamed_fields(fields, &attrs)?,
        Fields::Unit => (TokenStream::new(), ParamPrefix::Question),
    };

    let ty_name = &input.ident;
    let num_fields = data.fields.len();

    Ok(quote!{
        impl #impl_gen ::nanosql::Param for #ty_name #ty_gen #where_clause {
            const PREFIX: ::nanosql::ParamPrefix = ::nanosql::ParamPrefix::#prefix;

            fn bind(&self, statement: &mut ::nanosql::Statement<'_>) -> ::nanosql::Result<()> {
                let expected = statement.parameter_count();
                let actual = #num_fields;

                if actual != expected {
                    return ::nanosql::Result::Err(
                        ::nanosql::Error::ParamCountMismatch { expected, actual }
                    );
                }

                #body

                ::nanosql::Result::Ok(())
            }
        }
    })
}

fn expand_named_fields(
    fields: &FieldsNamed,
    attrs: &ParamAttributes,
) -> Result<(TokenStream, ParamPrefix), Error> {
    let prefix = attrs.param_prefix.unwrap_or(ParamPrefix::Dollar);

    match prefix {
        ParamPrefix::Dollar | ParamPrefix::At | ParamPrefix::Colon => {}
        ParamPrefix::Question => {
            return Err(Error::new_spanned(
                fields,
                format_args!("parameter prefix `{prefix}` is not allowed with named fields")
            ));
        }
    }

    let body = fields.named
        .iter()
        .map(|field| {
            let field_name = field.ident.as_ref().ok_or_else(|| {
                Error::new_spanned(field, "named field has no name")
            })?;

            // If the field name is a raw identifier, still use just the non-raw
            // part for naming the parameter, because that's what people expect.
            // However, still use the original field name in the field access
            // expression, otherwise raw identifiers would cause a syntax error.
            let literal_field_name = field_name.unraw();
            let param_name = format!("{prefix}{literal_field_name}");

            Ok(quote!{
                let index = statement.parameter_index(#param_name)?;
                let index = index.ok_or(::nanosql::Error::unknown_param(#param_name))?;
                statement.raw_bind_parameter(index, &self.#field_name)?;
            })
        })
        .collect::<Result<TokenStream, Error>>()?;

    Ok((body, prefix))
}

fn expand_unnamed_fields(
    fields: &FieldsUnnamed,
    attrs: &ParamAttributes,
) -> Result<(TokenStream, ParamPrefix), Error> {
    let prefix = attrs.param_prefix.unwrap_or(ParamPrefix::Dollar);

    match prefix {
        ParamPrefix::Question => {}
        ParamPrefix::Dollar | ParamPrefix::At | ParamPrefix::Colon => {
            return Err(Error::new_spanned(
                fields,
                format_args!("parameter prefix `{prefix}` is not allowed for tuple structs")
            ));
        }
    }

    let body = fields.unnamed
        .iter()
        .enumerate()
        .map(|(idx, field)| {
            // If the field name is a raw identifier, still use just the non-raw
            // part for naming the parameter, because that's what people expect.
            // However, still use the original field name in the field access
            // expression, otherwise raw identifiers would cause a syntax error.
            let field_name = syn::Index {
                index: idx as u32,
                span: field.span(),
            };

            Ok(quote!{
                // unlike columns, parameter indexes are one-based
                statement.raw_bind_parameter(#idx + 1, &self.#field_name)?;
            })
        })
        .collect::<Result<TokenStream, Error>>()?;

    Ok((body, prefix))
}

/// Implements the bulk of the logic for an `enum` with all unit-like variants.
fn expand_enum(
    input: &DeriveInput,
    attrs: ParamAttributes,
    data: &DataEnum,
) -> Result<TokenStream, Error> {
    let prefix = attrs.param_prefix.unwrap_or(ParamPrefix::Question);
    let ty_name = &input.ident;

    match prefix {
        ParamPrefix::Question => {}
        ParamPrefix::Dollar | ParamPrefix::At | ParamPrefix::Colon => {
            return Err(Error::new_spanned(
                input,
                format_args!("parameter prefix `{prefix}` is not allowed for enums")
            ));
        }
    }

    // add `where Self: ToSql` bound for clearer error message
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();
    let mut where_clause = where_clause.cloned().unwrap_or_else(|| {
        WhereClause {
            where_token: Default::default(),
            predicates: Default::default(),
        }
    });
    where_clause.predicates.push(parse_quote!(Self: ::nanosql::ToSql));

    // ensure that all variants are unit-like
    for variant in &data.variants {
        let Fields::Unit = variant.fields else {
            return Err(Error::new_spanned(variant, "only unit-like variants are allowed"));
        };
    }

    Ok(quote!{
        impl #impl_gen ::nanosql::Param for #ty_name #ty_gen #where_clause {
            const PREFIX: ::nanosql::ParamPrefix = ::nanosql::ParamPrefix::#prefix;

            fn bind(&self, statement: &mut ::nanosql::Statement<'_>) -> ::nanosql::Result<()> {
                let expected = statement.parameter_count();
                let actual = 1;

                if actual != expected {
                    return ::nanosql::Result::Err(
                        ::nanosql::Error::ParamCountMismatch { expected, actual }
                    );
                }

                statement.raw_bind_parameter(1, self)?;

                ::nanosql::Result::Ok(())
            }
        }
    })
}

/// Represents the allowed (and compulsory) first character of a parameter
/// name, in a way that's parseable and emittable in a Syn/Quote context.
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum ParamPrefix {
    Dollar = b'$',
    Colon = b':',
    Question = b'?',
    At = b'@',
}

impl ParamPrefix {
    /// Returns the underlying raw byte.
    pub const fn as_byte(self) -> u8 {
        self as u8
    }

    /// Returns the underlying raw character.
    pub const fn as_char(self) -> char {
        self as u8 as char
    }
}

impl From<ParamPrefix> for u8 {
    fn from(prefix: ParamPrefix) -> Self {
        prefix.as_byte()
    }
}

impl From<ParamPrefix> for char {
    fn from(prefix: ParamPrefix) -> Self {
        prefix.as_char()
    }
}

impl TryFrom<char> for ParamPrefix {
    type Error = Error;

    fn try_from(ch: char) -> Result<Self, Self::Error> {
        match ch {
            '$' => Ok(ParamPrefix::Dollar),
            ':' => Ok(ParamPrefix::Colon),
            '?' => Ok(ParamPrefix::Question),
            '@' => Ok(ParamPrefix::At),
            _   => Err(Error::new(
                Span::call_site(),
                format_args!("invalid parameter prefix: `{ch}`")
            )),
        }
    }
}

impl TryFrom<u8> for ParamPrefix {
    type Error = Error;

    fn try_from(byte: u8) -> Result<Self, Self::Error> {
        char::from(byte).try_into()
    }
}

impl Display for ParamPrefix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_char(self.as_char())
    }
}

impl Parse for ParamPrefix {
    fn parse(stream: ParseStream<'_>) -> Result<Self, Error> {
        let lit = Lit::parse(stream)?;

        match lit {
            Lit::Char(lit) => match lit.value() {
                '$' => Ok(ParamPrefix::Dollar),
                ':' => Ok(ParamPrefix::Colon),
                '?' => Ok(ParamPrefix::Question),
                '@' => Ok(ParamPrefix::At),
                ch  => Err(stream.error(format_args!("invalid parameter prefix: `{ch}`"))),
            },
            Lit::Str(lit) => match lit.value().as_str() {
                "$" => Ok(ParamPrefix::Dollar),
                ":" => Ok(ParamPrefix::Colon),
                "?" => Ok(ParamPrefix::Question),
                "@" => Ok(ParamPrefix::At),
                s   => Err(stream.error(format_args!("invalid parameter prefix: `{s}`"))),
            },
            _ => Err(stream.error("expected character or string literal")),
        }
    }
}

impl ToTokens for ParamPrefix {
    fn to_tokens(&self, ts: &mut TokenStream) {
        let variant_name = match *self {
            ParamPrefix::Dollar   => "Dollar",
            ParamPrefix::Colon    => "Colon",
            ParamPrefix::Question => "Question",
            ParamPrefix::At       => "At",
        };
        Ident::new(variant_name, Span::call_site()).to_tokens(ts);
    }
}

impl ParseMetaItem for ParamPrefix {
    fn parse_meta_item(stream: ParseStream<'_>, _: deluxe::ParseMode) -> Result<Self, Error> {
        ParamPrefix::parse(stream)
    }
}
