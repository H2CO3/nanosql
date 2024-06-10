use core::fmt::{self, Display, Formatter, Write};
use std::collections::HashSet;
use proc_macro::TokenStream as TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Span, Ident};
use syn::{Error, Token, Fields, WhereClause, WherePredicate, TypeParamBound, Lit, Type, Lifetime};
use syn::parse_quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use quote::ToTokens;
use deluxe::{ParseAttributes, ParseMetaItem};


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

/// Top-level attributes on a struct or enum definition.
#[derive(Clone, Debug, ParseAttributes)]
#[deluxe(attributes(nanosql))]
pub struct ContainerAttributes {
    /// For `#[derive(Param)]`: specifies the prefix character before the parameter name.
    #[deluxe(alias = prefix, alias = param_pfx)]
    pub param_prefix: Option<ParamPrefix>,
    /// For `#[derive(Table)]`: changes the `InsertInput` associated type from `Self`.
    #[deluxe(default = parse_quote!(Self))]
    pub insert_input_ty: Type,
    /// For `#[derive(Table)]`: changes the declared lifetime parameter
    /// of the  `InsertInput` associated type from the default `'p`.
    #[deluxe(default = parse_quote!('p))]
    pub insert_input_lt: Lifetime,
    /// For `#[derive(Table)]`: the name of the table itself.
    pub rename: Option<String>,
    /// For various macros: rename all fields or variants,
    /// according to the specified case conversion.
    #[deluxe(default)]
    #[deluxe(with = deluxe::with::syn)]
    pub rename_all: CaseConversion,
}

/// Attributes on a struct field or an enum variant.
#[derive(Clone, Debug, ParseAttributes)]
#[deluxe(attributes(nanosql))]
pub struct FieldAttributes {
    /// For various derive macros: parse and serialize the field or variant
    /// with the given name, instead of the original field or variant name.
    pub rename: Option<String>,
    /// For `#[derive(Table)]`: specifies an alternate SQL type source
    /// (`AsSqlTy`) for the column, instead of using the field's type.
    pub sql_ty: Option<Type>,
    /// For `#[derive(Table)]`: declares that the field must be unique.
    #[deluxe(default = false)]
    pub unique: bool,
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

impl ParseMetaItem for ParamPrefix {
    fn parse_meta_item(stream: ParseStream<'_>, _: deluxe::ParseMode) -> Result<Self, Error> {
        <ParamPrefix as Parse>::parse(stream)
    }
}

impl ToTokens for ParamPrefix {
    fn to_tokens(&self, ts: &mut TokenStream2) {
        let variant_name = match *self {
            ParamPrefix::Dollar   => "Dollar",
            ParamPrefix::Colon    => "Colon",
            ParamPrefix::Question => "Question",
            ParamPrefix::At       => "At",
        };
        Ident::new(variant_name, Span::call_site()).to_tokens(ts);
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, Debug)]
pub enum CaseConversion {
    #[default]
    Identity,
    LowerSnakeCase,
    UpperSnakeCase,
    LowerCamelCase,
    UpperCamelCase,
    LowerKebabCase,
    UpperKebabCase,
    TitleCase,
    TrainCase,
}

impl Parse for CaseConversion {
    fn parse(stream: ParseStream<'_>) -> Result<Self, Error> {
        let lookahead = stream.lookahead1();
        if lookahead.peek(syn::LitStr) {
            stream.parse::<syn::LitStr>()?.parse()
        } else if lookahead.peek(syn::Ident) {
            let s = <syn::Ident as syn::ext::IdentExt>::parse_any(stream)?.to_string();

            Ok(match s.as_ref() {
                "identity" => CaseConversion::Identity,
                "lower_snake_case" => CaseConversion::LowerSnakeCase,
                "UPPER_SNAKE_CASE" => CaseConversion::UpperSnakeCase,
                "lowerCamelCase" => CaseConversion::LowerCamelCase,
                "UpperCamelCase" => CaseConversion::UpperCamelCase,
                "lower-kebab-case" => CaseConversion::LowerKebabCase,
                "UPPER-KEBAB-CASE" => CaseConversion::UpperKebabCase,
                "Title Case" => CaseConversion::TitleCase,
                "Train-Case" => CaseConversion::TrainCase,
                _ => return Err(stream.error("invalid case conversion method")),
            })
        } else {
            Err(lookahead.error())
        }
    }
}

impl CaseConversion {
    pub fn display<T>(self, value: T) -> CaseConversionDisplay<T> {
        CaseConversionDisplay {
            value,
            conversion: self,
        }
    }
}

pub struct CaseConversionDisplay<T> {
    value: T,
    conversion: CaseConversion,
}

impl<T: Display> Display for CaseConversionDisplay<T> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        use heck::*;
        use CaseConversion::*;

        match self.conversion {
            Identity       => self.value.fmt(formatter),
            LowerSnakeCase => AsSnakeCase(self.value.to_string()).fmt(formatter),
            UpperSnakeCase => AsShoutySnakeCase(self.value.to_string()).fmt(formatter),
            LowerCamelCase => AsLowerCamelCase(self.value.to_string()).fmt(formatter),
            UpperCamelCase => AsUpperCamelCase(self.value.to_string()).fmt(formatter),
            LowerKebabCase => AsKebabCase(self.value.to_string()).fmt(formatter),
            UpperKebabCase => AsShoutyKebabCase(self.value.to_string()).fmt(formatter),
            TitleCase      => AsTitleCase(self.value.to_string()).fmt(formatter),
            TrainCase      => AsTrainCase(self.value.to_string()).fmt(formatter),
        }
    }
}
