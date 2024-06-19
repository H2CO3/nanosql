use core::fmt::{self, Display, Formatter, Write};
use std::hash::{Hash, Hasher};
use std::collections::HashSet;
use proc_macro::TokenStream as TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Span, Ident};
use syn::{Token, Fields, WhereClause, WherePredicate, TypeParamBound, Lit, LitStr, Type, Lifetime};
use syn::parse_quote;
use syn::parse::{Parse, ParseStream, Error};
use syn::punctuated::Punctuated;
use syn::ext::IdentExt;
use quote::ToTokens;
use deluxe::{ParseAttributes, ParseMetaItem, ParseMode, SpannedValue};


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
    pub rename: Option<IdentOrStr>,
    /// For `#[derive(InsertInput)]`: the `Table` associated type of the insert input.
    pub table: Option<Type>,
    /// For various macros: rename all fields or variants,
    /// according to the specified case conversion.
    #[deluxe(default, with = deluxe::with::syn)]
    pub rename_all: CaseConversion,
    /// For `#[derive(Table)]`: the Primary Key columns.
    #[deluxe(alias = pk)]
    pub primary_key: Option<SpannedValue<Vec<IdentOrStr>>>,
    /// For `#[derive(Table)]`: the Foreign Key columns.
    #[deluxe(append, alias = fk)]
    pub foreign_key: Vec<TableForeignKey>,
    /// For `#[derive(Table)]`: applies `UNIQUE` constraints on many columns.
    #[deluxe(append)]
    pub unique: Vec<SpannedValue<Vec<IdentOrStr>>>,
    /// For `#[derive(Table)]`: applies additional `CHECK` constraints.
    #[deluxe(append)]
    pub check: Vec<String>,
}

/// Attributes on a struct field or an enum variant.
#[derive(Clone, Debug, ParseAttributes)]
#[deluxe(attributes(nanosql))]
pub struct FieldAttributes {
    /// For various derive macros: parse and serialize the field or variant
    /// with the given name, instead of the original field or variant name.
    pub rename: Option<IdentOrStr>,
    /// For `#[derive(Table)]`: specifies an alternate SQL type source
    /// (`AsSqlTy`) for the column, instead of using the field's type.
    pub sql_ty: Option<Type>,
    /// For `#[derive(Table)]`: marks the field as the PRIMARY KEY.
    #[deluxe(alias = pk, default = SpannedValue::new(false))]
    pub primary_key: SpannedValue<bool>,
    #[deluxe(alias = fk)]
    pub foreign_key: Option<FieldForeignKey>,
    /// For `#[derive(Table)]`: declares that the field must be unique.
    #[deluxe(default = false)]
    pub unique: bool,
    /// For `#[derive(Table)]`: applies additional CHECK constraints.
    #[deluxe(append)]
    pub check: Vec<String>,
    /// For `#[derive(Table)]`: provides a default value when column
    /// value is omitted during insertion.
    pub default: Option<String>,
    /// For `#[derive(Table)]`: specifies that the column should be
    /// generated based on some expression involving other columns.
    pub generated: Option<GeneratedColumnSpec>,
}

#[derive(Clone, Debug)]
pub struct FieldForeignKey {
    pub table: IdentOrStr,
    pub column: IdentOrStr,
}

impl ParseMetaItem for FieldForeignKey {
    fn parse_meta_item(input: ParseStream<'_>, _mode: ParseMode) -> Result<Self, Error> {
        let table: IdentOrStr = input.parse()?;

        let lookahead = input.lookahead1();
        if lookahead.peek(Token![=>]) {
            let _: Token![=>] = input.parse()?;
        } else if lookahead.peek(Token![::]) {
            let _: Token![::] = input.parse()?;
        } else {
            return Err(lookahead.error());
        }

        let column: IdentOrStr = input.parse()?;

        Ok(FieldForeignKey { table, column })
    }
}

#[derive(Clone, Debug)]
pub struct TableForeignKey {
    pub table: IdentOrStr,
    pub columns: Punctuated<(IdentOrStr, IdentOrStr), Token![,]>,
}

impl ParseMetaItem for TableForeignKey {
    fn parse_meta_item(input: ParseStream<'_>, _mode: ParseMode) -> Result<Self, Error> {
        let table: IdentOrStr = input.parse()?;
        let _: Token![=>] = input.parse()?;
        let paren_inner;
        let _ = syn::parenthesized!(paren_inner in input);

        let columns = Punctuated::parse_terminated_with(&paren_inner, |stream| {
            let this: IdentOrStr = stream.parse()?;
            let _: Token![=] = stream.parse()?;
            let other: IdentOrStr = stream.parse()?;
            Ok((this, other))
        })?;

        Ok(TableForeignKey { table, columns })
    }
}

#[derive(Clone, Debug)]
pub struct GeneratedColumnSpec {
    pub mode: GeneratedColumnMode,
    pub expr: String,
}

impl ParseMetaItem for GeneratedColumnSpec {
    fn parse_meta_item(input: ParseStream<'_>, _mode: ParseMode) -> Result<Self, Error> {
        let mode: GeneratedColumnMode = input.parse()?;
        let _eq: Token![=] = input.parse()?;
        let lit: LitStr = input.parse()?;
        let expr = lit.value();

        Ok(GeneratedColumnSpec { mode, expr })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum GeneratedColumnMode {
    Virtual,
    Stored,
}

impl Parse for GeneratedColumnMode {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let mode: IdentOrStr = input.parse()?;

        match mode.to_string().as_str() {
            "virtual" => Ok(GeneratedColumnMode::Virtual),
            "stored" => Ok(GeneratedColumnMode::Stored),
            _ => Err(Error::new_spanned(mode, "generated column must be `virtual` or `stored`")),
        }
    }
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
        let raw: IdentOrStr = stream.parse()?;

        Ok(match raw.to_string().as_str() {
            "identity" => CaseConversion::Identity,
            "lower_snake_case" => CaseConversion::LowerSnakeCase,
            "UPPER_SNAKE_CASE" => CaseConversion::UpperSnakeCase,
            "lowerCamelCase" => CaseConversion::LowerCamelCase,
            "UpperCamelCase" => CaseConversion::UpperCamelCase,
            "lower-kebab-case" => CaseConversion::LowerKebabCase,
            "UPPER-KEBAB-CASE" => CaseConversion::UpperKebabCase,
            "Title Case" => CaseConversion::TitleCase,
            "Train-Case" => CaseConversion::TrainCase,
            _ => return Err(Error::new_spanned(&raw, "invalid case conversion method")),
        })
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

/// A generic helper for parsing a string from either a string literal or an identifier.
#[derive(Clone, Eq, Debug)]
pub enum IdentOrStr {
    Ident(Ident),
    Str(LitStr),
}

impl Parse for IdentOrStr {
    fn parse(stream: ParseStream<'_>) -> Result<Self, Error> {
        let lookahead = stream.lookahead1();

        if lookahead.peek(Ident::peek_any) {
            Ident::parse_any(stream).map(|ident| IdentOrStr::Ident(ident.unraw()))
        } else if lookahead.peek(LitStr) {
            stream.parse::<LitStr>().map(IdentOrStr::Str)
        } else {
            Err(lookahead.error())
        }
    }
}

impl ParseMetaItem for IdentOrStr {
    fn parse_meta_item(stream: ParseStream<'_>, _: deluxe::ParseMode) -> Result<Self, Error> {
        <IdentOrStr as Parse>::parse(stream)
    }
}

impl ToTokens for IdentOrStr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            IdentOrStr::Ident(ident) => {
                let lit = LitStr::new(&ident.to_string(), ident.span());
                lit.to_tokens(tokens);
            }
            IdentOrStr::Str(lit) => lit.to_tokens(tokens)
        }
    }
}

impl Display for IdentOrStr {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            IdentOrStr::Ident(ident) => Display::fmt(ident, formatter),
            IdentOrStr::Str(lit) => Display::fmt(&lit.value(), formatter),
        }
    }
}

impl PartialEq for IdentOrStr {
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}

impl Hash for IdentOrStr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}
