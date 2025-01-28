use core::fmt::{self, Display, Formatter, Write};
use std::hash::{Hash, Hasher};
use std::collections::HashSet;
use proc_macro::TokenStream as TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Span, Ident};
use syn::{Token, Fields, WhereClause, WherePredicate, TypeParamBound, Type, Lifetime};
use syn::{parse_quote, Lit, LitBool, LitStr};
use syn::parse::{Parse, ParseStream, Error};
use syn::punctuated::Punctuated;
use syn::ext::IdentExt;
use quote::{quote, quote_spanned, ToTokens};
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

/// Helper for implementing `ParseMetaItem` for more complex types
macro_rules! return_ok_if_eof_or_trailing {
    (stream: $stream:expr, sep: $sep:ty, value: $value:expr $(,)?) => {
        // if we are already at EOF, return
        if $stream.is_empty() {
            return Ok($value);
        }

        // otherwise, expect a separator
        let _: $sep = $stream.parse()?;

        // if this was a trailing separator, return
        if $stream.is_empty() {
            return Ok($value);
        }
    }
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
    #[deluxe(alias = insert_ty, alias = insert_type, alias = insert_input_type)]
    pub insert_input_ty: Type,
    /// For `#[derive(Table)]`: changes the `PrimaryKey` associated type from the default
    /// (single scalar for column-level PK, tuple for table-level PK). Used for giving
    /// names to the columns of a compound PK, for readability and correctness.
    #[deluxe(alias = pk_ty, alias = pk_type, alias = primary_key_type)]
    pub primary_key_ty: Option<Type>,
    /// For `#[derive(Table)]`: changes the declared lifetime parameter of the
    /// `InsertInput` and `PrimaryKey` associated types from the default `'p`.
    #[deluxe(default = parse_quote!('p))]
    pub input_lt: Lifetime,
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
    /// For `#[derive(Table)]`: add an explicit index.
    #[deluxe(append)]
    pub index: Vec<TableIndexSpec>,
    /// For `#[derive(Table)]`: applies `UNIQUE` constraints on many columns.
    #[deluxe(append)]
    pub unique: Vec<SpannedValue<Vec<IdentOrStr>>>,
    /// For `#[derive(Table)]`: applies additional `CHECK` constraints.
    #[deluxe(append)]
    pub check: Vec<SqlExprStr>,
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
    /// For `#[derive(Table)]`: marks the field as a FOREIGN KEY.
    #[deluxe(alias = fk)]
    pub foreign_key: Option<ColumnForeignKey>,
    /// For `#[derive(Table)]`: adds an index to this column.
    pub index: Option<ColumnIndexSpec>,
    /// For `#[derive(Table)]`: declares that the field must be unique.
    #[deluxe(default = false)]
    pub unique: bool,
    /// For `#[derive(Table)]`: applies additional CHECK constraints.
    #[deluxe(append)]
    pub check: Vec<SqlExprStr>,
    /// For `#[derive(Table)]`: provides a default value when column
    /// value is omitted during insertion.
    pub default: Option<SqlExprStr>,
    /// For `#[derive(Table)]`: specifies that the column should be
    /// generated based on some expression involving other columns.
    pub generated: Option<GeneratedColumnSpec>,
}

/// Ensures that an SQL expression that is going to be interpolated
/// verbatim into the generated SQL is at leasy syntactically valid.
/// This transparently parses from/serializes to a string literal,
/// and if the `expr-check` feature is enabled, it validates the
/// syntax of its contetns, as if it were an SQL expression.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct SqlExprStr {
    /// A `LitStr` and not a plain `String`, to preserve span information
    sql: LitStr,
}

#[cfg(feature = "expr-check")]
impl SqlExprStr {
    fn check_sql_expr_syntax(fragment: &LitStr) -> Result<(), Error> {
        let value = fragment.value();
        let dialect = sqlparser::dialect::SQLiteDialect {};
        let mut parser = sqlparser::parser::Parser::new(&dialect)
            .try_with_sql(&value)
            .map_err(|sqlerr| Error::new_spanned(fragment, sqlerr))?;

        // parse the SQL expression into an AST but throw it away; only forward errors
        let _ = parser
            .parse_expr()
            .map_err(|sqlerr| Error::new_spanned(fragment, sqlerr))?;

        // ensure there isn't any trailing junk
        parser
            .expect_token(&sqlparser::tokenizer::Token::EOF)
            .map_err(|sqlerr| Error::new_spanned(fragment, sqlerr))?;

        Ok(())
    }
}

impl Parse for SqlExprStr {
    fn parse(input: ParseStream<'_>) -> Result<Self, Error> {
        let sql: LitStr = input.parse()?;

        #[cfg(feature = "expr-check")]
        SqlExprStr::check_sql_expr_syntax(&sql)?;

        Ok(SqlExprStr { sql })
    }
}

impl ToTokens for SqlExprStr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.sql.to_tokens(tokens);
    }
}

impl ParseMetaItem for SqlExprStr {
    fn parse_meta_item(input: ParseStream<'_>, _mode: ParseMode) -> Result<Self, Error> {
        input.parse::<SqlExprStr>()
    }
}

#[derive(Clone, Debug)]
pub struct ColumnForeignKey {
    pub table: IdentOrStr,
    pub column: IdentOrStr,
}

impl ToTokens for ColumnForeignKey {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let table = &self.table;
        let column = &self.column;

        tokens.extend(quote!{
            .foreign_key(#table, #column)
        });
    }
}

impl ParseMetaItem for ColumnForeignKey {
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

        Ok(ColumnForeignKey { table, column })
    }
}

/// Miraculously, the `Default` impl does exactly the right thing.
#[derive(Clone, Default, Debug)]
pub struct ColumnIndexSpec {
    pub unique: bool,
    pub sort_order: SortOrder,
    pub predicate: Option<SqlExprStr>,
}

impl ToTokens for ColumnIndexSpec {
    fn to_tokens(&self, ts: &mut TokenStream2) {
        let &ColumnIndexSpec { unique, sort_order, ref predicate } = self;
        let predicate_tokens = match predicate {
            None => quote!(::core::option::Option::<&'static ::core::primitive::str>::None),
            Some(expr) => quote!(::core::option::Option::Some(#expr)),
        };

        ts.extend(quote!{
            .set_index(
                #unique,
                #sort_order,
                #predicate_tokens,
            )
        })
    }
}

impl ParseMetaItem for ColumnIndexSpec {
    fn parse_meta_item(input: ParseStream<'_>, _mode: ParseMode) -> Result<Self, Error> {
        mod kw {
            syn::custom_keyword!(unique);
            syn::custom_keyword!(predicate);
            syn::custom_keyword!(asc);
            syn::custom_keyword!(desc);
        }

        let mut index_spec = ColumnIndexSpec::default();

        if input.peek(kw::unique) {
            let _: kw::unique = input.parse()?;

            if input.peek(Token![=]) {
                let _: Token![=] = input.parse()?;
                let bool_lit: LitBool = input.parse()?;
                index_spec.unique = bool_lit.value();
            } else {
                index_spec.unique = true;
            }

            return_ok_if_eof_or_trailing!(
                stream: input,
                sep: Token![,],
                value: index_spec,
            );
        }

        if input.peek(kw::asc) {
            let _: kw::asc = input.parse()?;
            index_spec.sort_order = SortOrder::Ascending;

            return_ok_if_eof_or_trailing!(
                stream: input,
                sep: Token![,],
                value: index_spec,
            );
        } else if input.peek(kw::desc) {
            let _: kw::desc = input.parse()?;
            index_spec.sort_order = SortOrder::Descending;

            return_ok_if_eof_or_trailing!(
                stream: input,
                sep: Token![,],
                value: index_spec,
            );
        }

        if input.peek(Token![where]) {
            let _: Token![where] = input.parse()?;
            let _: Token![=] = input.parse()?;
            let predicate: SqlExprStr = input.parse()?;

            index_spec.predicate = Some(predicate);

            // eat trailing comma, if any
            if input.peek(Token![,]) {
                let _: Token![,] = input.parse()?;
            }
        }

        Ok(index_spec)
    }

    fn parse_meta_item_flag(_span: Span) -> Result<Self, Error> {
        // without any arguments, just return the default config:
        // non-unique, ascending, total index (no filter predicate)
        Ok(ColumnIndexSpec::default())
    }
}

#[derive(Clone, Debug)]
pub struct TableIndexSpec {
    pub unique: bool,
    pub columns: Punctuated<(IdentOrStr, SortOrder), Token![,]>,
    pub predicate: Option<SqlExprStr>,
    pub span: Span,
}

impl Default for TableIndexSpec {
    fn default() -> Self {
        TableIndexSpec {
            unique: false,
            columns: Punctuated::new(),
            predicate: None,
            span: Span::call_site(),
        }
    }
}

impl ToTokens for TableIndexSpec {
    fn to_tokens(&self, ts: &mut TokenStream2) {
        let &TableIndexSpec { unique, ref columns, ref predicate, span } = self;
        let predicate_tokens = match predicate {
            None => quote!(::core::option::Option::<&'static ::core::primitive::str>::None),
            Some(expr) => quote!(::core::option::Option::Some(#expr)),
        };
        let column_names = columns.iter().map(|(col_name, _)| col_name);
        let sort_orders = columns.iter().map(|&(_, sort_order)| sort_order);

        ts.extend(quote_spanned!{ span =>
            .add_index(
                #unique,
                [#((#column_names, #sort_orders),)*],
                #predicate_tokens,
            )
        });
    }
}

impl ParseMetaItem for TableIndexSpec {
    fn parse_meta_item(input: ParseStream<'_>, _mode: ParseMode) -> Result<Self, Error> {
        mod kw {
            syn::custom_keyword!(unique);
            syn::custom_keyword!(columns);
            syn::custom_keyword!(predicate);
            syn::custom_keyword!(asc);
            syn::custom_keyword!(desc);
        }

        let mut index_spec = TableIndexSpec::default();

        if input.peek(kw::unique) {
            let _: kw::unique = input.parse()?;

            if input.peek(Token![=]) {
                let _: Token![=] = input.parse()?;
                let bool_lit: LitBool = input.parse()?;
                index_spec.unique = bool_lit.value();
            } else {
                index_spec.unique = true;
            }

            // unconditionally expect a comma because the following `columns` part is mandatory
            let _: Token![,] = input.parse()?;
        }

        let _: kw::columns = input.parse()?;
        let paren_content;
        let paren_delims = syn::parenthesized!(paren_content in input);

        // we only care that this is on the right line
        index_spec.span = paren_delims.span.join();
        index_spec.columns = Punctuated::parse_terminated_with(&paren_content, |inner_stream| {
            let col_name: IdentOrStr = inner_stream.parse()?;
            let mut sort_order = SortOrder::default();

            if inner_stream.peek(Token![=]) {
                let _: Token![=] = inner_stream.parse()?;
                let lookahead = inner_stream.lookahead1();

                if lookahead.peek(kw::asc) {
                    let _: kw::asc = inner_stream.parse()?;
                    sort_order = SortOrder::Ascending;
                } else if lookahead.peek(kw::desc) {
                    let _: kw::desc = inner_stream.parse()?;
                    sort_order = SortOrder::Descending;
                } else {
                    return Err(lookahead.error());
                }
            }

            Ok((col_name, sort_order))
        })?;

        return_ok_if_eof_or_trailing!(stream: input, sep: Token![,], value: index_spec);

        if input.peek(Token![where]) {
            let _: Token![where] = input.parse()?;
            let _: Token![=] = input.parse()?;
            let predicate: SqlExprStr = input.parse()?;

            index_spec.predicate = Some(predicate);

            // eat trailing comma, if any
            if input.peek(Token![,]) {
                let _: Token![,] = input.parse()?;
            }
        }

        Ok(index_spec)
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, Debug)]
pub enum SortOrder {
    #[default]
    Ascending,
    Descending,
}

impl ToTokens for SortOrder {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(match *self {
            SortOrder::Ascending => quote!(::nanosql::table::SortOrder::Ascending),
            SortOrder::Descending => quote!(::nanosql::table::SortOrder::Descending),
        });
    }
}

#[derive(Clone, Debug)]
pub struct TableForeignKey {
    pub table: IdentOrStr,
    pub columns: Punctuated<(IdentOrStr, IdentOrStr), Token![,]>,
}

impl ToTokens for TableForeignKey {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let table_name = &self.table;
        let own_cols = self.columns.iter().map(|pair| &pair.0);
        let other_cols = self.columns.iter().map(|pair| &pair.1);

        tokens.extend(quote!{
            .foreign_key(#table_name, [#((#own_cols, #other_cols),)*])
        });
    }
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
    pub expr: SqlExprStr,
}

impl ToTokens for GeneratedColumnSpec {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let expr = &self.expr;

        tokens.extend(match self.mode {
            GeneratedColumnMode::Virtual => quote!{
                .generate_virtual(#expr)
            },
            GeneratedColumnMode::Stored => quote!{
                .generate_stored(#expr)
            },
        });
    }
}

impl ParseMetaItem for GeneratedColumnSpec {
    fn parse_meta_item(input: ParseStream<'_>, _mode: ParseMode) -> Result<Self, Error> {
        let mode: GeneratedColumnMode = input.parse()?;
        let _eq: Token![=] = input.parse()?;
        let expr: SqlExprStr = input.parse()?;

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

impl IdentOrStr {
    fn validate_ident(ident: &str, span: Span) -> Result<(), Error> {
        let mut chars = ident.chars();
        let Some(first) = chars.next() else {
            return Err(Error::new(span, "identifier must not be empty"));
        };
        if !first.is_ascii_alphabetic() && first != '_' {
            return Err(
                Error::new(span, format_args!("invalid leading character `{first}` in identifier"))
            );
        }
        if let Some(bad) = chars.find(|&c| !c.is_ascii_alphanumeric() && c != '_') {
            return Err(
                Error::new(span, format_args!("invalid character `{bad}` in identifier"))
            );
        }
        Ok(())
    }
}

impl Parse for IdentOrStr {
    fn parse(stream: ParseStream<'_>) -> Result<Self, Error> {
        let lookahead = stream.lookahead1();

        if lookahead.peek(Ident::peek_any) {
            Ident::parse_any(stream).and_then(|ident| {
                let ident = ident.unraw();
                Self::validate_ident(&ident.to_string(), ident.span())?;
                Ok(IdentOrStr::Ident(ident))
            })
        } else if lookahead.peek(LitStr) {
            stream.parse::<LitStr>().and_then(|lit| {
                Self::validate_ident(&lit.value(), lit.span())?;
                Ok(IdentOrStr::Str(lit))
            })
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
