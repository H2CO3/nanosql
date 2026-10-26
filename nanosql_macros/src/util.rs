use core::fmt::{self, Display, Formatter, Write};
use core::ops::Deref;
use std::hash::{Hash, Hasher};
use std::collections::HashSet;

use proc_macro::TokenStream as TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Span, Ident};
use syn::{Token, Field, Fields, WhereClause, WherePredicate, TypeParamBound, Type, Lifetime};
use syn::{parse_quote, Lit, LitBool, LitStr};
use syn::parse::{Parse, ParseStream, Error};
use syn::punctuated::Punctuated;
use syn::ext::IdentExt;
use quote::{quote, quote_spanned, ToTokens};
use attribute_derive::{
    FromAttr,
    from_partial::{FromPartial, Partial},
    utils::FlagOrValue,
    parsing::{SpannedValue, AttributeValue, AttributeBase, PositionalValue},
};


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
    bounds_for_ignored: Option<Punctuated<TypeParamBound, Token![+]>>,
) -> Result<WhereClause, Error> {
    // we use two separate sets, as this allows us to de-duplicate
    // types, yet still subject the same type to both the regular
    // and the "ignored" bounds (usually, Default) simultaneously.
    let mut uniq_types_for_bounds = HashSet::with_capacity(fields.len());
    let mut uniq_types_for_ignored = HashSet::with_capacity(fields.len());

    match fields {
        Fields::Unit => {}
        Fields::Named(fields) => {
            for field in &fields.named {
                let attrs = FieldAttributes::from_attributes(&field.attrs)?;
                if attrs.ignore.is_set() {
                    uniq_types_for_ignored.insert(&field.ty);
                } else {
                    uniq_types_for_bounds.insert(&field.ty);
                }
            }
        }
        Fields::Unnamed(fields) => {
            for field in &fields.unnamed {
                let attrs = FieldAttributes::from_attributes(&field.attrs)?;
                if attrs.ignore.is_set() {
                    uniq_types_for_ignored.insert(&field.ty);
                } else {
                    uniq_types_for_bounds.insert(&field.ty);
                }
            }
        }
    };

    let mut where_clause = where_clause.cloned().unwrap_or_else(|| {
        WhereClause {
            where_token: Default::default(),
            predicates: Default::default(),
        }
    });

    where_clause.predicates.extend(
        uniq_types_for_bounds.iter().map(|ty| -> WherePredicate {
            parse_quote!(#ty: #bounds)
        })
    );
    if let Some(ignored_bounds) = bounds_for_ignored.as_ref() {
        where_clause.predicates.extend(
            uniq_types_for_ignored.iter().map(|ty| -> WherePredicate {
                parse_quote!(#ty: #ignored_bounds)
            })
        );
    }

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
#[derive(Clone, Debug, FromAttr)]
#[attribute(ident = nanosql)]
pub struct ContainerAttributes {
    /// For `#[derive(Param)]`: specifies the prefix character before the parameter name.
    pub param_prefix: Option<ParamPrefix>,
    /// For `#[derive(Table)]`: changes the `InsertInput` associated type from `Self`.
    #[attribute(default = parse_quote!(Self))]
    pub insert_input_ty: Type,
    /// For `#[derive(Table)]`: changes the `PrimaryKey` associated type from the default
    /// (single scalar for column-level PK, tuple for table-level PK). Used for giving
    /// names to the columns of a compound PK, for readability and correctness.
    pub pk_ty: Option<Type>,
    /// For `#[derive(Table)]`: changes the declared lifetime parameter of the
    /// `InsertInput` and `PrimaryKey` associated types from the default `'p`.
    #[attribute(default = parse_quote!('p))]
    pub input_lt: Lifetime,
    /// For `#[derive(Table)]`: the name of the table itself.
    pub rename: Option<IdentOrStr>,
    /// For `#[derive(InsertInput)]`: the `Table` associated type of the insert input.
    pub table: Option<Type>,
    /// For various macros: rename all fields or variants,
    /// according to the specified case conversion.
    #[attribute(default = CaseConversion::default())]
    pub rename_all: CaseConversion,
    /// For `#[derive(Table)]`: the Primary Key columns.
    pub primary_key: Option<Vec<IdentOrStr>>,
    /// For `#[derive(Table)]`: the Foreign Key columns.
    #[attribute(default = Default::default())]
    pub foreign_key: Any<TableForeignKey>,
    /// For `#[derive(Table)]`: add an explicit index.
    #[attribute(default = Any::default())]
    pub index: Any<TableIndexSpec>,
    /// For `#[derive(Table)]`: applies `UNIQUE` constraints on many columns.
    #[attribute(default = Default::default())]
    pub unique: Any<Vec<IdentOrStr>>,
    /// For `#[derive(Table)]`: applies additional `CHECK` constraints.
    #[attribute(default = Default::default())]
    pub check: Any<SqlExprStr>,
}

/// Attributes on a struct field or an enum variant.
#[derive(Debug, FromAttr)]
#[attribute(ident = nanosql)]
pub struct FieldAttributes {
    /// For various derive macros: parse and serialize the field or variant
    /// with the given name, instead of the original field or variant name.
    pub rename: Option<IdentOrStr>,
    /// For `#[derive(Table)]`: specifies an alternate SQL type source
    /// (`AsSqlTy`) for the column, instead of using the field's type.
    pub sql_ty: Option<Type>,
    /// For `#[derive(Table)]`: marks the field as the PRIMARY KEY.
    pub primary_key: FlagOrValue<LitBool>,
    /// For `#[derive(Table)]`: marks the field as a FOREIGN KEY.
    pub foreign_key: Option<ColumnForeignKey>,
    /// For `#[derive(Table)]`: adds an index to this column.
    pub index: FlagOrValue<ColumnIndexSpec>,
    /// For `#[derive(Table)]`: declares that the field must be unique.
    pub unique: FlagOrValue<LitBool>,
    /// For `#[derive(Table)]`: applies additional CHECK constraints.
    #[attribute(default = Any::default())]
    pub check: Any<SqlExprStr>,
    /// For `#[derive(Table)]`: provides a default value when column
    /// value is omitted during insertion.
    pub default: Option<SqlExprStr>,
    /// For `#[derive(Table)]`: specifies that the column should be
    /// generated based on some expression involving other columns.
    pub generated: Option<GeneratedColumnSpec>,
    /// For `#[derive(Table, Param, ResultRecord)]`: do not serialize or
    /// deserialize this field. It will be skipped when binding parameters,
    /// and its default value will be used when retrieving the result set.
    pub ignore: FlagOrValue<LitBool>,
}

#[derive(Debug)]
pub struct FieldAndColumn {
    /// The `syn` AST for the field declaration.
    pub field: Field,
    /// The (possibly renamed) column name, derived from the field's name.
    pub column_name: String,
    /// `#[nanosql(...)]` attributes.
    pub attributes: FieldAttributes,
}

impl FieldAndColumn {
    /// Return the field's SQL type, respecting the `#[nanosql(sql_ty = "...")]` attribute
    pub fn sql_ty(&self) -> &Type {
        self.attributes.sql_ty.as_ref().unwrap_or(&self.field.ty)
    }
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

impl AttributeBase for SqlExprStr {
    type Partial = Self;
}

impl AttributeValue for SqlExprStr {
    fn parse_value(input: ParseStream) -> syn::Result<SpannedValue<Self::Partial>> {
        input.parse::<SqlExprStr>().map(SpannedValue::from_to_tokens)
    }
}

impl PositionalValue for SqlExprStr {}

#[derive(Clone, Debug)]
pub struct ColumnForeignKey {
    pub table: IdentOrStr,
    pub column: IdentOrStr,
    pub conflict_actions: ForeignKeyConflictActions,
}

impl ToTokens for ColumnForeignKey {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let ColumnForeignKey {
            table,
            column,
            conflict_actions: ForeignKeyConflictActions {
                on_delete,
                on_update,
            },
        } = self;

        tokens.extend(quote!{
            .foreign_key(#table, #column, #on_delete, #on_update)
        });
    }
}

impl AttributeBase for ColumnForeignKey {
    type Partial = Self;
}

impl AttributeValue for ColumnForeignKey {
    fn parse_value(input: ParseStream) -> syn::Result<SpannedValue<Self::Partial>> {
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
        let mut fk_spec = ColumnForeignKey {
            table,
            column,
            conflict_actions: ForeignKeyConflictActions::default(),
        };

        if input.peek(Token![,]) {
            let _: Token![,] = input.parse()?;

            fk_spec.conflict_actions = input.parse()?;

            // eat trailing comma, if any
            if input.peek(Token![,]) {
                let _: Token![,] = input.parse()?;
            }
        }

        Ok(SpannedValue::from_to_tokens(fk_spec))
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

impl AttributeBase for ColumnIndexSpec {
    type Partial = Self;
}

impl AttributeValue for ColumnIndexSpec {
    fn parse_value(input: ParseStream) -> syn::Result<SpannedValue<Self::Partial>> {
        mod kw {
            syn::custom_keyword!(unique);
            syn::custom_keyword!(asc);
            syn::custom_keyword!(desc);
        }

        if input.is_empty() {
            return Err(input.error("empty index spec"));
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
                value: SpannedValue::from_to_tokens(index_spec),
            );
        }

        if input.peek(kw::asc) {
            let _: kw::asc = input.parse()?;
            index_spec.sort_order = SortOrder::Ascending;

            return_ok_if_eof_or_trailing!(
                stream: input,
                sep: Token![,],
                value: SpannedValue::from_to_tokens(index_spec),
            );
        } else if input.peek(kw::desc) {
            let _: kw::desc = input.parse()?;
            index_spec.sort_order = SortOrder::Descending;

            return_ok_if_eof_or_trailing!(
                stream: input,
                sep: Token![,],
                value: SpannedValue::from_to_tokens(index_spec),
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

        Ok(SpannedValue::from_to_tokens(index_spec))
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

impl AttributeBase for TableIndexSpec {
    type Partial = Self;
}

impl AttributeValue for TableIndexSpec {
    fn parse_value(input: ParseStream) -> syn::Result<SpannedValue<Self::Partial>> {
        mod kw {
            syn::custom_keyword!(unique);
            syn::custom_keyword!(columns);
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

        return_ok_if_eof_or_trailing!(
            stream: input,
            sep: Token![,],
            value: SpannedValue::from_to_tokens(index_spec)
        );

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

        Ok(SpannedValue::from_to_tokens(index_spec))
    }
}

impl PositionalValue for TableIndexSpec {}

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

#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, Debug)]
pub struct ForeignKeyConflictActions {
    pub on_delete: ForeignKeyConflictAction,
    pub on_update: ForeignKeyConflictAction,
}

impl Parse for ForeignKeyConflictActions {
    fn parse(input: ParseStream<'_>) -> Result<Self, Error> {
        mod kw {
            syn::custom_keyword!(on_delete);
            syn::custom_keyword!(on_update);
        }

        let mut actions = Self::default();

        loop {
            let lah = input.lookahead1();
            if lah.peek(kw::on_delete) {
                let _: kw::on_delete = input.parse()?;
                let _: Token![=] = input.parse()?;
                actions.on_delete = input.parse()?;
            } else if input.peek(kw::on_update) {
                let _: kw::on_update = input.parse()?;
                let _: Token![=] = input.parse()?;
                actions.on_update = input.parse()?;
            } else {
                break;
            }

            // eat comma separator
            if input.peek(Token![,]) {
                let _: Token![,] = input.parse()?;
            }
        }

        Ok(actions)
    }
}

#[derive(Clone, Debug)]
pub struct TableForeignKey {
    pub table: IdentOrStr,
    pub columns: Punctuated<(IdentOrStr, IdentOrStr), Token![,]>,
    pub conflict_actions: ForeignKeyConflictActions,
}

impl ToTokens for TableForeignKey {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let TableForeignKey {
            table,
            columns,
            conflict_actions: ForeignKeyConflictActions {
                on_delete,
                on_update,
            },
        } = self;

        let own_cols = columns.iter().map(|pair| &pair.0);
        let other_cols = columns.iter().map(|pair| &pair.1);

        tokens.extend(quote!{
            .foreign_key(#table, [#((#own_cols, #other_cols),)*], #on_delete, #on_update)
        });
    }
}

impl AttributeBase for TableForeignKey {
    type Partial = Self;
}

impl AttributeValue for TableForeignKey {
    fn parse_value(input: ParseStream) -> syn::Result<SpannedValue<Self::Partial>> {
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

        let mut fk_spec = TableForeignKey {
            table,
            columns,
            conflict_actions: ForeignKeyConflictActions::default(),
        };

        if input.peek(Token![,]) {
            let _: Token![,] = input.parse()?;

            fk_spec.conflict_actions = input.parse()?;

            // eat trailing comma, if any
            if input.peek(Token![,]) {
                let _: Token![,] = input.parse()?;
            }
        }

        Ok(SpannedValue::from_to_tokens(fk_spec))
    }
}

impl PositionalValue for TableForeignKey {}

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

impl AttributeBase for GeneratedColumnSpec {
    type Partial = Self;
}

impl AttributeValue for GeneratedColumnSpec {
    fn parse_value(input: ParseStream) -> syn::Result<SpannedValue<Self::Partial>> {
        let mode: GeneratedColumnMode = input.parse()?;
        let _eq: Token![=] = input.parse()?;
        let expr: SqlExprStr = input.parse()?;

        Ok(SpannedValue::from_to_tokens(GeneratedColumnSpec { mode, expr }))
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
        <char as From<u8>>::from(byte).try_into()
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

impl AttributeBase for ParamPrefix {
    type Partial = Self;
}

impl AttributeValue for ParamPrefix {
    fn parse_value(
        input: ParseStream<'_>,
    ) -> syn::Result<SpannedValue<Self::Partial>> {
        <ParamPrefix as Parse>::parse(input).map(SpannedValue::from_to_tokens)
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

impl CaseConversion {
    pub fn display<T>(self, value: T) -> CaseConversionDisplay<T> {
        CaseConversionDisplay {
            value,
            conversion: self,
        }
    }
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

impl AttributeBase for CaseConversion {
    type Partial = Self;
}

impl AttributeValue for CaseConversion {
    fn parse_value(input: ParseStream) -> syn::Result<SpannedValue<Self::Partial>> {
        let start = input.span();
        let value: CaseConversion = input.parse()?;
        let end = input.span();

        Ok(SpannedValue {
            value,
            span: start..end,
        })
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

impl AttributeBase for IdentOrStr {
    type Partial = Self;
}

impl AttributeValue for IdentOrStr {
    fn parse_value(input: ParseStream) -> syn::Result<SpannedValue<Self::Partial>> {
        use syn::spanned::Spanned;

        let value: IdentOrStr = input.parse()?;
        let span = value.span();

        Ok(SpannedValue::new(value, span))
    }
}

impl PositionalValue for IdentOrStr {}

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

impl<T> PartialEq<T> for IdentOrStr
where
    T: AsRef<str>,
{
    fn eq(&self, other: &T) -> bool {
        self.to_string() == other.as_ref()
    }
}

impl Hash for IdentOrStr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, Debug)]
pub enum ForeignKeyConflictAction {
    #[default]
    NoAction,
    Restrict,
    SetNull,
    SetDefault,
    Cascade,
}

impl Parse for ForeignKeyConflictAction {
    fn parse(stream: ParseStream<'_>) -> Result<Self, Error> {
        let raw: IdentOrStr = stream.parse()?;

        Ok(match raw.to_string().as_str() {
            "no_action"   => ForeignKeyConflictAction::NoAction,
            "restrict"    => ForeignKeyConflictAction::Restrict,
            "set_null"    => ForeignKeyConflictAction::SetNull,
            "set_default" => ForeignKeyConflictAction::SetDefault,
            "cascade"     => ForeignKeyConflictAction::Cascade,
            _ => return Err(Error::new_spanned(&raw, "invalid foreign key conflict resolution action")),
        })
    }
}

impl ToTokens for ForeignKeyConflictAction {
    fn to_tokens(&self, ts: &mut TokenStream2) {
        let variant_name = match *self {
            ForeignKeyConflictAction::NoAction   => "NoAction",
            ForeignKeyConflictAction::Restrict   => "Restrict",
            ForeignKeyConflictAction::SetNull    => "SetNull",
            ForeignKeyConflictAction::SetDefault => "SetDefault",
            ForeignKeyConflictAction::Cascade    => "Cascade",
        };
        let variant_ident = Ident::new(variant_name, Span::call_site());

        ts.extend(quote!{
            ::nanosql::table::ForeignKeyConflictAction::#variant_ident
        });
    }
}

pub trait FlagExt {
    fn is_set(&self) -> bool;
}

impl FlagExt for bool {
    fn is_set(&self) -> bool {
        *self
    }
}

impl FlagExt for LitBool {
    fn is_set(&self) -> bool {
        self.value
    }
}

impl FlagExt for FlagOrValue<LitBool> {
    fn is_set(&self) -> bool {
        match self {
            FlagOrValue::None => false,
            FlagOrValue::Flag => true,
            FlagOrValue::Value(lit) => lit.value,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Any<T> {
    inner: Vec<T>,
    span: Span,
}

impl<T> Any<T> {
    pub fn into_inner(self) -> Vec<T> {
        self.inner
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl<T> Default for Any<T> {
    fn default() -> Self {
        Any {
            inner: Vec::new(),
            span: Span::call_site(),
        }
    }
}

impl<T> From<Vec<T>> for Any<T> {
    fn from(inner: Vec<T>) -> Self {
        Any {
            inner,
            span: Span::call_site(),
        }
    }
}

impl<T> Deref for Any<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.inner.as_slice()
    }
}

impl<T> IntoIterator for Any<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Any<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<T> AttributeBase for Any<T> {
    type Partial = Vec<T>;
}

impl<T> attribute_derive::from_partial::FromPartial<Vec<T>> for Any<T> {
    fn from(partial: Vec<T>) -> syn::Result<Self> {
        Ok(Any {
            inner: partial,
            span: Span::call_site(),
        })
    }

    fn from_option(partial: Option<Vec<T>>, _error_missing: &str) -> syn::Result<Self> {
        attribute_derive::from_partial::FromPartial::from(
            partial.into_iter().flatten().collect::<Vec<_>>()
        )
    }

    fn join(
        first: Option<SpannedValue<Vec<T>>>,
        second: SpannedValue<Vec<T>>,
        _specified_twice_error: &str,
    ) -> syn::Result<Option<SpannedValue<Vec<T>>>> {
        if let Some(SpannedValue { mut value, span }) = first {
            value.extend(second.value);
            Ok(Some(SpannedValue::new(value, span.start..second.span.end)))
        } else {
            Ok(Some(second))
        }
    }
}

impl<T> AttributeValue for Any<T>
where
    T: AttributeValue,
    Vec<T>: FromPartial<Partial<Vec<T::Partial>>>,
{
    fn parse_value(input: ParseStream) -> syn::Result<SpannedValue<Self::Partial>> {
        let item = <T as AttributeValue>::parse_value(input)?;
        let value = SpannedValue {
            value: vec![FromPartial::from(item.value)?],
            span: item.span,
        };
        Ok(value)
    }
}
