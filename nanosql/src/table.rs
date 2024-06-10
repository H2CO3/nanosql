//! Utility queries for common tasks: `CREATE TABLE`, `INSERT`, etc.

use core::marker::PhantomData;
use core::num::{
    NonZeroI8,
    NonZeroU8,
    NonZeroI16,
    NonZeroU16,
    NonZeroI32,
    NonZeroU32,
    NonZeroI64,
    NonZeroU64,
    NonZeroIsize,
    NonZeroUsize,
};
use core::fmt::{self, Display, Debug, Formatter, Write};
use std::borrow::Cow;
use std::collections::BTreeSet;
use std::rc::Rc;
use std::sync::Arc;
use crate::{
    query::Query,
    param::{Param, ParamPrefix},
    error::Result,
};
#[cfg(feature = "not-nan")]
use ordered_float::NotNan;


/// Implemented by UDTs that map to an SQL table. This is a convenience helper trait
/// for enabling automatic generation of `CREATE TABLE` and `INSERT` statements; it
/// is not strictly necessary for using the table type as a parameter set or a result
/// tuple. That functionality is provided by the `Param` and `ResultRecord` traits.
///
/// This trait can be automatically derived on `struct`s with named fields.
///
/// Supported `struct`-level attributes are:
///
/// * `#[nanosql(rename = "TableName")]` changes the name of the table to the given
///   string, instead of using the name of the `struct` itself.
/// * `#[nanosql(rename_all = "casing")]` renames all fields based on the
///   specified case-transforming convention. Valid casing conventions are:
///
///   * `lower_snake_case`
///   * `UPPER_SNAKE_CASE`
///   * `lowerCamelCase`
///   * `UpperCamelCase` (Pascal case)
///   * `lower-kebab-case` (Lisp case)
///   * `UPPER-KEBAB-CASE`
///   * `Title Case`
///   * `Train-Case`
///
/// Supported field-level attributes are:
///
/// * `#[nanosql(rename = "column_name")]`: changes the name of the column
///   to the specified string, instead of using the name of the field.
/// * `#[nanosql(sql_ty = path::to::AsSqlTy)]`: forwards the `AsSqlTy` impl
///   to the specified type, instead of using the field's own declared type.
/// * `#[nanosql(unique)]`: imposes an SQL `UNIQUE` constraint on the field.
/// * `#[nanosql(check = "expression1", check = "expression2", ...)]`:
///    imposes additional `CHECK` constraints.
pub trait Table {
    /// The parameter set bound to an `INSERT` statement inserting into this table.
    /// In simple cases, it can be `Self`. For a more efficient implementation (no
    /// allocation) or to support default fields, specify a different input type.
    type InsertInput<'p>: Param;

    /// The value-level description of the table.
    fn description() -> TableDesc;
}

impl<T> Table for &T
where
    T: ?Sized + Table
{
    type InsertInput<'p> = T::InsertInput<'p>;

    fn description() -> TableDesc {
        T::description()
    }
}

impl<T> Table for &mut T
where
    T: ?Sized + Table
{
    type InsertInput<'p> = T::InsertInput<'p>;

    fn description() -> TableDesc {
        T::description()
    }
}

impl<T> Table for Box<T>
where
    T: ?Sized + Table
{
    type InsertInput<'p> = T::InsertInput<'p>;

    fn description() -> TableDesc {
        T::description()
    }
}

/// Describes the structure of an SQL table.
#[derive(Clone, Debug)]
pub struct TableDesc {
    /// The name of the table itself.
    pub name: String,
    /// The description (name, type, constraints, etc.) of its columns.
    pub columns: Vec<Column>,
    /// The table-level constraints.
    pub constraints: BTreeSet<TableConstraint>,
}

impl TableDesc {
    /// Creates a table with the given name, no columns, and no constraints.
    pub fn new(name: impl Into<String>) -> Self {
        TableDesc {
            name: name.into(),
            columns: Vec::new(),
            constraints: BTreeSet::new(),
        }
    }

    /// Adds a column to the table description.
    pub fn column(mut self, column: Column) -> Self {
        self.columns.push(column);
        self
    }

    /// Adds a constraint to the table description.
    pub fn constrain(mut self, constraint: TableConstraint) -> Self {
        self.constraints.insert(constraint);
        self
    }

    /// Marks some columns as a primary key.
    pub fn pk<I>(self, columns: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<String>,
    {
        self.constrain(TableConstraint::PrimaryKey {
            columns: columns.into_iter().map(Into::into).collect(),
        })
    }

    /// Adds a multi-column foreign key constraint.
    /// The iterator must return columns in pairs,
    /// where the first item is a column in this table,
    /// and the second item is a column in the foreign table.
    pub fn fk<T, I, K1, K2>(self, table: T, columns: I) -> Self
    where
        T: Into<String>,
        I: IntoIterator<Item = (K1, K2)>,
        K1: Into<String>,
        K2: Into<String>,
    {
        let column_pairs = columns
            .into_iter()
            .map(|(own, foreign)| (own.into(), foreign.into()))
            .collect();

        self.constrain(TableConstraint::ForeignKey {
            table: table.into(),
            column_pairs,
        })
    }

    /// Marks some columns as unique when considered together.
    pub fn unique<I>(self, columns: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<String>,
    {
        self.constrain(TableConstraint::Unique {
            columns: columns.into_iter().map(Into::into).collect(),
        })
    }

    /// Adds an arbitrary, table-level `CHECK` constraint.
    pub fn check(self, condition: impl Into<String>) -> Self {
        self.constrain(TableConstraint::Check {
            condition: condition.into(),
        })
    }
}

/// Describes the name, type, and constraints on a particular column within a table.
#[derive(Clone, Debug)]
pub struct Column {
    /// The name of the column.
    pub name: String,
    /// The raw SQL type of the column; `None` if it's untyped.
    pub ty: Option<SqlTy>,
    /// The set of constraints imposed on the column.
    pub constraints: BTreeSet<ColumnConstraint>,
}

impl Column {
    /// Creates a column with no type or constraints.
    pub fn new(name: impl Into<String>) -> Self {
        Column {
            name: name.into(),
            ty: None,
            constraints: BTreeSet::new(),
        }
    }

    /// Sets the type of this column.
    pub fn ty(mut self, ty: impl Into<SqlTy>) -> Self {
        self.ty = Some(ty.into());
        self
    }

    /// Adds a generic constraint to this column.
    pub fn constrain(mut self, constraint: ColumnConstraint) -> Self {
        self.constraints.insert(constraint);
        self
    }

    /// Sets this column as the `PRIMARY KEY`.
    pub fn pk(self) -> Self {
        self.constrain(ColumnConstraint::PrimaryKey)
    }

    /// Set this column as a `FOREIGN KEY`.
    pub fn fk(self, table: impl Into<String>, column: impl Into<String>) -> Self {
        self.constrain(ColumnConstraint::ForeignKey {
            table: table.into(),
            column: column.into(),
        })
    }

    /// Enforces uniqueness of this column.
    pub fn unique(self) -> Self {
        self.constrain(ColumnConstraint::Unique)
    }

    /// Provides a default value for the column upon insertion.
    pub fn default_value(self, expr: impl Into<String>) -> Self {
        self.constrain(ColumnConstraint::Default {
            expr: expr.into(),
        })
    }

    /// Generates the column value based on an expression on-demand.
    pub fn generate_virtual(self, expr: impl Into<String>) -> Self {
        self.constrain(ColumnConstraint::Generated {
            expr: expr.into(),
            kind: GeneratedColumnKind::Virtual,
        })
    }

    /// Generates the column value based on an expression and stores it.
    pub fn generate_stored(self, expr: impl Into<String>) -> Self {
        self.constrain(ColumnConstraint::Generated {
            expr: expr.into(),
            kind: GeneratedColumnKind::Stored,
        })
    }

    /// Enforces that an arbitrary boolean predicate is true.
    pub fn check(self, condition: impl Into<String>) -> Self {
        let condition = condition.into();

        if condition.is_empty() {
            self
        } else {
            self.constrain(ColumnConstraint::Check { condition })
        }
    }
}

impl Display for Column {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(formatter, "\"{}\"", self.name)?;

        if let Some(ty) = self.ty {
            write!(formatter, " {ty}")?;
        }

        for c in &self.constraints {
            write!(formatter, " {c}")?;
        }

        Ok(())
    }
}

/// An SQLite type, potentially nullable.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct SqlTy {
    /// The underlying primitive, non-nullable type.
    pub prim: TyPrim,
    /// Whether this type is nullable.
    pub is_nullable: bool,
}

impl SqlTy {
    /// Creates a non-nullable type.
    pub const fn new(prim: TyPrim) -> Self {
        SqlTy { prim, is_nullable: false }
    }

    /// Creates a nullable type.
    pub const fn nullable(prim: TyPrim) -> Self {
        SqlTy { prim, is_nullable: true }
    }

    /// Converts this type to its nullable counterpart.
    /// Does nothing if the type is already nullable.
    pub const fn as_nullable(mut self) ->  Self {
        self.is_nullable = true;
        self
    }
}

impl From<TyPrim> for SqlTy {
    fn from(prim: TyPrim) -> Self {
        SqlTy::new(prim)
    }
}

impl Display for SqlTy {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "{} {}",
            self.prim,
            if self.is_nullable { "NULL" } else { "NOT NULL" },
        )
    }
}

/// A primitive (atomic) column type.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum TyPrim {
    /// A 64-bit signed integer type.
    Integer,
    /// A 64-bit IEEE-754 floating-point value.
    Real,
    /// A string encoded as UTF-8 bytes.
    Text,
    /// An arbitrary sequence of bytes.
    Blob,
}

impl Display for TyPrim {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.write_str(match *self {
            TyPrim::Integer => "INTEGER",
            TyPrim::Real    => "REAL",
            TyPrim::Text    => "TEXT",
            TyPrim::Blob    => "BLOB",
        })
    }
}

/// A constraint applied to a single column within a table.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum ColumnConstraint {
    /// Mark this column as the primary key.
    ///
    /// This needs to be the first variant because it must immediately
    /// follow the type in order for SQLite to correctly pick up the
    /// `INTEGER PRIMARY KEY` column (with all of its consequences).
    PrimaryKey,
    /// Mark this column as a foreign key referencing another table.
    ///
    /// TODO(H2CO3): allow the `ON DELETE | UPDATE ...` clause.
    ForeignKey {
        /// The name of the referenced table.
        table: String,
        /// The name of the referenced column within the referenced table.
        column: String,
    },
    /// Enforce that values of this column are unique across the table.
    Unique,
    /// A default literal value or expression.
    Default {
        /// The SQL expression specifying the default.
        expr: String,
    },
    /// An expression yielding the value of a generated column.
    Generated {
        /// The SQL expression specifying the generated value.
        expr: String,
        /// Whether the value is stored or computed on-demand.
        kind: GeneratedColumnKind,
    },
    /// An arbitrary `CHECK` constraint ensuring that an SQL expression
    /// depending on the column is `TRUE`.
    Check {
        /// The SQL expression that must evaluate to true.
        condition: String,
    },
}

impl Display for ColumnConstraint {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ColumnConstraint::PrimaryKey => {
                formatter.write_str("PRIMARY KEY")
            }
            ColumnConstraint::ForeignKey { table, column } => {
                write!(formatter, r#"REFERENCES "{table}"("{column}")"#)
            }
            ColumnConstraint::Unique => {
                formatter.write_str("UNIQUE")
            }
            ColumnConstraint::Default { expr } => {
                write!(formatter, "DEFAULT ({expr})")
            }
            ColumnConstraint::Generated { expr, kind } => {
                write!(formatter, "GENERATED ALWAYS AS ({expr}) {kind}")
            }
            ColumnConstraint::Check { condition } => {
                write!(formatter, "CHECK ({condition})")
            }
        }
    }
}

/// Whether the value of a `GENERATED` column is stored or always computed on-demand.
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum GeneratedColumnKind {
    /// The value is re-computed on-demand.
    #[default]
    Virtual,
    /// The computed value is stored.
    Stored,
}

impl Display for GeneratedColumnKind {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.write_str(match *self {
            GeneratedColumnKind::Virtual => "VIRTUAL",
            GeneratedColumnKind::Stored  => "STORED",
        })
    }
}

/// A top-level constraint applied to an entire table at once.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum TableConstraint {
    /// A multi-column (composite) Primary Key.
    PrimaryKey {
        /// The columns in this table that together constitute its composite primary key.
        columns: Vec<String>,
    },
    /// A list of columns corresponding to some other columns in another table.
    ForeignKey {
        /// The foreign table being referenced.
        table: String,
        /// The corresponding pairs of columns that make up the key in the tables.
        column_pairs: Vec<(String, String)>,
    },
    /// A multi-column uniqueness constraint.
    Unique {
        /// The columns in this table of which tuples must be unique.
        columns: Vec<String>,
    },
    /// An arbitrary `CHECK` constraint ensuring that an SQL expression
    /// depending on some or all columns of the table is `TRUE`.
    Check {
        /// The SQL expression that must evaluate to true.
        condition: String,
    },
}

impl Display for TableConstraint {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TableConstraint::PrimaryKey { columns } => {
                formatter.write_str("PRIMARY KEY(")?;

                let mut sep = "";
                for col in columns {
                    write!(formatter, r#"{sep}"{col}""#)?;
                    sep = ", ";
                }

                formatter.write_char(')')
            }
            TableConstraint::ForeignKey { table, column_pairs } => {
                write!(formatter, "FOREIGN KEY(")?;

                let mut sep = "";
                for (own_col, _) in column_pairs {
                    write!(formatter, r#"{sep}"{own_col}""#)?;
                    sep = ", ";
                }

                write!(formatter, r#") REFERENCES "{table}"("#)?;

                sep = "";
                for (_, foreign_col) in column_pairs {
                    write!(formatter, r#"{sep}"{foreign_col}""#)?;
                    sep = ", ";
                }

                formatter.write_char(')')
            }
            TableConstraint::Unique { columns } => {
                formatter.write_str("UNIQUE(")?;

                let mut sep = "";
                for col in columns {
                    write!(formatter, r#"{sep}"{col}""#)?;
                    sep = ", ";
                }

                formatter.write_char(')')
            }
            TableConstraint::Check { condition } => {
                write!(formatter, "CHECK ({condition})")
            }
        }
    }
}

/// A trait that associates Rust types with their SQL counterpart.
///
/// This must be implemented by atomic/primitive column (field)
/// types of a `struct`, for deriving `Table` automatically. Usually,
/// to be useful with `#[derive]`, a type that impls `AsSqlTy` must
/// also impl `ToSql` and `FromSql`.
///
/// This trait can be automatically derived on `enum`s with only unit
/// variants, and on newtype structs, along with `ToSql` and `FromSql`.
///
/// When derived on an `enum`, the `rename_all` type-level attribute
/// and the `rename` variant-level attribute work in the same way as
/// the equivalent attributes on [`Table`] (see its documentation).
pub trait AsSqlTy {
    /// The SQL type corresponding to this type.
    const SQL_TY: SqlTy;

    /// If the domain of this type requires a CHECK constraint,
    /// this method should write out the relevant criteria. The
    /// column name will be given as the `column` argument. If
    /// the body of this function doesn't write anything to the
    /// formatter, no `CHECK` constraint is going to be emitted.
    ///
    /// The default implementation does nothing.
    fn format_check_constraint(column: &dyn Display, formatter: &mut Formatter<'_>) -> fmt::Result {
        let _ = column;
        let _ = formatter;

        Ok(())
    }
}

macro_rules! impl_as_sql_ty_for_primitive {
    ($($rust_ty:ty => $sql_ty:expr,)*) => {$(
        impl AsSqlTy for $rust_ty {
            const SQL_TY: SqlTy = $sql_ty;
        }
    )*}
}

impl_as_sql_ty_for_primitive!{
    i8      => SqlTy::new(TyPrim::Integer),
    i16     => SqlTy::new(TyPrim::Integer),
    i32     => SqlTy::new(TyPrim::Integer),
    i64     => SqlTy::new(TyPrim::Integer),
    isize   => SqlTy::new(TyPrim::Integer),
    u8      => SqlTy::new(TyPrim::Integer),
    u16     => SqlTy::new(TyPrim::Integer),
    u32     => SqlTy::new(TyPrim::Integer),
    u64     => SqlTy::new(TyPrim::Integer),
    usize   => SqlTy::new(TyPrim::Integer),
    f32     => SqlTy::nullable(TyPrim::Real),
    f64     => SqlTy::nullable(TyPrim::Real),
    str     => SqlTy::new(TyPrim::Text),
    String  => SqlTy::new(TyPrim::Text),
    [u8]    => SqlTy::new(TyPrim::Blob),
    Vec<u8> => SqlTy::new(TyPrim::Blob),
}

macro_rules! impl_as_sql_ty_for_non_zero {
    ($($ty:ty,)*) => {$(
        impl AsSqlTy for $ty {
            const SQL_TY: SqlTy = SqlTy::new(TyPrim::Integer);

            fn format_check_constraint(
                column: &dyn Display,
                formatter: &mut Formatter<'_>,
            ) -> fmt::Result {
                write!(formatter, "{column} != 0")
            }
        }
    )*}
}

impl_as_sql_ty_for_non_zero!{
    NonZeroI8,
    NonZeroU8,
    NonZeroI16,
    NonZeroU16,
    NonZeroI32,
    NonZeroU32,
    NonZeroI64,
    NonZeroU64,
    NonZeroIsize,
    NonZeroUsize,
}

impl AsSqlTy for bool {
    const SQL_TY: SqlTy = SqlTy::new(TyPrim::Integer);

    fn format_check_constraint(column: &dyn Display, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(formatter, "{column} IN (0, 1)")
    }
}

#[cfg(feature = "not-nan")]
impl AsSqlTy for NotNan<f32> {
    const SQL_TY: SqlTy = SqlTy::new(TyPrim::Real);
}

#[cfg(feature = "not-nan")]
impl AsSqlTy for NotNan<f64> {
    const SQL_TY: SqlTy = SqlTy::new(TyPrim::Real);
}

impl<const N: usize> AsSqlTy for [u8; N] {
    const SQL_TY: SqlTy = SqlTy::new(TyPrim::Blob);
}

impl<T: AsSqlTy> AsSqlTy for Option<T> {
    const SQL_TY: SqlTy = T::SQL_TY.as_nullable();

    fn format_check_constraint(
        column: &dyn Display,
        formatter: &mut Formatter<'_>,
    ) -> fmt::Result {
        // no need to add a special case for NULL as it's ignored by CHECK constraints
        T::format_check_constraint(column, formatter)
    }
}

impl<T: ?Sized + AsSqlTy> AsSqlTy for &T {
    const SQL_TY: SqlTy = T::SQL_TY;

    fn format_check_constraint(
        column: &dyn Display,
        formatter: &mut Formatter<'_>,
    ) -> fmt::Result {
        T::format_check_constraint(column, formatter)
    }
}

impl<T: ?Sized + AsSqlTy> AsSqlTy for &mut T {
    const SQL_TY: SqlTy = T::SQL_TY;

    fn format_check_constraint(
        column: &dyn Display,
        formatter: &mut Formatter<'_>,
    ) -> fmt::Result {
        T::format_check_constraint(column, formatter)
    }
}

impl<T: ?Sized + AsSqlTy> AsSqlTy for Box<T> {
    const SQL_TY: SqlTy = T::SQL_TY;

    fn format_check_constraint(
        column: &dyn Display,
        formatter: &mut Formatter<'_>,
    ) -> fmt::Result {
        T::format_check_constraint(column, formatter)
    }
}

impl<T: ?Sized + AsSqlTy> AsSqlTy for Rc<T> {
    const SQL_TY: SqlTy = T::SQL_TY;

    fn format_check_constraint(
        column: &dyn Display,
        formatter: &mut Formatter<'_>,
    ) -> fmt::Result {
        T::format_check_constraint(column, formatter)
    }
}

impl<T: ?Sized + AsSqlTy> AsSqlTy for Arc<T> {
    const SQL_TY: SqlTy = T::SQL_TY;

    fn format_check_constraint(
        column: &dyn Display,
        formatter: &mut Formatter<'_>,
    ) -> fmt::Result {
        T::format_check_constraint(column, formatter)
    }
}

impl<T> AsSqlTy for Cow<'_, T>
where
    T: ?Sized + ToOwned + AsSqlTy
{
    const SQL_TY: SqlTy = T::SQL_TY;

    fn format_check_constraint(
        column: &dyn Display,
        formatter: &mut Formatter<'_>,
    ) -> fmt::Result {
        T::format_check_constraint(column, formatter)
    }
}

#[doc(hidden)]
#[derive(Clone, Copy, Debug)]
pub struct ColumnConstraintFormatter<'a, T: ?Sized> {
    name: &'a str,
    ty: PhantomData<fn() -> &'a T>,
}

impl<'a, T: ?Sized> ColumnConstraintFormatter<'a, T> {
    #[doc(hidden)]
    pub fn new(name: &'a str) -> Self {
        ColumnConstraintFormatter {
            name,
            ty: PhantomData,
        }
    }
}

impl<T: ?Sized + AsSqlTy> Display for ColumnConstraintFormatter<'_, T> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        T::format_check_constraint(&format_args!(r#""{}""#, self.name), formatter)
    }
}

/// A `CREATE TABLE IF NOT EXISTS` statement, for ensuring that this table exists.
pub struct Create<T>(PhantomData<fn() -> T>);

impl<T> Clone for Create<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Create<T> {}

impl<T> Default for Create<T> {
    fn default() -> Self {
        Create(PhantomData)
    }
}

impl<T: Table> Debug for Create<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Create").field(&T::description().name).finish()
    }
}

impl<T: Table> Query for Create<T> {
    type Input<'p> = ();
    type Output = ();

    fn sql(&self) -> Result<impl AsRef<str> + '_> {
        let desc = T::description();
        let mut sql = format!("CREATE TABLE IF NOT EXISTS \"{}\"(", desc.name);
        let mut sep = "";

        for column in &desc.columns {
            write!(sql, "{sep}\n    {column}")?;
            sep = ", ";
        }

        for constraint in &desc.constraints {
            write!(sql, "{sep}\n    {constraint}")?;
        }

        sql.push_str("\n);");

        Ok(sql)
    }
}

/// An `INSERT` statement for adding rows to a table.
pub struct Insert<T>(PhantomData<fn() -> T>);

impl<T> Clone for Insert<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Insert<T> {}

impl<T> Default for Insert<T> {
    fn default() -> Self {
        Insert(PhantomData)
    }
}

impl<T: Table> Debug for Insert<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Insert").field(&T::description().name).finish()
    }
}

impl<T: Table> Query for Insert<T> {
    type Input<'p> = T::InsertInput<'p>;
    type Output = ();

    /// TODO(H2CO3): respect optional/defaulted columns
    fn sql(&self) -> Result<impl AsRef<str> + '_> {
        let desc = T::description();
        let mut sql = format!(r#"INSERT INTO "{}"("#, desc.name);
        let mut sep = "";

        for col in &desc.columns {
            write!(sql, "{sep}\n    \"{col}\"", col = col.name)?;
            sep = ", ";
        }

        sql.push_str("\n)\nVALUES(");
        sep = "";

        for (idx, col) in (1_usize..).zip(&desc.columns) {
            // decide intelligently whether parameters should be named or numbered
            let param_name: &dyn Display = match Self::Input::PREFIX {
                ParamPrefix::Question => &idx,
                ParamPrefix::Dollar | ParamPrefix::At | ParamPrefix::Colon => &col.name
            };

            write!(sql, "{sep}\n    {pfx}{param_name}", pfx = Self::Input::PREFIX)?;

            sep = ", ";
        }

        sql.push_str("\n);");

        Ok(sql)
    }
}
