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
/// * `#[nanosql(insert_input_ty = my::awesome::InsertType)]` changes the insert
///   parameter type of the table (i.e., the [`Table::InsertInput`] associated type)
///   from `Self` (the default) to whatever you specify.
/// * `#[nanosql(insert_input_lt = 'foo)]` changes the default `'p` lifetime parameter
///   of the insert input type to the specified lifetime.
/// * `#[nanosql(rename = "TableName")]` changes the name of the table to the given
///   string, instead of using the name of the `struct` itself. The name may be either
///   a plain identifier (Rust keywords included), or a string literal.
/// * `#[nanosql(rename_all = "casing")]` renames all fields based on the
///   specified case-transforming convention. Valid `casing` conventions are:
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
/// * `#[nanosql(primary_key = ["column_1", "column_N"])]` or
///   `#[nanosql(pk = [column_1, column_N])]`: specifies that these columns form the
///   (compound) `PRIMARY KEY` of this table. You may not specify a field-level primary
///   key if you use this attribute. This attribute respects the `rename` and `rename_all`
///   attributes. The column names may be specified either as bare Rust identifiers or
///   as string literals (if needed). The tuple of columns may **not** be empty.
/// * `#[nanosql(foreign_key("TableName" => (my_col_1 = other_col_1, my_col_N = other_col_n)))]` or
///   `#[nanosql(fk(TableName => ("my_col_1" => "other_col_1", "my_col_N" => "other_col_N")))]`:
///   specifies a compound `FOREIGN KEY` on the table. The specified tuple of columns must
///   **not** be empty. You can repeat this attribute with different combinations of columns
///   as many times as you want. The table and column names may be identifiers or strings.
/// * `#[nanosql(unique = [column_1, column_N])]` or `#[nanosql(unique = ["column_1", "column_N"])]`:
///   adds a potentially multi-column `UNIQUE` constraint (and the corresponding implicit index)
///   to the table with the specified set of keys. The specified list of columns may **not** be
///   empty. You can apply this attribute many times.
/// * `#[nanosql(check = "SQL expression")]`: adds a table-level `CHECK` constraint,
///   which has access to all columns of the table. You can apply this attribute many times.
///   The derive macro will ensure that the expression you specify is syntactically valid.
/// * `#[nanosql(index(unique, columns(foo, "bar" = desc, qux = asc), where = "expression")]`:
///   adds an explicit index to the specified tuple, with each column being sorted according to
///   the given direction. If the sorting direction is not specified, it defaults to `asc`ending.
///
///   If the `where` clause is specified, a partial index will be created using the predicate.
///   (The derive macro will ensure that the predicate is syntactically valid.)
///
///   If `unique` is specified, then uniqueness of tuples _in the index_ will be enforced.
///   For partial indexes, this is different from the set of columns having all unique tuples.
///   You may apply this attribute many times to create multiple indexes.
///
/// Supported field-level attributes are:
///
/// * `#[nanosql(rename = "column_name")]`: changes the name of the column
///   to the specified string, instead of using the name of the field. The
///   name may be specified with either an identifier or a string literal.
/// * `#[nanosql(sql_ty = path::to::AsSqlTy)]`: forwards the `AsSqlTy` impl
///   to the specified type, instead of using the field's own declared type.
/// * `#[nanosql(unique)]`: imposes an SQL `UNIQUE` constraint on the field.
/// * `#[nanosql(check = "expression1", check = "expression2", ...)]`:
///    imposes additional `CHECK` constraints. The derive macro will ensure
///    that the expression you specify is syntactically valid.
/// * `#[nanosql(default = "expression")]`: apply a default value (literal
///   or full-blown SQL expression) upon an `INSERT` statements, when the
///   value for the column is omitted. The derive macro will ensure that
///   the expression you specify is syntactically valid.
/// * `#[nanosql(generated(virtual = "expression"))]` or
///   `#[nanosql(generated(stored = "expression"))]`: declares the column
///   as `GENERATED ALWAYS [VIRTUAL | STORED]`. The derive macro will ensure
///   that the expression you specify is syntactically valid.
/// * `#[nanosql(primary_key)]` or `#[nanosql(pk)]`: defines the column as
///   the `PRIMARY KEY` of the table. This may only be used on a single column
///   within any given table. This attribute may not be used together with the
///   table-level `primary_key` attribute!
/// * `#[nanosql(foreign_key = OtherTable::some_column)]` or
///   `#[nanosql(fk("OtherTable" => "some_column"))]`: defines a foreign key
///   relationship between this column and another column of a different table.
///   (The table on the other side may also be this table, for representing a
///   hierarchy.) You can specify multiple foreign key columns.
/// * `#[nanosql(index(unique, desc, where = "predicate"))]`: adds an explicit
///   index on this column with the specified ordering direction. If direction
///   is omitted, it defaults to `asc`ending.
///
///   If a `where` clause is included, a partial index will be created with
///   the corresponding predicate (bool) expression.
///   The derive macro will ensure that the predicate is syntactically valid.
///
///   If `unique` is given, the values _in the index_ have to be all distinct.
///   (NOTE: for a partial index, this is _not_ the same as the column having
///   unique values!)
pub trait Table {
    /// The parameter set used for performing `INSERT` queries.
    /// This is often just `Self`, but it may be a differen type,
    /// e.g. when the table contains optional columns (of a nullable
    /// type or with a `DEFAULT` value), and/or generated columns.
    type InsertInput<'p>: InsertInput<'p, Table = Self>;

    /// The value-level description of the table.
    fn description() -> TableDesc;
}

/// A blanket impl for the happy place of "everything is covariant".
///
/// Might be useful for someone.
impl<'p, T: Table> Table for &'p T
where
    T: Table<InsertInput<'p> = T>,
    T::InsertInput<'p>: 'p,
    T::InsertInput<'p>: InsertInput<'p, Table = Self>,
{
    type InsertInput<'q> = &'p T::InsertInput<'p>;

    fn description() -> TableDesc {
        <T as Table>::description()
    }
}

/// A trait for denoting types used as parameters for `INSERT`ing
/// into a given table. This is only a type-level marker, of which
/// the sole purpose is to link the table being inserted into via
/// the `Table` associated type. This is purely for convenience:
/// it allows us to avoid type annotations on the insertion method
/// of [`ConnectionExt`](crate::conn::ConnectionExt), namely:
/// [`insert_batch`](crate::conn::ConnectionExt::insert_batch).
///
/// A convenience blanket impl is provided for types that implement both
/// `Table` and `Param`, so that no additional `#[derive(InsertInput)]` is
/// needed in the simple case when the table is its own primary insert input.
///
/// When automatically derived, the following container-level attributes apply:
///
/// * `#[nanosql(table = type)]`: allows specifying the `Table` associated type.
///   This attribute is **obligatory.** (It _could_ technically default to `Self`
///   for types that also implement `Table`, but that would be useless, due to
///   the blanket impl preventing another, conflicting impl on the same type.)
/// * `#[nanosql(insert_input_lt = 'p)]`: allows specifying the type parameter
///   of the trait in the impl. It defaults to `'p` (for parameters). This is
///   **not** added to the list of generic arguments, so it should be an already
///   existing generic lifetime parameter of the input type itself.
pub trait InsertInput<'p>: Param {
    /// The table that uses this parameter set as its primary insertion input.
    type Table: Table<InsertInput<'p> = Self>;
}

impl<'p, T> InsertInput<'p> for T
where
    T: Param + Table<InsertInput<'p> = Self>
{
    type Table = Self;
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
    /// The table-level indexes.
    pub indexes: Vec<TableIndexSpec>,
}

impl TableDesc {
    /// Creates a table with the given name, no columns, and no constraints.
    pub fn new(name: impl Into<String>) -> Self {
        TableDesc {
            name: name.into(),
            columns: Vec::new(),
            constraints: BTreeSet::new(),
            indexes: Vec::new(),
        }
    }

    /// Returns columns necessary for inserting into this table.
    /// For example, this skips `GENERATED` columns.
    fn columns_for_insert(&self) -> impl Iterator<Item = &Column> {
        self.columns.iter().filter(|column| !column.is_generated())
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
    pub fn primary_key<I>(self, columns: I) -> Self
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
    pub fn foreign_key<T, I, K1, K2>(self, table: T, columns: I) -> Self
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

    /// Adds a table-level index, potentially on multiple columns.
    pub fn add_index<S1, S2, I>(mut self, unique: bool, columns: I, predicate: Option<S2>) -> Self
    where
        S1: Into<String>,
        S2: Into<String>,
        I: IntoIterator<Item = (S1, SortOrder)>,
    {
        let columns: Vec<_> = columns
            .into_iter()
            .map(|(col, order)| (col.into(), order))
            .collect();

        let index = TableIndexSpec {
            table: self.name.clone(),
            id: self.indexes.len() + 1,
            unique,
            columns,
            predicate: predicate.map(Into::into),
        };

        self.indexes.push(index);
        self
    }

    /// Returns the index descriptions associated with this table.
    ///
    /// This includes explicit indexes added manually, and implicit
    /// indexes (e.g., those created for `FOREIGN KEY` clauses).
    pub fn index_specs(&self) -> Vec<TableIndexSpec> {
        // start with table-level explicit indexes
        let mut indexes = self.indexes.clone();

        // then, append table-level implicit indexes (resulting from FKs etc.)
        for constraint in &self.constraints {
            let TableConstraint::ForeignKey { column_pairs, .. } = constraint else {
                continue;
            };
            let id = indexes.len() + 1;
            let columns = column_pairs
                .iter()
                .map(|(own_name, _)| (own_name.clone(), SortOrder::Ascending))
                .collect();

            indexes.push(TableIndexSpec {
                table: self.name.clone(),
                id,
                unique: false,
                columns,
                predicate: None,
            });
        }

        // then, append column-level explicit and implicit indexes as well
        for column in &self.columns {
            let Some(col_spec) = column.index_spec() else { continue };
            let id = indexes.len() + 1;
            let spec = TableIndexSpec::single_column(self.name.clone(), id, col_spec);

            indexes.push(spec);
        }

        indexes
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
    /// The index on this column, if any.
    pub index: Option<ColumnIndexSpec>,
}

impl Column {
    /// Creates a column with no type or constraints.
    pub fn new(name: impl Into<String>) -> Self {
        Column {
            name: name.into(),
            ty: None,
            constraints: BTreeSet::new(),
            index: None,
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
    pub fn primary_key(self) -> Self {
        self.constrain(ColumnConstraint::PrimaryKey)
    }

    /// Set this column as a `FOREIGN KEY`.
    pub fn foreign_key(self, table: impl Into<String>, column: impl Into<String>) -> Self {
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

    /// Adds an explicit index for this column.
    pub fn set_index(
        mut self,
        unique: bool,
        sort_order: SortOrder,
        predicate: Option<impl Into<String>>,
    ) -> Self {
        self.index = Some(ColumnIndexSpec {
            name: self.name.clone(),
            unique,
            sort_order,
            predicate: predicate.map(Into::into),
        });
        self
    }

    /// Returns `true` if and only if this column is generated.
    pub fn is_generated(&self) -> bool {
        self.constraints.iter().any(|constraint| {
            matches!(constraint, ColumnConstraint::Generated { .. })
        })
    }

    /// Returns the column name and the `SortOrder` associated with an index on this column,
    /// if any. This may be either an explicit or an implicit index (e.g., a FOREIGN KEY).
    pub fn index_spec(&self) -> Option<ColumnIndexSpec> {
        if let Some(index) = self.index.as_ref() {
            return Some(index.clone());
        }

        self.constraints
            .iter()
            .find_map(|c| {
                let ColumnConstraint::ForeignKey { .. } = c else {
                    return None;
                };

                Some(ColumnIndexSpec {
                    name: self.name.clone(),
                    unique: false,
                    sort_order: SortOrder::Ascending,
                    predicate: None,
                })
            })
    }
}

impl Display for Column {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(formatter, r#""{}""#, self.name)?;

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
                write!(formatter, r#"REFERENCES "{table}"("{column}") DEFERRABLE INITIALLY DEFERRED"#)
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

/// Specifies whether sorting (in an index or in an `ORDER BY`
/// clause) happens in ascending or descending order.
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum SortOrder {
    /// Sort values in increasing numeric or lexicographical order.
    #[default]
    Ascending,
    /// Sort values in decreasing numeric or lexicographical order.
    Descending,
}

impl Display for SortOrder {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.write_str(match *self {
            SortOrder::Ascending => "ASC",
            SortOrder::Descending => "DESC",
        })
    }
}

/// The properties of an index, corresponding to a single column.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ColumnIndexSpec {
    /// The name of the indexed column.
    pub name: String,
    /// Whether items in this index must be unique.
    pub unique: bool,
    /// The order in which the values are sorted in the index.
    pub sort_order: SortOrder,
    /// The predicate expression (`WHERE` clause) for a partial index.
    pub predicate: Option<String>,
}

/// The properties of a potentially multi-column index on a table.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct TableIndexSpec {
    /// The name of the table that this index is indexing.
    pub table: String,
    /// The id of the index. Must be unique within the table that the index belongs to.
    pub id: usize,
    /// Whether items in this index must be unique.
    pub unique: bool,
    /// The columns included in this index, from leftmost to rightmost.
    pub columns: Vec<(String, SortOrder)>,
    /// The predicate expression (`WHERE` clause) for a partial index.
    pub predicate: Option<String>,
}

impl TableIndexSpec {
    /// Turns a single-column index spec into a table-level index spec.
    pub fn single_column(table: String, id: usize, column: ColumnIndexSpec) -> Self {
        let ColumnIndexSpec { name: col_name, unique, sort_order, predicate } = column;

        TableIndexSpec {
            table,
            id,
            unique,
            columns: vec![(col_name, sort_order)],
            predicate,
        }
    }
}

impl Query for TableIndexSpec {
    type Input<'p> = ();
    type Output = ();

    fn format_sql(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        let &TableIndexSpec { ref table, id, unique, ref columns, ref predicate } = self;
        let create_stmt = if unique { "CREATE UNIQUE INDEX" } else { "CREATE INDEX" };
        let mut sep = "";

        write!(
            formatter,
            r#"{create_stmt} IF NOT EXISTS "__nanosql_index_{table}_{id}" ON "{table}"("#,
        )?;

        for (col_name, sort_order) in columns {
            write!(formatter, "{sep}\n    \"{col_name}\" {sort_order}")?;
            sep = ",";
        }

        formatter.write_str("\n)")?;

        if let Some(predicate) = predicate.as_ref() {
            write!(formatter, " WHERE (\n    {predicate}\n)")?;
        }

        formatter.write_char(';')
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

                formatter.write_str(") DEFERRABLE INITIALLY DEFERRED")
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
pub struct CreateTable<T>(PhantomData<fn() -> T>);

impl<T> Clone for CreateTable<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for CreateTable<T> {}

impl<T> Default for CreateTable<T> {
    fn default() -> Self {
        CreateTable(PhantomData)
    }
}

impl<T: Table> Debug for CreateTable<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("CreateTable").field(&T::description().name).finish()
    }
}

impl<T: Table> Query for CreateTable<T> {
    type Input<'p> = ();
    type Output = ();

    fn format_sql(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        let desc = T::description();
        let mut sep = "";

        write!(formatter, r#"CREATE TABLE IF NOT EXISTS "{}"("#, desc.name)?;

        for column in &desc.columns {
            write!(formatter, "{sep}\n    {column}")?;
            sep = ", ";
        }

        for constraint in &desc.constraints {
            write!(formatter, "{sep}\n    {constraint}")?;
        }

        formatter.write_str("\n);")
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
    fn format_sql(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        let desc = T::description();
        let mut sep = "";

        write!(formatter, r#"INSERT INTO "{}"("#, desc.name)?;

        for col in desc.columns_for_insert() {
            write!(formatter, "{sep}\n    \"{col}\"", col = col.name)?;
            sep = ", ";
        }

        formatter.write_str("\n)\nVALUES(")?;
        sep = "";

        for (idx, col) in (1_usize..).zip(desc.columns_for_insert()) {
            // decide intelligently whether parameters should be named or numbered
            let param_name: &dyn Display = match Self::Input::PREFIX {
                ParamPrefix::Question => &idx,
                ParamPrefix::Dollar | ParamPrefix::At | ParamPrefix::Colon => &col.name
            };

            write!(formatter, "{sep}\n    {pfx}{param_name}", pfx = Self::Input::PREFIX)?;

            sep = ", ";
        }

        formatter.write_str("\n);")
    }
}
