//! Utility queries for common tasks: `CREATE TABLE`, `INSERT`, etc.

use core::marker::PhantomData;
use core::fmt::{self, Display, Debug, Formatter, Write};
use std::collections::BTreeSet;
use crate::{
    query::Query,
    param::{Param, ParamPrefix},
    error::Result,
};


/// Implemented by UDTs that map to an SQL table.
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
        self.constrain(ColumnConstraint::Check {
            condition: condition.into(),
        })
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
}

impl Display for TableConstraint {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TableConstraint::PrimaryKey { columns } => {
                formatter.write_str("PRIMARY KEY(")?;

                let mut sep = "";
                for col in columns {
                    write!(formatter, "{sep}{col}")?;
                    sep = ", ";
                }

                formatter.write_char(')')
            }
        }
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

    fn sql(&self) -> Result<impl AsRef<str> + '_> {
        let desc = T::description();
        let mut sql = format!(r#"INSERT INTO "{}"("#, desc.name);
        let mut sep = "";

        for col in &desc.columns {
            write!(sql, "{}\n    \"{}\"", sep, col.name)?;
            sep = ", ";
        }

        sql.push_str("\n)\nVALUES(");
        sep = "";

        for (idx, col) in (1..).zip(&desc.columns) {
            // decide intelligently whether parameters should be named or numbered
            let param_name: &dyn Display = match Self::Input::PREFIX {
                ParamPrefix::Question => &idx,
                ParamPrefix::Dollar | ParamPrefix::At | ParamPrefix::Colon => &col.name
            };

            write!(sql, "{}\n    {}{}", sep, Self::Input::PREFIX, param_name)?;

            sep = ", ";
        }

        sql.push_str("\n);");

        Ok(sql)
    }
}
