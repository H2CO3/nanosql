//! Utility queries for common tasks: `CREATE TABLE`, `INSERT`, etc.

use core::marker::PhantomData;
use core::fmt::{self, Debug, Formatter, Write};
use crate::{
    query::Query,
    param::Param,
    error::Result,
};


/// Implemented by UDTs that map to an SQL table.
pub trait Table {
    /// The parameter set bound to an `INSERT` statement inserting into this table.
    /// In simple cases, it can be `Self`. For a more efficient implementation (no
    /// allocation) or to support default fields, specify a different input type.
    type Input<'p>: Param;

    /// The value-level description of the table.
    const DESCRIPTION: TableDesc<'static>;
}

impl<T> Table for &T
where
    T: ?Sized + Table
{
    type Input<'p> = T::Input<'p>;

    const DESCRIPTION: TableDesc<'static> = T::DESCRIPTION;
}

impl<T> Table for &mut T
where
    T: ?Sized + Table
{
    type Input<'p> = T::Input<'p>;

    const DESCRIPTION: TableDesc<'static> = T::DESCRIPTION;
}

impl<T> Table for Box<T>
where
    T: ?Sized + Table
{
    type Input<'p> = T::Input<'p>;

    const DESCRIPTION: TableDesc<'static> = T::DESCRIPTION;
}

/// Describes the structure of an SQL table.
#[derive(Clone, Copy, Debug)]
pub struct TableDesc<'a> {
    /// The name of the table itself.
    pub name: &'a str,
    /// The description (name, type, constraints, etc.) of its columns.
    pub columns: &'a [Column<'a>],
}

/// Describes the name, type, and constraints on a particular column within a table.
#[derive(Clone, Copy, Debug)]
pub struct Column<'a> {
    /// The name of the column.
    pub name: &'a str,
    /// The raw SQL type of the column, unless it's untyped.
    pub ty: Option<&'a str>,
    /// `true` when this column is the `PRIMARY KEY`. (For compound PKs, this is `false`.)
    pub is_pk: bool,
    /// `true` when this column must be `UNIQUE`.
    pub is_unique: bool,
}

impl<'a> Column<'a> {
    /// Creates a column with no type or constraints.
    pub const fn new(name: &'a str) -> Self {
        Column {
            name,
            ty: None,
            is_pk: false,
            is_unique: false,
        }
    }

    /// Sets the type of this column.
    pub const fn ty(mut self, ty: &'a str) -> Self {
        self.ty = Some(ty);
        self
    }

    /// Sets this column as the `PRIMARY KEY`.
    pub const fn pk(mut self) -> Self {
        self.is_pk = true;
        self
    }

    /// Enforces uniqueness of this column.
    pub const fn unique(mut self) -> Self {
        self.is_unique = true;
        self
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
        f.debug_tuple("Create").field(&T::DESCRIPTION.name).finish()
    }
}

impl<T: Table> Query for Create<T> {
    type Input<'p> = ();
    type Output = ();

    fn sql(&self) -> Result<impl AsRef<str> + '_> {
        let desc = T::DESCRIPTION;
        let mut sql = format!("CREATE TABLE IF NOT EXISTS \"{}\"(", desc.name);
        let mut sep = "";

        for &Column { name, ty, is_pk, is_unique } in desc.columns {
            let ty = ty.unwrap_or("");
            let pk = if is_pk { " PRIMARY KEY" } else { "" };
            let uniq = if is_unique { " UNIQUE" } else { "" };

            write!(sql, "{sep}\n    \"{name}\" {ty}{pk}{uniq}")?;
            sep = ", ";
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
        f.debug_tuple("Insert").field(&T::DESCRIPTION.name).finish()
    }
}

impl<T: Table> Query for Insert<T> {
    type Input<'p> = T::Input<'p>;
    type Output = ();

    fn sql(&self) -> Result<impl AsRef<str> + '_> {
        let desc = T::DESCRIPTION;
        let mut sql = format!(r#"INSERT INTO "{}"("#, desc.name);
        let mut sep = "";

        for col in desc.columns {
            write!(sql, "{}\n    \"{}\"", sep, col.name)?;
            sep = ", ";
        }

        sql.push_str("\n)\nVALUES(");
        sep = "";

        for col in desc.columns {
            write!(sql, "{}\n    {}{}", sep, Self::Input::PARAM_PREFIX, col.name)?;
            sep = ", ";
        }

        sql.push_str("\n);");

        Ok(sql)
    }
}
