//! Meta-queries for high-level query plan and low-level bytecode program debugging

use rusqlite::{Row, types::Value};
use crate::query::Query;
use crate::error::{Error, Result};
use crate::row::ResultRecord;


/// A helper for explaining a query at the virtual machine instruction level.
///
/// ```
/// # use nanosql::{define_query, Connection, ConnectionExt, Query, Table, Param, ResultRecord};
/// # use nanosql::explain::{ExplainVdbeProgram, VdbeInstruction};
/// #[derive(Clone, Debug, Table, Param, ResultRecord)]
/// #[nanosql(rename = test_table)]
/// struct TestTable {
///     #[nanosql(pk)]
///     id: u32,
///     #[nanosql(unique)]
///     name: String,
/// }
///
/// define_query!{
///     TestQuery<'p>: (u32, &'p str) => Option<TestTable> {
///         "SELECT id AS id, name AS name FROM test_table WHERE (id, name) = (?, ?)"
///     }
/// }
///
/// # fn main() -> nanosql::Result<()> {
/// let conn = Connection::open_in_memory()?;
///
/// conn.create_table::<TestTable>()?;
///
/// let mut stmt = conn.compile(ExplainVdbeProgram { query: TestQuery })?;
/// let explanation: Vec<VdbeInstruction> = stmt.invoke(())?;
///
/// assert!(
///     !explanation.is_empty(),
///     "no VDBE instructions returned from `EXPLAIN`"
/// );
///
/// // the `ConnectionExt` trait provides a shorthand for the above.
/// let explanation_using_shorthand = conn.explain_vdbe_program(TestQuery)?;
/// assert_eq!(explanation, explanation_using_shorthand);
/// # Ok(())
/// # }
/// ```
#[derive(Clone, Copy, Default, Debug)]
pub struct ExplainVdbeProgram<Q> {
    /// The query to be explained.
    pub query: Q,
}

impl<Q> ExplainVdbeProgram<Q> {
    /// Gives back ownership of the wrapped query.
    pub fn into_inner(self) -> Q {
        self.query
    }
}

impl<Q> From<Q> for ExplainVdbeProgram<Q> {
    fn from(query: Q) -> Self {
        ExplainVdbeProgram { query }
    }
}

impl<Q: Query> Query for ExplainVdbeProgram<Q> {
    /// The EXPLAIN statement takes no input,
    /// regardless of the input type of the underlying query.
    type Input<'p> = ();

    /// The EXPLAIN statement produces the same output,
    /// regardless of the input type of the underlying query.
    type Output = Vec<VdbeInstruction>;

    fn sql(&self) -> Result<impl AsRef<str>> {
        self.query.sql().map(|sql| format!("EXPLAIN {}", sql.as_ref()))
    }
}

/// Represents a single instruction of the Virtual Database Engine.
///
/// The fields of this structure are based on the official SQLite
/// [VDBE documentation](https://sqlite.org/opcode.html#registers).
#[derive(Clone, PartialEq, Debug)]
pub struct VdbeInstruction {
    /// The address of this instruction.
    pub addr: u64,
    /// Human-readable representation of an opcode.
    /// This is not an `enum` because opcode names change over time.
    pub opcode: Box<str>,
    /// The first operand.
    pub p1: Option<i32>,
    /// The second operand.
    pub p2: Option<i32>,
    /// The third operand.
    pub p3: Option<i32>,
    /// The fourth operand.
    pub p4: Value,
    /// The fifth operand.
    pub p5: Option<u16>,
    /// Arbitrary further explanation.
    pub comment: Option<Box<str>>,
}

impl ResultRecord for VdbeInstruction {
    fn from_row(row: &Row<'_>) -> Result<Self> {
        let expected = 8;
        let actual = row.as_ref().column_count();

        if actual == expected {
            Ok(VdbeInstruction {
                addr: row.get("addr")?,
                opcode: row.get("opcode")?,
                p1: row.get("p1")?,
                p2: row.get("p2")?,
                p3: row.get("p3")?,
                p4: row.get("p4")?,
                p5: row.get("p5")?,
                comment: row.get("comment")?,
            })
        } else {
            Err(Error::ColumnCountMismatch { expected, actual })
        }
    }
}

/// A helper for explaining a query at the higher, query plan level.
///
/// ```
/// # use nanosql::{define_query, Connection, ConnectionExt, Query, Table, Param, ResultRecord};
/// # use nanosql::explain::{ExplainQueryPlan, QueryPlanNode};
/// #[derive(Clone, Debug, Table, Param, ResultRecord)]
/// #[nanosql(rename = test_table)]
/// struct TestTable {
///     #[nanosql(pk)]
///     id: u32,
///     #[nanosql(unique)]
///     name: String,
/// }
///
/// define_query!{
///     TestQuery<'p>: (u32, &'p str) => Option<TestTable> {
///         "SELECT id AS id, name AS name FROM test_table WHERE (id, name) = (?, ?)"
///     }
/// }
///
/// # fn main() -> nanosql::Result<()> {
/// let conn = Connection::open_in_memory()?;
///
/// conn.create_table::<TestTable>()?;
///
/// let mut stmt = conn.compile(ExplainQueryPlan { query: TestQuery })?;
/// let explanation: Vec<QueryPlanNode> = stmt.invoke(())?;
///
/// assert!(
///     !explanation.is_empty(),
///     "no query plan nodes returned from `EXPLAIN QUERY PLAN`"
/// );
///
/// // the `ConnectionExt` trait provides a shorthand for the above.
/// let explanation_using_shorthand = conn.explain_query_plan(TestQuery)?;
/// assert_eq!(explanation, explanation_using_shorthand);
/// # Ok(())
/// # }
/// ```
#[derive(Clone, Copy, Default, Debug)]
pub struct ExplainQueryPlan<Q> {
    /// The query to be explained.
    pub query: Q,
}

impl<Q> ExplainQueryPlan<Q> {
    /// Gives back ownership of the wrapped query.
    pub fn into_inner(self) -> Q {
        self.query
    }
}

impl<Q> From<Q> for ExplainQueryPlan<Q> {
    fn from(query: Q) -> Self {
        ExplainQueryPlan { query }
    }
}

impl<Q: Query> Query for ExplainQueryPlan<Q> {
    /// The EXPLAIN QUERY PLAN statement takes no input,
    /// regardless of the input type of the underlying query.
    type Input<'p> = ();

    /// The EXPLAIN QUERY PLAN statement produces the same output,
    /// regardless of the input type of the underlying query.
    type Output = Vec<QueryPlanNode>;

    fn sql(&self) -> Result<impl AsRef<str>> {
        self.query.sql().map(|sql| format!("EXPLAIN QUERY PLAN {}", sql.as_ref()))
    }
}

/// One node in the (flat) output of the query plan.
/// Parent IDs link nodes together.
#[derive(Clone, Default, PartialEq, Eq, Hash, Debug)]
pub struct QueryPlanNode {
    /// The unique node ID.
    pub id: i64,
    /// The node ID of the parent of this node.
    pub parent: Option<i64>,
    /// Auxiliary value, currently unused.
    pub aux: Option<i64>,
    /// Details as a human-readable string.
    pub detail: Option<Box<str>>,
}

impl ResultRecord for QueryPlanNode {
    fn from_row(row: &Row<'_>) -> Result<Self> {
        let expected = 4;
        let actual = row.as_ref().column_count();

        if actual == expected {
            // extract columns by index, because their name, unlike
            // EXPLAIN, seems to change across SQLite versions.
            Ok(QueryPlanNode {
                id: row.get(0)?,
                parent: row.get(1)?,
                aux: row.get(2)?,
                detail: row.get(3)?,
            })
        } else {
            Err(Error::ColumnCountMismatch { expected, actual })
        }
    }
}
