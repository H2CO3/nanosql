//! Meta-queries for high-level query plan and low-level bytecode program debugging.
//!
//! See the [SQLite docs](https://www.sqlite.org/lang_explain.html) for details.

use std::fmt::{self, Formatter};
use std::collections::{BTreeMap, HashSet};
use thiserror::Error;
use rusqlite::{Row, Rows, types::Value};
use crate::query::Query;
use crate::error::{Error, Result};
use crate::row::{ResultRecord, ResultSet};

#[cfg(feature = "pretty-eqp")]
use {
    std::io,
    std::borrow::Cow,
    ptree::{TreeItem, Style}
};

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
/// let mut conn = Connection::connect_in_memory()?;
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

    fn format_sql(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.write_str("EXPLAIN ")?;
        self.query.format_sql(formatter)
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
/// See the [SQLite docs](https://www.sqlite.org/eqp.html) for details.
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
/// let mut conn = Connection::connect_in_memory()?;
///
/// conn.create_table::<TestTable>()?;
///
/// let mut stmt = conn.compile(ExplainQueryPlan { query: TestQuery })?;
/// let explanation = stmt.invoke(())?;
///
/// assert!(
///     !explanation.root.children.is_empty(),
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
    type Output = QueryPlan;

    fn format_sql(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.write_str("EXPLAIN QUERY PLAN ")?;
        self.query.format_sql(formatter)
    }
}

/// One node in the (flat) output of the query plan.
/// Parent IDs link nodes together.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct QueryPlanNode {
    /// The unique node ID.
    pub id: i64,
    /// The node ID of the parent of this node.
    pub parent: Option<i64>,
    /// Auxiliary value, currently unused.
    pub aux: Option<i64>,
    /// Details as a human-readable string.
    pub detail: Option<Box<str>>,
    /// Child nodes.
    pub children: Vec<QueryPlanNode>,
}

impl QueryPlanNode {
    /// Returns a node that is suitable for use as the root of a query plan.
    pub fn root() -> Self {
        QueryPlanNode {
            id: 0,
            parent: None,
            aux: None,
            detail: Some("QUERY PLAN".into()),
            children: Vec::new(),
        }
    }

    /// Returns `true` if and only if this is the root node.
    pub const fn is_root(&self) -> bool {
        self.parent.is_none()
    }
}

/// The `Default` impl simply returns [`QueryPlanNode::root()`].
impl Default for QueryPlanNode {
    fn default() -> Self {
        QueryPlanNode::root()
    }
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
                children: Vec::new(),
            })
        } else {
            Err(Error::ColumnCountMismatch { expected, actual })
        }
    }
}

#[cfg(feature = "pretty-eqp")]
impl TreeItem for QueryPlanNode {
    type Child = Self;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        write!(
            f,
            "{}",
            style.paint(format_args!(
                "{label} (#{id})",
                label = self.detail.as_deref().unwrap_or("<NO DETAIL>"),
                id = self.id,
            ))
        )
    }

    fn children(&self) -> Cow<'_, [Self::Child]> {
        Cow::Borrowed(self.children.as_slice())
    }
}

/// A query plan, in tree form.
#[derive(Clone, Default, PartialEq, Debug)]
pub struct QueryPlan {
    /// The root of the tree of `QueryPlanNode`s.
    pub root: QueryPlanNode,
}

impl ResultSet for QueryPlan {
    fn from_rows(rows: Rows<'_>) -> Result<Self> {
        let nodes: Vec<QueryPlanNode> = Vec::from_rows(rows)?;
        let root = build_query_plan_tree(nodes)?;

        Ok(QueryPlan { root })
    }
}

#[cfg(feature = "pretty-eqp")]
impl fmt::Display for QueryPlan {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct IoFmt<'a, 'b>(&'a mut fmt::Formatter<'b>);

        impl io::Write for IoFmt<'_, '_> {
            fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
                let string = core::str::from_utf8(buf).map_err(io::Error::other)?;
                self.0.write_str(string).map_err(io::Error::other)?;
                Ok(string.len())
            }

            fn flush(&mut self) -> io::Result<()> {
                Ok(())
            }
        }

        ptree::write_tree(&self.root, IoFmt(formatter)).map_err(|_| fmt::Error)
    }
}

/// The possible errors when building a query plan tree from a flat set of rows
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Error)]
pub enum QueryPlanBuildErrorKind {
    /// Two or more nodes among the returned rows had the same node ID.
    #[error("found node with duplicate ID")]
    DuplicateNodeId,
    /// A node returned by SQLite had a missing parent ID.
    #[error("found more than one root")]
    MoreThanOneRoot,
    /// A node returned by SQLite had an unrecognized parent ID,
    /// or the graph described by parent relationship was cyclic.
    #[error("found orphan (child belonging to an invalid parent) or the graph is cyclic")]
    OrphanNodeOrCycle,
    /// A node appeared more than once when building subtrees.
    #[error("node was visited more than once")]
    NodeRevisited,
}

/// A tree building error kind, along with the node that caused the error.
#[derive(Clone, Debug, Error)]
#[error("{kind} (at node #{node_id})", node_id = .node.id)]
pub struct QueryPlanBuildError {
    /// The kind of error that happened while building the query plan tree.
    pub kind: QueryPlanBuildErrorKind,
    /// The node that caused the error.
    pub node: QueryPlanNode,
}

impl From<QueryPlanBuildError> for Error {
    fn from(reason: QueryPlanBuildError) -> Self {
        Error::other(reason)
    }
}

fn build_query_plan_tree(
    nodes: impl IntoIterator<Item = QueryPlanNode>
) -> Result<QueryPlanNode, QueryPlanBuildError> {
    let root = QueryPlanNode::root();
    let iter = nodes.into_iter();
    let (capacity, _) = iter.size_hint();

    let mut node_ids = HashSet::with_capacity(capacity.saturating_add(1)); // +1 for the root
    let mut children_by_parent_id = BTreeMap::<i64, Vec<_>>::new();

    node_ids.insert(root.id);

    for node in iter {
        if !node_ids.insert(node.id) {
            return Err(QueryPlanBuildError {
                kind: QueryPlanBuildErrorKind::DuplicateNodeId,
                node,
            });
        }

        let Some(parent_id) = node.parent else {
            return Err(QueryPlanBuildError{
                kind: QueryPlanBuildErrorKind::MoreThanOneRoot,
                node,
            });
        };

        children_by_parent_id.entry(parent_id).or_default().push(node);
    }

    // no node besides the root
    if children_by_parent_id.is_empty() {
        return Ok(root);
    }

    let root = build_recursively_from_parent_map(root, &mut children_by_parent_id)?;

    if let Some(orphan) = children_by_parent_id.values_mut().find_map(Vec::pop) {
        return Err(QueryPlanBuildError {
            kind: QueryPlanBuildErrorKind::OrphanNodeOrCycle,
            node: orphan,
        });
    }

    Ok(root)
}

fn build_recursively_from_parent_map(
    mut root: QueryPlanNode,
    children_by_parent_id: &mut BTreeMap<i64, Vec<QueryPlanNode>>,
) -> Result<QueryPlanNode, QueryPlanBuildError> {
    if !root.children.is_empty() {
        return Err(QueryPlanBuildError {
            kind: QueryPlanBuildErrorKind::NodeRevisited,
            node: root,
        });
    }

    // leaf nodes will not be found in the map
    let children = children_by_parent_id.remove(&root.id).unwrap_or_default();

    root.children.reserve_exact(children.len());

    for child in children {
        let subtree = build_recursively_from_parent_map(child, children_by_parent_id)?;
        root.children.push(subtree);
    }

    // this is not strictly necessary, only here to make testing easier.
    root.children.sort_unstable_by_key(|node| node.id);

    Ok(root)
}

#[cfg(test)]
mod tests {
    use crate::{Connection, ConnectionExt, Result, define_query};
    use super::{build_query_plan_tree, QueryPlanNode, QueryPlanBuildError, QueryPlanBuildErrorKind};

    define_query!{
        /// Example taken directly from: https://sqlite.org/eqp.html#subqueries
        ComplexQuery<'p>: (i16, &'p [u8], f64) => Vec<(Box<str>, u16)> {
            r#"
            SELECT * FROM
                (SELECT * FROM t1 WHERE a=1 ORDER BY b LIMIT 2) AS x,
                (SELECT * FROM t2 WHERE c=1 ORDER BY d LIMIT 2) AS y;
            "#
        }
    }

    #[test]
    fn explain_complex_query_plan() -> Result<()> {
        let conn = Connection::connect_in_memory()?;
        conn.execute("CREATE TABLE t1(a, b);", [])?;
        conn.execute("CREATE TABLE t2(c, d);", [])?;

        let eqp = conn.explain_query_plan(ComplexQuery)?;

        println!("{eqp}");
        assert!(!eqp.root.children.is_empty());

        Ok(())
    }

    #[test]
    fn empty_tree() -> Result<(), QueryPlanBuildError> {
        assert!(build_query_plan_tree([])?.children.is_empty());
        Ok(())
    }

    #[test]
    fn singleton_tree() -> Result<(), QueryPlanBuildError> {
        let tree = build_query_plan_tree([
            QueryPlanNode {
                id: 137,
                parent: Some(0),
                ..QueryPlanNode::default()
            },
        ])?;

        assert_eq!(tree.children.len(), 1);
        assert_eq!(tree.children[0].id, 137);

        Ok(())
    }

    #[test]
    fn complex_tree() -> Result<(), QueryPlanBuildError> {
        let root = build_query_plan_tree([
            QueryPlanNode {
                id: 12,
                parent: Some(10),
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 5,
                parent: Some(10),
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 10,
                parent: Some(0),
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 7,
                parent: Some(5),
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 1,
                parent: Some(5),
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 6,
                parent: Some(12),
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 8,
                parent: Some(1),
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 11,
                parent: Some(0),
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 3,
                parent: Some(0),
                ..QueryPlanNode::default()
            },
        ])?;

        assert_eq!(root.id, 0);
        assert_eq!(
            root.children.iter().map(|node| node.id).collect::<Vec<_>>(),
            [3, 10, 11]
        );

        assert!(root.children[0].children.is_empty());
        assert!(root.children[2].children.is_empty());

        assert_eq!(
            root.children[1].children.iter().map(|node| node.id).collect::<Vec<_>>(),
            [5, 12]
        );

        assert_eq!(root.children[1].children[0].children.len(), 2);
        assert_eq!(root.children[1].children[0].children[0].id, 1);
        assert_eq!(root.children[1].children[0].children[1].id, 7);

        assert_eq!(root.children[1].children[1].children.len(), 1);
        assert_eq!(root.children[1].children[1].children[0].id, 6);

        assert_eq!(root.children[1].children[0].children[0].children.len(), 1);
        assert_eq!(root.children[1].children[0].children[0].children[0].id, 8);

        assert!(root.children[1].children[0].children[0].children[0].children.is_empty());

        Ok(())
    }

    #[test]
    fn cyclic_graph_direct() {
        let result = build_query_plan_tree([
            QueryPlanNode {
                id: 137,
                parent: Some(137),
                ..QueryPlanNode::default()
            },
        ]);
        let error = result.unwrap_err();

        assert_eq!(error.node.id, 137);
        assert_eq!(error.kind, QueryPlanBuildErrorKind::OrphanNodeOrCycle);
    }

    #[test]
    fn cyclic_graph_indirect() {
        let result = build_query_plan_tree([
            QueryPlanNode {
                id: 137,
                parent: Some(42),
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 99, // this one won't cause a cycle
                parent: Some(137),
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 42,
                parent: Some(137),
                ..QueryPlanNode::default()
            },
        ]);
        let error = result.unwrap_err();

         // we don't specify exactly which one should error
        assert!(
            [
                (42, Some(137)),
                (137, Some(42)),
            ].contains(
                &(error.node.id, error.node.parent)
            )
        );
        assert_eq!(error.kind, QueryPlanBuildErrorKind::OrphanNodeOrCycle);
    }

    #[test]
    fn orphan_node() {
        let result = build_query_plan_tree([
            QueryPlanNode {
                id: 137,
                parent: Some(0),
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 99,
                parent: Some(43), // this doesn't exist
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 42,
                parent: Some(137),
                ..QueryPlanNode::default()
            },
        ]);
        let error = result.unwrap_err();

        assert_eq!(error.kind, QueryPlanBuildErrorKind::OrphanNodeOrCycle);
        assert_eq!(error.node.id, 99);
    }

    #[test]
    fn more_than_one_root() {
        let result = build_query_plan_tree([QueryPlanNode { id: 1, ..QueryPlanNode::root() }]);
        let error = result.unwrap_err();

        assert_eq!(error.kind, QueryPlanBuildErrorKind::MoreThanOneRoot);
        assert_eq!(error.node.id, 1);
    }

    #[test]
    fn duplicate_node_id() {
        let error = build_query_plan_tree([QueryPlanNode::root()]).unwrap_err();
        assert_eq!(error.kind, QueryPlanBuildErrorKind::DuplicateNodeId);
        assert_eq!(error.node.id, 0);

        let result = build_query_plan_tree([
            QueryPlanNode {
                id: 3,
                parent: Some(0),
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 4,
                parent: Some(0),
                ..QueryPlanNode::default()
            },
            QueryPlanNode {
                id: 3,
                parent: Some(4),
                ..QueryPlanNode::default()
            },
        ]);
        let error = result.unwrap_err();

        assert_eq!(error.kind, QueryPlanBuildErrorKind::DuplicateNodeId);
        assert_eq!(error.node.id, 3);
    }
}
