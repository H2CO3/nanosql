//! This is an example that showcases the most important features of `nanosql`.
//!
//! The domain being modeled is a simple taxonomy database like the NCBI Tax Dump.
//! It features primary and foreign keys, recursive queries over a hierarchy,
//! `enum`s, and other interesting problems that you may encounter in real data.

use std::io;
use nanosql::{define_query, Result, Connection, ConnectionExt};
use nanosql::{AsSqlTy, ToSql, FromSql, Table, Param, ResultRecord, InsertInput};


#[derive(Clone, Debug, Table, Param, ResultRecord)]
#[nanosql(rename = "taxon")]
struct Taxon {
    #[nanosql(pk)]
    tax_id: u64,
    rank: Rank,
    #[nanosql(fk = Taxon::tax_id)]
    parent_tax_id: Option<u64>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, AsSqlTy, ToSql, FromSql)]
#[nanosql(rename_all = lower_snake_case)]
enum Rank {
    Kingdom,
    Phylum,
    Class,
    Order,
    Family,
    Genus,
    Species,
    Strain,
}

/// Rows of `Name` correspond to 0 or more names for any given taxon.
/// Scientific names must be unique per taxon, but any other kind of
/// name may be repeated. For this reason, there is a unique partial
/// index on `(tax_id, kind)`, when the kind is `ScientificName`.
///
/// Looking up taxon names by taxonomy ID is fast, because the Tax ID
/// is a `FOREIGN KEY` of this table, which `nanosql` automatically
/// indexes, without adding an explicit index.
#[derive(Clone, Debug, Table, ResultRecord)]
#[nanosql(rename = "name", insert_input_ty = InsertName<'p>)]
#[nanosql(index(unique, columns(tax_id, kind), where = "kind = 'scientific_name'"))]
struct Name {
    #[nanosql(pk)]
    name_id: u64,
    #[nanosql(fk = Taxon::tax_id)]
    tax_id: u64,
    #[nanosql(index)]
    name: String,
    kind: NameKind,
}

/// This is an insert input for the `Name` table. This is a separate type,
/// so that one doesn't need to allocate a new `String` for the name, just
/// for the sake of inserting.
#[derive(Param, InsertInput)]
#[nanosql(table = Name)]
struct InsertName<'p> {
    /// We allow this to be `NULL`, in which case SQLite will assign a
    /// fresh integer row ID upon insertion.
    name_id: Option<u64>,
    tax_id: u64,
    name: &'p str,
    kind: NameKind,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, AsSqlTy, ToSql, FromSql)]
#[nanosql(rename_all = lower_snake_case)]
enum NameKind {
    ScientificName,
    Synonym,
    CommonName,
    Acronym,
}

/// This is the result of a LEFT OUTER JOIN. It's not usable as a
/// table on its own, and you can't use it for inserting records.
#[derive(Clone, Debug, ResultRecord)]
struct NamedTaxon {
    tax_id: u64,
    rank: Rank,
    parent_id: Option<u64>,
    /// you can rename both parameter and result columns/fields
    #[nanosql(rename = name)]
    tax_name: Option<String>,
    name_kind: Option<NameKind>,
}

define_query!{
    /// Query the lineage of a particular clade/taxon,
    /// sorted by taxonomic rank, from broadest to narrowest.
    /// This involves a recursive query over the parent FK.
    #[allow(non_camel_case_types)]
    LINEAGE_OF_TAXON<'p>: u64 => Vec<Taxon> {
        r#"
        WITH lineage AS (
                SELECT tax_id, rank, parent_tax_id, 1 AS level
                FROM taxon
                WHERE tax_id = ?
            UNION ALL
                SELECT taxon.tax_id, taxon.rank, taxon.parent_tax_id, level + 1
                FROM lineage
                INNER JOIN taxon
                ON lineage.parent_tax_id = taxon.tax_id
        )
        SELECT tax_id AS tax_id, rank AS rank, parent_tax_id AS parent_tax_id
        FROM lineage
        ORDER BY level DESC
        "#
    }
    /// Retrieves all names of the given taxon. Always returns at least one record.
    #[allow(non_camel_case_types)]
    TAXON_WITH_NAMES<'p>: u64 => Vec<NamedTaxon> {
        r#"
        -- Use a LEFT JOIN in order to force the database to always
        -- return at least one row, even if the taxon has no name.
        SELECT
            taxon.tax_id AS tax_id,
            taxon.parent_tax_id AS parent_id,
            taxon.rank AS rank,
            name.name AS name,
            name.kind AS name_kind
        FROM taxon
        LEFT JOIN name
        ON name.tax_id = taxon.tax_id
        WHERE taxon.tax_id = ?
        "#
    }
}

fn fill_db(conn: &mut Connection) -> Result<()> {
    // It's fine to insert in parent -> child order, because foreign
    // keys are deferred, so within the same transaction, one can
    // temporarily violate them -- and `insert_batch()` does open
    // a transaction.
    conn.insert_batch([
        // E. coli
        Taxon {
            tax_id: 562,
            rank: Rank::Species,
            parent_tax_id: Some(561),
        },
        // E. hominis
        Taxon {
            tax_id: 2764324,
            rank: Rank::Species,
            parent_tax_id: Some(561),
        },
        // E. marmotae
        Taxon {
            tax_id: 1499973,
            rank: Rank::Species,
            parent_tax_id: Some(561),
        },
        // Escherichia
        Taxon {
            tax_id: 561,
            rank: Rank::Genus,
            parent_tax_id: Some(543),
        },
        // Enterobacteriaceae
        Taxon {
            tax_id: 543,
            rank: Rank::Family,
            parent_tax_id: Some(1224),
        },
        // Proteobacteria
        Taxon {
            tax_id: 1224,
            rank: Rank::Phylum,
            parent_tax_id: Some(2),
        },
        // Bacteria
        Taxon {
            tax_id: 2,
            rank: Rank::Kingdom,
            parent_tax_id: None,
        },
    ])?;

    // Now, insert some names as well -- not for all entries in `taxon`.
    conn.insert_batch([
        InsertName {
            name_id: None,
            tax_id: 2,
            name: "Bacteria",
            kind: NameKind::ScientificName,
        },
        InsertName {
            name_id: None,
            tax_id: 2,
            name: "Bacterium",
            kind: NameKind::Synonym,
        },
        InsertName {
            name_id: None,
            tax_id: 562,
            name: "Escherichia coli",
            kind: NameKind::ScientificName,
        },
        InsertName {
            name_id: None,
            tax_id: 562,
            name: "E. coli",
            kind: NameKind::Acronym,
        },
        InsertName {
            name_id: None,
            tax_id: 562,
            name: "coli bacterium",
            kind: NameKind::CommonName,
        },
    ])?;

    Ok(())
}

fn main() -> Result<()> {
    let db_path = "/tmp/nanosql-example-taxdb.sqlite3";

    match std::fs::remove_file(db_path) {
        Ok(()) => {}
        Err(error) if error.kind() == io::ErrorKind::NotFound => {}
        Err(error) => return Err(error.into()),
    }

    let mut conn = Connection::connect(db_path)?;

    conn.create_table::<Taxon>()?;
    conn.create_table::<Name>()?;

    // First, fill the DB with data.
    fill_db(&mut conn)?;

    // Query a table using a recursive CTE.
    let mut stmt_lineage = dbg!(conn.compile(LINEAGE_OF_TAXON)?);
    let lineage_e_coli = dbg!(stmt_lineage.invoke(562)?);
    let lineage_enterobacteria = dbg!(stmt_lineage.invoke(543)?);

    assert_eq!(lineage_e_coli.len(), 5);
    assert_eq!(lineage_enterobacteria.len(), 3);

    // Query and JOIN two tables.
    let mut stmt_names = dbg!(conn.compile(TAXON_WITH_NAMES)?);
    let names_bacteria = dbg!(stmt_names.invoke(2)?);
    let names_e_coli = dbg!(stmt_names.invoke(562)?);
    let names_escherichia = dbg!(stmt_names.invoke(561)?);

    assert_eq!(names_bacteria.len(), 2);
    assert_eq!(names_e_coli.len(), 3);
    assert_eq!(names_escherichia.len(), 1);
    assert_eq!(names_escherichia[0].tax_id, 561);
    assert_eq!(names_escherichia[0].tax_name, None);

    Ok(())
}
