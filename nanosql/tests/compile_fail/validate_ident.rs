#![allow(mixed_script_confusables)]

use nanosql::{Param, ResultRecord, Table};

/// User-specified identifiers (such as those in `rename`, `pk`, `fk`, etc.)
/// must always respect lexical rules (mainly quoting and escaping).
///
/// For the time being, the implementation of this is greatly simplified by
/// treating parameter and column/table names uniformly (eg. we avoid needing
/// to escape column and table names), which means restricting to a common
/// subset. This should be fine, as one shouldn't generally give weird
/// (eg. non-alphanumeric) names to columns/tables/host parameters anyway.
#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(rename = "foo bar")]
//~^ ERROR invalid character ` ` in identifier
struct BadTableNameStr {
    id: i32,
}

#[derive(Clone, Default, Debug, Param, Table)]
struct BadTableColumnNameStr {
    uid: i64,
    #[nanosql(rename = "abc'def")]
    //~^ ERROR invalid character `'` in identifier
    other: Vec<u8>,
}

#[derive(Clone, Default, Debug, Param)]
struct BadParamNameStr {
    uid: i64,
    #[nanosql(rename = "$wut12345")]
    //~^ ERROR invalid leading character `$` in identifier
    other: String,
    some_column: &'static str,
}

#[derive(Clone, Default, Debug, ResultRecord)]
struct BadResultColumnNameStr {
    #[nanosql(rename = "lol@quux")]
    //~^ ERROR invalid character `@` in identifier
    my_result_column: f64,
}

#[derive(Clone, Default, Debug, Param, Table)]
#[nanosql(rename = mixed_ιδεντιφιερ)]
//~^ ERROR invalid character `ι` in identifier
struct BadTableNameIdent {
    id: i32,
}

#[derive(Clone, Default, Debug, Param)]
struct BadParamNameIdent {
    uid: i64,
    #[nanosql(rename = cityМосква)]
    //~^ ERROR invalid character `М` in identifier
    other: String,
    some_column: &'static str,
}

#[derive(Clone, Default, Debug, ResultRecord)]
struct BadResultColumnNameIdent {
    #[nanosql(rename = 東京)]
    //~^ ERROR invalid leading character `東` in identifier
    my_result_column: f64,
}

#[derive(Clone, Debug, Param, Table)]
#[nanosql(rename = "")]
    //~^ ERROR identifier must not be empty
struct BadTableNameEmpty {
    has_column: std::borrow::Cow<'static, str>,
}

#[derive(Clone, Debug, Param, Table)]
struct BadTableColumnNameEmpty {
    #[nanosql(rename = "")]
    //~^ ERROR identifier must not be empty
    has_column: std::borrow::Cow<'static, str>,
}

#[derive(Clone, Debug, Param)]
struct BadParamNameEmpty {
    #[nanosql(rename = "")]
    //~^ ERROR identifier must not be empty
    bad_column: &'static Box<i16>,
}

#[derive(Clone, Debug, ResultRecord)]
struct BadResultColumnNameEmpty {
    #[nanosql(rename = "")]
    //~^ ERROR identifier must not be empty
    bad_result: String,
}

#[derive(Clone, Debug, Param)]
struct IdentMustNotStartWithDigit {
    #[nanosql(rename = "23andMe")]
    //~^ ERROR invalid leading character `2` in identifier
    chromosomes: core::num::NonZeroUsize,
}

/// This one SHOULD compile successfully.
#[derive(Clone, Debug, Param)]
struct RawRustIdentIsValid {
    #[nanosql(rename = r#where)]
    condition: bool,
}

fn main() {}
