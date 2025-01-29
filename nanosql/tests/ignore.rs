use std::borrow::Cow;
use std::rc::Rc;
use nanosql::{define_query, Result, Connection, ConnectionExt, Single};
use nanosql::{Table, Param, ResultRecord, AsSqlTy, ToSql, FromSql};


#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct DoesNotImplementDefaultOrToSqlOrParam;

#[derive(Copy, Clone, Default, PartialEq, Eq, Debug)]
struct NotFromSqlButDefault;

#[derive(Clone, PartialEq, Eq, Debug, Table, Param, ResultRecord)]
struct Appliance {
    #[nanosql(unique)]
    name: String,
    brand: Cow<'static, str>,
    power_draw_watt: u32,
}

#[derive(Clone, Copy, Debug, Param)]
struct IgnoredParamFirstNamed<'a> {
    #[nanosql(ignore)]
    does_not_matter: i64,
    name: &'a str,
}

#[derive(Clone, Debug, Param)]
struct IgnoredParamLastNamed {
    brand: &'static str,
    #[nanosql(ignore)]
    trailing_junk: Vec<DoesNotImplementDefaultOrToSqlOrParam>,
}

#[derive(Clone, Debug, Param)]
struct IgnoredParamManyNamed {
    max_power: u64,
    #[nanosql(ignore)]
    ignored_1: f32,
    brand: String,
    #[nanosql(ignore)]
    another_ignored: Vec<Option<bool>>,
    name: Box<str>,
}

#[derive(Clone, Copy, Debug, Param)]
struct IgnoredParamFirstUnnamed(#[nanosql(ignore)] DoesNotImplementDefaultOrToSqlOrParam, u32);

#[derive(Clone, Copy, Debug, Param)]
struct IgnoredParamLastUnnamed(f64, #[nanosql(ignore)] DoesNotImplementDefaultOrToSqlOrParam);

#[derive(Clone, Debug, Param)]
struct IgnoredParamManyUnnamed<'p>(
    &'p str,
    // test the `skip` alias as well
    #[nanosql(skip)] DoesNotImplementDefaultOrToSqlOrParam,
    #[nanosql(ignore)] Cow<'static, str>,
    u16,
);

#[derive(Clone, PartialEq, Eq, Debug, ResultRecord)]
struct IgnoredResultRecordFirstNamed {
    #[nanosql(ignore)]
    this_is_default: Vec<DoesNotImplementDefaultOrToSqlOrParam>,
    brand: Box<str>,
    #[nanosql(rename = ident)]
    id: Cow<'static, str>,
}

#[derive(Clone, PartialEq, Eq, Debug, ResultRecord)]
struct IgnoredResultRecordManyNamed {
    #[nanosql(rename = "power_draw_watt")]
    power_draw: usize,
    #[nanosql(ignore)]
    ignore_middle_1: Option<u64>,
    #[nanosql(skip)]
    power_draw_watt: NotFromSqlButDefault, // tricky potential name collision
    #[nanosql(rename = "brand")]
    label: Rc<str>,
}

#[derive(PartialEq, Debug, ResultRecord)]
struct IgnoreResultRecordUnnamed(
    #[nanosql(ignore)] u32,
    String,
    #[nanosql(ignore)] NotFromSqlButDefault,
    Box<str>,
    #[nanosql(ignore)] i64,
);

define_query!{
    ApplianceByName<'p>: IgnoredParamFirstNamed<'p> => Option<Appliance> {
        r#"
        SELECT
            name AS name,
            brand AS brand,
            power_draw_watt AS power_draw_watt
        FROM appliance
        WHERE name = $name
        "#
    }
    ApplianceByBrand<'p>: IgnoredParamLastNamed => Vec<Appliance> {
        r#"
        SELECT
            name AS name,
            brand AS brand,
            power_draw_watt AS power_draw_watt
        FROM appliance
        WHERE brand = $brand
        ORDER BY name
        "#
    }
    ApplianceByMaxPowerDraw<'p>: IgnoredParamManyNamed => Option<Appliance> {
        r#"
        SELECT
            name AS name,
            brand AS brand,
            power_draw_watt AS power_draw_watt
        FROM appliance
        WHERE
            name = $name
          AND
            brand = $brand
          AND
            power_draw_watt <= $max_power
        "#
    }
    ApplianceByMinPowerDraw<'p>: IgnoredParamFirstUnnamed => Vec<Appliance> {
        r#"
        SELECT
            name AS name,
            brand AS brand,
            power_draw_watt AS power_draw_watt
        FROM appliance
        WHERE power_draw_watt >= ?1
        ORDER BY power_draw_watt
        "#
    }
    BrandsByPowerDraw<'p>: IgnoredParamLastUnnamed => Vec<String> {
        r#"
        SELECT brand AS brand
        FROM appliance
        WHERE power_draw_watt == ?1
        ORDER BY brand
        "#
    }
    ApplianceNamesByBrandAndMaxPower<'brand>: IgnoredParamManyUnnamed<'brand> => Vec<String> {
        r#"
        SELECT name AS name
        FROM appliance
        WHERE
            power_draw_watt <= ?2
          AND
            brand = ?1
        ORDER BY name
        "#
    }
    ApplianceNamesAndBrands<'p>: () => Vec<IgnoredResultRecordFirstNamed> {
        r#"
        SELECT
            name AS ident,
            brand AS brand
        FROM appliance
        ORDER BY brand, name DESC
        "#
    }
    BrandAndPowerByName<'lt>: &'lt str => Single<IgnoredResultRecordManyNamed> {
        r#"
        SELECT
            brand AS brand,
            power_draw_watt AS power_draw_watt
        FROM appliance
        WHERE name = ?1
        ORDER BY name
        LIMIT 1
        "#
    }
    NamesByCaseInsensitiveBrand<'b>: &'b str => Vec<IgnoreResultRecordUnnamed> {
        r#"
        SELECT
            brand AS brand,
            name AS name
        FROM appliance
        WHERE brand LIKE ?1
        ORDER BY name
        "#
    }
}

fn setup() -> Result<Connection> {
    let mut conn = Connection::connect_in_memory()?;

    conn.create_table::<Appliance>()?;
    conn.insert_batch([
        Appliance {
            name: "washing machine".into(),
            brand: "LG".into(),
            power_draw_watt: 1800,
        },
        Appliance {
            name: "espresso machine".into(),
            brand: "Jura".into(),
            power_draw_watt: 1200,
        },
        Appliance {
            name: "television".into(),
            brand: "LG".into(),
            power_draw_watt: 350,
        },
        Appliance {
            name: "vacuum cleaner".into(),
            brand: "Bosch".into(),
            power_draw_watt: 800,
        },
    ])?;

    Ok(conn)
}

#[test]
fn ignored_param_named() -> Result<()> {
    let conn = setup()?;

    // ignored field should not affect query results
    let app1 = conn.compile_invoke(ApplianceByName, IgnoredParamFirstNamed {
        does_not_matter: 3333,
        name: "espresso machine",
    })?.expect(
        "espresso machine not found in DB"
    );
    let app2 = conn.compile_invoke(ApplianceByName, IgnoredParamFirstNamed {
        does_not_matter: 777,
        name: "espresso machine",
    })?.expect(
        "espresso machine not found in DB"
    );
    assert_eq!(app1, app2);

    let apps3 = conn.compile_invoke(ApplianceByBrand, IgnoredParamLastNamed {
        brand: "LG",
        trailing_junk: vec![DoesNotImplementDefaultOrToSqlOrParam],
    })?;
    assert_eq!(apps3.len(), 2);
    assert_eq!(apps3[0].name, "television");
    assert_eq!(apps3[1].name, "washing machine");

    let apps4 = conn.compile_invoke(ApplianceByBrand, IgnoredParamLastNamed {
        brand: "Jura",
        trailing_junk: vec![
            DoesNotImplementDefaultOrToSqlOrParam,
            DoesNotImplementDefaultOrToSqlOrParam,
            DoesNotImplementDefaultOrToSqlOrParam,
        ],
    })?;
    assert_eq!(apps4, vec![app2]);

    let apps5 = conn.compile_invoke(ApplianceByBrand, IgnoredParamLastNamed {
        brand: "Electrolux",
        trailing_junk: vec![
            DoesNotImplementDefaultOrToSqlOrParam,
            DoesNotImplementDefaultOrToSqlOrParam,
        ],
    })?;
    assert_eq!(apps5, []);

    let app6 = conn.compile_invoke(ApplianceByMaxPowerDraw, IgnoredParamManyNamed {
        max_power: 1000,
        ignored_1: 13.37,
        brand: "LG".into(),
        another_ignored: vec![Some(false), None, Some(true)],
        name: Box::from("television"),
    })?.expect(
        "television not found in DB"
    );
    assert_eq!(app6, apps3[0]);

    Ok(())
}

#[test]
fn ignored_param_unnamed() -> Result<()> {
    let conn = setup()?;

    let apps = conn.compile_invoke(
        ApplianceByMinPowerDraw,
        IgnoredParamFirstUnnamed(DoesNotImplementDefaultOrToSqlOrParam, 1000),
    )?;
    assert_eq!(apps.len(), 2);
    assert_eq!(apps[0].name, "espresso machine");
    assert_eq!(apps[0].brand, "Jura");
    assert_eq!(apps[0].power_draw_watt, 1200);
    assert_eq!(apps[1].name, "washing machine");
    assert_eq!(apps[1].brand, "LG");
    assert_eq!(apps[1].power_draw_watt, 1800);

    let brands = conn.compile_invoke(
        BrandsByPowerDraw,
        IgnoredParamLastUnnamed(800.0, DoesNotImplementDefaultOrToSqlOrParam),
    )?;
    assert_eq!(brands, vec!["Bosch"]);

    let names = conn.compile_invoke(
        ApplianceNamesByBrandAndMaxPower,
        IgnoredParamManyUnnamed(
            "LG",
            DoesNotImplementDefaultOrToSqlOrParam,
            Cow::Borrowed("Bosch"), // should be ignored
            1500,
        )
    )?;
    assert_eq!(names, vec!["television"]);

    Ok(())
}

#[test]
fn ignored_resultrecord_named() -> Result<()> {
    let conn = setup()?;

    let appls = conn.compile_invoke(ApplianceNamesAndBrands, ())?;
    assert_eq!(appls, vec![
        IgnoredResultRecordFirstNamed {
            this_is_default: Vec::new(),
            brand: Box::from("Bosch"),
            id: Cow::Owned("vacuum cleaner".to_string()),
        },
        IgnoredResultRecordFirstNamed {
            this_is_default: vec![],
            brand: Box::from("Jura"),
            id: Cow::Borrowed("espresso machine"),
        },
        IgnoredResultRecordFirstNamed {
            this_is_default: vec![DoesNotImplementDefaultOrToSqlOrParam; 0],
            brand: Box::from("LG"),
            id: Cow::Owned("washing machine".to_owned()),
        },
        IgnoredResultRecordFirstNamed {
            this_is_default: Vec::new(),
            brand: Box::from("LG"),
            id: Cow::from("television"),
        },
    ]);

    let Single(appl) = conn.compile_invoke(BrandAndPowerByName, "vacuum cleaner")?;
    assert_eq!(appl, IgnoredResultRecordManyNamed {
        power_draw: 800,
        ignore_middle_1: None,
        power_draw_watt: NotFromSqlButDefault,
        label: Rc::from("Bosch"),
    });

    Ok(())
}

#[test]
fn ignored_resultrecord_unnamed() -> Result<()> {
    let conn = setup()?;
    let appls = conn.compile_invoke(NamesByCaseInsensitiveBrand, "lg")?;

    assert_eq!(appls, vec![
        IgnoreResultRecordUnnamed(
            0_u32,
            String::from("LG"),
            NotFromSqlButDefault,
            Box::from("television"),
            0_i64,
        ),
        IgnoreResultRecordUnnamed(
            0_u32,
            String::from("LG"),
            NotFromSqlButDefault,
            Box::from("washing machine"),
            0_i64,
        ),
    ]);

    Ok(())
}

fn assert_as_sql_ty<T, U>()
where
    T: AsSqlTy,
    for<'p> U: AsSqlTy<Borrowed<'p> = T::Borrowed<'p>>,
{
    assert_eq!(T::SQL_TY, U::SQL_TY);
}

#[test]
fn ignored_assqlty_named() {
    #[derive(Debug, AsSqlTy)]
    struct Test {
        #[nanosql(ignore)]
        first_ignored: String,
        payload: i64,
        #[nanosql(ignore)]
        second_ignored: DoesNotImplementDefaultOrToSqlOrParam,
    }

    assert_as_sql_ty::<Test, i64>();
}

#[test]
fn ignored_assqlty_unnamed() {
    #[derive(Debug, AsSqlTy)]
    struct Test(
        #[nanosql(ignore)] DoesNotImplementDefaultOrToSqlOrParam,
        Box<str>,
        #[nanosql(ignore)] Cow<'static, [u8]>,
    );

    assert_as_sql_ty::<Test, Box<str>>();
}

#[test]
fn ignored_tosql_named() -> Result<()> {
    #[derive(ToSql)]
    struct Test<'lt> {
        #[nanosql(ignore)]
        foo: DoesNotImplementDefaultOrToSqlOrParam,
        payload: &'lt str,
        #[nanosql(ignore)]
        bar: u32,
    }

    let payload = "the only non-ignored field";
    let test = Test { payload, foo: DoesNotImplementDefaultOrToSqlOrParam, bar: 192837 };

    assert_eq!(test.to_sql()?, payload.to_sql()?);

    Ok(())
}

#[test]
fn ignored_tosql_unnamed() -> Result<()> {
    #[derive(ToSql)]
    struct Test<T, U>(
        #[nanosql(ignore)]
        T,
        U,
        #[nanosql(ignore)]
        DoesNotImplementDefaultOrToSqlOrParam,
    );

    let payload = b"value for ToSql";
    let test = Test(1337_i64, payload, DoesNotImplementDefaultOrToSqlOrParam);

    assert_eq!(test.to_sql()?, payload.to_sql()?);

    Ok(())
}

#[test]
fn ignored_fromsql_named() -> Result<()> {
    #[derive(PartialEq, Debug, FromSql)]
    struct Test1 {
        #[nanosql(ignore)]
        empty: String,
        useful: String,
    }

    #[derive(PartialEq, Debug, FromSql)]
    struct Test2 {
        real_content: i16,
        #[nanosql(ignore)]
        other_number: u32,
        #[nanosql(ignore)]
        empty_blob: Vec<u8>,
    }

    #[derive(PartialEq, Debug, FromSql)]
    struct Test3 {
        #[nanosql(ignore)]
        leading_junk: NotFromSqlButDefault,
        actual_data: [u8; 10],
        #[nanosql(ignore)]
        trailing_junk: Option<DoesNotImplementDefaultOrToSqlOrParam>,
    }

    let content = "this is the expected content";
    let test1 = Test1::column_result(content.into())?;
    assert_eq!(test1, Test1 {
        empty: String::default(),
        useful: content.to_owned(),
    });

    let test2 = Test2::column_result(nanosql::ValueRef::Integer(-7331))?;
    assert_eq!(test2, Test2 {
        real_content: -7331,
        other_number: 0,
        empty_blob: vec![],
    });

    let test3 = Test3::column_result(nanosql::ValueRef::Blob(&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]))?;
    assert_eq!(test3, Test3 {
        leading_junk: NotFromSqlButDefault,
        actual_data: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
        trailing_junk: None,
    });

    Ok(())
}

#[test]
fn ignored_fromsql_unnamed() -> Result<()> {
    #[derive(PartialEq, Debug, FromSql)]
    struct Test1(
        #[nanosql(ignore)] String,
        String,
    );

    #[derive(PartialEq, Debug, FromSql)]
    struct Test2(
        i16,
        #[nanosql(ignore)] u32,
        #[nanosql(ignore)] Box<[u8]>,
    );

    #[derive(PartialEq, Debug, FromSql)]
    struct Test3(
        #[nanosql(ignore)] Option<DoesNotImplementDefaultOrToSqlOrParam>,
        [u8; 10],
        #[nanosql(ignore)] NotFromSqlButDefault,
    );

    let content = "let's see some more text";
    let test1 = Test1::column_result(content.into())?;
    assert_eq!(test1, Test1(String::new(), content.to_string()));

    let test2 = Test2::column_result(nanosql::ValueRef::Integer(-7331))?;
    assert_eq!(test2, Test2(-7331, 0, Box::new([])));

    let test3 = Test3::column_result(nanosql::ValueRef::Blob(&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]))?;
    assert_eq!(
        test3,
        Test3(
            None,
            [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            NotFromSqlButDefault,
        ),
    );

    Ok(())
}
