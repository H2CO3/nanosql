use std::borrow::Cow;
use nanosql::{define_query, Result, Error};
use nanosql::{Table, Param, ResultRecord, Connection, ConnectionExt};


#[derive(Copy, Clone, Debug)]
struct DoesNotImplementToSqlOrParam;

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
    trailing_junk: Vec<DoesNotImplementToSqlOrParam>,
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
struct IgnoredParamFirstUnnamed(#[nanosql(ignore)] DoesNotImplementToSqlOrParam, u32);

#[derive(Clone, Copy, Debug, Param)]
struct IgnoredParamLastUnnamed(f64, #[nanosql(ignore)] DoesNotImplementToSqlOrParam);

#[derive(Clone, Debug, Param)]
struct IgnoredParamManyUnnamed<'p>(
    &'p str,
    #[nanosql(ignore)] DoesNotImplementToSqlOrParam,
    #[nanosql(ignore)] Cow<'static, str>,
    u16,
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
        trailing_junk: vec![DoesNotImplementToSqlOrParam],
    })?;
    assert_eq!(apps3.len(), 2);
    assert_eq!(apps3[0].name, "television");
    assert_eq!(apps3[1].name, "washing machine");

    let apps4 = conn.compile_invoke(ApplianceByBrand, IgnoredParamLastNamed {
        brand: "Jura",
        trailing_junk: vec![
            DoesNotImplementToSqlOrParam,
            DoesNotImplementToSqlOrParam,
            DoesNotImplementToSqlOrParam,
        ],
    })?;
    assert_eq!(apps4, vec![app2]);

    let apps5 = conn.compile_invoke(ApplianceByBrand, IgnoredParamLastNamed {
        brand: "Electrolux",
        trailing_junk: vec![
            DoesNotImplementToSqlOrParam,
            DoesNotImplementToSqlOrParam,
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
        IgnoredParamFirstUnnamed(DoesNotImplementToSqlOrParam, 1000),
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
        IgnoredParamLastUnnamed(800.0, DoesNotImplementToSqlOrParam),
    )?;
    assert_eq!(brands, vec!["Bosch"]);

    let names = conn.compile_invoke(
        ApplianceNamesByBrandAndMaxPower,
        IgnoredParamManyUnnamed(
            "LG",
            DoesNotImplementToSqlOrParam,
            Cow::Borrowed("Bosch"), // should be ignored
            1500,
        )
    )?;
    assert_eq!(names, vec!["television"]);

    Ok(())
}
