use std::collections::HashSet;
use proc_macro2::TokenStream;
use syn::Error;
use syn::{DeriveInput, Data, Fields, FieldsNamed};
use syn::ext::IdentExt;
use quote::quote;
use deluxe::SpannedValue;
use crate::util::{
    ContainerAttributes, FieldAttributes, GeneratedColumnMode, TableForeignKey, IdentOrStr
};


pub fn expand(ts: TokenStream) -> Result<TokenStream, Error> {
    let input: DeriveInput = syn::parse2(ts)?;
    let attrs: ContainerAttributes = deluxe::parse_attributes(&input)?;

    match &input.data {
        Data::Struct(data) => {
            match &data.fields {
                Fields::Named(fields) => expand_struct(&input, attrs, fields),
                Fields::Unnamed(_) => {
                    Err(Error::new_spanned(
                        &input,
                        "#[derive(Table)] is not supported for tuple structs"
                    ))
                }
                Fields::Unit => {
                    Err(Error::new_spanned(
                        &input,
                        "#[derive(Table)] is not supported for unit structs"
                    ))
                }
            }
        }
        Data::Enum(_) => {
            Err(Error::new_spanned(&input, "#[derive(Table)] is not supported for enums"))
        }
        Data::Union(_) => {
            Err(Error::new_spanned(&input, "#[derive(Table)] is not supported for unions"))
        }
    }
}

fn expand_struct(
    input: &DeriveInput,
    attrs: ContainerAttributes,
    fields: &FieldsNamed,
) -> Result<TokenStream, Error> {
    let ty_name = &input.ident;
    let table_name = attrs.rename
        .as_ref()
        .map_or_else(|| ty_name.unraw().to_string(), <_>::to_string);

    let insert_input_ty = attrs.insert_input_ty;
    let insert_input_lt = attrs.insert_input_lt;

    let attrs_for_each_field: Vec<FieldAttributes> = fields.named
        .iter()
        .map(deluxe::parse_attributes)
        .collect::<Result<_, _>>()?;

    let col_name_str: Vec<_> = fields.named
        .iter()
        .zip(&attrs_for_each_field)
        .map(|(f, field_attrs)| {
            let field_name = f.ident.as_ref().expect("named field is unnamed?");

            field_attrs.rename
                .as_ref()
                .map_or_else(
                    || attrs.rename_all.display(field_name).to_string(),
                    <_>::to_string,
                )
        })
        .collect();

    validate_primary_key(attrs.primary_key.as_ref(), &attrs_for_each_field, &col_name_str)?;
    validate_foreign_keys(&attrs.foreign_key, &attrs_for_each_field, &col_name_str)?;
    validate_unique_constraints(&attrs.unique, &attrs_for_each_field, &col_name_str)?;

    let col_ty = fields.named
        .iter()
        .zip(&attrs_for_each_field)
        .map(|(f, field_attrs)| {
            field_attrs.sql_ty.clone().unwrap_or_else(|| f.ty.clone())
        });

    let primary_key = attrs_for_each_field
        .iter()
        .map(|field_attrs| {
            field_attrs.primary_key.then_some(quote!(.primary_key()))
        });

    let foreign_key = attrs_for_each_field
        .iter()
        .map(|field_attrs| {
            field_attrs
                .foreign_key
                .as_ref()
                .map(|fk| {
                    let table = &fk.table;
                    let column = &fk.column;
                    quote!(.foreign_key(#table, #column))
                })
        });

    let uniq_constraint = attrs_for_each_field
        .iter()
        .map(|field_attrs| {
            field_attrs.unique.then_some(quote!(.unique()))
        });

    let check_constraint = attrs_for_each_field
        .iter()
        .map(|field_attrs| {
            let constraints = field_attrs.check.as_slice();

            quote!{
                #(.check(#constraints))*
            }
        });

    let default_value = attrs_for_each_field
        .iter()
        .map(|field_attrs| {
            field_attrs.default.as_ref().map(|expr| {
                quote!{.default_value(#expr)}
            })
        });

    let generated = attrs_for_each_field
        .iter()
        .map(|field_attrs| {
            field_attrs.generated.as_ref().map(|spec| {
                let expr = &spec.expr;

                match spec.mode {
                    GeneratedColumnMode::Virtual => quote!{
                        .generate_virtual(#expr)
                    },
                    GeneratedColumnMode::Stored => quote!{
                        .generate_stored(#expr)
                    },
                }
            })
        });

    let table_primary_key = attrs.primary_key.as_ref().map(|pk| {
        let columns = pk.iter();
        quote!{
            .primary_key([#(#columns,)*])
        }
    });
    let table_foreign_keys = attrs.foreign_key
        .iter()
        .map(|fk_spec| {
            let table_name = &fk_spec.table;
            let own_cols = fk_spec.columns.iter().map(|pair| &pair.0);
            let other_cols = fk_spec.columns.iter().map(|pair| &pair.1);

            quote!{
                .foreign_key(#table_name, [#((#own_cols, #other_cols),)*])
            }
        });

    let table_unique_constraints = attrs.unique
        .iter()
        .map(|val| val.iter())
        .map(|cols| quote!(.unique([#(#cols,)*])));
    let table_check_constraints = attrs.check
        .iter()
        .map(|expr| quote!(.check(#expr)));

    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();

    Ok(quote!{
        impl #impl_gen ::nanosql::Table for #ty_name #ty_gen #where_clause {
            type InsertInput<#insert_input_lt> = #insert_input_ty;

            fn description() -> ::nanosql::TableDesc {
                ::nanosql::TableDesc::new(#table_name) #(
                    .column(
                        ::nanosql::Column::new(#col_name_str)
                            .ty(<#col_ty as ::nanosql::AsSqlTy>::SQL_TY)
                            .check(
                                ::std::string::ToString::to_string(
                                    &::nanosql::table::ColumnConstraintFormatter::<#col_ty>::new(
                                        #col_name_str
                                    )
                                )
                            )
                            #primary_key
                            #foreign_key
                            #uniq_constraint
                            #check_constraint
                            #default_value
                            #generated
                    )
                )*
                #table_primary_key
                #(#table_foreign_keys)*
                #(#table_unique_constraints)*
                #(#table_check_constraints)*
            }
        }
    })
}

fn validate_primary_key(
    table_pk_cols: Option<&SpannedValue<Vec<IdentOrStr>>>,
    field_attrs: &[FieldAttributes],
    all_cols: &[String],
) -> Result<(), Error> {
    assert_eq!(field_attrs.len(), all_cols.len());

    // if the table-level PK is declared, check it
    if let Some(table_pk_cols) = table_pk_cols {
        // ensure that there is at least 1 column in the PK
        if table_pk_cols.is_empty() {
            return Err(Error::new_spanned(table_pk_cols, "primary key may not be an empty tuple"));
        }

        // ensure that columns referenced in the table-level PK do in fact exist
        if let Some(err_col) = table_pk_cols.iter().find(|col| !all_cols.contains(&col.to_string())) {
            return Err(Error::new_spanned(
                err_col,
                format_args!("unknown column `{err_col}` in primary key")
            ));
        }

        // ensure that the referenced columns are unique
        let mut column_set = HashSet::new();
        for col in table_pk_cols.as_slice() {
            if !column_set.insert(col) {
                return Err(Error::new_spanned(col, "duplicate columns in primary key"));
            }
        }
    }

    // always check the field-level PK attributes
    let mut column_pk_iter = field_attrs.iter().filter(|a| *a.primary_key);

    if let Some(column_pk) = column_pk_iter.next() {
        // ensure that a column-level PK does not conflict with the table-level PK
        if table_pk_cols.is_some() {
            return Err(Error::new_spanned(
                column_pk.primary_key,
                "primary key declared at both the table and the column level"
            ));
        }

        // ensure that at most one column is marked as the PK
        if let Some(dup_pk) = column_pk_iter.next() {
            return Err(Error::new_spanned(
                dup_pk.primary_key,
                "more than one primary key column; use the table-level attribute instead"
            ));
        }
    }

    Ok(())
}

fn validate_foreign_keys(
    table_fk_specs: &[TableForeignKey],
    field_attrs: &[FieldAttributes],
    all_cols: &[String],
) -> Result<(), Error> {
    table_fk_specs
        .iter()
        .try_for_each(|fk| validate_one_foreign_key(fk, field_attrs, all_cols))
}

fn validate_one_foreign_key(
    table_fk_spec: &TableForeignKey,
    field_attrs: &[FieldAttributes],
    all_cols: &[String],
) -> Result<(), Error> {
    assert_eq!(field_attrs.len(), all_cols.len());

    // ensure that the foreign key spec contains at least 1 column
    if table_fk_spec.columns.is_empty() {
        return Err(Error::new_spanned(
            &table_fk_spec.table,
            "foreign key may not be an empty tuple"
        ));
    }

    // ensure that supposedly _referencing_ columns do in fact exist in _this_ table
    if let Some(err_col) = table_fk_spec.columns
        .iter()
        .find_map(|(col, _)| {
            if all_cols.contains(&col.to_string()) {
                None
            } else {
                Some(col)
            }
        })
    {
        return Err(Error::new_spanned(
            err_col,
            format_args!("unknown column `{err_col}` in foreign key")
        ));
    }

    // ensure that both the referencing and the referenced columns are unique
    let mut referencing_columns = HashSet::new();
    let mut referenced_columns = HashSet::new();

    for (own, other) in &table_fk_spec.columns {
        if !referencing_columns.insert(own) {
            return Err(Error::new_spanned(own, "duplicate referencing column in foreign key"));
        }
        if !referenced_columns.insert(other) {
            return Err(Error::new_spanned(other, "duplicate referenced column in foreign key"));
        }
    }

    // Unlike a PRIMARY KEY, there may be more than one FOREIGN KEY on a table,
    // so we don't need to check for that kind of (non-)conflict. Yay! \o/

    Ok(())
}

fn validate_unique_constraints(
    unique_cols: &[SpannedValue<Vec<IdentOrStr>>],
    field_attrs: &[FieldAttributes],
    all_cols: &[String],
) -> Result<(), Error> {
    unique_cols
        .iter()
        .try_for_each(|cols| validate_one_unique_constraint(cols, field_attrs, all_cols))
}

fn validate_one_unique_constraint(
    unique_cols: &SpannedValue<Vec<IdentOrStr>>,
    field_attrs: &[FieldAttributes],
    all_cols: &[String],
) -> Result<(), Error> {
    assert_eq!(field_attrs.len(), all_cols.len());

    // ensure that the foreign key spec contains at least 1 column
    if unique_cols.is_empty() {
        return Err(Error::new_spanned(
            unique_cols,
            "unique constraint must refer to at least 1 column"
        ));
    }

    // ensure that the referenced columns do in fact exist
    if let Some(err_col) = unique_cols.iter().find(|col| !all_cols.contains(&col.to_string())) {
        return Err(Error::new_spanned(
            err_col,
            format_args!("unknown column `{err_col}` in unique constraint")
        ));
    }

    // ensure that the referenced columns are unique
    let mut column_set = HashSet::new();
    for col in unique_cols.as_slice() {
        if !column_set.insert(col) {
            return Err(Error::new_spanned(col, "duplicate columns in unique constraint"));
        }
    }

    Ok(())
}
