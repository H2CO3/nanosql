use std::collections::{HashSet, HashMap};
use proc_macro2::TokenStream;
use syn::Error;
use syn::{DeriveInput, Data, Fields, FieldsNamed, Type};
use syn::ext::IdentExt;
use quote::quote;
use deluxe::SpannedValue;
use crate::util::{
    ContainerAttributes, FieldAttributes, IdentOrStr, TableIndexSpec, TableForeignKey,
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

    let insert_input_ty = &attrs.insert_input_ty;
    let input_lt = &attrs.input_lt;

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
    validate_indexes(&attrs.index, &attrs_for_each_field, &col_name_str)?;
    validate_unique_constraints(&attrs.unique, &attrs_for_each_field, &col_name_str)?;

    let pk_ty = primary_key_type(fields, &attrs, &attrs_for_each_field, &col_name_str)?;
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
        .map(|field_attrs| field_attrs.foreign_key.as_ref());

    let index_specs = attrs_for_each_field
        .iter()
        .map(|field_attrs| &field_attrs.index);

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
        .map(|field_attrs| field_attrs.generated.as_ref());

    let table_primary_key = attrs.primary_key.as_ref().map(|pk| {
        let columns = pk.iter();
        quote!{
            .primary_key([#(#columns,)*])
        }
    });
    let table_foreign_keys = &attrs.foreign_key;
    let table_indexes = &attrs.index;

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
            type InsertInput<#input_lt> = #insert_input_ty;
            type PrimaryKey<#input_lt> = #pk_ty;

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
                            #index_specs
                            #uniq_constraint
                            #check_constraint
                            #default_value
                            #generated
                    )
                )*
                #table_primary_key
                #(#table_foreign_keys)*
                #(#table_indexes)*
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
    let mut internal_columns = HashSet::new();
    let mut external_columns = HashSet::new();

    for (own, foreign) in &table_fk_spec.columns {
        if !internal_columns.insert(own) {
            return Err(Error::new_spanned(own, "duplicate internal column in foreign key"));
        }
        if !external_columns.insert(foreign) {
            return Err(Error::new_spanned(foreign, "duplicate external column in foreign key"));
        }
    }

    // Unlike a PRIMARY KEY, there may be more than one FOREIGN KEY on a table,
    // so we don't need to check for that kind of (non-)conflict. Yay! \o/

    Ok(())
}

fn validate_indexes(
    indexes: &[TableIndexSpec],
    field_attrs: &[FieldAttributes],
    all_cols: &[String],
) -> Result<(), Error> {
    indexes
        .iter()
        .try_for_each(|index| validate_one_index(index, field_attrs, all_cols))
}

fn validate_one_index(
    index: &TableIndexSpec,
    field_attrs: &[FieldAttributes],
    all_cols: &[String],
) -> Result<(), Error> {
    assert_eq!(field_attrs.len(), all_cols.len());

    // ensure that the index spec contains at least 1 column
    if index.columns.is_empty() {
        return Err(Error::new_spanned(
            index,
            "table-level index must refer to at least 1 column"
        ));
    }

    // ensure that columns do in fact exist in the table
    if let Some(err_col) = index.columns
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
            format_args!("unknown column `{err_col}` in table-level index")
        ));
    }

    // ensure that the referenced columns are unique
    let mut column_set = HashSet::new();
    for (col, _) in &index.columns {
        if !column_set.insert(col) {
            return Err(Error::new_spanned(col, "duplicate columns in table-level index"));
        }
    }

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

fn primary_key_type(
    fields: &FieldsNamed,
    container_attrs: &ContainerAttributes,
    field_attrs: &[FieldAttributes],
    col_names: &[String],
) -> Result<TokenStream, Error> {
    assert_eq!(fields.named.len(), field_attrs.len());
    assert_eq!(fields.named.len(), col_names.len());

    let input_lt = &container_attrs.input_lt;
    let names_to_sql_types: HashMap<&str, &Type> = fields.named
        .iter()
        .zip(field_attrs)
        .zip(col_names)
        .map(|((field, attrs), name)| {
            // respect `#[nanosql(sql_ty = "...")]` attribute
            (name.as_str(), attrs.sql_ty.as_ref().unwrap_or(&field.ty))
        })
        .collect();

    if let Some(pk_cols) = container_attrs.primary_key.as_ref() {
        let types: Vec<TokenStream> = pk_cols
            .iter()
            .map(|col| {
                names_to_sql_types
                    .get(col.to_string().as_str())
                    .map(|sql_ty| quote!{
                        <#sql_ty as ::nanosql::AsSqlTy>::Borrowed<#input_lt>
                    })
                    .ok_or_else(|| Error::new_spanned(col, format_args!("unknown PK column: {col}")))
            })
            .collect::<Result<_, _>>()?;

        // add an explicit trailing comma in case the PK is composed of a single column only
        Ok(quote!{
            (#(#types,)*)
        })
    } else if let Some((field, attrs)) = fields.named
        .iter()
        .zip(field_attrs)
        .find(|(_, attrs)| SpannedValue::into_inner(attrs.primary_key))
    {
        // respect `[nanosql(sql_ty = "...")]` attribute
        let sql_ty = attrs.sql_ty.as_ref().unwrap_or(&field.ty);
        Ok(quote!(<#sql_ty as ::nanosql::AsSqlTy>::Borrowed<#input_lt>))
    } else {
        Ok(quote!(::nanosql::table::PrimaryKeyMissing))
    }
}
