use std::fmt::Display;

use crate::{
    SemanticError,
    grammar::{AttributeItems, Expr, Ident},
    semantic::error::{AttributeName, Result},
    span::{HasLocation, ItemLocation},
};

macro_rules! parse_integer_attribute {
    ($function_name:ident, $attr_name:ident) => {
        pub fn $function_name(
            attr_ident: &Ident,
            items: &AttributeItems,
            fallback_location: &ItemLocation,
        ) -> Result<Option<usize>> {
            parse_single_integer(
                AttributeName::$attr_name,
                attr_ident,
                items,
                fallback_location,
            )
        }
    };
}

parse_integer_attribute!(parse_address, Address);
parse_integer_attribute!(parse_size, Size);
parse_integer_attribute!(parse_min_size, MinSize);
parse_integer_attribute!(parse_align, Align);
parse_integer_attribute!(parse_singleton, Singleton);
parse_integer_attribute!(parse_index, Index);

pub fn assert_function_argument_count<'a>(
    items: &'a AttributeItems,
    target_name: AttributeName,
    length: usize,
    fallback_location: &ItemLocation,
) -> Result<Vec<&'a Expr>> {
    let exprs = items.exprs_vec();
    if exprs.len() != length {
        return Err(SemanticError::InvalidAttributeFunctionArgumentCount {
            attribute_name: target_name,
            expected_count: length,
            actual_count: exprs.len(),
            location: *fallback_location,
        });
    }
    Ok(exprs)
}

fn parse_single_integer<T: TryFrom<isize> + Display>(
    target_name: AttributeName,
    attr_ident: &Ident,
    items: &AttributeItems,
    fallback_location: &ItemLocation,
) -> Result<Option<T>> {
    fn parse_single_integer_attribute_impl<T: TryFrom<isize> + Display>(
        target_name: AttributeName,
        items: &AttributeItems,
        fallback_location: &ItemLocation,
    ) -> Result<T> {
        let exprs = assert_function_argument_count(items, target_name, 1, fallback_location)?;
        integer_expr(exprs[0], target_name)
    }

    (attr_ident.as_str() == target_name.to_string())
        .then(|| parse_single_integer_attribute_impl(target_name, items, fallback_location))
        .transpose()
}

fn integer_expr<T: TryFrom<isize> + Display>(
    expr: &Expr,
    attribute_name: AttributeName,
) -> Result<T> {
    let Expr::IntLiteral { value, .. } = expr else {
        return Err(SemanticError::InvalidAttributeValue {
            attribute_name,
            expected_type: std::any::type_name::<T>().into(),
            location: *expr.location(),
        });
    };
    let value = (*value)
        .try_into()
        .map_err(|_| SemanticError::IntegerConversion {
            value: value.to_string(),
            target_type: std::any::type_name::<T>().into(),
            location: *expr.location(),
        })?;
    Ok(value)
}
