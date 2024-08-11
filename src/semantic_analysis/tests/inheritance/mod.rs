//! Tests for inheritance of types with optional vftables.

use super::*;

mod multiple_levels;
mod one_base_class;
mod two_base_classes;

fn vfunc_grammar(name: &str) -> F {
    F::new(
        V::Public,
        name,
        [
            Ar::MutSelf,
            Ar::named("arg0", T::ident("u32")),
            Ar::named("arg1", T::ident("f32")),
        ],
    )
    .with_return_type(T::ident("i32"))
}

fn vfunc_region(name: &str, self_type: &str) -> SR {
    SR::field(
        SV::Public,
        name,
        ST::function(
            SCC::Thiscall,
            vec![
                ("this", ST::raw(self_type).mut_pointer()),
                ("arg0", ST::raw("u32")),
                ("arg1", ST::raw("f32")),
            ],
            ST::raw("i32"),
        ),
    )
}

fn vfunc_semantic(name: &str) -> SF {
    SF::new(SV::Public, name)
        .with_arguments([
            SAr::MutSelf,
            SAr::field("arg0", ST::raw("u32")),
            SAr::field("arg1", ST::raw("f32")),
        ])
        .with_return_type(ST::raw("i32"))
}

fn pad_up_to_8_region() -> SR {
    SR::field(
        SV::Private,
        format!("_field_{}", pointer_size()),
        ST::array(ST::raw("u8"), 8 - pointer_size()),
    )
}

fn filter_out_empty_regions(regions: impl IntoIterator<Item = SR>) -> Vec<SR> {
    regions
        .into_iter()
        .filter(|r| !matches!(&r.type_ref, ST::Array(_, 0)))
        .collect()
}
