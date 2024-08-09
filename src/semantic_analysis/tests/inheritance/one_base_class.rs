use super::*;
use dsl::*;

fn vfunc(name: impl Into<String>) -> IF {
    IF::new(
        name,
        [
            IFA::MutSelf,
            IFA::field("arg0", IPT::U32),
            IFA::field("arg1", IPT::F32),
        ],
        IPT::I32,
    )
}

#[test]
fn b0_d0() {
    let base = IT::new(
        "Base",
        8,
        [
            (None, "field_1".to_string(), IPT::I32, vec![]),
            (None, "_field_4".to_string(), IPT::Unknown(4), vec![]),
            (None, "field_2".to_string(), IPT::U64, vec![]),
        ],
    );

    let derived = IT::new(
        "Derived",
        8,
        [(
            None,
            "base".to_string(),
            IPT::Named("Base".to_string(), 16),
            vec![A::base()],
        )],
    );

    assert_ast_produces_type_definitions(
        M::new().with_definitions([base.to_grammar(), derived.to_grammar()]),
        [
            base.to_semantic(None, true),
            derived.to_semantic(None, true),
        ],
    );
}

#[test]
fn b0_d1() {
    let base = IT::new(
        "Base",
        8,
        [
            (None, "field_1".to_string(), IPT::I32, vec![]),
            (None, "_field_4".to_string(), IPT::Unknown(4), vec![]),
            (None, "field_2".to_string(), IPT::U64, vec![]),
        ],
    );

    let derived_vftable = IV::new("Derived", "DerivedVftable", [vfunc("derived_vfunc")]);
    let derived = IT::new(
        "Derived",
        8,
        [(
            Some(8),
            "base".to_string(),
            IPT::Named("Base".to_string(), 16),
            vec![A::base()],
        )],
    )
    .with_vftable(derived_vftable.clone());

    assert_ast_produces_type_definitions(
        M::new().with_definitions([base.to_grammar(), derived.to_grammar()]),
        [
            base.to_semantic(None, true),
            derived.to_semantic(None, true),
            derived_vftable.to_semantic(),
        ],
    );
}

#[test]
fn b1_d0() {
    let base_vftable = IV::new("Base", "BaseVftable", [vfunc("base_vfunc")]);
    let base = IT::new(
        "Base",
        8,
        [
            (Some(8), "field_1".to_string(), IPT::I32, vec![]),
            (None, "_field_c".to_string(), IPT::Unknown(4), vec![]),
            (None, "field_2".to_string(), IPT::U64, vec![]),
        ],
    )
    .with_vftable(base_vftable.clone());

    let derived = IT::new(
        "Derived",
        8,
        [(
            None,
            "base".to_string(),
            IPT::Named("Base".to_string(), 24),
            vec![A::base()],
        )],
    );

    assert_ast_produces_type_definitions(
        M::new().with_definitions([base.to_grammar(), derived.to_grammar()]),
        [
            base.to_semantic(None, true),
            derived.to_semantic(
                Some(STV {
                    functions: base_vftable.to_semantic_functions(),
                    field_path: vec!["base".to_string(), "vftable".to_string()],
                    type_: base_vftable.to_semantic_pointer_type(),
                }),
                false,
            ),
            base_vftable.to_semantic(),
        ],
    );
}

#[test]
fn b1_d1() {
    let base_vftable = IV::new("Base", "BaseVftable", [vfunc("base_vfunc")]);
    let base = IT::new(
        "Base",
        8,
        [
            (Some(8), "field_1".to_string(), IPT::I32, vec![]),
            (None, "_field_c".to_string(), IPT::Unknown(4), vec![]),
            (None, "field_2".to_string(), IPT::U64, vec![]),
        ],
    )
    .with_vftable(base_vftable.clone());

    let derived_vftable = IV::new(
        "Derived",
        "DerivedVftable",
        [vfunc("base_vfunc"), vfunc("derived_vfunc")],
    );
    let derived = IT::new(
        "Derived",
        8,
        [(
            None,
            "base".to_string(),
            IPT::Named("Base".to_string(), 24),
            vec![A::base()],
        )],
    )
    .with_vftable(derived_vftable.clone());

    assert_ast_produces_type_definitions(
        M::new().with_definitions([base.to_grammar(), derived.to_grammar()]),
        [
            base.to_semantic(None, true),
            derived.to_semantic(
                Some(STV {
                    functions: derived_vftable.to_semantic_functions(),
                    field_path: vec!["base".to_string(), "vftable".to_string()],
                    type_: derived_vftable.to_semantic_pointer_type(),
                }),
                false,
            ),
            base_vftable.to_semantic(),
            derived_vftable.to_semantic(),
        ],
    );
}
