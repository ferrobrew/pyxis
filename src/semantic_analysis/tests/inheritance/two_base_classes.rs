use super::*;
use dsl::*;

fn derived_vfunc() -> IF {
    IF::new(
        "derived_vfunc",
        [
            IFA::MutSelf,
            IFA::field("arg0", IPT::U32),
            IFA::field("arg1", IPT::F32),
        ],
        IPT::I32,
    )
}

#[test]
fn a0_b0_d0() {
    let base_a = IT::new(
        "BaseA",
        8,
        [
            (None, "field_1".to_string(), IPT::I32, vec![]),
            (None, "_field_4".to_string(), IPT::Unknown(4), vec![]),
            (None, "field_2".to_string(), IPT::U64, vec![]),
        ],
    );

    let base_b = IT::new(
        "BaseB",
        8,
        [
            (None, "field_1".to_string(), IPT::U64, vec![]),
            (None, "_field_8".to_string(), IPT::Unknown(4), vec![]),
            (None, "field_2".to_string(), IPT::I32, vec![]),
        ],
    );

    let derived = IT::new(
        "Derived",
        8,
        [
            (
                None,
                "base_a".to_string(),
                IPT::Named("BaseA".to_string(), 16),
                vec![A::base()],
            ),
            (
                None,
                "base_b".to_string(),
                IPT::Named("BaseB".to_string(), 16),
                vec![A::base()],
            ),
        ],
    );

    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            base_a.to_grammar(),
            base_b.to_grammar(),
            derived.to_grammar(),
        ]),
        [
            base_a.to_semantic(None, true),
            base_b.to_semantic(None, true),
            derived.to_semantic(None, true),
        ],
    );
}

#[test]
fn a0_b0_d1() {
    let base_a = IT::new(
        "BaseA",
        8,
        [
            (None, "field_1".to_string(), IPT::I32, vec![]),
            (None, "_field_4".to_string(), IPT::Unknown(4), vec![]),
            (None, "field_2".to_string(), IPT::U64, vec![]),
        ],
    );

    let base_b = IT::new(
        "BaseB",
        8,
        [
            (None, "field_1".to_string(), IPT::U64, vec![]),
            (None, "_field_8".to_string(), IPT::Unknown(4), vec![]),
            (None, "field_2".to_string(), IPT::I32, vec![]),
        ],
    );

    let derived_vftable = IV::new("Derived", "DerivedVftable", [derived_vfunc()]);
    let derived = IT::new(
        "Derived",
        8,
        [
            (
                Some(8),
                "base_a".to_string(),
                IPT::Named("BaseA".to_string(), 16),
                vec![A::base()],
            ),
            (
                None,
                "base_b".to_string(),
                IPT::Named("BaseB".to_string(), 16),
                vec![A::base()],
            ),
        ],
    )
    .with_vftable(derived_vftable.clone());

    assert_ast_produces_type_definitions(
        M::new().with_definitions([
            base_a.to_grammar(),
            base_b.to_grammar(),
            derived.to_grammar(),
        ]),
        [
            base_a.to_semantic(None, true),
            base_b.to_semantic(None, true),
            derived.to_semantic(None, true),
            derived_vftable.to_semantic(),
        ],
    );
}
