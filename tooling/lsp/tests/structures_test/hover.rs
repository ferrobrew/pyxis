use pyxis_lsp::state::ServerState;

use crate::helpers::*;

#[test]
fn structural_hovers() {
    let (st, uri) = project();

    let (l, c) = at("GameObject", 7); // the type name
    assert!(hover_text(&st, &uri, l, c).contains("**type** `GameObject`"));

    let (l, c) = at("transform", 0); // field name
    let h = hover_text(&st, &uri, l, c);
    assert!(
        h.contains("pub transform: Matrix4"),
        "field hover shows its signature: {h}"
    );

    let (l, c) = at("destructor", 0); // vftable fn
    assert!(hover_text(&st, &uri, l, c).contains("**fn** `destructor`"));

    let (l, c) = at("Matrix4", 12); // field type
    assert!(hover_text(&st, &uri, l, c).contains("**type** `Matrix4`"));

    let (l, c) = at("Matrix4", 2); // type inside a cfg-gated `use`
    assert!(hover_text(&st, &uri, l, c).contains("**type** `Matrix4`"));

    let (l, c) = at("release", 0); // impl method (in a cfg-gated block)
    assert!(hover_text(&st, &uri, l, c).contains("**fn** `release`"));

    let (l, c) = at("GameObject", 17); // impl target
    assert!(hover_text(&st, &uri, l, c).contains("**type** `GameObject`"));
}

#[test]
fn references_resolve_despite_size_error() {
    // #[size(0x18)] is wrong (real size differs) → GameObject has a semantic
    // error and drops from the type registry, but hover/nav must still work.
    let (st, uri) = project();
    let (l, c) = at("GameObject", 17); // impl target of the errored type
    assert!(
        hover_text(&st, &uri, l, c).contains("**type** `GameObject`"),
        "references to error-state types must still resolve"
    );
}

#[test]
fn field_hover_shows_offset() {
    // All-u64 fields lay out with no implicit padding, so the type resolves
    // and field offsets are available.
    let src = "pub type S {\n    pub a: u64,\n    pub b: u64,\n    pub c: u64,\n}\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("s.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "s.pyxis");
    let c = src.lines().nth(2).unwrap().find('b').unwrap() as u32 + 1;
    let h = hover_text(&st, &uri, 2, c);
    assert!(
        h.contains("offset `0x8`"),
        "field b should be at offset 0x8: {h}"
    );
}

#[test]
fn backend_for_type_navigates() {
    let main = "use types::shared::SharedPtr;\n\n#[cfg(backend = \"cpp\")]\nepilogue for SharedPtr r#\"// code\"#;\n";
    let st = ServerState::in_memory(&[(
        "/proj",
        8,
        &[
            (
                "types/shared.pyxis",
                "pub type SharedPtr {\n    pub ptr: u64,\n}\n",
            ),
            ("m.pyxis", main),
        ],
    )]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");
    let c = main.lines().nth(3).unwrap().find("SharedPtr").unwrap() as u32 + 2;
    assert!(
        hover_text(&st, &uri, 3, c).contains("**type** `SharedPtr`"),
        "hovering the `for <Type>` target should describe the type"
    );
}

#[test]
fn predefined_field_type_hovers_the_type() {
    let src = "pub type S {\n    pub flag: bool,\n}\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("s.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "s.pyxis");
    let c = src.lines().nth(1).unwrap().find("bool").unwrap() as u32 + 1;
    let h = hover_text(&st, &uri, 1, c);
    assert!(
        h.contains("**builtin** `bool`"),
        "hovering bool should describe bool, not the field: {h}"
    );
}

#[test]
fn function_args_and_self() {
    let src = "pub type Foo {\n    pub x: u64,\n    vftable {\n        pub fn doit(&mut self, count: u32);\n    },\n}\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("foo.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "foo.pyxis");
    let l = src.lines().nth(3).unwrap();
    assert!(
        hover_text(&st, &uri, 3, l.find("count").unwrap() as u32 + 1).contains("**arg** `count`")
    );
    assert!(
        hover_text(&st, &uri, 3, l.find("self").unwrap() as u32 + 1).contains("**type** `Foo`")
    );
}

#[test]
fn enum_variant_shows_value() {
    let src = "pub enum E: u32 {\n    A,\n    B = 5,\n    C,\n}\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("e.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "e.pyxis");
    // auto-incremented after B=5 → C=6
    let cc = src.lines().nth(3).unwrap().find('C').unwrap() as u32;
    let h = hover_text(&st, &uri, 3, cc);
    assert!(
        h.contains("**variant** `C`"),
        "should describe the variant, not the enum: {h}"
    );
    assert!(
        h.contains("value `6`"),
        "auto-incremented value should be 6: {h}"
    );
}

#[test]
fn attribute_hover_describes_attribute() {
    let src = "#[size(0x10)]\npub type Foo {\n    #[base]\n    pub p: u64,\n}\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("foo.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "foo.pyxis");
    let h = hover_text(&st, &uri, 0, 3); // #[size(0x10)]
    assert!(
        h.contains("**attribute**") && h.contains("#[size(0x10)]"),
        "got {h}"
    );
    let b = hover_text(&st, &uri, 2, 7); // #[base]
    assert!(
        b.contains("**attribute**") && b.contains("base class"),
        "got {b}"
    );
}

#[test]
fn cfg_attribute_on_use_and_splice_hovers() {
    // A `#[cfg(...)]` gate on a `use` or a `prologue`/`epilogue` splice must
    // hover as an attribute, just like a cfg on an item.
    let src = "#[cfg(backend = \"cpp\")]\nuse types::math::Matrix4;\n\n#[cfg(backend = \"rust\")]\nepilogue r#\"// x\"#;\n";
    let st = ServerState::in_memory(&[(
        "/proj",
        8,
        &[
            (
                "types/math.pyxis",
                "pub type Matrix4 {\n    pub data: [f32; 16],\n}\n",
            ),
            ("m.pyxis", src),
        ],
    )]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");

    // The cfg gate on the `use` (line 0).
    let cu = src.lines().next().unwrap().find("cfg").unwrap() as u32 + 1;
    let hu = hover_text(&st, &uri, 0, cu);
    assert!(
        hu.contains("**attribute**") && hu.contains("cfg"),
        "cfg on a use should hover as an attribute: {hu}"
    );

    // The cfg gate on the splice (line 3).
    let cs = src.lines().nth(3).unwrap().find("cfg").unwrap() as u32 + 1;
    let hs = hover_text(&st, &uri, 3, cs);
    assert!(
        hs.contains("**attribute**") && hs.contains("cfg"),
        "cfg on a splice should hover as an attribute: {hs}"
    );
}

#[test]
fn free_functions_hover() {
    let src = "pub type T {\n    pub x: u64,\n}\nfn free_fn(item: *const T) -> bool;\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");
    let l = src.lines().nth(3).unwrap();
    assert!(
        hover_text(&st, &uri, 3, l.find("free_fn").unwrap() as u32 + 1)
            .contains("**fn** `free_fn`")
    );
    assert!(
        hover_text(&st, &uri, 3, l.find("item").unwrap() as u32 + 1).contains("**arg** `item`")
    );
    assert!(hover_text(&st, &uri, 3, l.find('T').unwrap() as u32).contains("**type** `T`"));
}

#[test]
fn extern_value_and_type_hover() {
    // extern value references a type defined later (forward ref).
    let src = "#[address(0x100)]\nextern foo: Bar;\n\npub type Bar {\n    pub x: u64,\n}\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");
    let l = src.lines().nth(1).unwrap();
    assert!(
        hover_text(&st, &uri, 1, l.find("foo").unwrap() as u32 + 1)
            .contains("**extern value** `foo`")
    );
    assert!(
        hover_text(&st, &uri, 1, l.find("Bar").unwrap() as u32 + 1).contains("**type** `Bar`"),
        "forward-referenced extern type should resolve"
    );
}

#[test]
fn extern_value_hover_shows_address_and_resolves_types() {
    // A module-level extern with a pointer type, a nested extern in a type body,
    // and the required `#[address]` surfaced on the name hover.
    let src = "#[address(0x100)]\npub extern g_engine: *mut Bar;\n\npub type Owner {\n    #[address(0x200)]\n    pub extern g_inst: *mut Bar,\n    pub x: u64,\n}\n\npub type Bar {\n    pub x: u64,\n}\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");
    let lines: Vec<&str> = src.lines().collect();

    // The name hover shows the extern's fixed address.
    let l1 = lines[1];
    let name_hover = hover_text(&st, &uri, 1, l1.find("g_engine").unwrap() as u32 + 1);
    assert!(
        name_hover.contains("**extern value** `g_engine`") && name_hover.contains("0x100"),
        "extern name hover should show the address: got {name_hover:?}"
    );

    // The pointee type resolves on a module-level extern...
    assert!(
        hover_text(&st, &uri, 1, l1.find("Bar").unwrap() as u32 + 1).contains("**type** `Bar`"),
        "module-level extern pointee should resolve"
    );

    // ...and on a nested extern inside a type body.
    let l5 = lines[5];
    assert!(
        hover_text(&st, &uri, 5, l5.find("Bar").unwrap() as u32 + 1).contains("**type** `Bar`"),
        "nested extern pointee should resolve"
    );
}

#[test]
fn pointer_and_array_shells() {
    let src = "pub type Foo {\n    pub x: u64,\n}\npub type T {\n    pub p: *mut Foo,\n    pub arr: [Foo; 4],\n}\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");
    let l4 = src.lines().nth(4).unwrap();
    assert!(hover_text(&st, &uri, 4, l4.find('*').unwrap() as u32).contains("**pointer**"));
    assert!(hover_text(&st, &uri, 4, l4.find("Foo").unwrap() as u32).contains("**type** `Foo`"));
    let l5 = src.lines().nth(5).unwrap();
    assert!(hover_text(&st, &uri, 5, l5.find('[').unwrap() as u32).contains("**array**"));
    assert!(hover_text(&st, &uri, 5, l5.find("Foo").unwrap() as u32).contains("**type** `Foo`"));
}

#[test]
fn splice_terms_hover() {
    let src = "pub type Foo {\n    pub x: u64,\n}\n#[cfg(backend = \"rust\")]\nepilogue for Foo r#\"\n    for x in 0..3 {}\n\"#;\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");
    // The `epilogue` keyword hovers as a splice term.
    assert!(
        hover_text(
            &st,
            &uri,
            4,
            src.lines().nth(4).unwrap().find("epilogue").unwrap() as u32
        )
        .contains("**splice**")
    );
    // `for` in the `for Foo` attribution clause hovers as a splice term.
    assert!(
        hover_text(
            &st,
            &uri,
            4,
            src.lines().nth(4).unwrap().find("for").unwrap() as u32
        )
        .contains("**splice**")
    );
    // `for` inside the spliced code must NOT be treated as a splice keyword.
    let c5 = src.lines().nth(5).unwrap().find("for").unwrap() as u32;
    let h5 = hover_text(&st, &uri, 5, c5);
    assert!(h5.is_empty() || !h5.contains("**splice**"));
}

#[test]
fn vftable_keyword_describes_struct() {
    let src = "pub type Foo {\n    vftable {\n        pub fn a(&mut self);\n        pub fn b(&mut self);\n    },\n}\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");
    let c = src.lines().nth(1).unwrap().find("vftable").unwrap() as u32 + 2;
    let h = hover_text(&st, &uri, 1, c);
    assert!(
        h.contains("**vftable**") && h.contains("`2` virtual"),
        "got {h}"
    );
}

#[test]
fn pointer_shell_in_function_signature() {
    let src = "pub type Foo {\n    vftable {\n        pub fn f(&mut self, mat: *const f32) -> *const u32;\n    },\n}\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");
    let l = src.lines().nth(2).unwrap();
    // the `*const` of the argument and of the return type both describe the pointer
    assert!(hover_text(&st, &uri, 2, l.find("*const f32").unwrap() as u32).contains("**pointer**"));
    assert!(hover_text(&st, &uri, 2, l.find("*const u32").unwrap() as u32).contains("**pointer**"));
    assert!(hover_text(&st, &uri, 2, l.find("f32").unwrap() as u32).contains("**builtin** `f32`"));
}

#[test]
fn shell_in_type_alias_target() {
    // Hovering the `*const` shell of a type-alias target describes the pointer,
    // while the pointee still resolves to its type.
    let src = "pub type Foo {\n    pub x: u64,\n}\npub type Alias = *const Foo;\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");
    let l = src.lines().nth(3).unwrap();
    assert!(
        hover_text(&st, &uri, 3, l.find('*').unwrap() as u32).contains("**pointer**"),
        "alias target pointer shell describes the pointer"
    );
    assert!(
        hover_text(&st, &uri, 3, l.find("Foo").unwrap() as u32).contains("**type** `Foo`"),
        "alias target pointee still resolves"
    );
}

#[test]
fn shell_in_extern_value_type() {
    // Hovering the `*const` shell of an extern value's type describes the pointer.
    let src = "pub type Foo {\n    pub x: u64,\n}\n#[address(0x100)]\nextern p: *const Foo;\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");
    let l = src.lines().nth(4).unwrap();
    assert!(
        hover_text(&st, &uri, 4, l.find('*').unwrap() as u32).contains("**pointer**"),
        "extern value pointer shell describes the pointer"
    );
    assert!(
        hover_text(&st, &uri, 4, l.find("Foo").unwrap() as u32).contains("**type** `Foo`"),
        "extern value pointee still resolves"
    );
}

#[test]
fn shell_in_generic_argument() {
    // Hovering the `*const` shell *inside* a generic argument describes the
    // inner pointer, not the outer generic type.
    let src = "pub type Foo {\n    pub x: u64,\n}\npub type SharedPtr<T> {\n    pub ptr: *mut T,\n}\npub type Holder {\n    pub h: SharedPtr<*const Foo>,\n}\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");
    let l = src.lines().nth(7).unwrap(); // `    pub h: SharedPtr<*const Foo>,`
    // The `*const` inside the generic arg is a pointer shell, not SharedPtr.
    let star = l.find('*').unwrap() as u32;
    let h = hover_text(&st, &uri, 7, star);
    assert!(
        h.contains("**pointer**"),
        "inner generic-arg pointer shell describes the pointer: {h}"
    );
    // The outer generic name still resolves to its type.
    assert!(
        hover_text(&st, &uri, 7, l.find("SharedPtr").unwrap() as u32)
            .contains("**type** `SharedPtr`"),
        "outer generic still resolves"
    );
    // The innermost pointee still resolves to its type.
    assert!(
        hover_text(&st, &uri, 7, l.find("Foo").unwrap() as u32).contains("**type** `Foo`"),
        "generic-arg pointee still resolves"
    );
}

#[test]
fn attribute_hover_on_free_function_and_extern() {
    let src =
        "#[address(0x100)]\nfn do_thing(x: u32) -> bool;\n\n#[address(0x200)]\nextern foo: u32;\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");
    assert!(
        hover_text(&st, &uri, 0, 3).contains("**attribute**"),
        "free-function attribute"
    );
    assert!(
        hover_text(&st, &uri, 3, 3).contains("**attribute**"),
        "extern value attribute"
    );
}

#[test]
fn vftable_function_hover_shows_index_and_offset() {
    let src = "pub type Foo {\n    vftable {\n        pub fn a(&mut self);\n        pub fn b(&mut self);\n        #[index(5)]\n        pub fn c(&mut self);\n    },\n}\n";
    let st = ServerState::in_memory(&[("/proj", 8, &[("m.pyxis", src)])]);
    let uri = ServerState::document_uri("/proj", "m.pyxis");
    let col = |l: usize, n: &str| src.lines().nth(l).unwrap().find(n).unwrap() as u32;
    let a = hover_text(&st, &uri, 2, col(2, "a"));
    assert!(
        a.contains("index `0`") && a.contains("vftable offset `0x0`"),
        "{a}"
    );
    let b = hover_text(&st, &uri, 3, col(3, "b"));
    assert!(
        b.contains("index `1`") && b.contains("vftable offset `0x8`"),
        "{b}"
    );
    // #[index(5)] resets the running counter → offset 5 * 8 = 0x28.
    let c = hover_text(&st, &uri, 5, col(5, "c"));
    assert!(
        c.contains("index `5`") && c.contains("vftable offset `0x28`"),
        "{c}"
    );
}
