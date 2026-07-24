//! Struct/vftable/method rendering (the C++ backend's largest single
//! surface): field layout, vftable accessors, method signatures/bodies, and
//! nested-item bodies for struct-shaped items.

use std::{collections::BTreeSet, fmt::Write};

use super::{RenderCtx, RenderedItem, items::format_const_value, template_clause};
use crate::{
    backends::{Result, cpp::runtime},
    semantic::types::{
        Argument, CallingConvention, ConstValue, Function, FunctionBody, ItemDefinitionInner,
        TypeDefinition, TypeVftable, Visibility,
    },
    span::ItemLocation,
};

#[allow(clippy::too_many_arguments)]
pub(super) fn render_struct(
    name: &str,
    td: &TypeDefinition,
    size: usize,
    alignment: usize,
    ctx: RenderCtx,
    visibility: Visibility,
    type_parameters: &[String],
    location: &ItemLocation,
) -> Result<RenderedItem> {
    let name = &*super::cpp_ident(name);
    let is_generic = !type_parameters.is_empty();

    // Names this class introduces into its own scope (data members and
    // methods). A same-module type reference whose leaf matches one of these
    // would resolve to the member instead of the type, so `render_path`
    // qualifies those references. Normalize through `cpp_ident` so the
    // comparison is against the emitted C++ names.
    let shadowed_members: BTreeSet<String> = td
        .regions
        .iter()
        .filter_map(|r| r.name.as_deref())
        .chain(td.associated_functions.iter().map(|f| f.name.as_str()))
        .chain(
            td.vftable
                .iter()
                .flat_map(|v| v.functions.iter().map(|f| f.name.as_str())),
        )
        .map(|n| super::cpp_ident(n).into_owned())
        .collect();
    let ctx = ctx.with_shadowed_members(&shadowed_members);

    let mut out = String::new();
    super::types::render_doc(&mut out, &td.doc, 0, ctx, location)?;
    if td.packed {
        writeln!(out, "#pragma pack(push, 1)")?;
    }
    let template = template_clause(type_parameters);
    let header = if is_generic {
        // Templates: alignment depends on T, so let the compiler infer via the
        // by-value field; skip explicit `alignas(N)`.
        format!("{template}struct {name}")
    } else {
        format!("struct alignas({alignment}) {name}")
    };

    // Build the struct body separately so we can detect "empty body" and
    // emit `Name {};` on one line instead of two.
    let mut body = String::new();

    // Deferred out-of-class constant definitions: for struct/array/const-ref
    // constants that can't be initialized in-class (C++ incomplete-type or
    // non-literal-type restrictions), we declare `static const T name;` in
    // the body and define `inline const T Class::name = ...;` after the class.
    let mut deferred_consts: Vec<(String, String, String)> = Vec::new();

    // Fields. Vftable structs (named `<ParentType>Vftable`) get their
    // function-pointer slots' first param replaced with `void*` so the
    // wrappers on derived types can pass `this` without an explicit cast
    // through the base chain. The slot's intent is "any pointer to the
    // declaring type or one of its bases" - encoding that in C++'s type
    // system without inheritance is awkward, so we use `void*` as the
    // ABI-compatible escape hatch.
    let is_vftable_struct = name.ends_with("Vftable");
    for region in &td.regions {
        super::items::render_field(&mut body, region, ctx, is_vftable_struct)?;
    }

    // Conversion operators for #[base] regions (composition-based upcast).
    for region in &td.regions {
        if !region.is_base {
            continue;
        }
        let Some(field_name) = region.name.as_deref() else {
            continue;
        };
        let base_type = super::render_type(&region.type_ref, ctx)?;
        writeln!(body)?;
        writeln!(
            body,
            "    operator {base_type}&() {{ return this->{field_name}; }}"
        )?;
        writeln!(
            body,
            "    operator const {base_type}&() const {{ return this->{field_name}; }}"
        )?;
    }

    // Singleton accessor (static).
    if td.singleton.is_some() {
        writeln!(body)?;
        writeln!(body, "    static {name}* singleton();")?;
    }

    // Vftable accessor + virtual-method wrapper signatures. Pyxis's
    // pub/private distinction is rust-only; in C++ we emit every method
    // (callers are free to ignore the rust-private ones, but `backend cpp
    // epilogue` blocks need to be able to call into them by name).
    if let Some(vftable) = &td.vftable {
        render_vftable_accessor_decl(&mut body, vftable, ctx)?;
        for func in &vftable.functions {
            if !ctx.cfg_passes(&func.cfg) {
                continue;
            }
            render_method_signature(&mut body, func, ctx)?;
        }
    }

    // Associated functions (impl block, e.g. `#[address(0x...)] pub fn foo()`).
    for func in &td.associated_functions {
        if !ctx.cfg_passes(&func.cfg) {
            continue;
        }
        render_method_signature(&mut body, func, ctx)?;
    }

    // Pinned types: delete copy/move constructors and assignment operators so
    // the type cannot be relocated in memory.
    if td.pinned {
        writeln!(body)?;
        writeln!(body, "    {name}(const {name}&) = delete;")?;
        writeln!(body, "    {name}({name}&&) = delete;")?;
        writeln!(body, "    {name}& operator=(const {name}&) = delete;")?;
        writeln!(body, "    {name}& operator=({name}&&) = delete;")?;
    }

    // Nested item declarations (enums, types, bitflags, type aliases)
    // are rendered inside the struct body.
    if !td.nested_item_paths.is_empty() {
        let mut prev_was_constant = false;
        for nested_path in &td.nested_item_paths {
            if let Ok(nested_item) = ctx.registry.get(nested_path, &ItemLocation::internal()) {
                if let Some(nested_resolved) = nested_item.resolved() {
                    let curr_is_constant =
                        matches!(&nested_resolved.inner, ItemDefinitionInner::Constant(_));
                    // Add blank line before nested item if body has content,
                    // unless both the previous and current items are constants
                    // (consecutive constants form a contiguous block).
                    if !body.trim().is_empty() && !(prev_was_constant && curr_is_constant) {
                        writeln!(body)?;
                    }
                    prev_was_constant = curr_is_constant;
                    let nested_name = nested_path
                        .last()
                        .map(|s| s.as_str().to_string())
                        .unwrap_or_default();
                    let nested_name = super::cpp_ident(&nested_name);
                    match &nested_resolved.inner {
                        ItemDefinitionInner::Type(nested_td) => {
                            super::types::render_doc(
                                &mut body,
                                &nested_td.doc,
                                1,
                                ctx,
                                &nested_item.location,
                            )?;
                            writeln!(body, "    struct {nested_name} {{")?;
                            let nested_had_fields = !nested_td.regions.is_empty();
                            for region in &nested_td.regions {
                                super::items::render_field_indented(
                                    &mut body, region, ctx, false, 2,
                                )?;
                            }
                            // Render nested constants inside the nested struct
                            let nested_has_consts = nested_td.nested_item_paths.iter().any(|p| {
                                ctx.registry
                                    .get(p, &ItemLocation::internal())
                                    .ok()
                                    .and_then(|i| i.resolved())
                                    .is_some_and(|r| {
                                        matches!(r.inner, ItemDefinitionInner::Constant(_))
                                    })
                            });
                            // Add blank line between fields and constants
                            if nested_had_fields && nested_has_consts {
                                writeln!(body)?;
                            }
                            for nested_nested_path in &nested_td.nested_item_paths {
                                if let Ok(nested_nested_item) = ctx
                                    .registry
                                    .get(nested_nested_path, &ItemLocation::internal())
                                    && let Some(nested_nested_resolved) =
                                        nested_nested_item.resolved()
                                    && let ItemDefinitionInner::Constant(nested_cd) =
                                        &nested_nested_resolved.inner
                                {
                                    let nested_const_name = nested_nested_path
                                        .last()
                                        .map(|s| s.as_str().to_string())
                                        .unwrap_or_default();
                                    let nested_const_name = super::cpp_ident(&nested_const_name);
                                    super::types::render_doc(
                                        &mut body,
                                        &nested_cd.doc,
                                        2,
                                        ctx,
                                        &nested_nested_item.location,
                                    )?;
                                    let bf_type = super::render_type(&nested_cd.type_, ctx)?;
                                    let value_str =
                                        format_const_value(&nested_cd.value, &nested_cd.type_);
                                    writeln!(
                                        body,
                                        "        static constexpr {bf_type} {nested_const_name} = {value_str};"
                                    )?;
                                }
                            }
                            writeln!(body, "    }};")?;
                        }
                        ItemDefinitionInner::Enum(nested_ed) => {
                            super::types::render_doc(
                                &mut body,
                                &nested_ed.doc,
                                1,
                                ctx,
                                &nested_item.location,
                            )?;
                            writeln!(
                                body,
                                "    enum class {nested_name} : {} {{",
                                super::render_type(&nested_ed.type_, ctx)?
                            )?;
                            for variant in &nested_ed.variants {
                                writeln!(
                                    body,
                                    "        {} = {},",
                                    super::cpp_ident(&variant.name),
                                    variant.value
                                )?;
                            }
                            writeln!(body, "    }};")?;
                        }
                        ItemDefinitionInner::Bitflags(nested_bd) => {
                            super::types::render_doc(
                                &mut body,
                                &nested_bd.doc,
                                1,
                                ctx,
                                &nested_item.location,
                            )?;
                            writeln!(body, "    struct {nested_name} {{")?;
                            let bf_type = super::render_type(&nested_bd.type_, ctx)?;
                            for flag in &nested_bd.flags {
                                writeln!(
                                    body,
                                    "        static constexpr {bf_type} {} = {};",
                                    super::cpp_ident(&flag.name),
                                    flag.value
                                )?;
                            }
                            writeln!(body, "    }};")?;
                        }
                        ItemDefinitionInner::TypeAlias(nested_ta) => {
                            super::types::render_doc(
                                &mut body,
                                &nested_ta.doc,
                                1,
                                ctx,
                                &nested_item.location,
                            )?;
                            writeln!(
                                body,
                                "    using {nested_name} = {};",
                                super::render_type(&nested_ta.target, ctx)?
                            )?;
                        }
                        ItemDefinitionInner::Constant(nested_cd) => {
                            super::types::render_doc(
                                &mut body,
                                &nested_cd.doc,
                                1,
                                ctx,
                                &nested_item.location,
                            )?;
                            let bf_type = super::render_type(&nested_cd.type_, ctx)?;
                            let value_str = format_const_value(&nested_cd.value, &nested_cd.type_);
                            // For scalar/POD types, use `static constexpr` with
                            // in-class initialization. For struct/array/const-ref
                            // types, the type may not be a C++ literal type (trivial
                            // ctor/dtor), and if the type is the enclosing class
                            // itself, it's incomplete inside the body. So declare
                            // `static const T name;` here (no initializer) and
                            // define `inline const T Class::name = ...;` after
                            // the class body (in `post_header`).
                            let needs_out_of_class = matches!(
                                &nested_cd.value,
                                ConstValue::Struct { .. }
                                    | ConstValue::Array(_)
                                    | ConstValue::ConstRef(_)
                            );
                            if needs_out_of_class {
                                writeln!(body, "    static const {bf_type} {nested_name};")?;
                                deferred_consts.push((nested_name.to_string(), bf_type, value_str));
                            } else {
                                writeln!(
                                    body,
                                    "    static constexpr {bf_type} {nested_name} = {value_str};"
                                )?;
                            }
                        }
                        ItemDefinitionInner::ExternValue(nested_ev) => {
                            // A nested extern value (e.g. a C++ class's static
                            // global) becomes a static accessor over its address.
                            // Declared here; defined out-of-class below (in the
                            // `.cpp` for non-templates, like the singleton
                            // accessor and member functions).
                            super::types::render_doc(
                                &mut body,
                                &nested_ev.doc,
                                1,
                                ctx,
                                &nested_item.location,
                            )?;
                            let ev_type = super::render_type(&nested_ev.type_, ctx)?;
                            writeln!(body, "    static {ev_type}& get_{nested_name}();")?;
                        }
                    }
                }
            }
        }
    }

    if body.trim().is_empty() {
        writeln!(out, "{header} {{}};")?;
    } else {
        writeln!(out, "{header} {{")?;
        out.push_str(&body);
        writeln!(out, "}};")?;
    }
    if td.packed {
        writeln!(out, "#pragma pack(pop)")?;
    }

    // Layout assertions. Generic templates can't sizeof/alignof at the
    // declaration site (size depends on T), so skip those.
    if !is_generic {
        if size > 0 {
            writeln!(out, "static_assert(sizeof({name}) == 0x{size:X});")?;
        }
        writeln!(out, "static_assert(alignof({name}) == {alignment});")?;
    }
    // Per-field offsetof asserts come in Phase 4 once the dep graph carries
    // the resolved offsets; size+alignment asserts above are the immediate
    // load-bearing checks.
    let _ = visibility;

    // Out-of-class definitions split between `post_header` and `post_cpp`:
    // - templates' member definitions must stay header-visible (the cpp
    //   compiler instantiates them at every use site).
    // - non-template member definitions land in the `.cpp` so the header
    //   sticks to declarations.
    //
    // Methods declared on a template type via a method-level template
    // parameter (`template<class Y>` clause on the method) must also
    // stay header-visible for the same reason.
    let mut post_header = String::new();
    let mut post_cpp = String::new();

    // Deferred out-of-class constant definitions: emit after the struct body
    // so the type is complete. `inline` (C++17) avoids ODR violations when
    // the header is included in multiple translation units.
    for (const_name, const_type, const_value) in &deferred_consts {
        writeln!(
            post_header,
            "inline const {const_type} {name}::{const_name} = {const_value};"
        )?;
    }
    if is_generic {
        if let Some(vftable) = &td.vftable {
            render_vftable_accessor_definition(&mut post_header, name, vftable, ctx)?;
            for func in &vftable.functions {
                if !ctx.cfg_passes(&func.cfg) {
                    continue;
                }
                render_method_definition(&mut post_header, name, func, ctx)?;
            }
        }
        for func in &td.associated_functions {
            if !ctx.cfg_passes(&func.cfg) {
                continue;
            }
            render_method_definition(&mut post_header, name, func, ctx)?;
        }
    } else {
        if let Some(addr) = td.singleton {
            writeln!(post_cpp, "{name}* {name}::singleton() {{")?;
            writeln!(
                post_cpp,
                "    return *reinterpret_cast<{name}**>(0x{addr:X});"
            )?;
            writeln!(post_cpp, "}}")?;
            writeln!(post_cpp)?;
        }
        if let Some(vftable) = &td.vftable {
            render_vftable_accessor_definition(&mut post_cpp, name, vftable, ctx)?;
            for func in &vftable.functions {
                if !ctx.cfg_passes(&func.cfg) {
                    continue;
                }
                if !func.method_type_parameters.is_empty() {
                    render_method_definition(&mut post_header, name, func, ctx)?;
                } else {
                    render_method_definition(&mut post_cpp, name, func, ctx)?;
                }
            }
        }
        for func in &td.associated_functions {
            if !ctx.cfg_passes(&func.cfg) {
                continue;
            }
            if !func.method_type_parameters.is_empty() {
                render_method_definition(&mut post_header, name, func, ctx)?;
            } else {
                render_method_definition(&mut post_cpp, name, func, ctx)?;
            }
        }
    }

    // Nested extern values: out-of-class static accessor definitions, declared
    // as `static T& get_<name>();` in the body above. Non-template parents put
    // them in the `.cpp`; templates must keep them header-visible.
    {
        let def_out = if is_generic {
            &mut post_header
        } else {
            &mut post_cpp
        };
        for nested_path in &td.nested_item_paths {
            let Ok(nested_item) = ctx.registry.get(nested_path, &ItemLocation::internal()) else {
                continue;
            };
            let Some(nested_resolved) = nested_item.resolved() else {
                continue;
            };
            if let ItemDefinitionInner::ExternValue(nested_ev) = &nested_resolved.inner {
                let value_name = nested_path.last().map(|s| s.as_str()).unwrap_or_default();
                let value_name = super::cpp_ident(value_name);
                let ev_type = super::render_type(&nested_ev.type_, ctx)?;
                writeln!(def_out, "{ev_type}& {name}::get_{value_name}() {{")?;
                writeln!(
                    def_out,
                    "    return *reinterpret_cast<{ev_type}*>(0x{addr:X});",
                    addr = nested_ev.address
                )?;
                writeln!(def_out, "}}")?;
                writeln!(def_out)?;
            }
        }
    }

    Ok(RenderedItem {
        decl: out,
        post_header,
        post_cpp,
    })
}

fn render_vftable_accessor_decl(
    out: &mut String,
    vftable: &TypeVftable,
    ctx: RenderCtx,
) -> Result<()> {
    let vftable_type = super::render_type(&vftable.type_, ctx)?;
    writeln!(out)?;
    writeln!(out, "    {vftable_type} _vftable_ptr() const;")?;
    Ok(())
}

fn render_vftable_accessor_definition(
    out: &mut String,
    parent_name: &str,
    vftable: &TypeVftable,
    ctx: RenderCtx,
) -> Result<()> {
    let vftable_type = super::render_type(&vftable.type_, ctx)?;
    writeln!(out, "{vftable_type} {parent_name}::_vftable_ptr() const {{")?;
    if let Some(base_field) = &vftable.base_field {
        writeln!(
            out,
            "    return reinterpret_cast<{vftable_type}>(this->{base_field}._vftable_ptr());"
        )?;
    } else {
        writeln!(out, "    return this->vftable;")?;
    }
    writeln!(out, "}}")?;
    // Trailing blank to keep adjacent definitions separated.
    writeln!(out)?;
    Ok(())
}

/// In-class method declaration (signature only).
fn render_method_signature(out: &mut String, func: &Function, ctx: RenderCtx) -> Result<()> {
    if func.name.starts_with("_vfunc_") {
        return Ok(());
    }
    super::types::render_doc(out, &func.doc, 1, ctx, &func.location)?;
    let (return_text, sig_args_text, const_qual) = method_sig_parts(func, ctx)?;
    let static_kw = if func_has_self(func) { "" } else { "static " };
    // Method-level template parameters (e.g. `Y` in
    // `impl<T, Y> Foo<T> { fn cast() -> Foo<Y>; }`) become a `template
    // <class Y>` clause on the in-class declaration. The struct's own
    // template clause is emitted separately by `render_struct`.
    if !func.method_type_parameters.is_empty() {
        let params = func
            .method_type_parameters
            .iter()
            .map(|p| format!("class {p}"))
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(out, "    template <{params}>")?;
    }
    writeln!(
        out,
        "    {static_kw}{return_text} {fn_name}({sig_args_text}){const_qual};",
        fn_name = super::cpp_ident(&func.name)
    )?;
    Ok(())
}

fn func_has_self(func: &Function) -> bool {
    func.arguments
        .iter()
        .any(|a| matches!(a, Argument::ConstSelf { .. } | Argument::MutSelf { .. }))
}

/// Out-of-class method definition. The caller decides whether the
/// definition lands in the header (templates) or the .cpp (everything
/// else); the `header_inline` flag controls the leading `inline` keyword
/// and any required `template <...>` clauses for template / method-level
/// template parameters.
fn render_method_definition(
    out: &mut String,
    parent_name: &str,
    func: &Function,
    ctx: RenderCtx,
) -> Result<()> {
    if func.name.starts_with("_vfunc_") {
        return Ok(());
    }
    // External-body methods get their out-of-class definition from the
    // user's `backend cpp epilogue`; the in-class declaration was already
    // emitted by `render_method_signature`.
    if func.body.is_external() {
        return Ok(());
    }
    let (return_text, sig_args_text, const_qual) = method_sig_parts(func, ctx)?;
    let body_lines = method_body_lines(func, ctx)?;
    // Method-level template parameters require a `template <...>` clause
    // on the out-of-class definition. Without method-level templates, the
    // definition lands in the .cpp file where `inline` would be wrong;
    // with them, it lands in the header and stays implicitly inline as a
    // template.
    if !func.method_type_parameters.is_empty() {
        let params = func
            .method_type_parameters
            .iter()
            .map(|p| format!("class {p}"))
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(out, "template <{params}>")?;
    }
    // Out-of-class definitions never repeat `static` — that keyword belongs
    // only on the in-class declaration.
    writeln!(
        out,
        "{return_text} {parent_name}::{fn_name}({sig_args_text}){const_qual} {{",
        fn_name = super::cpp_ident(&func.name)
    )?;
    for line in &body_lines {
        writeln!(out, "    {line}")?;
    }
    writeln!(out, "}}")?;
    // Trailing blank line so adjacent method definitions get separated.
    writeln!(out)?;
    Ok(())
}

pub(super) fn method_sig_parts(
    func: &Function,
    ctx: RenderCtx,
) -> Result<(String, String, &'static str)> {
    let return_text = if let Some(ret) = &func.return_type {
        super::render_type(ret, ctx)?
    } else {
        "void".to_string()
    };

    let mut sig_args: Vec<String> = Vec::new();
    let mut self_kind: Option<&'static str> = None;
    for arg in &func.arguments {
        match arg {
            Argument::ConstSelf { .. } => self_kind = Some("const"),
            Argument::MutSelf { .. } => self_kind = Some("mut"),
            Argument::Field { name, type_, .. } => {
                let ty = super::render_type(type_, ctx)?;
                let escaped = super::cpp_ident(name);
                sig_args.push(format!("{ty} {escaped}"));
            }
        }
    }
    let const_qual = if matches!(self_kind, Some("const")) {
        " const"
    } else {
        ""
    };
    Ok((return_text, sig_args.join(", "), const_qual))
}

fn method_body_lines(func: &Function, ctx: RenderCtx) -> Result<Vec<String>> {
    let return_text = if let Some(ret) = &func.return_type {
        super::render_type(ret, ctx)?
    } else {
        "void".to_string()
    };
    let mut call_args: Vec<String> = Vec::new();
    let mut has_self = false;
    for arg in &func.arguments {
        match arg {
            Argument::ConstSelf { .. } | Argument::MutSelf { .. } => has_self = true,
            Argument::Field { name, .. } => call_args.push(super::cpp_ident(name).into_owned()),
        }
    }
    let ret_kw = if return_text == "void" { "" } else { "return " };
    Ok(match &func.body {
        FunctionBody::Address { address } => {
            let cc = calling_conv_macro(func.calling_convention);
            let arg_types_text = func
                .arguments
                .iter()
                .map(|a| match a {
                    Argument::ConstSelf { .. } => Ok("const void*".to_string()),
                    Argument::MutSelf { .. } => Ok("void*".to_string()),
                    Argument::Field { type_, .. } => super::render_type(type_, ctx),
                })
                .collect::<Result<Vec<_>>>()?
                .join(", ");
            let mut call_payload = String::new();
            if has_self {
                call_payload.push_str("this");
                if !call_args.is_empty() {
                    call_payload.push_str(", ");
                }
            }
            call_payload.push_str(&call_args.join(", "));
            vec![
                format!("using fn_t = {return_text} ({cc}*)({arg_types_text});"),
                format!("{ret_kw}reinterpret_cast<fn_t>(0x{address:X})({call_payload});"),
            ]
        }
        FunctionBody::Vftable { function_name } => {
            // Vftable slots take `void*` for the receiver (see the matching
            // change in vftable-struct emission). Any derived type's `this`
            // implicitly converts to `void*`, so the wrapper compiles
            // regardless of where in the base chain the slot was declared.
            let mut call_payload = String::new();
            call_payload.push_str("this");
            if !call_args.is_empty() {
                call_payload.push_str(", ");
            }
            call_payload.push_str(&call_args.join(", "));
            let function_name = super::cpp_ident(function_name);
            vec![format!(
                "{ret_kw}_vftable_ptr()->{function_name}({call_payload});"
            )]
        }
        FunctionBody::Field {
            field,
            function_name,
        } => {
            let call_payload = call_args.join(", ");
            let field = super::cpp_ident(field);
            let function_name = super::cpp_ident(function_name);
            vec![format!(
                "{ret_kw}this->{field}.{function_name}({call_payload});"
            )]
        }
        // External-body methods are declared in-class but defined in the
        // user's `backend cpp epilogue` block; render_method_definition
        // checks for this and skips emitting an out-of-class definition.
        FunctionBody::External => {
            let _ = (has_self, ret_kw);
            Vec::new()
        }
    })
}

/// Map a calling convention to its `PYXIS_*` shim macro (with a
/// trailing space, suitable for concatenation into a function-pointer
/// signature). Defined in [`runtime`] so both this lookup and the
/// runtime header's `#define`s share the same table.
pub(super) fn calling_conv_macro(cc: CallingConvention) -> &'static str {
    runtime::macro_emit(cc)
}
