//! Struct/vftable/method rendering (the C++ backend's largest single
//! surface): field layout, vftable accessors, method signatures/bodies, and
//! nested-item bodies for struct-shaped items.

use std::{collections::BTreeSet, fmt::Write};

use super::{RenderCtx, RenderedItem, template_clause};
use crate::{
    backends::{Result, cpp::runtime},
    semantic::types::{
        Argument, CallingConvention, Function, FunctionBody, TypeDefinition, TypeVftable,
        Visibility,
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
    let shadowed_members = compute_shadowed_members(td);
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

    // Type-declaring nested items (types, unions, enums, bitflags, aliases)
    // come before the fields so a field inside this struct may reference one
    // (`Outer::Inner`). Value items (constants, extern values) are emitted
    // after the fields.
    super::nested::render_type_declarations(&mut body, &td.nested_item_paths, ctx)?;

    for region in &td.regions {
        super::items::render_field(&mut body, region, ctx, is_vftable_struct)?;
    }

    render_base_conversion_operators(&mut body, td, ctx)?;
    render_singleton_declaration(&mut body, name, td)?;
    render_vftable_declarations(&mut body, td, ctx)?;
    render_associated_function_declarations(&mut body, td, ctx)?;
    super::nested::render_value_declarations(
        &mut body,
        &td.nested_item_paths,
        ctx,
        &mut deferred_consts,
    )?;
    render_deleted_special_members(&mut body, name, td)?;

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

    render_layout_assertions(&mut out, name, size, alignment, is_generic)?;
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
    render_out_of_class_method_definitions(
        &mut post_header,
        &mut post_cpp,
        name,
        td,
        is_generic,
        ctx,
    )?;

    // Nested extern values: out-of-class static accessor definitions, declared
    // as `static T& get_<name>();` in the body above. Non-template parents put
    // them in the `.cpp`; templates must keep them header-visible.
    let def_out = if is_generic {
        &mut post_header
    } else {
        &mut post_cpp
    };
    super::nested::render_extern_value_definitions(def_out, &td.nested_item_paths, name, ctx)?;

    Ok(RenderedItem {
        decl: out,
        post_header,
        post_cpp,
    })
}

/// Names this class introduces into its own scope (data members and methods).
/// A same-module type reference whose leaf matches one of these would resolve
/// to the member instead of the type, so `render_path` qualifies those
/// references. Normalized through `cpp_ident` so the comparison is against the
/// emitted C++ names.
fn compute_shadowed_members(td: &TypeDefinition) -> BTreeSet<String> {
    td.regions
        .iter()
        .filter_map(|r| r.name.as_deref())
        .chain(td.associated_functions.iter().map(|f| f.name.as_str()))
        .chain(
            td.vftable
                .iter()
                .flat_map(|v| v.functions.iter().map(|f| f.name.as_str())),
        )
        .map(|n| super::cpp_ident(n).into_owned())
        .collect()
}

/// Conversion operators for `#[base]` regions (composition-based upcast).
fn render_base_conversion_operators(
    body: &mut String,
    td: &TypeDefinition,
    ctx: RenderCtx,
) -> Result<()> {
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
    Ok(())
}

/// Static singleton accessor declaration.
fn render_singleton_declaration(body: &mut String, name: &str, td: &TypeDefinition) -> Result<()> {
    if td.singleton.is_some() {
        writeln!(body)?;
        writeln!(body, "    static {name}* singleton();")?;
    }
    Ok(())
}

/// Vftable accessor + virtual-method wrapper signatures. Pyxis's pub/private
/// distinction is rust-only; in C++ we emit every method (callers are free to
/// ignore the rust-private ones, but `backend cpp epilogue` blocks need to be
/// able to call into them by name).
fn render_vftable_declarations(
    body: &mut String,
    td: &TypeDefinition,
    ctx: RenderCtx,
) -> Result<()> {
    if let Some(vftable) = &td.vftable {
        render_vftable_accessor_decl(body, vftable, ctx)?;
        for func in &vftable.functions {
            if !ctx.cfg_passes(&func.cfg) {
                continue;
            }
            render_method_signature(body, func, ctx)?;
        }
    }
    Ok(())
}

/// Associated function signatures (impl block, e.g. `#[address(0x...)] pub fn foo()`).
fn render_associated_function_declarations(
    body: &mut String,
    td: &TypeDefinition,
    ctx: RenderCtx,
) -> Result<()> {
    for func in &td.associated_functions {
        if !ctx.cfg_passes(&func.cfg) {
            continue;
        }
        render_method_signature(body, func, ctx)?;
    }
    Ok(())
}

/// Pinned types: delete copy/move constructors and assignment operators so the
/// type cannot be relocated in memory.
fn render_deleted_special_members(
    body: &mut String,
    name: &str,
    td: &TypeDefinition,
) -> Result<()> {
    render_deleted_special_members_if(body, name, td.pinned)
}

pub(super) fn render_deleted_special_members_if(
    body: &mut String,
    name: &str,
    pinned: bool,
) -> Result<()> {
    if pinned {
        writeln!(body)?;
        writeln!(body, "    {name}(const {name}&) = delete;")?;
        writeln!(body, "    {name}({name}&&) = delete;")?;
        writeln!(body, "    {name}& operator=(const {name}&) = delete;")?;
        writeln!(body, "    {name}& operator=({name}&&) = delete;")?;
    }
    Ok(())
}

/// Layout assertions. Generic templates can't sizeof/alignof at the
/// declaration site (size depends on T), so skip those.
fn render_layout_assertions(
    out: &mut String,
    name: &str,
    size: usize,
    alignment: usize,
    is_generic: bool,
) -> Result<()> {
    if !is_generic {
        if size > 0 {
            writeln!(out, "static_assert(sizeof({name}) == 0x{size:X});")?;
        }
        writeln!(out, "static_assert(alignof({name}) == {alignment});")?;
    }
    Ok(())
}

/// Out-of-class method/accessor definitions. Templates' member definitions
/// must stay header-visible (the compiler instantiates them at every use
/// site); non-template members land in the `.cpp`. Methods carrying a
/// method-level template parameter also stay header-visible for the same
/// reason.
fn render_out_of_class_method_definitions(
    post_header: &mut String,
    post_cpp: &mut String,
    name: &str,
    td: &TypeDefinition,
    is_generic: bool,
    ctx: RenderCtx,
) -> Result<()> {
    if is_generic {
        if let Some(vftable) = &td.vftable {
            render_vftable_accessor_definition(post_header, name, vftable, ctx)?;
            for func in &vftable.functions {
                if !ctx.cfg_passes(&func.cfg) {
                    continue;
                }
                render_method_definition(post_header, name, func, ctx)?;
            }
        }
        for func in &td.associated_functions {
            if !ctx.cfg_passes(&func.cfg) {
                continue;
            }
            render_method_definition(post_header, name, func, ctx)?;
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
            render_vftable_accessor_definition(post_cpp, name, vftable, ctx)?;
            for func in &vftable.functions {
                if !ctx.cfg_passes(&func.cfg) {
                    continue;
                }
                if !func.method_type_parameters.is_empty() {
                    render_method_definition(post_header, name, func, ctx)?;
                } else {
                    render_method_definition(post_cpp, name, func, ctx)?;
                }
            }
        }
        for func in &td.associated_functions {
            if !ctx.cfg_passes(&func.cfg) {
                continue;
            }
            if !func.method_type_parameters.is_empty() {
                render_method_definition(post_header, name, func, ctx)?;
            } else {
                render_method_definition(post_cpp, name, func, ctx)?;
            }
        }
    }
    Ok(())
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
    let (sig_args_text, const_qual) = method_sig_parts(func, ctx)?;
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
    let declaration = method_declaration(
        func,
        &super::cpp_ident(&func.name),
        &sig_args_text,
        const_qual,
        ctx,
    )?;
    writeln!(out, "    {static_kw}{declaration};")?;
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
    let (sig_args_text, const_qual) = method_sig_parts(func, ctx)?;
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
    let declaration = method_declaration(
        func,
        &format!(
            "{parent_name}::{fn_name}",
            fn_name = super::cpp_ident(&func.name)
        ),
        &sig_args_text,
        const_qual,
        ctx,
    )?;
    writeln!(out, "{declaration} {{")?;
    for line in &body_lines {
        writeln!(out, "    {line}")?;
    }
    writeln!(out, "}}")?;
    // Trailing blank line so adjacent method definitions get separated.
    writeln!(out)?;
    Ok(())
}

/// The parameter list and cv-qualifier of a method signature. The return type
/// is deliberately not part of this: a function-pointer return type can't be
/// written as a leading `R`, so the declaration is assembled by
/// [`method_declaration`] instead of being concatenated from pieces.
pub(super) fn method_sig_parts(func: &Function, ctx: RenderCtx) -> Result<(String, &'static str)> {
    let mut sig_args: Vec<String> = Vec::new();
    let mut self_kind: Option<&'static str> = None;
    for arg in &func.arguments {
        match arg {
            Argument::ConstSelf { .. } => self_kind = Some("const"),
            Argument::MutSelf { .. } => self_kind = Some("mut"),
            Argument::Field { name, type_, .. } => {
                let escaped = super::cpp_ident(name);
                sig_args.push(super::render_declaration(type_, &escaped, ctx)?);
            }
        }
    }
    let const_qual = if matches!(self_kind, Some("const")) {
        " const"
    } else {
        ""
    };
    Ok((sig_args.join(", "), const_qual))
}

/// Assemble a complete method declaration: `R name(args) const`, or the
/// declarator form when the return type is itself a function pointer
/// (`void (*name(args) const)(...)`).
pub(super) fn method_declaration(
    func: &Function,
    qualified_name: &str,
    sig_args_text: &str,
    const_qual: &str,
    ctx: RenderCtx,
) -> Result<String> {
    let core = format!("{qualified_name}({sig_args_text}){const_qual}");
    match &func.return_type {
        Some(ret) => super::render_declaration(ret, &core, ctx),
        None => Ok(format!("void {core}")),
    }
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
                    Argument::Field { type_, .. } => super::render_parameter_type(type_, ctx),
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
            // The alias target goes through the declarator builder so a
            // function-pointer return type nests instead of being glued on.
            let fn_t = match &func.return_type {
                Some(ret) => {
                    super::render_declaration(ret, &format!("({cc}*)({arg_types_text})"), ctx)?
                }
                None => format!("void ({cc}*)({arg_types_text})"),
            };
            vec![
                format!("using fn_t = {fn_t};"),
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
