//! Item → C++ text rendering for the C++ backend.
//!
//! Owns the conversion of resolved IR items (structs, enums, bitflags, type
//! aliases, vftables, generics, extern bindings) into the textual output for
//! `.hpp` and `.cpp` files.

use std::collections::BTreeMap;
use std::fmt::Write;

use crate::{
    backends::{Result, cpp::extern_bindings::CppExternBinding},
    grammar::ItemPath,
    semantic::{
        TypeRegistry,
        types::{
            Argument, BitflagField, BitflagsDefinition, CallingConvention, EnumDefinition,
            EnumVariant, ExternValue, Function, FunctionBody, ItemDefinition, ItemDefinitionInner,
            PredefinedItem, Region, Type, TypeAliasDefinition, TypeDefinition, TypeVftable,
            Visibility,
        },
    },
};

/// Bundle of state every render call needs: the module being rendered (for
/// "same-module → bare name vs cross-module → fully qualified" decisions),
/// the resolved type registry, the project's extern-binding table, and the
/// active backend's cfg context (for filtering items that have `#[cfg(...)]`
/// predicates).
#[derive(Copy, Clone)]
pub struct RenderCtx<'a> {
    pub module_path: &'a ItemPath,
    pub registry: &'a TypeRegistry,
    pub bindings: &'a BTreeMap<ItemPath, CppExternBinding>,
    pub cfg_ctx: crate::parser::cfg::CfgContext<'a>,
}

impl<'a> RenderCtx<'a> {
    pub fn new(
        module_path: &'a ItemPath,
        registry: &'a TypeRegistry,
        bindings: &'a BTreeMap<ItemPath, CppExternBinding>,
        cfg_ctx: crate::parser::cfg::CfgContext<'a>,
    ) -> Self {
        Self {
            module_path,
            registry,
            bindings,
            cfg_ctx,
        }
    }

    /// Returns true if the function should be emitted under the current
    /// cfg context (or has no cfg).
    pub fn cfg_passes(&self, cfg: &Option<crate::parser::cfg::CfgPredicate>) -> bool {
        match cfg {
            Some(p) => p.evaluate(&self.cfg_ctx),
            None => true,
        }
    }
}

/// Two-phase output for an item: the struct/enum body that goes inside the
/// namespace at first pass, plus any out-of-class inline method definitions
/// that have to come after every peer type is fully declared.
#[derive(Default)]
pub struct RenderedItem {
    pub decl: String,
    pub post: String,
}

/// Render a single item as a C++ definition. Returns `None` if the item
/// doesn't produce direct output in this phase (predefined, or an extern
/// type — extern bindings are inlined at use sites via `render_path`).
pub fn render_item(item: &ItemDefinition, ctx: RenderCtx) -> Result<Option<RenderedItem>> {
    if item.is_predefined() {
        return Ok(None);
    }
    if matches!(item.category, crate::semantic::types::ItemCategory::Extern) {
        // Externs with a cpp_name binding are substituted at every use site
        // via `render_path`; we don't emit a `using` alias because the
        // pyxis leaf name can contain generic syntax (`Foo<Bar<u32>>`),
        // which is invalid C++ on the LHS of `using`.
        return Ok(None);
    }
    let resolved = match item.resolved() {
        Some(r) => r,
        None => return Ok(None),
    };

    let name = item
        .path
        .last()
        .map(|s| s.as_str().to_string())
        .unwrap_or_else(|| "Unnamed".to_string());

    let rendered = match &resolved.inner {
        ItemDefinitionInner::Type(td) => render_struct(
            &name,
            td,
            resolved.size,
            resolved.alignment,
            ctx,
            item.visibility,
            &item.type_parameters,
        )?,
        ItemDefinitionInner::Enum(ed) => RenderedItem {
            decl: render_enum(&name, ed, resolved.size, ctx)?,
            post: String::new(),
        },
        ItemDefinitionInner::Bitflags(bd) => RenderedItem {
            decl: render_bitflags(&name, bd, resolved.size, ctx)?,
            post: String::new(),
        },
        ItemDefinitionInner::TypeAlias(ta) => RenderedItem {
            decl: render_type_alias(&name, ta, ctx, &item.type_parameters)?,
            post: String::new(),
        },
    };
    Ok(Some(rendered))
}

fn template_clause(type_parameters: &[String]) -> String {
    if type_parameters.is_empty() {
        return String::new();
    }
    let params = type_parameters
        .iter()
        .map(|p| format!("class {p}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!("template <{params}>\n")
}

fn render_struct(
    name: &str,
    td: &TypeDefinition,
    size: usize,
    alignment: usize,
    ctx: RenderCtx,
    visibility: Visibility,
    type_parameters: &[String],
) -> Result<RenderedItem> {
    let is_generic = !type_parameters.is_empty();
    let mut out = String::new();
    render_doc(&mut out, &td.doc, 0)?;
    if td.packed {
        writeln!(out, "#pragma pack(push, 1)")?;
    }
    let template = template_clause(type_parameters);
    if is_generic {
        // Templates: alignment depends on T, so let the compiler infer via the
        // by-value field; skip explicit `alignas(N)`.
        write!(out, "{template}struct {name} {{")?;
        writeln!(out)?;
    } else {
        writeln!(out, "struct alignas({alignment}) {name} {{")?;
    }

    // Fields
    for region in &td.regions {
        render_field(&mut out, region, ctx)?;
    }

    // Conversion operators for #[base] regions (composition-based upcast).
    for region in &td.regions {
        if !region.is_base {
            continue;
        }
        let Some(field_name) = region.name.as_deref() else {
            continue;
        };
        let base_type = render_type(&region.type_ref, ctx)?;
        writeln!(out)?;
        writeln!(
            out,
            "    operator {base_type}&() {{ return this->{field_name}; }}"
        )?;
        writeln!(
            out,
            "    operator const {base_type}&() const {{ return this->{field_name}; }}"
        )?;
    }

    // Singleton accessor (static).
    if td.singleton.is_some() {
        writeln!(out)?;
        writeln!(out, "    static {name}* singleton();")?;
    }

    // Vftable accessor + virtual-method wrapper signatures. Pyxis's
    // pub/private distinction is rust-only; in C++ we emit every method
    // (callers are free to ignore the rust-private ones, but `backend cpp
    // epilogue` blocks need to be able to call into them by name).
    if let Some(vftable) = &td.vftable {
        render_vftable_accessor_decl(&mut out, vftable, ctx)?;
        for func in &vftable.functions {
            if !ctx.cfg_passes(&func.cfg) {
                continue;
            }
            render_method_signature(&mut out, func, ctx)?;
        }
    }

    // Associated functions (impl block, e.g. `#[address(0x...)] pub fn foo()`).
    for func in &td.associated_functions {
        if !ctx.cfg_passes(&func.cfg) {
            continue;
        }
        render_method_signature(&mut out, func, ctx)?;
    }

    writeln!(out, "}};")?;
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

    // Out-of-class inline method definitions go in the `post` bucket so the
    // orchestrator can place them after every peer struct in the namespace
    // is fully defined (vftable structs reference parent types and vice
    // versa). Generic templates currently have no out-of-class methods —
    // method definitions on a template can stay inline.
    let mut post = String::new();
    if !is_generic {
        if let Some(addr) = td.singleton {
            writeln!(
                post,
                "inline {name}* {name}::singleton() {{ return *reinterpret_cast<{name}**>(0x{addr:X}); }}"
            )?;
        }
        if let Some(vftable) = &td.vftable {
            render_vftable_accessor_definition(&mut post, name, vftable, ctx)?;
            for func in &vftable.functions {
                if !ctx.cfg_passes(&func.cfg) {
                    continue;
                }
                render_method_definition(&mut post, name, func, ctx)?;
            }
        }
        for func in &td.associated_functions {
            if !ctx.cfg_passes(&func.cfg) {
                continue;
            }
            render_method_definition(&mut post, name, func, ctx)?;
        }
    }

    Ok(RenderedItem { decl: out, post })
}

fn render_vftable_accessor_decl(
    out: &mut String,
    vftable: &TypeVftable,
    ctx: RenderCtx,
) -> Result<()> {
    let vftable_type = render_type(&vftable.type_, ctx)?;
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
    let vftable_type = render_type(&vftable.type_, ctx)?;
    if let Some(base_field) = &vftable.base_field {
        writeln!(
            out,
            "inline {vftable_type} {parent_name}::_vftable_ptr() const {{ return reinterpret_cast<{vftable_type}>(this->{base_field}._vftable_ptr()); }}"
        )?;
    } else {
        writeln!(
            out,
            "inline {vftable_type} {parent_name}::_vftable_ptr() const {{ return this->vftable; }}"
        )?;
    }
    Ok(())
}

/// In-class method declaration (signature only).
fn render_method_signature(out: &mut String, func: &Function, ctx: RenderCtx) -> Result<()> {
    if func.name.starts_with("_vfunc_") {
        return Ok(());
    }
    render_doc(out, &func.doc, 1)?;
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
        fn_name = func.name
    )?;
    Ok(())
}

fn func_has_self(func: &Function) -> bool {
    func.arguments
        .iter()
        .any(|a| matches!(a, Argument::ConstSelf { .. } | Argument::MutSelf { .. }))
}

/// Out-of-class inline method definition.
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
    // Out-of-class definitions never repeat `static` — that keyword belongs
    // only on the in-class declaration.
    writeln!(
        out,
        "inline {return_text} {parent_name}::{fn_name}({sig_args_text}){const_qual} {{",
        fn_name = func.name
    )?;
    for line in &body_lines {
        writeln!(out, "    {line}")?;
    }
    writeln!(out, "}}")?;
    Ok(())
}

fn method_sig_parts(func: &Function, ctx: RenderCtx) -> Result<(String, String, &'static str)> {
    let return_text = if let Some(ret) = &func.return_type {
        render_type(ret, ctx)?
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
                let ty = render_type(type_, ctx)?;
                let escaped = cpp_ident(name);
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
        render_type(ret, ctx)?
    } else {
        "void".to_string()
    };
    let mut call_args: Vec<String> = Vec::new();
    let mut has_self = false;
    for arg in &func.arguments {
        match arg {
            Argument::ConstSelf { .. } | Argument::MutSelf { .. } => has_self = true,
            Argument::Field { name, .. } => call_args.push(cpp_ident(name).into_owned()),
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
                    Argument::Field { type_, .. } => render_type(type_, ctx),
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
            let mut call_payload = String::new();
            call_payload.push_str("this");
            if !call_args.is_empty() {
                call_payload.push_str(", ");
            }
            call_payload.push_str(&call_args.join(", "));
            vec![format!(
                "{ret_kw}_vftable_ptr()->{function_name}({call_payload});"
            )]
        }
        FunctionBody::Field {
            field,
            function_name,
        } => {
            let call_payload = call_args.join(", ");
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

/// Map a calling convention to the MSVC keyword. We always target MSVC ABI;
/// `pyxis_runtime.hpp` provides a macro shim that no-ops these on non-MSVC
/// hosts so the backend output still compile-checks against `clang++` for
/// quick dev-loop iteration.
fn calling_conv_macro(cc: CallingConvention) -> &'static str {
    match cc {
        CallingConvention::C | CallingConvention::Cdecl => "PYXIS_CDECL ",
        CallingConvention::Stdcall => "PYXIS_STDCALL ",
        CallingConvention::Fastcall => "PYXIS_FASTCALL ",
        CallingConvention::Thiscall => "PYXIS_THISCALL ",
        CallingConvention::Vectorcall => "PYXIS_VECTORCALL ",
        CallingConvention::System => "",
    }
}

fn render_field(out: &mut String, region: &Region, ctx: RenderCtx) -> Result<()> {
    render_doc(out, &region.doc, 1)?;
    let Some(field_name) = region.name.as_deref() else {
        // Should not happen post-resolution, but be defensive.
        writeln!(out, "    // <unnamed region skipped>")?;
        return Ok(());
    };
    let field_name = cpp_ident(field_name);
    // Arrays render as `T name[N]`; function pointers as `R (cc *name)(args)`;
    // everything else as a plain `T name`.
    match &region.type_ref {
        Type::Array(inner, n) => {
            let inner_text = render_type(inner, ctx)?;
            writeln!(out, "    {inner_text} {field_name}[{n}];")?;
        }
        Type::Function(cc, args, ret) => {
            let decl = render_function_pointer_decl(&field_name, *cc, args, ret.as_deref(), ctx)?;
            writeln!(out, "    {decl};")?;
        }
        _ => {
            let ty_text = render_type(&region.type_ref, ctx)?;
            writeln!(out, "    {ty_text} {field_name};")?;
        }
    }
    Ok(())
}

/// Render `R (cc *name)(args)` for a function-pointer-typed declaration
/// (struct field, parameter, ...).
fn render_function_pointer_decl(
    name: &str,
    cc: CallingConvention,
    args: &[(String, Box<Type>)],
    ret: Option<&Type>,
    ctx: RenderCtx,
) -> Result<String> {
    let cc_macro = calling_conv_macro(cc);
    let ret_text = ret
        .map(|t| render_type(t, ctx))
        .transpose()?
        .unwrap_or_else(|| "void".to_string());
    let arg_types = args
        .iter()
        .map(|(_, t)| render_type(t, ctx))
        .collect::<Result<Vec<_>>>()?
        .join(", ");
    Ok(format!("{ret_text} ({cc_macro}*{name})({arg_types})"))
}

fn render_enum(name: &str, ed: &EnumDefinition, size: usize, ctx: RenderCtx) -> Result<String> {
    let mut out = String::new();
    render_doc(&mut out, &ed.doc, 0)?;
    let underlying = render_type(&ed.type_, ctx)?;
    writeln!(out, "enum class {name} : {underlying} {{")?;
    for variant in &ed.variants {
        let EnumVariant {
            name: variant_name,
            value,
            ..
        } = variant;
        writeln!(out, "    {variant_name} = {value},")?;
    }
    writeln!(out, "}};")?;
    if let Some(addr) = ed.singleton {
        writeln!(
            out,
            "inline {name} {name}_singleton() {{ return *reinterpret_cast<{name}*>(0x{addr:X}); }}"
        )?;
    }
    if size > 0 {
        writeln!(out, "static_assert(sizeof({name}) == 0x{size:X});")?;
    }
    Ok(out)
}

fn render_bitflags(
    name: &str,
    bd: &BitflagsDefinition,
    size: usize,
    ctx: RenderCtx,
) -> Result<String> {
    let mut out = String::new();
    render_doc(&mut out, &bd.doc, 0)?;
    let underlying = render_type(&bd.type_, ctx)?;
    writeln!(out, "enum class {name} : {underlying} {{")?;
    for flag in &bd.flags {
        let BitflagField {
            name: flag_name,
            value,
            ..
        } = flag;
        writeln!(out, "    {flag_name} = 0x{value:X},")?;
    }
    writeln!(out, "}};")?;
    // Bitwise operators so `a | b` and friends type-check.
    writeln!(
        out,
        "constexpr {name} operator|({name} a, {name} b) noexcept {{"
    )?;
    writeln!(
        out,
        "    return static_cast<{name}>(static_cast<{underlying}>(a) | static_cast<{underlying}>(b));"
    )?;
    writeln!(out, "}}")?;
    writeln!(
        out,
        "constexpr {name} operator&({name} a, {name} b) noexcept {{"
    )?;
    writeln!(
        out,
        "    return static_cast<{name}>(static_cast<{underlying}>(a) & static_cast<{underlying}>(b));"
    )?;
    writeln!(out, "}}")?;
    writeln!(
        out,
        "constexpr {name} operator^({name} a, {name} b) noexcept {{"
    )?;
    writeln!(
        out,
        "    return static_cast<{name}>(static_cast<{underlying}>(a) ^ static_cast<{underlying}>(b));"
    )?;
    writeln!(out, "}}")?;
    writeln!(out, "constexpr {name} operator~({name} a) noexcept {{")?;
    writeln!(
        out,
        "    return static_cast<{name}>(~static_cast<{underlying}>(a));"
    )?;
    writeln!(out, "}}")?;
    if let Some(addr) = bd.singleton {
        writeln!(
            out,
            "inline {name} {name}_singleton() {{ return *reinterpret_cast<{name}*>(0x{addr:X}); }}"
        )?;
    }
    if size > 0 {
        writeln!(out, "static_assert(sizeof({name}) == 0x{size:X});")?;
    }
    Ok(out)
}

fn render_type_alias(
    name: &str,
    ta: &TypeAliasDefinition,
    ctx: RenderCtx,
    type_parameters: &[String],
) -> Result<String> {
    let mut out = String::new();
    render_doc(&mut out, &ta.doc, 0)?;
    let target = render_type(&ta.target, ctx)?;
    let template = template_clause(type_parameters);
    writeln!(out, "{template}using {name} = {target};")?;
    Ok(out)
}

/// Render a free function (`fn foo()` at module scope). For `#[address]`
/// bodies this emits an `extern const fn_t name;` declaration suitable for
/// the `.hpp`; for `#[external_body]` bodies it emits a plain function
/// declaration whose body is supplied by the user's `backend cpp` block.
pub fn render_free_function_decl(func: &Function, ctx: RenderCtx) -> Result<Option<String>> {
    let mut out = String::new();
    render_doc(&mut out, &func.doc, 0)?;
    match &func.body {
        FunctionBody::Address { .. } => {
            let alias = function_pointer_alias(func, ctx)?;
            writeln!(out, "{alias}")?;
            writeln!(out, "extern const {0}_t {0};", func.name)?;
            Ok(Some(out))
        }
        FunctionBody::External => {
            let (return_text, sig_args_text) = free_function_sig_parts(func, ctx)?;
            writeln!(out, "{return_text} {0}({sig_args_text});", func.name)?;
            Ok(Some(out))
        }
        _ => Ok(None),
    }
}

/// Render the `.cpp` definition of a free function bound by `#[address]`.
/// External-body functions get their definitions from the user's
/// `backend cpp epilogue` and don't need .cpp output here.
pub fn render_free_function_definition(func: &Function, ctx: RenderCtx) -> Result<Option<String>> {
    let FunctionBody::Address { address } = &func.body else {
        return Ok(None);
    };
    let alias = function_pointer_alias(func, ctx)?;
    let mut out = String::new();
    writeln!(out, "{alias}")?;
    writeln!(
        out,
        "const {0}_t {0} = reinterpret_cast<{0}_t>(0x{1:X});",
        func.name, address
    )?;
    Ok(Some(out))
}

fn free_function_sig_parts(func: &Function, ctx: RenderCtx) -> Result<(String, String)> {
    let return_text = func
        .return_type
        .as_ref()
        .map(|t| render_type(t, ctx))
        .transpose()?
        .unwrap_or_else(|| "void".to_string());
    let mut sig_args = Vec::new();
    for arg in &func.arguments {
        if let Argument::Field { name, type_, .. } = arg {
            let ty = render_type(type_, ctx)?;
            let escaped = cpp_ident(name);
            sig_args.push(format!("{ty} {escaped}"));
        }
    }
    Ok((return_text, sig_args.join(", ")))
}

/// `using foo_t = R (CC*)(P1, P2);`
fn function_pointer_alias(func: &Function, ctx: RenderCtx) -> Result<String> {
    let return_text = func
        .return_type
        .as_ref()
        .map(|t| render_type(t, ctx))
        .transpose()?
        .unwrap_or_else(|| "void".to_string());
    let cc = calling_conv_macro(func.calling_convention);
    let mut arg_types: Vec<String> = Vec::new();
    for arg in &func.arguments {
        let ty = match arg {
            Argument::ConstSelf { .. } => "const void*".to_string(),
            Argument::MutSelf { .. } => "void*".to_string(),
            Argument::Field { type_, .. } => render_type(type_, ctx)?,
        };
        arg_types.push(ty);
    }
    let args_text = arg_types.join(", ");
    Ok(format!(
        "using {0}_t = {return_text} ({cc}*)({args_text});",
        func.name
    ))
}

/// Header-side declaration of an `extern <name>: <type>` value: a getter
/// returning a reference to the value at the address.
pub fn render_extern_value_decl(ev: &ExternValue, ctx: RenderCtx) -> Result<String> {
    let ty = render_type(&ev.type_, ctx)?;
    Ok(format!("{ty}& get_{0}();\n", ev.name))
}

/// `.cpp` definition for an `extern` value's getter.
pub fn render_extern_value_definition(ev: &ExternValue, ctx: RenderCtx) -> Result<String> {
    let ty = render_type(&ev.type_, ctx)?;
    Ok(format!(
        "{ty}& get_{0}() {{ return *reinterpret_cast<{ty}*>(0x{1:X}); }}\n",
        ev.name, ev.address
    ))
}

fn render_doc(out: &mut String, doc: &[String], indent_levels: usize) -> Result<()> {
    let pad = "    ".repeat(indent_levels);
    for line in doc {
        let trimmed = line.trim();
        writeln!(out, "{pad}/// {trimmed}")?;
    }
    Ok(())
}

/// Render a `Type` as a C++ type expression. For arrays the caller is
/// responsible for placing the `[N]` suffix after the field name.
pub fn render_type(ty: &Type, ctx: RenderCtx) -> Result<String> {
    Ok(match ty {
        Type::Unresolved(_) => "/* unresolved */ void".to_string(),
        Type::TypeParameter(name) => name.clone(),
        Type::Raw(path) => render_path(path, ctx),
        Type::Generic(base, args) => {
            let base_str = render_path(base, ctx);
            let args_str = args
                .iter()
                .map(|a| render_type(a, ctx))
                .collect::<Result<Vec<_>>>()?
                .join(", ");
            format!("{base_str}<{args_str}>")
        }
        Type::ConstPointer(inner) => format!("const {}*", render_type(inner, ctx)?),
        Type::MutPointer(inner) => format!("{}*", render_type(inner, ctx)?),
        Type::Array(inner, _n) => {
            // Fields handle the `[N]` suffix themselves; for nested contexts
            // (like template args) emit the bare element type.
            render_type(inner, ctx)?
        }
        Type::Function(cc, args, ret) => {
            // Bare function-pointer expression (no name) for use in template
            // arguments / type aliases. `render_function_pointer_decl`
            // handles the with-name field/parameter case.
            let cc_macro = calling_conv_macro(*cc);
            let ret_text = match ret.as_deref() {
                Some(t) => render_type(t, ctx)?,
                None => "void".to_string(),
            };
            let arg_types = args
                .iter()
                .map(|(_, t)| render_type(t, ctx))
                .collect::<Result<Vec<_>>>()?
                .join(", ");
            format!("{ret_text} ({cc_macro}*)({arg_types})")
        }
    })
}

fn render_path(path: &ItemPath, ctx: RenderCtx) -> String {
    // Predefined items map to C++ primitives directly (no namespace).
    if let Ok(item) = ctx
        .registry
        .get(path, &crate::span::ItemLocation::internal())
    {
        if let Some(predef) = item.predefined {
            return predefined_to_cpp(predef).to_string();
        }
        // Externs with a #[cpp_name] binding: substitute the C++ name
        // verbatim (the pyxis leaf is allowed to contain generic syntax
        // that can't be a C++ identifier).
        if matches!(item.category, crate::semantic::types::ItemCategory::Extern) {
            if let Some(binding) = ctx.bindings.get(path) {
                if let Some(name) = &binding.name {
                    return name.clone();
                }
            }
        }
    }
    // Same-module: bare name.
    let target_module = path.parent().unwrap_or_else(ItemPath::empty);
    let leaf = path.last().map(|s| s.as_str()).unwrap_or("");
    if &target_module == ctx.module_path {
        return leaf.to_string();
    }
    // Cross-module: fully qualified.
    let mut out = String::new();
    out.push_str("::");
    for (i, seg) in path.iter().enumerate() {
        if i > 0 {
            out.push_str("::");
        }
        out.push_str(seg.as_str());
    }
    out
}

/// Escape pyxis identifiers that collide with C++ reserved words by
/// suffixing an underscore. Idempotent for non-conflicting names.
pub fn cpp_ident(name: &str) -> std::borrow::Cow<'_, str> {
    const KEYWORDS: &[&str] = &[
        "alignas",
        "alignof",
        "and",
        "and_eq",
        "asm",
        "auto",
        "bitand",
        "bitor",
        "bool",
        "break",
        "case",
        "catch",
        "char",
        "char8_t",
        "char16_t",
        "char32_t",
        "class",
        "compl",
        "concept",
        "const",
        "consteval",
        "constexpr",
        "constinit",
        "const_cast",
        "continue",
        "co_await",
        "co_return",
        "co_yield",
        "decltype",
        "default",
        "delete",
        "do",
        "double",
        "dynamic_cast",
        "else",
        "enum",
        "explicit",
        "export",
        "extern",
        "false",
        "float",
        "for",
        "friend",
        "goto",
        "if",
        "inline",
        "int",
        "long",
        "mutable",
        "namespace",
        "new",
        "noexcept",
        "not",
        "not_eq",
        "nullptr",
        "operator",
        "or",
        "or_eq",
        "private",
        "protected",
        "public",
        "register",
        "reinterpret_cast",
        "requires",
        "return",
        "short",
        "signed",
        "sizeof",
        "static",
        "static_assert",
        "static_cast",
        "struct",
        "switch",
        "template",
        "this",
        "thread_local",
        "throw",
        "true",
        "try",
        "typedef",
        "typeid",
        "typename",
        "union",
        "unsigned",
        "using",
        "virtual",
        "void",
        "volatile",
        "wchar_t",
        "while",
        "xor",
        "xor_eq",
    ];
    if KEYWORDS.contains(&name) {
        std::borrow::Cow::Owned(format!("{name}_"))
    } else {
        std::borrow::Cow::Borrowed(name)
    }
}

fn predefined_to_cpp(p: PredefinedItem) -> &'static str {
    match p {
        PredefinedItem::Void => "void",
        PredefinedItem::Bool => "bool",
        PredefinedItem::U8 => "::std::uint8_t",
        PredefinedItem::U16 => "::std::uint16_t",
        PredefinedItem::U32 => "::std::uint32_t",
        PredefinedItem::U64 => "::std::uint64_t",
        PredefinedItem::U128 => "::std::uint64_t /* u128 */",
        PredefinedItem::I8 => "::std::int8_t",
        PredefinedItem::I16 => "::std::int16_t",
        PredefinedItem::I32 => "::std::int32_t",
        PredefinedItem::I64 => "::std::int64_t",
        PredefinedItem::I128 => "::std::int64_t /* i128 */",
        PredefinedItem::F32 => "float",
        PredefinedItem::F64 => "double",
        PredefinedItem::CChar => "char",
        // Atomics get real bindings in Phase 3 via #[cpp_header]/<atomic>;
        // for now use opaque size-correct placeholders.
        PredefinedItem::AtomicBool => "::pyxis::AtomicBool",
        PredefinedItem::AtomicU8 => "::pyxis::AtomicU8",
        PredefinedItem::AtomicU16 => "::pyxis::AtomicU16",
        PredefinedItem::AtomicU32 => "::pyxis::AtomicU32",
        PredefinedItem::AtomicU64 => "::pyxis::AtomicU64",
        PredefinedItem::AtomicI8 => "::pyxis::AtomicI8",
        PredefinedItem::AtomicI16 => "::pyxis::AtomicI16",
        PredefinedItem::AtomicI32 => "::pyxis::AtomicI32",
        PredefinedItem::AtomicI64 => "::pyxis::AtomicI64",
    }
}
