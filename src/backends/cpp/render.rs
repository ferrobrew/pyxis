//! Item → C++ text rendering for the C++ backend.
//!
//! Owns the conversion of resolved IR items (structs, enums, bitflags, type
//! aliases, vftables, generics, extern bindings) into the textual output for
//! `.hpp` and `.cpp` files.

use std::{borrow::Cow, collections::BTreeMap, fmt::Write};

use crate::{
    backends::{
        Result,
        cpp::{extern_bindings::CppExternBinding, runtime},
    },
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
    span::ItemLocation,
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
    pub cfg_ctx: crate::parser::cfg::CfgContext,
}

impl<'a> RenderCtx<'a> {
    pub fn new(
        module_path: &'a ItemPath,
        registry: &'a TypeRegistry,
        bindings: &'a BTreeMap<ItemPath, CppExternBinding>,
        cfg_ctx: crate::parser::cfg::CfgContext,
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
    /// In-class declarations + the struct/enum/etc. body itself. Lands
    /// in the `.hpp`.
    pub decl: String,
    /// Out-of-class definitions that must stay header-visible (template
    /// methods, accessors on generic templates, etc.). Lands in the
    /// `.hpp` after `decl`.
    pub post_header: String,
    /// Out-of-class definitions for non-template methods/accessors.
    /// Lands in the `.cpp` so the header sticks to declarations.
    pub post_cpp: String,
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
        ItemDefinitionInner::Enum(ed) => {
            let (decl, post_cpp) = render_enum(&name, ed, resolved.size, ctx)?;
            RenderedItem {
                decl,
                post_header: String::new(),
                post_cpp,
            }
        }
        ItemDefinitionInner::Bitflags(bd) => {
            let (decl, post_cpp) = render_bitflags(&name, bd, resolved.size, ctx)?;
            RenderedItem {
                decl,
                post_header: String::new(),
                post_cpp,
            }
        }
        ItemDefinitionInner::TypeAlias(ta) => RenderedItem {
            decl: render_type_alias(&name, ta, ctx, &item.type_parameters)?,
            post_header: String::new(),
            post_cpp: String::new(),
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
    let name = &*cpp_ident(name);
    let is_generic = !type_parameters.is_empty();
    let mut out = String::new();
    render_doc(&mut out, &td.doc, 0)?;
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

    // Fields. Vftable structs (named `<ParentType>Vftable`) get their
    // function-pointer slots' first param replaced with `void*` so the
    // wrappers on derived types can pass `this` without an explicit cast
    // through the base chain. The slot's intent is "any pointer to the
    // declaring type or one of its bases" - encoding that in C++'s type
    // system without inheritance is awkward, so we use `void*` as the
    // ABI-compatible escape hatch.
    let is_vftable_struct = name.ends_with("Vftable");
    for region in &td.regions {
        render_field(&mut body, region, ctx, is_vftable_struct)?;
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
        for nested_path in &td.nested_item_paths {
            if let Ok(nested_item) = ctx.registry.get(nested_path, &ItemLocation::internal()) {
                if let Some(nested_resolved) = nested_item.resolved() {
                    // Add blank line before nested item if body has content
                    if !body.trim().is_empty() {
                        writeln!(body)?;
                    }
                    let nested_name = nested_path
                        .last()
                        .map(|s| s.as_str().to_string())
                        .unwrap_or_default();
                    let nested_name = cpp_ident(&nested_name);
                    match &nested_resolved.inner {
                        ItemDefinitionInner::Type(nested_td) => {
                            render_doc(&mut body, &nested_td.doc, 1)?;
                            writeln!(body, "    struct {nested_name} {{")?;
                            for region in &nested_td.regions {
                                render_field_indented(&mut body, region, ctx, false, 2)?;
                            }
                            writeln!(body, "    }};")?;
                        }
                        ItemDefinitionInner::Enum(nested_ed) => {
                            render_doc(&mut body, &nested_ed.doc, 1)?;
                            writeln!(
                                body,
                                "    enum class {nested_name} : {} {{",
                                render_type(&nested_ed.type_, ctx)?
                            )?;
                            for variant in &nested_ed.variants {
                                writeln!(
                                    body,
                                    "        {} = {},",
                                    cpp_ident(&variant.name),
                                    variant.value
                                )?;
                            }
                            writeln!(body, "    }};")?;
                        }
                        ItemDefinitionInner::Bitflags(nested_bd) => {
                            render_doc(&mut body, &nested_bd.doc, 1)?;
                            writeln!(body, "    struct {nested_name} {{")?;
                            let bf_type = render_type(&nested_bd.type_, ctx)?;
                            for flag in &nested_bd.flags {
                                writeln!(
                                    body,
                                    "        static constexpr {bf_type} {} = {};",
                                    cpp_ident(&flag.name),
                                    flag.value
                                )?;
                            }
                            writeln!(body, "    }};")?;
                        }
                        ItemDefinitionInner::TypeAlias(nested_ta) => {
                            render_doc(&mut body, &nested_ta.doc, 1)?;
                            writeln!(
                                body,
                                "    using {nested_name} = {};",
                                render_type(&nested_ta.target, ctx)?
                            )?;
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
        fn_name = cpp_ident(&func.name)
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
        fn_name = cpp_ident(&func.name)
    )?;
    for line in &body_lines {
        writeln!(out, "    {line}")?;
    }
    writeln!(out, "}}")?;
    // Trailing blank line so adjacent method definitions get separated.
    writeln!(out)?;
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
            let function_name = cpp_ident(function_name);
            vec![format!(
                "{ret_kw}_vftable_ptr()->{function_name}({call_payload});"
            )]
        }
        FunctionBody::Field {
            field,
            function_name,
        } => {
            let call_payload = call_args.join(", ");
            let field = cpp_ident(field);
            let function_name = cpp_ident(function_name);
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
fn calling_conv_macro(cc: CallingConvention) -> &'static str {
    runtime::macro_emit(cc)
}

fn render_field(
    out: &mut String,
    region: &Region,
    ctx: RenderCtx,
    rewrite_self_arg_to_void_ptr: bool,
) -> Result<()> {
    render_field_indented(out, region, ctx, rewrite_self_arg_to_void_ptr, 1)
}

fn render_field_indented(
    out: &mut String,
    region: &Region,
    ctx: RenderCtx,
    rewrite_self_arg_to_void_ptr: bool,
    indent: usize,
) -> Result<()> {
    let pad = " ".repeat(indent * 4);
    render_doc(out, &region.doc, indent)?;
    let Some(field_name) = region.name.as_deref() else {
        // Should not happen post-resolution, but be defensive.
        writeln!(out, "{pad}// <unnamed region skipped>")?;
        return Ok(());
    };
    let field_name = cpp_ident(field_name);
    // Arrays render as `T name[N]`; function pointers as `R (cc *name)(args)`;
    // everything else as a plain `T name`.
    match &region.type_ref {
        Type::Array(inner, n) => {
            let inner_text = render_type(inner, ctx)?;
            writeln!(out, "{pad}{inner_text} {field_name}[{n}];")?;
        }
        Type::Function(cc, args, ret) => {
            let decl = render_function_pointer_decl(
                &field_name,
                *cc,
                args,
                ret.as_deref(),
                ctx,
                rewrite_self_arg_to_void_ptr,
            )?;
            writeln!(out, "{pad}{decl};")?;
        }
        _ => {
            let ty_text = render_type(&region.type_ref, ctx)?;
            writeln!(out, "{pad}{ty_text} {field_name};")?;
        }
    }
    Ok(())
}

/// Render `R (cc *name)(args)` for a function-pointer-typed declaration
/// (struct field, parameter, ...). When `rewrite_self_arg_to_void_ptr`,
/// the first arg's pointer type is replaced with `void*` (or `const void*`
/// preserving const-ness) - used for vftable struct slots so derived
/// types can pass their `this` without explicit base-chain casts.
fn render_function_pointer_decl(
    name: &str,
    cc: CallingConvention,
    args: &[(String, Box<Type>)],
    ret: Option<&Type>,
    ctx: RenderCtx,
    rewrite_self_arg_to_void_ptr: bool,
) -> Result<String> {
    let cc_macro = calling_conv_macro(cc);
    let ret_text = ret
        .map(|t| render_type(t, ctx))
        .transpose()?
        .unwrap_or_else(|| "void".to_string());
    let arg_types = args
        .iter()
        .enumerate()
        .map(|(i, (_, t))| {
            if rewrite_self_arg_to_void_ptr && i == 0 {
                Ok(match t.as_ref() {
                    Type::ConstPointer(_) => "const void*".to_string(),
                    Type::MutPointer(_) => "void*".to_string(),
                    _ => render_type(t, ctx)?,
                })
            } else {
                render_type(t, ctx)
            }
        })
        .collect::<Result<Vec<_>>>()?
        .join(", ");
    Ok(format!("{ret_text} ({cc_macro}*{name})({arg_types})"))
}

fn render_enum(
    name: &str,
    ed: &EnumDefinition,
    size: usize,
    ctx: RenderCtx,
) -> Result<(String, String)> {
    let name = &*cpp_ident(name);
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
    let mut post_cpp = String::new();
    if let Some(addr) = ed.singleton {
        writeln!(out, "{name} {name}_singleton();")?;
        writeln!(post_cpp, "{name} {name}_singleton() {{")?;
        writeln!(
            post_cpp,
            "    return *reinterpret_cast<{name}*>(0x{addr:X});"
        )?;
        writeln!(post_cpp, "}}")?;
        writeln!(post_cpp)?;
    }
    if size > 0 {
        writeln!(out, "static_assert(sizeof({name}) == 0x{size:X});")?;
    }
    Ok((out, post_cpp))
}

fn render_bitflags(
    name: &str,
    bd: &BitflagsDefinition,
    size: usize,
    ctx: RenderCtx,
) -> Result<(String, String)> {
    let name = &*cpp_ident(name);
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
    let mut post_cpp = String::new();
    if let Some(addr) = bd.singleton {
        writeln!(out, "{name} {name}_singleton();")?;
        writeln!(post_cpp, "{name} {name}_singleton() {{")?;
        writeln!(
            post_cpp,
            "    return *reinterpret_cast<{name}*>(0x{addr:X});"
        )?;
        writeln!(post_cpp, "}}")?;
        writeln!(post_cpp)?;
    }
    if size > 0 {
        writeln!(out, "static_assert(sizeof({name}) == 0x{size:X});")?;
    }
    Ok((out, post_cpp))
}

fn render_type_alias(
    name: &str,
    ta: &TypeAliasDefinition,
    ctx: RenderCtx,
    type_parameters: &[String],
) -> Result<String> {
    let name = &*cpp_ident(name);
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
    let name = cpp_ident(&func.name);
    match &func.body {
        FunctionBody::Address { .. } => {
            let alias = function_pointer_alias(func, ctx)?;
            writeln!(out, "{alias}")?;
            writeln!(out, "extern const {name}_t {name};")?;
            Ok(Some(out))
        }
        FunctionBody::External => {
            let (return_text, sig_args_text) = free_function_sig_parts(func, ctx)?;
            writeln!(out, "{return_text} {name}({sig_args_text});")?;
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
    let name = cpp_ident(&func.name);
    let mut out = String::new();
    writeln!(out, "{alias}")?;
    writeln!(
        out,
        "const {name}_t {name} = reinterpret_cast<{name}_t>(0x{address:X});",
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
    let name = cpp_ident(&func.name);
    Ok(format!(
        "using {name}_t = {return_text} ({cc}*)({args_text});"
    ))
}

/// Header-side declaration of an `extern <name>: <type>` value: a getter
/// returning a reference to the value at the address.
pub fn render_extern_value_decl(ev: &ExternValue, ctx: RenderCtx) -> Result<String> {
    let ty = render_type(&ev.type_, ctx)?;
    Ok(format!("{ty}& get_{0}();\n", ev.name))
}

/// `.cpp` definition for an `extern` value's getter. Three lines plus
/// a trailing blank, so adjacent getters render with the same single-
/// blank rhythm as out-of-class member definitions.
pub fn render_extern_value_definition(ev: &ExternValue, ctx: RenderCtx) -> Result<String> {
    let ty = render_type(&ev.type_, ctx)?;
    Ok(format!(
        "{ty}& get_{name}() {{\n    return *reinterpret_cast<{ty}*>(0x{addr:X});\n}}\n\n",
        name = ev.name,
        addr = ev.address,
    ))
}

fn render_doc(out: &mut String, doc: &[String], indent_levels: usize) -> Result<()> {
    let pad = "    ".repeat(indent_levels);
    for line in doc {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            // Blank doc line - emit just `///` with no trailing space.
            writeln!(out, "{pad}///")?;
        } else {
            writeln!(out, "{pad}/// {trimmed}")?;
        }
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
        if matches!(item.category, crate::semantic::types::ItemCategory::Extern)
            && let Some(binding) = ctx.bindings.get(path)
            && let Some(name) = &binding.name
        {
            return name.clone();
        }
    }
    // Same-module: bare name.
    let target_module = path.parent().unwrap_or_else(ItemPath::empty);
    let leaf = path.last().map(|s| s.as_str()).unwrap_or("");
    if &target_module == ctx.module_path {
        return cpp_ident(leaf).into_owned();
    }
    // Cross-module: fully qualified. Module segments are namespaces (escaped
    // against C-runtime globals too); the leaf is the type name.
    let mut out = String::new();
    out.push_str("::");
    let last = path.len().saturating_sub(1);
    for (i, seg) in path.iter().enumerate() {
        if i > 0 {
            out.push_str("::");
        }
        let escaped = if i == last {
            cpp_ident(seg.as_str())
        } else {
            cpp_namespace_ident(seg.as_str())
        };
        out.push_str(&escaped);
    }
    out
}

/// C-runtime identifiers that live in the *global* namespace of every
/// generated translation unit, because the runtime header's mandatory
/// includes pull them in (notably `<atomic>` transitively includes
/// `<time.h>`, which declares `::clock`). A pyxis module emitted as a
/// global `namespace` with one of these names is a "redefinition as a
/// different kind of symbol", so they're escaped in *namespace position*
/// only — they're harmless as field/member/type names (those are scoped).
const CPP_RESERVED_GLOBALS: &[&str] = &[
    "clock",
    "time",
    "difftime",
    "mktime",
    "asctime",
    "ctime",
    "gmtime",
    "localtime",
    "strftime",
    "clock_t",
    "time_t",
    "tm",
    "timespec",
];

/// Escape pyxis identifiers that collide with C++ reserved words by
/// suffixing an underscore. Idempotent for non-conflicting names.
pub fn cpp_ident(name: &str) -> Cow<'_, str> {
    if CPP_KEYWORDS.contains(&name) {
        Cow::Owned(format!("{name}_"))
    } else {
        Cow::Borrowed(name)
    }
}

/// Like [`cpp_ident`], but for module/namespace segments: also escapes
/// C-runtime globals (see [`CPP_RESERVED_GLOBALS`]) that collide with a
/// global `namespace` of the same name.
pub fn cpp_namespace_ident(name: &str) -> Cow<'_, str> {
    if CPP_KEYWORDS.contains(&name) || CPP_RESERVED_GLOBALS.contains(&name) {
        Cow::Owned(format!("{name}_"))
    } else {
        Cow::Borrowed(name)
    }
}

const CPP_KEYWORDS: &[&str] = &[
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

#[cfg(test)]
mod ident_tests {
    use super::{cpp_ident, cpp_namespace_ident};

    #[test]
    fn keywords_are_escaped_everywhere() {
        assert_eq!(cpp_ident("class"), "class_");
        assert_eq!(cpp_namespace_ident("class"), "class_");
    }

    #[test]
    fn plain_identifiers_are_untouched() {
        assert_eq!(cpp_ident("Clock"), "Clock");
        assert_eq!(cpp_namespace_ident("world"), "world");
    }

    #[test]
    fn runtime_globals_are_escaped_only_in_namespace_position() {
        // `clock` collides with the C-runtime `::clock` only as a global
        // namespace; it's fine as a field/type name.
        assert_eq!(cpp_namespace_ident("clock"), "clock_");
        assert_eq!(cpp_ident("clock"), "clock");
    }
}
