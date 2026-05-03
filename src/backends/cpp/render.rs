//! Item → C++ text rendering for the C++ backend.
//!
//! Owns the conversion of resolved IR items (structs, enums, bitflags, type
//! aliases) into the textual output for `.hpp` files. Vftables, functions,
//! generics, externs, and per-module prologue/epilogue are introduced in
//! later phases.

use std::fmt::Write;

use std::collections::BTreeMap;

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
/// type without a `#[cpp_name]` binding).
pub fn render_item(
    item: &ItemDefinition,
    module_path: &ItemPath,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
) -> Result<Option<RenderedItem>> {
    if item.is_predefined() {
        return Ok(None);
    }
    if matches!(item.category, crate::semantic::types::ItemCategory::Extern) {
        if let Some(binding) = bindings.get(&item.path) {
            if let Some(name) = &binding.name {
                let leaf = item
                    .path
                    .last()
                    .map(|s| s.as_str().to_string())
                    .unwrap_or_default();
                return Ok(Some(RenderedItem {
                    decl: format!("using {leaf} = {name};\n"),
                    post: String::new(),
                }));
            }
        }
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
            module_path,
            td,
            resolved.size,
            resolved.alignment,
            registry,
            item.visibility,
            &item.type_parameters,
        )?,
        ItemDefinitionInner::Enum(ed) => RenderedItem {
            decl: render_enum(&name, ed, resolved.size, registry)?,
            post: String::new(),
        },
        ItemDefinitionInner::Bitflags(bd) => RenderedItem {
            decl: render_bitflags(&name, bd, resolved.size, registry)?,
            post: String::new(),
        },
        ItemDefinitionInner::TypeAlias(ta) => RenderedItem {
            decl: render_type_alias(&name, ta, module_path, registry, &item.type_parameters)?,
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

#[allow(clippy::too_many_arguments)]
fn render_struct(
    name: &str,
    module_path: &ItemPath,
    td: &TypeDefinition,
    size: usize,
    alignment: usize,
    registry: &TypeRegistry,
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
        render_field(&mut out, region, module_path, registry)?;
    }

    // Conversion operators for #[base] regions (composition-based upcast).
    for region in &td.regions {
        if !region.is_base {
            continue;
        }
        let Some(field_name) = region.name.as_deref() else {
            continue;
        };
        let base_type = render_type(&region.type_ref, module_path, registry)?;
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

    // Vftable accessor + virtual-method wrapper signatures.
    if let Some(vftable) = &td.vftable {
        render_vftable_accessor_decl(&mut out, vftable, module_path, registry)?;
        for func in &vftable.functions {
            if func.visibility != Visibility::Public {
                continue;
            }
            render_method_signature(&mut out, func, module_path, registry)?;
        }
    }

    // Associated functions (impl block, e.g. `#[address(0x...)] pub fn foo()`).
    for func in &td.associated_functions {
        if func.visibility != Visibility::Public {
            continue;
        }
        render_method_signature(&mut out, func, module_path, registry)?;
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
            render_vftable_accessor_definition(&mut post, name, vftable, module_path, registry)?;
            for func in &vftable.functions {
                if func.visibility != Visibility::Public {
                    continue;
                }
                render_method_definition(&mut post, name, func, module_path, registry)?;
            }
        }
        for func in &td.associated_functions {
            if func.visibility != Visibility::Public {
                continue;
            }
            render_method_definition(&mut post, name, func, module_path, registry)?;
        }
    }

    Ok(RenderedItem { decl: out, post })
}

fn render_vftable_accessor_decl(
    out: &mut String,
    vftable: &TypeVftable,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<()> {
    let vftable_type = render_type(&vftable.type_, module_path, registry)?;
    writeln!(out)?;
    writeln!(out, "    {vftable_type} _vftable_ptr() const;")?;
    Ok(())
}

fn render_vftable_accessor_definition(
    out: &mut String,
    parent_name: &str,
    vftable: &TypeVftable,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<()> {
    let vftable_type = render_type(&vftable.type_, module_path, registry)?;
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
fn render_method_signature(
    out: &mut String,
    func: &Function,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<()> {
    if func.name.starts_with("_vfunc_") {
        return Ok(());
    }
    render_doc(out, &func.doc, 1)?;
    let (return_text, sig_args_text, const_qual) = method_sig_parts(func, module_path, registry)?;
    writeln!(
        out,
        "    {return_text} {fn_name}({sig_args_text}){const_qual};",
        fn_name = func.name
    )?;
    Ok(())
}

/// Out-of-class inline method definition.
fn render_method_definition(
    out: &mut String,
    parent_name: &str,
    func: &Function,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<()> {
    if func.name.starts_with("_vfunc_") {
        return Ok(());
    }
    let (return_text, sig_args_text, const_qual) = method_sig_parts(func, module_path, registry)?;
    let body_lines = method_body_lines(func, module_path, registry)?;
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

fn method_sig_parts(
    func: &Function,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<(String, String, &'static str)> {
    let return_text = if let Some(ret) = &func.return_type {
        render_type(ret, module_path, registry)?
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
                let ty = render_type(type_, module_path, registry)?;
                sig_args.push(format!("{ty} {name}"));
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

fn method_body_lines(
    func: &Function,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<Vec<String>> {
    let return_text = if let Some(ret) = &func.return_type {
        render_type(ret, module_path, registry)?
    } else {
        "void".to_string()
    };
    let mut call_args: Vec<String> = Vec::new();
    let mut has_self = false;
    for arg in &func.arguments {
        match arg {
            Argument::ConstSelf { .. } | Argument::MutSelf { .. } => has_self = true,
            Argument::Field { name, .. } => call_args.push(name.clone()),
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
                    Argument::Field { type_, .. } => render_type(type_, module_path, registry),
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

fn render_field(
    out: &mut String,
    region: &Region,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<()> {
    render_doc(out, &region.doc, 1)?;
    let Some(field_name) = region.name.as_deref() else {
        // Should not happen post-resolution, but be defensive.
        writeln!(out, "    // <unnamed region skipped>")?;
        return Ok(());
    };
    // Arrays render as `T name[N]`; function pointers as `R (cc *name)(args)`;
    // everything else as a plain `T name`.
    match &region.type_ref {
        Type::Array(inner, n) => {
            let inner_text = render_type(inner, module_path, registry)?;
            writeln!(out, "    {inner_text} {field_name}[{n}];")?;
        }
        Type::Function(cc, args, ret) => {
            let decl = render_function_pointer_decl(
                field_name,
                *cc,
                args,
                ret.as_deref(),
                module_path,
                registry,
            )?;
            writeln!(out, "    {decl};")?;
        }
        _ => {
            let ty_text = render_type(&region.type_ref, module_path, registry)?;
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
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<String> {
    let cc_macro = calling_conv_macro(cc);
    let ret_text = ret
        .map(|t| render_type(t, module_path, registry))
        .transpose()?
        .unwrap_or_else(|| "void".to_string());
    let arg_types = args
        .iter()
        .map(|(_, t)| render_type(t, module_path, registry))
        .collect::<Result<Vec<_>>>()?
        .join(", ");
    Ok(format!("{ret_text} ({cc_macro}*{name})({arg_types})"))
}


fn render_enum(
    name: &str,
    ed: &EnumDefinition,
    size: usize,
    registry: &TypeRegistry,
) -> Result<String> {
    let mut out = String::new();
    render_doc(&mut out, &ed.doc, 0)?;
    let underlying = render_type(&ed.type_, &ItemPath::empty(), registry)?;
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
    registry: &TypeRegistry,
) -> Result<String> {
    let mut out = String::new();
    render_doc(&mut out, &bd.doc, 0)?;
    let underlying = render_type(&bd.type_, &ItemPath::empty(), registry)?;
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
    writeln!(out, "constexpr {name} operator|({name} a, {name} b) noexcept {{")?;
    writeln!(
        out,
        "    return static_cast<{name}>(static_cast<{underlying}>(a) | static_cast<{underlying}>(b));"
    )?;
    writeln!(out, "}}")?;
    writeln!(out, "constexpr {name} operator&({name} a, {name} b) noexcept {{")?;
    writeln!(
        out,
        "    return static_cast<{name}>(static_cast<{underlying}>(a) & static_cast<{underlying}>(b));"
    )?;
    writeln!(out, "}}")?;
    writeln!(out, "constexpr {name} operator^({name} a, {name} b) noexcept {{")?;
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
    module_path: &ItemPath,
    registry: &TypeRegistry,
    type_parameters: &[String],
) -> Result<String> {
    let mut out = String::new();
    render_doc(&mut out, &ta.doc, 0)?;
    let target = render_type(&ta.target, module_path, registry)?;
    let template = template_clause(type_parameters);
    writeln!(out, "{template}using {name} = {target};")?;
    Ok(out)
}

/// Render a free function (`fn foo()` at module scope) as an `extern const`
/// function-pointer declaration suitable for the `.hpp`. The matching `.cpp`
/// definition is produced by [`render_free_function_definition`].
pub fn render_free_function_decl(
    func: &Function,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<Option<String>> {
    let FunctionBody::Address { .. } = &func.body else {
        return Ok(None);
    };
    let mut out = String::new();
    render_doc(&mut out, &func.doc, 0)?;
    let alias = function_pointer_alias(func, module_path, registry)?;
    writeln!(out, "{alias}")?;
    writeln!(out, "extern const {0}_t {0};", func.name)?;
    Ok(Some(out))
}

/// Render the `.cpp` definition of a free function bound by `#[address]`.
pub fn render_free_function_definition(
    func: &Function,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<Option<String>> {
    let FunctionBody::Address { address } = &func.body else {
        return Ok(None);
    };
    let alias = function_pointer_alias(func, module_path, registry)?;
    let mut out = String::new();
    writeln!(out, "{alias}")?;
    writeln!(
        out,
        "const {0}_t {0} = reinterpret_cast<{0}_t>(0x{1:X});",
        func.name, address
    )?;
    Ok(Some(out))
}

/// `using foo_t = R (CC*)(P1, P2);`
fn function_pointer_alias(
    func: &Function,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<String> {
    let return_text = func
        .return_type
        .as_ref()
        .map(|t| render_type(t, module_path, registry))
        .transpose()?
        .unwrap_or_else(|| "void".to_string());
    let cc = calling_conv_macro(func.calling_convention);
    let mut arg_types: Vec<String> = Vec::new();
    for arg in &func.arguments {
        let ty = match arg {
            Argument::ConstSelf { .. } => "const void*".to_string(),
            Argument::MutSelf { .. } => "void*".to_string(),
            Argument::Field { type_, .. } => render_type(type_, module_path, registry)?,
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
pub fn render_extern_value_decl(
    ev: &ExternValue,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<String> {
    let ty = render_type(&ev.type_, module_path, registry)?;
    Ok(format!("{ty}& get_{0}();\n", ev.name))
}

/// `.cpp` definition for an `extern` value's getter.
pub fn render_extern_value_definition(
    ev: &ExternValue,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<String> {
    let ty = render_type(&ev.type_, module_path, registry)?;
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
pub fn render_type(ty: &Type, module_path: &ItemPath, registry: &TypeRegistry) -> Result<String> {
    Ok(match ty {
        Type::Unresolved(_) => "/* unresolved */ void".to_string(),
        Type::TypeParameter(name) => name.clone(),
        Type::Raw(path) => render_path(path, module_path, registry),
        Type::Generic(base, args) => {
            let base_str = render_path(base, module_path, registry);
            let args_str = args
                .iter()
                .map(|a| render_type(a, module_path, registry))
                .collect::<Result<Vec<_>>>()?
                .join(", ");
            format!("{base_str}<{args_str}>")
        }
        Type::ConstPointer(inner) => {
            format!("const {}*", render_type(inner, module_path, registry)?)
        }
        Type::MutPointer(inner) => {
            format!("{}*", render_type(inner, module_path, registry)?)
        }
        Type::Array(inner, _n) => {
            // Fields handle the `[N]` suffix themselves; for nested contexts
            // (like template args) emit the bare element type.
            render_type(inner, module_path, registry)?
        }
        Type::Function(cc, args, ret) => {
            // Bare function-pointer expression (no name) for use in template
            // arguments / type aliases. `render_function_pointer_decl`
            // handles the with-name field/parameter case.
            let cc_macro = calling_conv_macro(*cc);
            let ret_text = match ret.as_deref() {
                Some(t) => render_type(t, module_path, registry)?,
                None => "void".to_string(),
            };
            let arg_types = args
                .iter()
                .map(|(_, t)| render_type(t, module_path, registry))
                .collect::<Result<Vec<_>>>()?
                .join(", ");
            format!("{ret_text} ({cc_macro}*)({arg_types})")
        }
    })
}

fn render_path(path: &ItemPath, module_path: &ItemPath, registry: &TypeRegistry) -> String {
    // Predefined items map to C++ primitives directly (no namespace).
    if let Ok(item) = registry.get(path, &crate::span::ItemLocation::internal())
        && let Some(predef) = item.predefined
    {
        return predefined_to_cpp(predef).to_string();
    }
    // Same-module: bare name.
    let target_module = path.parent().unwrap_or_else(ItemPath::empty);
    let leaf = path.last().map(|s| s.as_str()).unwrap_or("");
    if &target_module == module_path {
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
