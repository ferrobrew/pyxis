//! Rendering for enum/bitflags/type-alias/const items, free functions,
//! extern values, and struct field declarations.

use std::fmt::Write;

use super::RenderCtx;
use crate::{
    backends::Result,
    grammar::ItemPath,
    semantic::types::{
        BitflagField, BitflagsDefinition, CallingConvention,
        ConstDefinition as SemanticConstDefinition, ConstValue, EnumDefinition, EnumVariant,
        ExternValueDefinition as SemanticExternValueDefinition, Function, FunctionBody,
        ItemDefinitionInner, Region, Type, TypeAliasDefinition,
    },
    span::ItemLocation,
};

use super::RenderedItem;

pub(super) fn render_enum(
    name: &str,
    ed: &EnumDefinition,
    size: usize,
    ctx: RenderCtx,
    location: &ItemLocation,
) -> Result<(String, String)> {
    let name = &*super::cpp_ident(name);
    let mut out = String::new();
    super::types::render_doc(&mut out, &ed.doc, 0, ctx, location)?;
    let underlying = super::render_type(&ed.type_, ctx)?;
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

pub(super) fn render_bitflags(
    name: &str,
    bd: &BitflagsDefinition,
    size: usize,
    ctx: RenderCtx,
    location: &ItemLocation,
) -> Result<(String, String)> {
    let name = &*super::cpp_ident(name);
    let mut out = String::new();
    super::types::render_doc(&mut out, &bd.doc, 0, ctx, location)?;
    let underlying = super::render_type(&bd.type_, ctx)?;
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

pub(super) fn render_type_alias(
    name: &str,
    ta: &TypeAliasDefinition,
    ctx: RenderCtx,
    type_parameters: &[String],
    location: &ItemLocation,
) -> Result<String> {
    let name = &*super::cpp_ident(name);
    let mut out = String::new();
    super::types::render_doc(&mut out, &ta.doc, 0, ctx, location)?;
    let target = super::render_type(&ta.target, ctx)?;
    let template = super::template_clause(type_parameters);
    writeln!(out, "{template}using {name} = {target};")?;
    Ok(out)
}

/// Emit nested value items (constants and extern values) as flat module-level
/// declarations with the parent name as a prefix (e.g., `Color_DEFAULT`,
/// `Color_get_g_current`). Used for enums and bitflags which don't have a struct
/// body to host `static` members. Constants become `constexpr`; extern values
/// become `inline` getters over their address.
pub(super) fn render_nested_values_cpp_flat(
    decl_out: &mut String,
    cpp_out: &mut String,
    ctx: RenderCtx,
    parent_path: &ItemPath,
    parent_name: &str,
) -> Result<()> {
    for (item_path, item) in ctx.registry.iter() {
        if item_path.parent().as_ref() != Some(parent_path) {
            continue;
        }
        let Some(resolved) = item.resolved() else {
            continue;
        };
        let value_name = item_path.last().map(|s| s.as_str()).unwrap_or_default();
        match &resolved.inner {
            ItemDefinitionInner::Constant(cd) => {
                // `constexpr` values must stay in the header.
                let flat_name = format!("{parent_name}_{}", super::cpp_ident(value_name));
                let type_str = super::render_type(&cd.type_, ctx)?;
                let value_str = format_const_value(&cd.value, &cd.type_);
                let storage = match &cd.value {
                    ConstValue::Struct { .. } | ConstValue::Array(_) | ConstValue::ConstRef(_) => {
                        "inline const"
                    }
                    _ => "constexpr",
                };
                super::types::render_doc(decl_out, &cd.doc, 0, ctx, &item.location)?;
                writeln!(decl_out, "{storage} {type_str} {flat_name} = {value_str};")?;
            }
            ItemDefinitionInner::ExternValue(ev) => {
                // Declared in the header, defined in the `.cpp` — matching the
                // module-level extern-value getters and the singleton accessor.
                let flat_name = format!("{parent_name}_get_{}", super::cpp_ident(value_name));
                let type_str = super::render_type(&ev.type_, ctx)?;
                super::types::render_doc(decl_out, &ev.doc, 0, ctx, &item.location)?;
                writeln!(decl_out, "{type_str}& {flat_name}();")?;
                writeln!(cpp_out, "{type_str}& {flat_name}() {{")?;
                writeln!(
                    cpp_out,
                    "    return *reinterpret_cast<{type_str}*>(0x{addr:X});",
                    addr = ev.address
                )?;
                writeln!(cpp_out, "}}")?;
                writeln!(cpp_out)?;
            }
            _ => {}
        }
    }

    Ok(())
}

pub(super) fn format_const_value(value: &ConstValue, type_: &Type) -> String {
    match value {
        ConstValue::Int(v) => v.to_string(),
        ConstValue::Float(bits) => {
            let f = f64::from_bits(*bits);
            let is_f32 = type_.is_f32();
            let s = format!("{f}");
            let has_decimal = s.contains('.') || s.contains('e') || s.contains('E');
            if is_f32 {
                if has_decimal {
                    format!("{s}f")
                } else {
                    format!("{s}.0f")
                }
            } else if has_decimal {
                s
            } else {
                format!("{s}.0")
            }
        }
        ConstValue::String(s) => {
            let mut esc = String::from("\"");
            for ch in s.chars() {
                match ch {
                    '"' => esc.push_str("\\\""),
                    '\\' => esc.push_str("\\\\"),
                    '\n' => esc.push_str("\\n"),
                    '\r' => esc.push_str("\\r"),
                    '\t' => esc.push_str("\\t"),
                    _ => esc.push(ch),
                }
            }
            esc.push('"');
            esc
        }
        ConstValue::CString(s) => {
            // C++ has no distinct C-string literal type; emit a regular
            // string literal (same escaping as `String`).
            let mut esc = String::from("\"");
            for ch in s.chars() {
                match ch {
                    '"' => esc.push_str("\\\""),
                    '\\' => esc.push_str("\\\\"),
                    '\n' => esc.push_str("\\n"),
                    '\r' => esc.push_str("\\r"),
                    '\t' => esc.push_str("\\t"),
                    _ => esc.push(ch),
                }
            }
            esc.push('"');
            esc
        }
        ConstValue::EnumValue(path) => path.to_string(),
        ConstValue::Struct { fields, .. } => {
            // C++ braced initialization is positional — emit values in
            // declaration order (the semantic layer already reordered them).
            let parts: Vec<String> = fields
                .iter()
                .map(|(_, v)| format_const_value(v, type_))
                .collect();
            format!("{{ {} }}", parts.join(", "))
        }
        ConstValue::Array(elements) => {
            let parts: Vec<String> = elements
                .iter()
                .map(|e| format_const_value(e, type_))
                .collect();
            format!("{{ {} }}", parts.join(", "))
        }
        ConstValue::ConstRef(path) => {
            // Flatten the path to match how nested constants are emitted in
            // C++: `Parent::Const` becomes `Parent_Const` (replace `::` with
            // `_`). Module prefixes are stripped (the C++ backend emits
            // everything into a flat namespace, so module-level constants are
            // just their leaf name).
            let segments: Vec<&str> = path.iter().map(|s| s.as_str()).collect();
            // Strip the first segment if it's a module name (heuristic: if
            // there's more than one segment, the first is likely a module).
            let name_segments = if segments.len() > 1 {
                // Check if the first segment is a module by seeing if the
                // remaining segments form a type+const or just a const.
                // For module-level consts like `consts::MAX_HEALTH`, we want
                // just `MAX_HEALTH`. For nested consts like
                // `Player::IDENTITY`, we want `Player_IDENTITY`.
                // Heuristic: if there are exactly 2 segments, the first is
                // a module → use just the last. If >2, the first is a module
                // and the rest are type+const → join with `_`.
                if segments.len() == 2 {
                    vec![segments[1]]
                } else {
                    segments[1..].to_vec()
                }
            } else {
                segments
            };
            name_segments.join("_")
        }
    }
}

pub(super) fn render_const(
    name: &str,
    cd: &SemanticConstDefinition,
    ctx: RenderCtx,
    location: &ItemLocation,
) -> Result<RenderedItem> {
    let name = &*super::cpp_ident(name);
    let mut decl = String::new();
    super::types::render_doc(&mut decl, &cd.doc, 0, ctx, location)?;
    let type_str = super::render_type(&cd.type_, ctx)?;
    let value_str = format_const_value(&cd.value, &cd.type_);
    // Use `constexpr` for scalar/POD types, `inline const` for
    // struct/array/const-ref types (C++ requires a literal type for constexpr;
    // a ConstRef may point to a struct constant, so treat it conservatively).
    let storage = match &cd.value {
        ConstValue::Struct { .. } | ConstValue::Array(_) | ConstValue::ConstRef(_) => {
            "inline const"
        }
        _ => "constexpr",
    };
    writeln!(decl, "{storage} {type_str} {name} = {value_str};")?;
    Ok(RenderedItem {
        decl,
        post_header: String::new(),
        post_cpp: String::new(),
    })
}

/// Render a free function (`fn foo()` at module scope). For `#[address]`
/// bodies this emits an `extern const fn_t name;` declaration suitable for
/// the `.hpp`; for `#[external_body]` bodies it emits a plain function
/// declaration whose body is supplied by the user's `backend cpp` block.
pub fn render_free_function_decl(func: &Function, ctx: RenderCtx) -> Result<Option<String>> {
    let mut out = String::new();
    super::types::render_doc(&mut out, &func.doc, 0, ctx, &func.location)?;
    let name = super::cpp_ident(&func.name);
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
    let name = super::cpp_ident(&func.name);
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
        .map(|t| super::render_type(t, ctx))
        .transpose()?
        .unwrap_or_else(|| "void".to_string());
    let mut sig_args = Vec::new();
    for arg in &func.arguments {
        if let crate::semantic::types::Argument::Field { name, type_, .. } = arg {
            let ty = super::render_type(type_, ctx)?;
            let escaped = super::cpp_ident(name);
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
        .map(|t| super::render_type(t, ctx))
        .transpose()?
        .unwrap_or_else(|| "void".to_string());
    let cc = super::structs::calling_conv_macro(func.calling_convention);
    let mut arg_types: Vec<String> = Vec::new();
    for arg in &func.arguments {
        let ty = match arg {
            crate::semantic::types::Argument::ConstSelf { .. } => "const void*".to_string(),
            crate::semantic::types::Argument::MutSelf { .. } => "void*".to_string(),
            crate::semantic::types::Argument::Field { type_, .. } => {
                super::render_type(type_, ctx)?
            }
        };
        arg_types.push(ty);
    }
    let args_text = arg_types.join(", ");
    let name = super::cpp_ident(&func.name);
    Ok(format!(
        "using {name}_t = {return_text} ({cc}*)({args_text});"
    ))
}

/// Header-side declaration of an `extern <name>: <type>` value: a getter
/// returning a reference to the value at the address.
pub(super) fn render_extern_value_decl(
    name: &str,
    ev: &SemanticExternValueDefinition,
    ctx: RenderCtx,
) -> Result<String> {
    let ty = super::render_type(&ev.type_, ctx)?;
    let name = super::cpp_ident(name);
    Ok(format!("{ty}& get_{name}();\n"))
}

/// `.cpp` definition for an `extern` value's getter. Three lines plus
/// a trailing blank, so adjacent getters render with the same single-
/// blank rhythm as out-of-class member definitions.
pub(super) fn render_extern_value_definition(
    name: &str,
    ev: &SemanticExternValueDefinition,
    ctx: RenderCtx,
) -> Result<String> {
    let ty = super::render_type(&ev.type_, ctx)?;
    let name = super::cpp_ident(name);
    Ok(format!(
        "{ty}& get_{name}() {{\n    return *reinterpret_cast<{ty}*>(0x{addr:X});\n}}\n\n",
        addr = ev.address,
    ))
}

pub(super) fn render_field(
    out: &mut String,
    region: &Region,
    ctx: RenderCtx,
    rewrite_self_arg_to_void_ptr: bool,
) -> Result<()> {
    render_field_indented(out, region, ctx, rewrite_self_arg_to_void_ptr, 1)
}

pub(super) fn render_field_indented(
    out: &mut String,
    region: &Region,
    ctx: RenderCtx,
    rewrite_self_arg_to_void_ptr: bool,
    indent: usize,
) -> Result<()> {
    let pad = " ".repeat(indent * 4);
    super::types::render_doc(out, &region.doc, indent, ctx, &region.location)?;
    let Some(field_name) = region.name.as_deref() else {
        // Should not happen post-resolution, but be defensive.
        writeln!(out, "{pad}// <unnamed region skipped>")?;
        return Ok(());
    };
    let field_name = super::cpp_ident(field_name);
    // Arrays render as `T name[N1][N2]...` (C++ array dimensions follow the
    // declarator, outer-to-inner); function pointers as `R (cc *name)(args)`;
    // everything else as a plain `T name`.
    match &region.type_ref {
        Type::Array(_, _) => {
            // Walk the nested Array chain to collect every dimension and find
            // the innermost element type, then emit `T name[N1][N2]...`.
            let mut dims: Vec<String> = Vec::new();
            let mut elem = &region.type_ref;
            while let Type::Array(inner, n) = elem {
                dims.push(n.to_string());
                elem = inner;
            }
            let inner_text = super::render_type(elem, ctx)?;
            let suffix = dims
                .iter()
                .map(|d| format!("[{d}]"))
                .collect::<Vec<_>>()
                .join("");
            writeln!(out, "{pad}{inner_text} {field_name}{suffix};")?;
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
            let ty_text = super::render_type(&region.type_ref, ctx)?;
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
    let cc_macro = super::structs::calling_conv_macro(cc);
    let ret_text = ret
        .map(|t| super::render_type(t, ctx))
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
                    _ => super::render_type(t, ctx)?,
                })
            } else {
                super::render_type(t, ctx)
            }
        })
        .collect::<Result<Vec<_>>>()?
        .join(", ");
    Ok(format!("{ret_text} ({cc_macro}*{name})({arg_types})"))
}
