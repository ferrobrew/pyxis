//! Item → C++ text rendering for the C++ backend.
//!
//! Owns the conversion of resolved IR items (structs, enums, bitflags, type
//! aliases) into the textual output for `.hpp` files. Vftables, functions,
//! generics, externs, and per-module prologue/epilogue are introduced in
//! later phases.

use std::fmt::Write;

use crate::{
    backends::Result,
    grammar::ItemPath,
    semantic::{
        TypeRegistry,
        types::{
            BitflagField, BitflagsDefinition, EnumDefinition, EnumVariant, ItemDefinition,
            ItemDefinitionInner, PredefinedItem, Region, Type, TypeAliasDefinition, TypeDefinition,
        },
    },
};

/// Render a single item as a C++ definition. Returns `None` if the item
/// doesn't produce direct output in this phase (generics, predefined, extern).
pub fn render_item(
    item: &ItemDefinition,
    module_path: &ItemPath,
    registry: &TypeRegistry,
) -> Result<Option<String>> {
    if item.is_predefined() {
        return Ok(None);
    }
    if matches!(item.category, crate::semantic::types::ItemCategory::Extern) {
        return Ok(None);
    }
    if item.is_generic() {
        // Generics become C++ templates in Phase 3.
        return Ok(Some(format!(
            "// TODO(cpp/phase 3): generic type `{}`\n",
            item.path
        )));
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

    let out = match &resolved.inner {
        ItemDefinitionInner::Type(td) => render_struct(
            &name,
            module_path,
            td,
            resolved.size,
            resolved.alignment,
            registry,
        )?,
        ItemDefinitionInner::Enum(ed) => render_enum(&name, ed, resolved.size, registry)?,
        ItemDefinitionInner::Bitflags(bd) => render_bitflags(&name, bd, resolved.size, registry)?,
        ItemDefinitionInner::TypeAlias(ta) => render_type_alias(&name, ta, module_path, registry)?,
    };
    Ok(Some(out))
}

fn render_struct(
    name: &str,
    module_path: &ItemPath,
    td: &TypeDefinition,
    size: usize,
    alignment: usize,
    registry: &TypeRegistry,
) -> Result<String> {
    let mut out = String::new();
    render_doc(&mut out, &td.doc, 0)?;
    if td.packed {
        writeln!(out, "#pragma pack(push, 1)")?;
    }
    writeln!(out, "struct alignas({alignment}) {name} {{")?;

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

    writeln!(out, "}};")?;
    if td.packed {
        writeln!(out, "#pragma pack(pop)")?;
    }

    // Layout assertions.
    if size > 0 {
        writeln!(out, "static_assert(sizeof({name}) == 0x{size:X});")?;
    }
    writeln!(out, "static_assert(alignof({name}) == {alignment});")?;
    // Per-field offsetof asserts come in Phase 4 once the dep graph carries
    // the resolved offsets; size+alignment asserts above are the immediate
    // load-bearing checks.

    Ok(out)
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
    // Arrays render as `T name[N]` — render_type returns the bare element
    // type for arrays so we wrap appropriately here.
    if let Type::Array(inner, n) = &region.type_ref {
        let inner_text = render_type(inner, module_path, registry)?;
        writeln!(out, "    {inner_text} {field_name}[{n}];")?;
    } else {
        let ty_text = render_type(&region.type_ref, module_path, registry)?;
        writeln!(out, "    {ty_text} {field_name};")?;
    }
    Ok(())
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
) -> Result<String> {
    let mut out = String::new();
    render_doc(&mut out, &ta.doc, 0)?;
    let target = render_type(&ta.target, module_path, registry)?;
    writeln!(out, "using {name} = {target};")?;
    Ok(out)
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
        Type::Function(_, _, _) => {
            // Phase 2 introduces real function-pointer typedefs with calling
            // conventions; for now treat as opaque.
            "void*".to_string()
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
