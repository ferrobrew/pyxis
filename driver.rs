use anyhow::Context;

use super::grammar;
use std::{collections::HashMap, fmt, io::Write, path::Path, vec};

// todo: factor architecture in
const POINTER_SIZE: usize = 4;

#[derive(PartialEq, Hash, Eq, Clone, Debug)]
struct ItemPathSegment(String);
impl From<&str> for ItemPathSegment {
    fn from(value: &str) -> Self {
        ItemPathSegment(value.to_string())
    }
}
impl fmt::Display for ItemPathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Hash, Eq, Clone, Debug)]
struct ItemPath(Vec<ItemPathSegment>);
impl ItemPath {
    fn empty() -> ItemPath {
        ItemPath(vec![])
    }

    fn from_str(path: &str) -> ItemPath {
        ItemPath(path.split("::").map(|s| s.into()).collect())
    }

    fn from_path(path: &Path) -> ItemPath {
        // consider making this a result
        assert!(path.is_relative() && path.starts_with("types"));

        ItemPath(
            path.strip_prefix("types")
                .unwrap()
                .parent()
                .map(Path::to_path_buf)
                .unwrap_or_default()
                .iter()
                .chain(std::iter::once(path.file_stem().unwrap_or_default()))
                .map(|s| s.to_string_lossy().as_ref().into())
                .collect(),
        )
    }

    fn parent(&self) -> Option<ItemPath> {
        (!self.0.is_empty()).then(|| ItemPath(self.0[..self.0.len() - 1].to_vec()))
    }

    fn join(&self, segment: ItemPathSegment) -> ItemPath {
        let mut path = self.0.clone();
        path.push(segment);
        ItemPath(path)
    }
}

impl fmt::Display for ItemPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (index, segment) in self.0.iter().enumerate() {
            if index > 0 {
                write!(f, "::")?;
            }
            write!(f, "{}", segment)?;
        }
        Ok(())
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum TypeRef {
    Raw(ItemPath),
    ConstPointer(Box<TypeRef>),
    MutPointer(Box<TypeRef>),
    Array(Box<TypeRef>, usize),
}
impl TypeRef {
    // does not include any pointers/type mutators
    #[allow(dead_code)]
    fn base_path(&self) -> ItemPath {
        match self {
            TypeRef::Raw(path) => path.clone(),
            TypeRef::ConstPointer(tr) => tr.base_path(),
            TypeRef::MutPointer(tr) => tr.base_path(),
            TypeRef::Array(tr, _) => tr.base_path(),
        }
    }

    fn size(&self, type_registry: &TypeRegistry) -> Option<usize> {
        match self {
            TypeRef::Raw(path) => type_registry.get(path).and_then(|t| t.size()),
            TypeRef::ConstPointer(_) => Some(POINTER_SIZE),
            TypeRef::MutPointer(_) => Some(POINTER_SIZE),
            TypeRef::Array(tr, count) => tr.size(type_registry).map(|s| s * count),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum Region {
    Field(String, TypeRef),
    Padding(usize),
}
impl Region {
    fn size(&self, type_registry: &TypeRegistry) -> Option<usize> {
        match self {
            Region::Field(_, type_ref) => type_ref.size(type_registry),
            Region::Padding(size) => Some(*size),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum TypeState {
    Unresolved(grammar::TypeDefinition),
    Resolved { size: usize, regions: Vec<Region> },
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct TypeDefinition {
    path: ItemPath,
    state: TypeState,
}

impl TypeDefinition {
    const fn new_resolved(path: ItemPath, size: usize) -> Self {
        Self {
            path,
            state: TypeState::Resolved {
                size,
                regions: vec![],
            },
        }
    }

    fn size(&self) -> Option<usize> {
        match self.state {
            TypeState::Resolved { size, .. } => Some(size),
            _ => None,
        }
    }
}

pub struct TypeRegistry(HashMap<ItemPath, TypeDefinition>);
impl TypeRegistry {
    fn new() -> TypeRegistry {
        let predefined_types = [
            ("bool", 1),
            ("u8", 1),
            ("u16", 2),
            ("u32", 4),
            ("u64", 8),
            ("u128", 16),
            ("i8", 1),
            ("i16", 2),
            ("i32", 4),
            ("i64", 8),
            ("i128", 16),
        ];

        TypeRegistry(
            predefined_types
                .into_iter()
                .map(|(name, size)| (ItemPath::from_str(name), size))
                .map(|(path, size)| (path.clone(), TypeDefinition::new_resolved(path, size)))
                .collect(),
        )
    }

    fn iter(&self) -> impl Iterator<Item = (&ItemPath, &TypeDefinition)> {
        self.0.iter()
    }

    #[allow(dead_code)]
    fn has(&self, item_path: &ItemPath) -> bool {
        self.0.contains_key(item_path)
    }

    fn get(&self, item_path: &ItemPath) -> Option<&TypeDefinition> {
        self.0.get(item_path)
    }

    fn get_mut(&mut self, item_path: &ItemPath) -> Option<&mut TypeDefinition> {
        self.0.get_mut(item_path)
    }

    fn add(&mut self, type_: TypeDefinition) {
        self.0.insert(type_.path.clone(), type_);
    }

    fn unresolved(&self) -> Vec<ItemPath> {
        self.0
            .iter()
            .filter(|(_, t)| matches!(t.state, TypeState::Unresolved(_)))
            .map(|(k, _)| k.clone())
            .collect()
    }

    fn find_type_by_name(&self, scope: &[&ItemPath], name: &str) -> Option<TypeRef> {
        std::iter::once(&ItemPath::empty())
            .chain(scope.iter().copied())
            .map(|ip| ip.join(name.into()))
            .find(|ip| self.0.contains_key(ip))
            .map(TypeRef::Raw)
    }

    fn resolve_type(&self, scope: &[&ItemPath], type_: &grammar::Type) -> Option<TypeRef> {
        // todo: consider building a better module import/scope system
        match type_ {
            grammar::Type::ConstPointer(t) => self
                .resolve_type(scope, t.as_ref())
                .map(|t| TypeRef::ConstPointer(Box::new(t))),
            grammar::Type::MutPointer(t) => self
                .resolve_type(scope, t.as_ref())
                .map(|t| TypeRef::MutPointer(Box::new(t))),
            grammar::Type::Ident(ident) => self.find_type_by_name(scope, ident.0.as_str()),
        }
    }

    fn resolve_typeref(&self, scope: &[&ItemPath], type_ref: &grammar::TypeRef) -> Option<TypeRef> {
        match type_ref {
            grammar::TypeRef::Type(type_) => self.resolve_type(scope, type_),
            grammar::TypeRef::Macro(macro_call) => match macro_call.match_repr() {
                ("unk", [grammar::Expr::IntLiteral(size)]) => self
                    .find_type_by_name(&[], "u8")
                    .map(|t| TypeRef::Array(Box::new(t), *size as usize)),
                _ => panic!("unsupported macro call"),
            },
        }
    }
}

pub struct Compiler {
    files: HashMap<ItemPath, grammar::Module>,
    type_registry: TypeRegistry,
}

impl Compiler {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            type_registry: TypeRegistry::new(),
        }
    }

    // todo: define an actual error type
    fn add_file(&mut self, path: &Path) -> anyhow::Result<()> {
        self.add_module(
            &super::parser::parse_str(&std::fs::read_to_string(path)?)?,
            &ItemPath::from_path(path),
        )
    }

    fn add_module(
        &mut self,
        module: &grammar::Module,
        path: &ItemPath,
    ) -> Result<(), anyhow::Error> {
        for definition in &module.definitions {
            let path = path.join(definition.name.0.as_str().into());
            self.type_registry.add(TypeDefinition {
                path,
                state: TypeState::Unresolved(definition.clone()),
            })
        }
        self.files.insert(path.clone(), module.clone());
        Ok(())
    }

    fn build(&mut self) -> anyhow::Result<()> {
        loop {
            let to_resolve = self.type_registry.unresolved();
            if to_resolve.is_empty() {
                break;
            }

            for resolvee_path in &to_resolve {
                if let TypeState::Unresolved(definition) = self
                    .type_registry
                    .get(resolvee_path)
                    .context("failed to get type")?
                    .state
                    .clone()
                {
                    self.build_type(resolvee_path, &definition)?;
                }
            }
        }

        Ok(())
    }

    fn build_type(
        &mut self,
        resolvee_path: &ItemPath,
        definition: &grammar::TypeDefinition,
    ) -> Result<(), anyhow::Error> {
        let parent_path = resolvee_path
            .parent()
            .unwrap_or_else(|| resolvee_path.clone());

        let build_region_from_field = |grammar::TypeField(ident, type_ref): &grammar::TypeField| {
            let type_ref = self
                .type_registry
                .resolve_typeref(&[&parent_path], type_ref);

            type_ref.map(|tr| (None, Region::Field(ident.0.clone(), tr)))
        };

        let mut target_size: Option<usize> = None;
        let mut regions: Vec<(Option<usize>, Region)> = vec![];
        for statement in &definition.statements {
            match statement {
                grammar::TypeStatement::Meta(fields) => {
                    if let Some(grammar::ExprField(_, grammar::Expr::IntLiteral(size))) =
                        fields.iter().find(|tf| tf.0 .0 == "size")
                    {
                        target_size = Some(*size as usize);
                    }
                }
                grammar::TypeStatement::Address(address, fields) => {
                    regions.push((Some(*address), Region::Padding(0)));
                    for type_field in fields {
                        if let Some(region_pair) = build_region_from_field(type_field) {
                            regions.push(region_pair);
                        } else {
                            return Ok(());
                        }
                    }
                }
                grammar::TypeStatement::Field(type_field) => {
                    if let Some(region_pair) = build_region_from_field(type_field) {
                        regions.push(region_pair);
                    } else {
                        return Ok(());
                    }
                }
                grammar::TypeStatement::Macro(macro_call) => {
                    if let ("padding", [grammar::Expr::IntLiteral(size)]) = macro_call.match_repr()
                    {
                        regions.push((None, Region::Padding(*size as usize)));
                    }
                }
                _ => (),
            };
        }

        // this resolution algorithm is very simple and doesn't handle overlapping regions
        // or regions that are out of order
        let mut last_address: usize = 0;
        let mut resolved_regions = vec![];
        for (offset, region) in regions {
            if let Some(offset) = offset {
                let size = offset - last_address;
                resolved_regions.push(Region::Padding(size));
                last_address += size;
            }

            let region_size = match region.size(&self.type_registry) {
                Some(size) => size,
                None => return Ok(()),
            };

            if region_size == 0 {
                continue;
            }

            resolved_regions.push(region);
            last_address += region_size;
        }

        if let Some(target_size) = target_size {
            if last_address < target_size {
                resolved_regions.push(Region::Padding(target_size - last_address));
            }
        }

        let sizes = resolved_regions
            .iter()
            .map(|r: &Region| r.size(&self.type_registry))
            .collect::<Option<Vec<_>>>();

        if let Some(sizes) = sizes {
            let size = sizes.into_iter().sum();
            self.type_registry.get_mut(resolvee_path).unwrap().state = TypeState::Resolved {
                size,
                regions: resolved_regions,
            };
        }
        Ok(())
    }

    pub fn type_registry(&self) -> &TypeRegistry {
        &self.type_registry
    }
}

pub fn build_type_definitions() -> anyhow::Result<()> {
    let mut compiler = Compiler::new();

    for path in glob::glob("types/**/*.rstl")?.filter_map(Result::ok) {
        compiler.add_file(&path)?;
    }
    compiler.build()?;

    let mut file = std::fs::File::create("rstl.md")?;
    writeln!(file, "# Types")?;
    writeln!(file)?;
    writeln!(file, "- Pointer size: {}", POINTER_SIZE)?;
    for (path, type_) in compiler.type_registry().iter() {
        writeln!(file, "- `{}`: {:?}", path, type_.state)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn build_type(module: &grammar::Module, path: &ItemPath) -> anyhow::Result<TypeDefinition> {
        let mut compiler = Compiler::new();
        compiler.add_module(module, &path.parent().context("failed to get path parent")?)?;
        compiler.build()?;
        compiler
            .type_registry()
            .get(path)
            .cloned()
            .context("failed to get type")
    }

    #[test]
    fn can_resolve_basic_struct() {
        let module = {
            use super::grammar::*;

            type TS = TypeStatement;
            type TR = TypeRef;

            Module::new(&[TypeDefinition::new(
                "TestType",
                &[
                    TS::field("field_1", TR::ident_type("i32")),
                    TS::macro_("padding", &[Expr::IntLiteral(4)]),
                    TS::field("field_2", TR::ident_type("u64")),
                ],
            )])
        };

        let path = ItemPath::from_str("test::TestType");
        let type_definition = TypeDefinition {
            path: path.clone(),
            state: TypeState::Resolved {
                size: 16,
                regions: vec![
                    Region::Field("field_1".into(), TypeRef::Raw(ItemPath::from_str("i32"))),
                    Region::Padding(4),
                    Region::Field("field_2".into(), TypeRef::Raw(ItemPath::from_str("u64"))),
                ],
            },
        };

        assert_eq!(build_type(&module, &path).unwrap(), type_definition);
    }

    #[test]
    fn can_resolve_pointer_to_another_struct() {
        let module = {
            use super::grammar::*;

            type TS = TypeStatement;
            type TR = TypeRef;

            Module::new(&[
                TypeDefinition::new("TestType1", &[TS::field("field_1", TR::ident_type("u64"))]),
                TypeDefinition::new(
                    "TestType2",
                    &[
                        TS::field("field_1", TR::ident_type("i32")),
                        TS::field("field_2", TR::ident_type("TestType1")),
                        TS::field(
                            "field_3",
                            TR::Type(Type::ident("TestType1").const_pointer()),
                        ),
                        TS::field("field_4", TR::Type(Type::ident("TestType1").mut_pointer())),
                    ],
                ),
            ])
        };

        let path = ItemPath::from_str("test::TestType2");
        let type_definition = TypeDefinition {
            path: path.clone(),
            state: TypeState::Resolved {
                size: 20,
                regions: vec![
                    Region::Field("field_1".into(), TypeRef::Raw(ItemPath::from_str("i32"))),
                    Region::Field(
                        "field_2".into(),
                        TypeRef::Raw(ItemPath::from_str("test::TestType1")),
                    ),
                    Region::Field(
                        "field_3".into(),
                        TypeRef::ConstPointer(Box::new(TypeRef::Raw(ItemPath::from_str(
                            "test::TestType1",
                        )))),
                    ),
                    Region::Field(
                        "field_4".into(),
                        TypeRef::MutPointer(Box::new(TypeRef::Raw(ItemPath::from_str(
                            "test::TestType1",
                        )))),
                    ),
                ],
            },
        };

        assert_eq!(build_type(&module, &path).unwrap(), type_definition);
    }

    #[test]
    fn can_resolve_complex_type() {
        let module = {
            use super::grammar::*;

            type T = Type;
            type TS = TypeStatement;
            type TR = TypeRef;
            type A = Argument;

            Module::new(&[
                TypeDefinition::new(
                    "TestType",
                    &[
                        TS::field("field_1", TR::ident_type("i32")),
                        TS::macro_("padding", &[Expr::IntLiteral(4)]),
                    ],
                ),
                TypeDefinition::new(
                    "Singleton",
                    &[
                        TS::meta(&[
                            ("size", Expr::IntLiteral(0x1750)),
                            ("singleton", Expr::IntLiteral(0x1_200_000)),
                        ]),
                        TS::address(
                            0x78,
                            &[
                                ("max_num_1", TR::ident_type("u16")),
                                ("max_num_2", TR::ident_type("u16")),
                            ],
                        ),
                        TS::address(
                            0xA00,
                            &[
                                ("test_type", TR::ident_type("TestType")),
                                ("settings", MacroCall::unk(804).into()),
                            ],
                        ),
                        TS::functions(&[(
                            "free",
                            &[Function::new(
                                "test_function",
                                &[Attribute::address(0x800_000)],
                                &[
                                    A::MutSelf,
                                    A::field("arg1", T::ident("TestType").mut_pointer().into()),
                                    A::field("arg2", TR::ident_type("i32")),
                                    A::field("arg3", T::ident("u32").const_pointer().into()),
                                ],
                                Some(Type::ident("TestType").mut_pointer()),
                            )],
                        )]),
                    ],
                ),
            ])
        };

        let path = ItemPath::from_str("test::Singleton");
        // todo: handle functions
        let type_definition = TypeDefinition {
            path: path.clone(),
            state: TypeState::Resolved {
                size: 0x1750,
                regions: vec![
                    Region::Padding(0x78),
                    Region::Field("max_num_1".into(), TypeRef::Raw(ItemPath::from_str("u16"))),
                    Region::Field("max_num_2".into(), TypeRef::Raw(ItemPath::from_str("u16"))),
                    Region::Padding(0x984),
                    Region::Field(
                        "test_type".into(),
                        TypeRef::Raw(ItemPath::from_str("test::TestType")),
                    ),
                    Region::Field(
                        "settings".into(),
                        TypeRef::Array(Box::new(TypeRef::Raw(ItemPath::from_str("u8"))), 804),
                    ),
                    Region::Padding(0xA24),
                ],
            },
        };

        assert_eq!(build_type(&module, &path).unwrap(), type_definition);
    }
}
