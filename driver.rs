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

    fn from_segments(segments: &[&str]) -> ItemPath {
        ItemPath(segments.iter().copied().map(|s| s.into()).collect())
    }

    fn from_str(path: &str) -> ItemPath {
        ItemPath(path.split("::").map(|s| s.into()).collect())
    }

    // ignores src
    fn from_path(path: &Path) -> ItemPath {
        // consider making this a result
        assert!(path.is_relative() && path.starts_with("src"));

        ItemPath(
            path.strip_prefix("src")
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
    fn size(&self, type_registry: &TypeRegistry) -> usize {
        match self {
            TypeRef::Raw(path) => type_registry
                .get(path)
                .and_then(|t| t.size())
                .expect("unresolved type size"),
            TypeRef::ConstPointer(_) => POINTER_SIZE,
            TypeRef::MutPointer(_) => POINTER_SIZE,
            TypeRef::Array(tr, count) => tr.size(type_registry) * count,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum Region {
    Field(String, TypeRef),
    Padding(usize),
}
impl Region {
    fn size(&self, type_registry: &TypeRegistry) -> usize {
        match self {
            Region::Field(_, type_ref) => type_ref.size(type_registry),
            Region::Padding(size) => *size,
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
        let module = super::parser::parse_str(&std::fs::read_to_string(path)?)?;

        let path = ItemPath::from_path(path);
        for definition in &module.definitions {
            let path = path.join(definition.name.0.as_str().into());
            self.type_registry.add(TypeDefinition {
                path,
                state: TypeState::Unresolved(definition.clone()),
            })
        }

        self.files.insert(path, module);

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
        let mut regions: Vec<(Option<usize>, Region)> = vec![];

        let parent_path = resolvee_path.parent().unwrap_or(resolvee_path.clone());
        for statement in &definition.statements {
            match statement {
                grammar::TypeStatement::Address(address, fields) => {
                    // regions.push((None, ))
                }
                grammar::TypeStatement::Field(grammar::TypeField(ident, type_ref)) => {
                    let type_ref = self
                        .type_registry
                        .resolve_typeref(&[&parent_path], type_ref);

                    if let Some(type_ref) = type_ref {
                        regions.push((None, Region::Field(ident.0.clone(), type_ref)));
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

        // todo: actual region resolution
        let regions: Vec<_> = regions.into_iter().map(|(_, r)| r).collect();
        self.type_registry.get_mut(resolvee_path).unwrap().state = TypeState::Resolved {
            size: regions
                .iter()
                .map(|r: &Region| r.size(&self.type_registry))
                .sum(),
            regions,
        };
        Ok(())
    }

    pub fn type_registry(&self) -> &TypeRegistry {
        &self.type_registry
    }
}

pub fn build_type_definitions() -> anyhow::Result<()> {
    let mut compiler = Compiler::new();

    for path in glob::glob("src/**/*.rstl")?.filter_map(Result::ok) {
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
