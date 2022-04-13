use anyhow::Context;

use super::grammar::{self, ItemPath};
use std::{
    collections::{HashMap, HashSet},
    fmt,
    path::Path,
    vec,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute {
    Address(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Argument {
    ConstSelf,
    MutSelf,
    Field(String, TypeRef),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub attributes: Vec<Attribute>,
    pub arguments: Vec<Argument>,
    pub return_type: Option<TypeRef>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TypeRef {
    Raw(ItemPath),
    ConstPointer(Box<TypeRef>),
    MutPointer(Box<TypeRef>),
    Array(Box<TypeRef>, usize),
    Function(Vec<(String, Box<TypeRef>)>, Option<Box<TypeRef>>),
}
impl TypeRef {
    fn size(&self, type_registry: &TypeRegistry) -> Option<usize> {
        match self {
            TypeRef::Raw(path) => type_registry.get(path).and_then(|t| t.size()),
            TypeRef::ConstPointer(_) => Some(type_registry.pointer_size()),
            TypeRef::MutPointer(_) => Some(type_registry.pointer_size()),
            TypeRef::Array(tr, count) => tr.size(type_registry).map(|s| s * count),
            TypeRef::Function(_, _) => Some(type_registry.pointer_size()),
        }
    }
}
impl fmt::Display for TypeRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeRef::Raw(path) => write!(f, "{}", path),
            TypeRef::ConstPointer(tr) => {
                write!(f, "*const ")?;
                tr.fmt(f)
            }
            TypeRef::MutPointer(tr) => {
                write!(f, "*mut ")?;
                tr.fmt(f)
            }
            TypeRef::Array(tr, size) => {
                write!(f, "[")?;
                tr.fmt(f)?;
                write!(f, "; {}]", size)
            }
            TypeRef::Function(args, return_type) => {
                write!(f, "fn (")?;
                for (index, (field, type_ref)) in args.iter().enumerate() {
                    write!(f, "{field}: ")?;
                    type_ref.fmt(f)?;
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")?;
                if let Some(type_ref) = return_type {
                    write!(f, " -> ")?;
                    type_ref.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Region {
    Field(String, TypeRef),
    Padding(usize),
}
impl Region {
    pub fn size(&self, type_registry: &TypeRegistry) -> Option<usize> {
        match self {
            Region::Field(_, type_ref) => type_ref.size(type_registry),
            Region::Padding(size) => Some(*size),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum MetadataValue {
    Integer(isize),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TypeStateResolved {
    pub size: usize,
    pub regions: Vec<Region>,
    pub functions: HashMap<String, Vec<Function>>,
    pub metadata: HashMap<String, MetadataValue>,
}

impl TypeStateResolved {
    pub fn new(size: usize) -> Self {
        Self {
            size,
            regions: Default::default(),
            functions: Default::default(),
            metadata: Default::default(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TypeState {
    Unresolved(grammar::TypeDefinition),
    Resolved(TypeStateResolved),
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum TypeCategory {
    Defined,
    Predefined,
    Extern,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TypeDefinition {
    pub path: ItemPath,
    pub state: TypeState,
    pub category: TypeCategory,
}
impl TypeDefinition {
    pub fn resolved(&self) -> Option<&TypeStateResolved> {
        match &self.state {
            TypeState::Resolved(tsr) => Some(tsr),
            _ => None,
        }
    }

    pub fn size(&self) -> Option<usize> {
        self.resolved().map(|r| r.size)
    }

    pub fn is_resolved(&self) -> bool {
        self.resolved().is_some()
    }

    pub fn is_defined(&self) -> bool {
        self.category == TypeCategory::Defined
    }

    pub fn is_predefined(&self) -> bool {
        self.category == TypeCategory::Predefined
    }

    pub fn is_extern(&self) -> bool {
        self.category == TypeCategory::Extern
    }

    pub fn category(&self) -> TypeCategory {
        self.category
    }
}

pub struct TypeRegistry {
    types: HashMap<ItemPath, TypeDefinition>,
    pointer_size: usize,
}
impl TypeRegistry {
    fn new(pointer_size: usize) -> TypeRegistry {
        TypeRegistry {
            types: HashMap::new(),
            pointer_size,
        }
    }

    pub fn pointer_size(&self) -> usize {
        self.pointer_size
    }

    pub fn iter(&self) -> impl Iterator<Item = (&ItemPath, &TypeDefinition)> {
        self.types.iter()
    }

    pub fn has(&self, item_path: &ItemPath) -> bool {
        self.types.contains_key(item_path)
    }

    pub fn get(&self, item_path: &ItemPath) -> Option<&TypeDefinition> {
        self.types.get(item_path)
    }

    pub fn get_mut(&mut self, item_path: &ItemPath) -> Option<&mut TypeDefinition> {
        self.types.get_mut(item_path)
    }

    fn resolved(&self) -> Vec<ItemPath> {
        self.types
            .iter()
            .filter(|(_, t)| !t.is_predefined() && t.is_resolved())
            .map(|(k, _)| k.clone())
            .collect()
    }

    fn unresolved(&self) -> Vec<ItemPath> {
        self.types
            .iter()
            .filter(|(_, t)| !t.is_predefined() && !t.is_resolved())
            .map(|(k, _)| k.clone())
            .collect()
    }

    fn add(&mut self, type_: TypeDefinition) {
        self.types.insert(type_.path.clone(), type_);
    }

    fn resolve_string(&self, scope: &[&ItemPath], name: &str) -> Option<TypeRef> {
        // todo: take scope_modules and scope_types instead of scope so that we don't need
        // to do this partitioning
        let (scope_types, scope_modules): (Vec<&ItemPath>, Vec<&ItemPath>) =
            scope.iter().partition(|ip| self.types.contains_key(ip));

        // If we find the relevant type within our scope, take the last one
        scope_types
            .into_iter()
            .rev()
            .find(|st| st.last().map(|i| i.as_str()) == Some(name))
            .map(|ip| TypeRef::Raw(ip.clone()))
            .or_else(|| {
                // Otherwise, search our scopes
                std::iter::once(&ItemPath::empty())
                    .chain(scope_modules.iter().copied())
                    .map(|ip| ip.join(name.into()))
                    .find(|ip| self.types.contains_key(ip))
                    .map(TypeRef::Raw)
            })
    }

    fn resolve_grammar_type(&self, scope: &[&ItemPath], type_: &grammar::Type) -> Option<TypeRef> {
        // todo: consider building a better module import/scope system
        match type_ {
            grammar::Type::ConstPointer(t) => self
                .resolve_grammar_type(scope, t.as_ref())
                .map(|t| TypeRef::ConstPointer(Box::new(t))),
            grammar::Type::MutPointer(t) => self
                .resolve_grammar_type(scope, t.as_ref())
                .map(|t| TypeRef::MutPointer(Box::new(t))),
            grammar::Type::Ident(ident) => self.resolve_string(scope, ident.as_str()),
        }
    }

    fn resolve_grammar_typeref(
        &self,
        scope: &[&ItemPath],
        type_ref: &grammar::TypeRef,
    ) -> Option<TypeRef> {
        match type_ref {
            grammar::TypeRef::Type(type_) => self.resolve_grammar_type(scope, type_),
            grammar::TypeRef::Macro(macro_call) => match macro_call.match_repr() {
                ("unk", [grammar::Expr::IntLiteral(size)]) => self
                    .resolve_string(&[], "u8")
                    .map(|t| TypeRef::Array(Box::new(t), *size as usize)),
                _ => panic!("unsupported macro call"),
            },
        }
    }
}

#[derive(Debug)]
pub struct Module {
    ast: grammar::Module,
    definition_paths: HashSet<ItemPath>,
}
impl Module {
    fn new(ast: grammar::Module) -> Self {
        Self {
            ast,
            definition_paths: HashSet::new(),
        }
    }

    pub fn ast(&self) -> &grammar::Module {
        &self.ast
    }

    pub fn uses(&self) -> &[ItemPath] {
        &self.ast.uses
    }

    pub fn definition_paths(&self) -> &HashSet<ItemPath> {
        &self.definition_paths
    }

    pub fn definitions<'a>(
        &'a self,
        type_registry: &'a TypeRegistry,
    ) -> impl Iterator<Item = &'a TypeDefinition> {
        self.definition_paths
            .iter()
            .filter_map(|p| type_registry.get(p))
    }
}

pub struct SemanticState {
    modules: HashMap<ItemPath, Module>,
    type_registry: TypeRegistry,
}

impl SemanticState {
    pub fn new(pointer_size: usize) -> Self {
        let mut semantic_state = Self {
            modules: HashMap::new(),
            type_registry: TypeRegistry::new(pointer_size),
        };

        // Insert the empty root module.
        semantic_state
            .modules
            .insert(ItemPath::empty(), Module::new(grammar::Module::default()));

        // Insert all of our predefined types.
        let predefined_types = [
            ("void", 0),
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
            ("f32", 4),
            ("f64", 8),
        ];

        for (name, size) in predefined_types {
            let path = ItemPath::from_colon_delimited_str(name);
            semantic_state
                .add_type(TypeDefinition {
                    path: path,
                    state: TypeState::Resolved(TypeStateResolved::new(size)),
                    category: TypeCategory::Predefined,
                })
                .expect("failed to add predefined type");
        }

        semantic_state
    }

    // todo: define an actual error type
    pub fn add_file(&mut self, path: &Path) -> anyhow::Result<()> {
        self.add_module(
            &super::parser::parse_str(&std::fs::read_to_string(path)?)
                .context(format!("{:?}", path))?,
            &ItemPath::from_path(path),
        )
    }

    pub fn add_module(&mut self, module: &grammar::Module, path: &ItemPath) -> anyhow::Result<()> {
        self.modules
            .insert(path.clone(), Module::new(module.clone()));

        for definition in &module.definitions {
            let path = path.join(definition.name.as_str().into());
            self.add_type(TypeDefinition {
                path: path.clone(),
                state: TypeState::Unresolved(definition.clone()),
                category: TypeCategory::Defined,
            })?;
        }

        for (extern_path, fields) in &module.externs {
            let singleton = fields
                .iter()
                .find(|ef| ef.ident_as_str() == "singleton")
                .and_then(|ef| ef.1.int_literal());

            let size = if singleton.is_some() {
                0
            } else {
                fields
                    .iter()
                    .find(|ef| ef.ident_as_str() == "size")
                    .context("failed to find size field in extern type for module")?
                    .1
                    .int_literal()
                    .context("size field of extern type is not an int literal")?
                    .try_into()
                    .context("the size could not be converted into an unsigned integer")?
            };

            let extern_path = path.join(
                extern_path
                    .last()
                    .context("failed to get extern path segment")?
                    .clone(),
            );

            let mut type_state_resolved = TypeStateResolved::new(size);
            if let Some(address) = singleton {
                type_state_resolved
                    .metadata
                    .insert("singleton".to_string(), MetadataValue::Integer(address));
            }

            let path = extern_path.clone();
            self.add_type(TypeDefinition {
                path,
                state: TypeState::Resolved(type_state_resolved),
                category: TypeCategory::Extern,
            })?;
        }
        Ok(())
    }

    pub fn add_type(&mut self, type_definition: TypeDefinition) -> anyhow::Result<()> {
        let parent_path = &type_definition
            .path
            .parent()
            .context("failed to get parent type")?;
        self.modules
            .get_mut(parent_path)
            .context("failed to get module")?
            .definition_paths
            .insert(type_definition.path.clone());
        Ok(self.type_registry.add(type_definition))
    }

    // todo: consider consuming self and returning a new type
    pub fn build(&mut self) -> anyhow::Result<()> {
        loop {
            let to_resolve = self.type_registry.unresolved();
            if to_resolve.is_empty() {
                break Ok(());
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

            if to_resolve == self.type_registry.unresolved() {
                // Oh no! We failed to resolve any new types!
                // Bail from the loop.
                return Err(anyhow::anyhow!(
                    "type resolution will not terminate, failed on types: {:?} (resolved types: {:?})",
                    Vec::from_iter(to_resolve.iter().map(|s| s.to_string())),
                    Vec::from_iter(self.type_registry.resolved().iter().map(|s| s.to_string())),
                ));
            }
        }
    }

    fn build_function(
        &self,
        scope: &[&ItemPath],
        function: &grammar::Function,
    ) -> Result<Function, anyhow::Error> {
        let attributes = function
            .attributes
            .iter()
            .map(|a| match a {
                grammar::Attribute::Function(ident, exprs) => match (ident.as_str(), &exprs[..]) {
                    ("address", [grammar::Expr::IntLiteral(address)]) => {
                        Ok(Attribute::Address(*address as usize))
                    }
                    (_, _) => Err(anyhow::anyhow!(
                        "failed to resolve function attribute, unsupported name"
                    )),
                },
            })
            .collect::<Result<Vec<_>, _>>()?;

        let arguments = function
            .arguments
            .iter()
            .map(|a| match a {
                grammar::Argument::ConstSelf => Ok(Argument::ConstSelf),
                grammar::Argument::MutSelf => Ok(Argument::MutSelf),
                grammar::Argument::Field(grammar::TypeField(name, type_ref)) => {
                    Ok(Argument::Field(
                        name.0.clone(),
                        self.type_registry
                            .resolve_grammar_typeref(scope, type_ref)
                            .ok_or_else(|| {
                                anyhow::anyhow!(
                                    "failed to resolve type of field {:?} ({:?}",
                                    name,
                                    type_ref
                                )
                            })?,
                    ))
                }
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        let return_type = function
            .return_type
            .as_ref()
            .and_then(|t| self.type_registry.resolve_grammar_type(scope, t));

        Ok(Function {
            name: function.name.0.clone(),
            attributes,
            arguments,
            return_type,
        })
    }

    fn build_type(
        &mut self,
        resolvee_path: &ItemPath,
        definition: &grammar::TypeDefinition,
    ) -> anyhow::Result<()> {
        let parent_path = resolvee_path
            .parent()
            .unwrap_or_else(|| resolvee_path.clone());

        let scope = {
            let mut v = vec![&parent_path];
            v.append(
                &mut self
                    .modules
                    .get(&parent_path)
                    .map(|f| f.uses().iter().collect())
                    .unwrap_or_default(),
            );
            v
        };

        let build_region_from_field = |grammar::TypeField(ident, type_ref): &grammar::TypeField| {
            self.type_registry
                .resolve_grammar_typeref(&scope, type_ref)
                .map(|tr| (None, Region::Field(ident.0.clone(), tr)))
        };

        let mut target_size: Option<usize> = None;
        let mut regions: Vec<(Option<usize>, Region)> = vec![];
        let mut metadata: HashMap<String, MetadataValue> = HashMap::new();
        let mut functions: HashMap<String, Vec<Function>> = HashMap::new();
        for statement in &definition.statements {
            match statement {
                grammar::TypeStatement::Meta(fields) => {
                    for field in fields {
                        if let grammar::ExprField(ident, grammar::Expr::IntLiteral(value)) = field {
                            if ident.0 == "size" {
                                target_size = Some(*value as usize);
                            } else if ident.0 == "singleton" {
                                metadata.insert(
                                    "singleton".to_string(),
                                    MetadataValue::Integer(*value as isize),
                                );
                            }
                        }
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
                grammar::TypeStatement::Functions(functions_by_category) => {
                    functions = functions_by_category
                        .iter()
                        .map(|(category, functions)| {
                            Ok((
                                category.0.clone(),
                                functions
                                    .iter()
                                    .map(|function| self.build_function(&scope, function))
                                    .collect::<Result<Vec<_>, _>>()?,
                            ))
                        })
                        .collect::<anyhow::Result<HashMap<_, _>>>()?;
                }
            };
        }

        // this resolution algorithm is very simple and doesn't handle overlapping regions
        // or regions that are out of order
        let mut last_address: usize = 0;
        let mut resolved_regions = vec![];

        let vftable = functions.get(&"vftable".to_string());
        if let (Some(vftable), Some(name)) = (vftable, resolvee_path.last()) {
            let resolvee_vtable_path = parent_path.join(format!("{}Vftable", name.as_str()).into());
            let function_to_field = |function: &Function| -> Region {
                let argument_to_type = |argument: &Argument| -> (String, Box<TypeRef>) {
                    match argument {
                        Argument::ConstSelf => (
                            "this".to_string(),
                            Box::new(TypeRef::ConstPointer(Box::new(TypeRef::Raw(
                                resolvee_path.clone(),
                            )))),
                        ),
                        Argument::MutSelf => (
                            "this".to_string(),
                            Box::new(TypeRef::MutPointer(Box::new(TypeRef::Raw(
                                resolvee_path.clone(),
                            )))),
                        ),
                        Argument::Field(name, type_ref) => {
                            (name.clone(), Box::new(type_ref.clone()))
                        }
                    }
                };
                let arguments = function.arguments.iter().map(argument_to_type).collect();
                let return_type = function.return_type.as_ref().map(|t| Box::new(t.clone()));

                Region::Field(
                    function.name.clone(),
                    TypeRef::Function(arguments, return_type),
                )
            };
            self.add_type(TypeDefinition {
                path: resolvee_vtable_path.clone(),
                state: TypeState::Resolved(TypeStateResolved {
                    size: 0,
                    regions: vftable.iter().map(function_to_field).collect(),
                    functions: HashMap::new(),
                    metadata: HashMap::new(),
                }),
                category: TypeCategory::Defined,
            })?;

            resolved_regions.push(Region::Field(
                "vftable".to_string(),
                TypeRef::ConstPointer(Box::new(TypeRef::Raw(resolvee_vtable_path))),
            ));
            last_address += self.type_registry.pointer_size();
        }

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
            self.type_registry.get_mut(resolvee_path).unwrap().state =
                TypeState::Resolved(TypeStateResolved {
                    size,
                    regions: resolved_regions,
                    functions,
                    metadata,
                });
        }
        Ok(())
    }

    pub fn type_registry(&self) -> &TypeRegistry {
        &self.type_registry
    }

    pub fn modules(&self) -> &HashMap<ItemPath, Module> {
        &self.modules
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn build_state(module: &grammar::Module, path: &ItemPath) -> anyhow::Result<SemanticState> {
        let mut semantic_state = SemanticState::new(4);
        semantic_state.add_module(module, &path.parent().context("failed to get path parent")?)?;
        semantic_state.build()?;
        Ok(semantic_state)
    }

    fn build_type(module: &grammar::Module, path: &ItemPath) -> anyhow::Result<TypeDefinition> {
        build_state(module, path)?
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

            Module::new(
                &[],
                &[],
                &[TypeDefinition::new(
                    "TestType",
                    &[
                        TS::field("field_1", TR::ident_type("i32")),
                        TS::macro_("padding", &[Expr::IntLiteral(4)]),
                        TS::field("field_2", TR::ident_type("u64")),
                    ],
                )],
            )
        };

        let path = ItemPath::from_colon_delimited_str("test::TestType");
        let type_definition = TypeDefinition {
            path: path.clone(),
            state: TypeState::Resolved(TypeStateResolved {
                size: 16,
                regions: vec![
                    Region::Field(
                        "field_1".into(),
                        TypeRef::Raw(ItemPath::from_colon_delimited_str("i32")),
                    ),
                    Region::Padding(4),
                    Region::Field(
                        "field_2".into(),
                        TypeRef::Raw(ItemPath::from_colon_delimited_str("u64")),
                    ),
                ],
                functions: HashMap::new(),
                metadata: HashMap::new(),
            }),
            category: TypeCategory::Defined,
        };

        assert_eq!(build_type(&module, &path).unwrap(), type_definition);
    }

    #[test]
    fn can_resolve_pointer_to_another_struct() {
        let module = {
            use super::grammar::*;

            type TS = TypeStatement;
            type TR = TypeRef;

            Module::new(
                &[],
                &[],
                &[
                    TypeDefinition::new(
                        "TestType1",
                        &[TS::field("field_1", TR::ident_type("u64"))],
                    ),
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
                ],
            )
        };

        let path = ItemPath::from_colon_delimited_str("test::TestType2");
        let type_definition = TypeDefinition {
            path: path.clone(),
            state: TypeState::Resolved(TypeStateResolved {
                size: 20,
                regions: vec![
                    Region::Field(
                        "field_1".into(),
                        TypeRef::Raw(ItemPath::from_colon_delimited_str("i32")),
                    ),
                    Region::Field(
                        "field_2".into(),
                        TypeRef::Raw(ItemPath::from_colon_delimited_str("test::TestType1")),
                    ),
                    Region::Field(
                        "field_3".into(),
                        TypeRef::ConstPointer(Box::new(TypeRef::Raw(
                            ItemPath::from_colon_delimited_str("test::TestType1"),
                        ))),
                    ),
                    Region::Field(
                        "field_4".into(),
                        TypeRef::MutPointer(Box::new(TypeRef::Raw(
                            ItemPath::from_colon_delimited_str("test::TestType1"),
                        ))),
                    ),
                ],
                functions: HashMap::new(),
                metadata: HashMap::new(),
            }),
            category: TypeCategory::Defined,
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

            Module::new(
                &[],
                &[],
                &[
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
                ],
            )
        };

        let path = ItemPath::from_colon_delimited_str("test::Singleton");
        let type_definition = TypeDefinition {
            path: path.clone(),
            state: TypeState::Resolved(TypeStateResolved {
                size: 0x1750,
                regions: vec![
                    Region::Padding(0x78),
                    Region::Field(
                        "max_num_1".into(),
                        TypeRef::Raw(ItemPath::from_colon_delimited_str("u16")),
                    ),
                    Region::Field(
                        "max_num_2".into(),
                        TypeRef::Raw(ItemPath::from_colon_delimited_str("u16")),
                    ),
                    Region::Padding(0x984),
                    Region::Field(
                        "test_type".into(),
                        TypeRef::Raw(ItemPath::from_colon_delimited_str("test::TestType")),
                    ),
                    Region::Field(
                        "settings".into(),
                        TypeRef::Array(
                            Box::new(TypeRef::Raw(ItemPath::from_colon_delimited_str("u8"))),
                            804,
                        ),
                    ),
                    Region::Padding(0xA24),
                ],
                functions: [(
                    "free".to_string(),
                    vec![Function {
                        name: "test_function".to_string(),
                        attributes: vec![Attribute::Address(0x800_000)],
                        arguments: vec![
                            Argument::MutSelf,
                            Argument::Field(
                                "arg1".to_string(),
                                TypeRef::MutPointer(Box::new(TypeRef::Raw(
                                    ItemPath::from_colon_delimited_str("test::TestType"),
                                ))),
                            ),
                            Argument::Field(
                                "arg2".to_string(),
                                TypeRef::Raw(ItemPath::from_colon_delimited_str("i32")),
                            ),
                            Argument::Field(
                                "arg3".to_string(),
                                TypeRef::ConstPointer(Box::new(TypeRef::Raw(
                                    ItemPath::from_colon_delimited_str("u32"),
                                ))),
                            ),
                        ],
                        return_type: Some(TypeRef::MutPointer(Box::new(TypeRef::Raw(
                            ItemPath::from_colon_delimited_str("test::TestType"),
                        )))),
                    }],
                )]
                .into_iter()
                .collect(),
                metadata: HashMap::from([(
                    "singleton".to_string(),
                    MetadataValue::Integer(0x1_200_000),
                )]),
            }),
            category: TypeCategory::Defined,
        };

        assert_eq!(build_type(&module, &path).unwrap(), type_definition);
    }

    #[test]
    fn will_eventually_terminate_with_an_unknown_type() {
        let module = {
            use super::grammar::*;

            type TS = TypeStatement;
            type TR = TypeRef;

            Module::new(
                &[],
                &[],
                &[TypeDefinition::new(
                    "TestType2",
                    &[TS::field("field_2", TR::ident_type("TestType1"))],
                )],
            )
        };

        let path = ItemPath::from_colon_delimited_str("test::TestType2");
        assert_eq!(
            build_type(&module, &path).err().unwrap().to_string(),
            r#"type resolution will not terminate, failed on types: ["test::TestType2"] (resolved types: [])"#
        );
    }

    #[test]
    fn can_use_type_from_another_module() {
        let module1 = {
            use super::grammar::*;

            type TS = TypeStatement;
            type TR = TypeRef;

            Module::new(
                &[ItemPath::from_colon_delimited_str("module2::TestType2")],
                &[],
                &[TypeDefinition::new(
                    "TestType1",
                    &[TS::field("field", TR::ident_type("TestType2"))],
                )],
            )
        };
        let module2 = {
            use super::grammar::*;

            type TS = TypeStatement;
            type TR = TypeRef;

            Module::new(
                &[],
                &[],
                &[TypeDefinition::new(
                    "TestType2",
                    &[TS::field("field", TR::ident_type("u32"))],
                )],
            )
        };

        let path = ItemPath::from_colon_delimited_str("module1::TestType1");
        let target_resolved_type = TypeDefinition {
            path: path.clone(),
            state: TypeState::Resolved(TypeStateResolved {
                size: 4,
                functions: HashMap::new(),
                regions: vec![Region::Field(
                    "field".into(),
                    TypeRef::Raw(ItemPath::from_colon_delimited_str("module2::TestType2")),
                )],
                metadata: HashMap::new(),
            }),
            category: TypeCategory::Defined,
        };

        let mut semantic_state = SemanticState::new(4);
        semantic_state
            .add_module(&module1, &ItemPath::from_colon_delimited_str("module1"))
            .unwrap();
        semantic_state
            .add_module(&module2, &ItemPath::from_colon_delimited_str("module2"))
            .unwrap();
        semantic_state.build().unwrap();

        let resolved_type = semantic_state
            .type_registry()
            .get(&path)
            .cloned()
            .context("failed to get type")
            .unwrap();
        assert_eq!(resolved_type, target_resolved_type);
    }

    #[test]
    fn will_fail_on_an_extern_without_size() {
        let module = {
            use super::grammar::*;

            Module::new(
                &[],
                &[(ItemPath::from_colon_delimited_str("TestType"), vec![])],
                &[],
            )
        };

        assert_eq!(
            build_type(&module, &ItemPath::from_colon_delimited_str("module"))
                .err()
                .unwrap()
                .to_string(),
            "failed to find size field in extern type for module"
        );
    }

    #[test]
    fn can_resolve_embed_of_an_extern() {
        let module = {
            use super::grammar::*;

            type TS = TypeStatement;
            type TR = TypeRef;

            Module::new(
                &[],
                &[(
                    ItemPath::from_colon_delimited_str("TestType1"),
                    vec![ExprField("size".into(), Expr::IntLiteral(16))],
                )],
                &[TypeDefinition::new(
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
                )],
            )
        };

        let path = ItemPath::from_colon_delimited_str("test::TestType2");
        let type_definition = TypeDefinition {
            path: path.clone(),
            state: TypeState::Resolved(TypeStateResolved {
                size: 28,
                regions: vec![
                    Region::Field(
                        "field_1".into(),
                        TypeRef::Raw(ItemPath::from_colon_delimited_str("i32")),
                    ),
                    Region::Field(
                        "field_2".into(),
                        TypeRef::Raw(ItemPath::from_colon_delimited_str("test::TestType1")),
                    ),
                    Region::Field(
                        "field_3".into(),
                        TypeRef::ConstPointer(Box::new(TypeRef::Raw(
                            ItemPath::from_colon_delimited_str("test::TestType1"),
                        ))),
                    ),
                    Region::Field(
                        "field_4".into(),
                        TypeRef::MutPointer(Box::new(TypeRef::Raw(
                            ItemPath::from_colon_delimited_str("test::TestType1"),
                        ))),
                    ),
                ],
                functions: HashMap::new(),
                metadata: HashMap::new(),
            }),
            category: TypeCategory::Defined,
        };

        assert_eq!(build_type(&module, &path).unwrap(), type_definition);
    }

    #[test]
    fn can_handle_an_extern_with_a_singleton() {
        let module = {
            use super::grammar::*;

            Module::new(
                &[],
                &[(
                    ItemPath::from_colon_delimited_str("TestType1"),
                    vec![ExprField("singleton".into(), Expr::IntLiteral(0x1337))],
                )],
                &[],
            )
        };

        let path = ItemPath::from_colon_delimited_str("module::TestType1");
        assert_eq!(
            build_type(&module, &path)
                .unwrap()
                .resolved()
                .unwrap()
                .metadata
                .get(&"singleton".to_string())
                .unwrap(),
            &MetadataValue::Integer(0x1337)
        );
    }

    #[test]
    fn can_generate_vftable() {
        let module = {
            use super::grammar::*;

            type TS = TypeStatement;
            type TR = TypeRef;

            Module::new(
                &[],
                &[],
                &[TypeDefinition::new(
                    "TestType",
                    &[TS::Functions(vec![(
                        "vftable".into(),
                        vec![
                            Function {
                                name: "test_function0".into(),
                                attributes: vec![],
                                arguments: vec![
                                    Argument::MutSelf,
                                    Argument::Field(TypeField(
                                        "arg0".into(),
                                        TR::ident_type("u32"),
                                    )),
                                    Argument::Field(TypeField(
                                        "arg1".into(),
                                        TR::ident_type("f32"),
                                    )),
                                ],
                                return_type: Some("i32".into()),
                            },
                            Function {
                                name: "test_function1".into(),
                                attributes: vec![],
                                arguments: vec![
                                    Argument::MutSelf,
                                    Argument::Field(TypeField(
                                        "arg0".into(),
                                        TR::ident_type("u32"),
                                    )),
                                    Argument::Field(TypeField(
                                        "arg1".into(),
                                        TR::ident_type("f32"),
                                    )),
                                ],
                                return_type: None,
                            },
                        ],
                    )])],
                )],
            )
        };

        let type_definition = TypeDefinition {
            path: ItemPath::from_colon_delimited_str("test::TestType"),
            state: TypeState::Resolved(TypeStateResolved {
                size: 4,
                regions: vec![Region::Field(
                    "vftable".to_string(),
                    TypeRef::ConstPointer(Box::new(TypeRef::Raw(
                        ItemPath::from_colon_delimited_str("test::TestTypeVftable"),
                    ))),
                )],
                functions: HashMap::from([(
                    "vftable".into(),
                    vec![
                        Function {
                            name: "test_function0".to_string(),
                            attributes: vec![],
                            arguments: vec![
                                Argument::MutSelf,
                                Argument::Field(
                                    "arg0".to_string(),
                                    TypeRef::Raw(ItemPath::from_colon_delimited_str("u32")),
                                ),
                                Argument::Field(
                                    "arg1".to_string(),
                                    TypeRef::Raw(ItemPath::from_colon_delimited_str("f32")),
                                ),
                            ],
                            return_type: Some(TypeRef::Raw(ItemPath::from_colon_delimited_str(
                                "i32",
                            ))),
                        },
                        Function {
                            name: "test_function1".to_string(),
                            attributes: vec![],
                            arguments: vec![
                                Argument::MutSelf,
                                Argument::Field(
                                    "arg0".to_string(),
                                    TypeRef::Raw(ItemPath::from_colon_delimited_str("u32")),
                                ),
                                Argument::Field(
                                    "arg1".to_string(),
                                    TypeRef::Raw(ItemPath::from_colon_delimited_str("f32")),
                                ),
                            ],
                            return_type: None,
                        },
                    ],
                )]),
                metadata: HashMap::new(),
            }),
            category: TypeCategory::Defined,
        };
        let vftable_type_definition = TypeDefinition {
            path: ItemPath::from_colon_delimited_str("test::TestTypeVftable"),
            state: TypeState::Resolved(TypeStateResolved {
                size: 0,
                regions: vec![
                    Region::Field(
                        "test_function0".to_string(),
                        TypeRef::Function(
                            vec![
                                (
                                    "this".to_string(),
                                    Box::new(TypeRef::MutPointer(Box::new(TypeRef::Raw(
                                        ItemPath::from_colon_delimited_str("test::TestType"),
                                    )))),
                                ),
                                (
                                    "arg0".to_string(),
                                    Box::new(TypeRef::Raw(ItemPath::from_colon_delimited_str(
                                        "u32",
                                    ))),
                                ),
                                (
                                    "arg1".to_string(),
                                    Box::new(TypeRef::Raw(ItemPath::from_colon_delimited_str(
                                        "f32",
                                    ))),
                                ),
                            ],
                            Some(Box::new(TypeRef::Raw(ItemPath::from_colon_delimited_str(
                                "i32",
                            )))),
                        ),
                    ),
                    Region::Field(
                        "test_function1".to_string(),
                        TypeRef::Function(
                            vec![
                                (
                                    "this".to_string(),
                                    Box::new(TypeRef::MutPointer(Box::new(TypeRef::Raw(
                                        ItemPath::from_colon_delimited_str("test::TestType"),
                                    )))),
                                ),
                                (
                                    "arg0".to_string(),
                                    Box::new(TypeRef::Raw(ItemPath::from_colon_delimited_str(
                                        "u32",
                                    ))),
                                ),
                                (
                                    "arg1".to_string(),
                                    Box::new(TypeRef::Raw(ItemPath::from_colon_delimited_str(
                                        "f32",
                                    ))),
                                ),
                            ],
                            None,
                        ),
                    ),
                ],
                functions: HashMap::new(),
                metadata: HashMap::new(),
            }),
            category: TypeCategory::Defined,
        };

        let test_type_path = ItemPath::from_colon_delimited_str("test::TestType");
        let test_type_vftable_path = ItemPath::from_colon_delimited_str("test::TestTypeVftable");

        let build_state = build_state(&module, &test_type_path).unwrap();
        let type_registry = build_state.type_registry();

        let test_module = build_state
            .modules()
            .get(&ItemPath::from_colon_delimited_str("test"))
            .unwrap();

        assert_eq!(
            test_module.definition_paths(),
            &HashSet::from_iter([test_type_path.clone(), test_type_vftable_path.clone()])
        );

        assert_eq!(
            test_module
                .definitions(type_registry)
                .find(|td| td.path == test_type_path)
                .unwrap(),
            &type_definition
        );
        assert_eq!(
            test_module
                .definitions(type_registry)
                .find(|td| td.path == test_type_vftable_path)
                .unwrap(),
            &vftable_type_definition
        );
    }
}
