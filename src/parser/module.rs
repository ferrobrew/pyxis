#[cfg(test)]
use crate::span::StripLocations;

use crate::{
    parser::{
        ParseError,
        attributes::Attributes,
        core::Parser,
        external::{Backend, ExternValue, UseTree},
        functions::{Function, FunctionBlock},
        items::{Comment, ItemDefinition},
        types::Ident,
    },
    span::{HasLocation, ItemLocation},
    tokenizer::TokenKind,
};

#[cfg(test)]
use super::paths::ItemPath;

/// Module-level items (preserves ordering and comments)
#[derive(Debug, Clone, PartialEq, Eq, HasLocation)]
#[cfg_attr(test, derive(StripLocations))]
pub enum ModuleItem {
    Comment {
        comment: Comment,
    },
    Use {
        tree: UseTree,
        location: ItemLocation,
    },
    ExternType {
        name: Ident,
        attributes: Attributes,
        doc_comments: Vec<String>,
        location: ItemLocation,
    },
    Backend {
        backend: Backend,
    },
    Definition {
        definition: ItemDefinition,
    },
    Impl {
        impl_block: FunctionBlock,
    },
    ExternValue {
        extern_value: ExternValue,
    },
    Function {
        function: Function,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Module {
    pub items: Vec<ModuleItem>,
    pub attributes: Attributes,
    pub doc_comments: Vec<String>,
}
impl Module {
    pub fn new() -> Self {
        Self::default()
    }
}
#[cfg(test)]
impl StripLocations for Module {
    fn strip_locations(&self) -> Self {
        Module {
            items: self
                .items
                .iter()
                .filter_map(|item| match item {
                    ModuleItem::Comment { .. } => None, // Filter out comments
                    _ => Some(item.strip_locations()),
                })
                .collect(),
            attributes: self.attributes.strip_locations(),
            doc_comments: self.doc_comments.strip_locations(),
        }
    }
}
#[cfg(test)]
impl Module {
    /// Add simple path-based use statements (convenience for tests)
    pub fn with_uses(mut self, uses: impl IntoIterator<Item = ItemPath>) -> Self {
        for path in uses.into_iter() {
            self.items.push(ModuleItem::Use {
                tree: UseTree::path(path),
                location: ItemLocation::test(),
            });
        }
        self
    }

    /// Add use statements with full UseTree support (for braced import tests)
    pub fn with_use_trees(mut self, trees: impl IntoIterator<Item = UseTree>) -> Self {
        for tree in trees.into_iter() {
            self.items.push(ModuleItem::Use {
                tree,
                location: ItemLocation::test(),
            });
        }
        self
    }
    pub fn with_extern_types(
        mut self,
        extern_types: impl IntoIterator<Item = (Ident, Attributes)>,
    ) -> Self {
        for (name, attributes) in extern_types.into_iter() {
            self.items.push(ModuleItem::ExternType {
                name,
                attributes,
                doc_comments: vec![],
                location: ItemLocation::test(),
            });
        }
        self
    }
    pub fn with_extern_values(
        mut self,
        extern_values: impl IntoIterator<Item = ExternValue>,
    ) -> Self {
        for extern_value in extern_values.into_iter() {
            self.items.push(ModuleItem::ExternValue { extern_value });
        }
        self
    }
    pub fn with_functions(mut self, functions: impl IntoIterator<Item = Function>) -> Self {
        for function in functions.into_iter() {
            self.items.push(ModuleItem::Function { function });
        }
        self
    }
    pub fn with_definitions(
        mut self,
        definitions: impl IntoIterator<Item = ItemDefinition>,
    ) -> Self {
        for definition in definitions.into_iter() {
            self.items.push(ModuleItem::Definition { definition });
        }
        self
    }
    pub fn with_impls(mut self, impls: impl IntoIterator<Item = FunctionBlock>) -> Self {
        for impl_block in impls.into_iter() {
            self.items.push(ModuleItem::Impl { impl_block });
        }
        self
    }
    pub fn with_backends(mut self, backends: impl IntoIterator<Item = Backend>) -> Self {
        for backend in backends.into_iter() {
            self.items.push(ModuleItem::Backend { backend });
        }
        self
    }
    pub fn with_attributes(mut self, attributes: impl Into<Attributes>) -> Self {
        self.attributes = attributes.into();
        self
    }
    pub fn with_doc_comments(mut self, doc_comments: Vec<String>) -> Self {
        self.doc_comments = doc_comments;
        self
    }
}
impl Module {
    pub fn uses(&self) -> impl Iterator<Item = &ModuleItem> {
        self.items
            .iter()
            .filter(|item| matches!(item, ModuleItem::Use { .. }))
    }
    pub fn extern_types(&self) -> impl Iterator<Item = &ModuleItem> {
        self.items
            .iter()
            .filter(|item| matches!(item, ModuleItem::ExternType { .. }))
    }
    pub fn extern_values(&self) -> impl Iterator<Item = &ExternValue> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::ExternValue { extern_value } => Some(extern_value),
            _ => None,
        })
    }
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Function { function } => Some(function),
            _ => None,
        })
    }
    pub fn definitions(&self) -> impl Iterator<Item = &ItemDefinition> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Definition { definition } => Some(definition),
            _ => None,
        })
    }
    pub fn impls(&self) -> impl Iterator<Item = &FunctionBlock> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Impl { impl_block } => Some(impl_block),
            _ => None,
        })
    }
    pub fn backends(&self) -> impl Iterator<Item = &Backend> {
        self.items.iter().filter_map(|item| match item {
            ModuleItem::Backend { backend } => Some(backend),
            _ => None,
        })
    }
}

impl Parser {
    /// Skip over attributes during lookahead, returning the position after all attributes.
    /// Uses safe token access that won't panic on out-of-bounds.
    fn skip_attributes_lookahead(&self, start_pos: usize) -> usize {
        let mut pos = start_pos;

        // Skip past attributes
        while self
            .tokens
            .get(pos)
            .is_some_and(|t| matches!(t.kind, TokenKind::Hash))
        {
            pos += 1; // skip #
            if self
                .tokens
                .get(pos)
                .is_some_and(|t| matches!(t.kind, TokenKind::LBracket))
            {
                pos += 1; // skip [
                // Skip until matching ]
                let mut depth = 1;
                while depth > 0 {
                    match self.tokens.get(pos).map(|t| &t.kind) {
                        Some(TokenKind::LBracket) => depth += 1,
                        Some(TokenKind::RBracket) => depth -= 1,
                        None => break, // EOF reached
                        _ => {}
                    }
                    pos += 1;
                }
            }
        }

        pos
    }

    /// Get the token kind at a position, returning None if out of bounds.
    fn peek_at(&self, pos: usize) -> Option<&TokenKind> {
        self.tokens.get(pos).map(|t| &t.kind)
    }

    /// Skip over all comments and whitespace
    pub fn parse_module(&mut self) -> Result<Module, ParseError> {
        let mut items = Vec::new();
        let mut module_doc_comments = Vec::new();

        // Collect module-level doc comments (//!)
        while matches!(self.peek(), TokenKind::DocInner(_)) {
            if let TokenKind::DocInner(text) = &self.advance().kind {
                let content = text.strip_prefix("//!").unwrap_or(text).to_string();
                module_doc_comments.push(content);
            }
        }

        while !matches!(self.peek(), TokenKind::Eof) {
            // Collect non-doc comments (doc comments will be collected by item parsers)
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                if let Some(comment) = self.collect_comment() {
                    items.push(ModuleItem::Comment { comment });
                }
            }

            if matches!(self.peek(), TokenKind::Eof) {
                break;
            }

            // Parse module-level items
            items.push(self.parse_module_item()?);

            // Add any pending comments that were collected during parsing (e.g., inline comments after attributes)
            for comment in self.pending_comments.drain(..) {
                items.push(ModuleItem::Comment { comment });
            }

            // Collect any inline comments that appeared after the item
            while matches!(
                self.peek(),
                TokenKind::Comment(_) | TokenKind::MultiLineComment(_)
            ) {
                if let Some(comment) = self.collect_comment() {
                    items.push(ModuleItem::Comment { comment });
                }
            }
        }

        Ok(Module {
            items,
            attributes: Default::default(),
            doc_comments: module_doc_comments,
        })
    }

    pub(crate) fn parse_module_item(&mut self) -> Result<ModuleItem, ParseError> {
        // Attributes can appear before any item
        let has_attributes = matches!(self.peek(), TokenKind::Hash);

        match self.peek() {
            TokenKind::Use => self.parse_use(),
            TokenKind::Extern if !has_attributes => {
                // Peek ahead to distinguish extern type from extern value
                if matches!(self.peek_nth(1), TokenKind::Type) {
                    self.parse_extern_type()
                } else {
                    self.parse_extern_value()
                        .map(|extern_value| ModuleItem::ExternValue { extern_value })
                }
            }
            TokenKind::Backend => self
                .parse_backend()
                .map(|backend| ModuleItem::Backend { backend }),
            TokenKind::Hash => {
                // Attributes - need to peek ahead to see what comes after
                let mut pos = self.skip_attributes_lookahead(self.pos);

                // Skip over any comments (including doc comments) after attributes in lookahead
                while matches!(
                    self.peek_at(pos),
                    Some(
                        TokenKind::Comment(_)
                            | TokenKind::MultiLineComment(_)
                            | TokenKind::DocOuter(_)
                            | TokenKind::DocInner(_)
                    )
                ) {
                    pos += 1;
                }

                // Now check what comes after attributes (and comments)
                match self.peek_at(pos) {
                    Some(TokenKind::Extern) => {
                        // Could be extern type or extern value
                        if matches!(self.peek_at(pos + 1), Some(TokenKind::Type)) {
                            self.parse_extern_type()
                        } else {
                            self.parse_extern_value()
                                .map(|extern_value| ModuleItem::ExternValue { extern_value })
                        }
                    }
                    Some(TokenKind::Pub) => {
                        // Could be pub extern value, pub fn, or pub item definition
                        match self.peek_at(pos + 1) {
                            Some(TokenKind::Extern) => self
                                .parse_extern_value()
                                .map(|extern_value| ModuleItem::ExternValue { extern_value }),
                            Some(TokenKind::Fn) => self
                                .parse_function()
                                .map(|function| ModuleItem::Function { function }),
                            _ => self
                                .parse_item_definition()
                                .map(|definition| ModuleItem::Definition { definition }),
                        }
                    }
                    Some(TokenKind::Type | TokenKind::Enum | TokenKind::Bitflags) => self
                        .parse_item_definition()
                        .map(|definition| ModuleItem::Definition { definition }),
                    Some(TokenKind::Impl) => self
                        .parse_impl_block()
                        .map(|impl_block| ModuleItem::Impl { impl_block }),
                    Some(TokenKind::Fn) => self
                        .parse_function()
                        .map(|function| ModuleItem::Function { function }),
                    _ => {
                        // Lookahead couldn't determine item type - this often happens with
                        // malformed attributes. Let parse_item_definition handle it, which
                        // will properly parse (and error on) the attributes.
                        self.parse_item_definition()
                            .map(|definition| ModuleItem::Definition { definition })
                    }
                }
            }
            TokenKind::DocOuter(_) => {
                // Peek ahead to see what comes after doc comments
                let mut pos = self.pos;
                while matches!(self.peek_at(pos), Some(TokenKind::DocOuter(_))) {
                    pos += 1;
                }

                // Check if this is an extern type - skip any attributes first
                if matches!(self.peek_at(pos), Some(TokenKind::Hash)) {
                    pos = self.skip_attributes_lookahead(pos);
                }

                // Now check what comes after doc comments (and possibly attributes)
                if matches!(self.peek_at(pos), Some(TokenKind::Extern))
                    && matches!(self.peek_at(pos + 1), Some(TokenKind::Type))
                {
                    self.parse_extern_type()
                } else {
                    self.parse_item_definition()
                        .map(|definition| ModuleItem::Definition { definition })
                }
            }
            TokenKind::Pub | TokenKind::Type | TokenKind::Enum | TokenKind::Bitflags => self
                .parse_item_definition()
                .map(|definition| ModuleItem::Definition { definition }),
            TokenKind::Impl => self
                .parse_impl_block()
                .map(|impl_block| ModuleItem::Impl { impl_block }),
            TokenKind::Fn => {
                // Freestanding function with attributes
                self.parse_function()
                    .map(|function| ModuleItem::Function { function })
            }
            _ => Err(ParseError::UnexpectedModuleToken {
                found: self.peek().clone(),
                location: self.current().location,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        grammar::{ModuleItem, test_aliases::*},
        parser::parse_str_for_tests,
        span::StripLocations,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn can_parse_basic_struct() {
        let text = r#"
        pub type TestType {
            field_1: i32,
            field_2: i32,
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([
                TS::field((V::Private, "field_1"), T::ident("i32")),
                TS::field((V::Private, "field_2"), T::ident("i32")),
            ]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_vftable() {
        let text = r#"
        type TestType {
            #[size(4)]
            vftable {
                pub fn test(&mut self, test2: i32);
            }
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Private, "TestType"),
            TD::new([TS::vftable([F::new(
                (V::Public, "test"),
                [Ar::mut_self(), Ar::named("test2", T::ident("i32"))],
            )])
            .with_attributes([A::size_decimal(4)])]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_vehicle_types() {
        let text = r#"
        type VehicleTypes {
            hash_edacd65b_likely_max_models: i32,
            hash_2ff58884: i32,

            pub maximum_gpu_cost: i32,
            pub maximum_cpu_cost: i32,

            field_10: i32,

            pub accumulated_gpu_cost: i32,
            pub accumulated_cpu_cost: i32,

            field_1c: i32,
            loaded_models: *const LoadedModel,
            _: unknown<0x10>,
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Private, "VehicleTypes"),
            TD::new([
                TS::field(
                    (V::Private, "hash_edacd65b_likely_max_models"),
                    T::ident("i32"),
                ),
                TS::field((V::Private, "hash_2ff58884"), T::ident("i32")),
                TS::field((V::Public, "maximum_gpu_cost"), T::ident("i32")),
                TS::field((V::Public, "maximum_cpu_cost"), T::ident("i32")),
                TS::field((V::Private, "field_10"), T::ident("i32")),
                TS::field((V::Public, "accumulated_gpu_cost"), T::ident("i32")),
                TS::field((V::Public, "accumulated_cpu_cost"), T::ident("i32")),
                TS::field((V::Private, "field_1c"), T::ident("i32")),
                TS::field(
                    (V::Private, "loaded_models"),
                    T::ident("LoadedModel").const_pointer(),
                ),
                TS::field((V::Private, "_"), T::unknown(0x10)),
            ]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_spawn_manager() {
        let text = r#"
        #[size(0x1754), singleton(0x1_191_918)]
        type SpawnManager {
            #[address(0x78)]
            pub max_num_characters: u16,
            pub max_num_vehicles: u16,

            #[address(0xA00)]
            pub world_sim: WorldSim,
            pub enemy_type_spawn_settings: unknown<804>,
            pub character_types: unknown<0x74>,
            pub vehicle_types: VehicleTypes,
        }
        impl SpawnManager {
            #[address(0x84C_4C0)]
            pub fn engine_spawn_vehicle(
                &mut self,
                vehicle: *mut SharedPtr<Vehicle>,
                context: i32,
                unk1: *mut StdString,
                model_id: *const u32,
                faction: u32,
                unk2: *mut StdString
            ) -> *mut SharedPtr<Vehicle>;

            #[address(0x73F_DB0)]
            pub fn request_vehicle_model(
                &mut self,
                model_id: *const u32,
                category: i32
            );
        }
        "#;

        let ast = M::new()
            .with_definitions([ID::new(
                (V::Private, "SpawnManager"),
                TD::new([
                    TS::field((V::Public, "max_num_characters"), T::ident("u16"))
                        .with_attributes([A::address(0x78)]),
                    TS::field((V::Public, "max_num_vehicles"), T::ident("u16")),
                    TS::field((V::Public, "world_sim"), T::ident("WorldSim"))
                        .with_attributes([A::address(0xA00)]),
                    TS::field((V::Public, "enemy_type_spawn_settings"), T::unknown(804)),
                    TS::field((V::Public, "character_types"), T::unknown(0x74)),
                    TS::field((V::Public, "vehicle_types"), T::ident("VehicleTypes")),
                ])
                .with_attributes([A::size(0x1754), A::singleton(0x1_191_918)]),
            )])
            .with_impls([FB::new(
                "SpawnManager",
                [
                    F::new(
                        (V::Public, "engine_spawn_vehicle"),
                        [
                            Ar::mut_self(),
                            Ar::named(
                                "vehicle",
                                T::generic("SharedPtr", [T::ident("Vehicle")]).mut_pointer(),
                            ),
                            Ar::named("context", T::ident("i32")),
                            Ar::named("unk1", T::ident("StdString").mut_pointer()),
                            Ar::named("model_id", T::ident("u32").const_pointer()),
                            Ar::named("faction", T::ident("u32")),
                            Ar::named("unk2", T::ident("StdString").mut_pointer()),
                        ],
                    )
                    .with_attributes([A::address(0x84C_4C0)])
                    .with_return_type(T::generic("SharedPtr", [T::ident("Vehicle")]).mut_pointer()),
                    F::new(
                        (V::Public, "request_vehicle_model"),
                        [
                            Ar::mut_self(),
                            Ar::named("model_id", T::ident("u32").const_pointer()),
                            Ar::named("category", T::ident("i32")),
                        ],
                    )
                    .with_attributes([A::address(0x73F_DB0)]),
                ],
            )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_address_field() {
        let text = r#"
        type Test {
            #[address(0x78)]
            pub max_num_characters: u16,
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Private, "Test"),
            TD::new([
                TS::field((V::Public, "max_num_characters"), T::ident("u16"))
                    .with_attributes([A::address(0x78)]),
            ]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_an_empty_type() {
        let text = r#"
        type Test;
        "#;

        let ast = M::new().with_definitions([ID::new((V::Private, "Test"), TD::new([]))]);
        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_array_field() {
        let text = r#"
        pub type TestType {
            field_1: [i32; 4],
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Public, "TestType"),
            TD::new([TS::field((V::Private, "field_1"), T::ident("i32").array(4))]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_raycast_result_with_pointers_and_arrays() {
        let text = r#"
        #[size(0x2C)]
        pub type RayCastResult {
            game_object: *mut u32,
            pub normal: Vector3,
            pub distance: f32,
            rigid_body: *mut u32,
            shape: *mut u32,
            unknown: [u32; 4],
        }
        "#;

        let ast = M::new().with_definitions([ID::new(
            (V::Public, "RayCastResult"),
            TD::new([
                TS::field((V::Private, "game_object"), T::ident("u32").mut_pointer()),
                TS::field((V::Public, "normal"), T::ident("Vector3")),
                TS::field((V::Public, "distance"), T::ident("f32")),
                TS::field((V::Private, "rigid_body"), T::ident("u32").mut_pointer()),
                TS::field((V::Private, "shape"), T::ident("u32").mut_pointer()),
                TS::field((V::Private, "unknown"), T::array(T::ident("u32"), 4)),
            ])
            .with_attributes([A::size(0x2C)]),
        )]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_doc_comments() {
        let text = r#"
        //! This is a module doc comment
        //! The best of its kind

        /// This is a doc comment
        type TestType {
            vftable {
                /// My test vfunc!
                fn test_vfunc(&self);
            },
            /// This is a field doc comment
            field_1: i32,
        }
        impl TestType {
            /// My test func!
            #[address(0x123)]
            fn test_func(&self);
        }
        "#;

        let ast = M::new()
            .with_definitions([ID::new(
                (V::Private, "TestType"),
                TD::new([
                    TS::vftable([F::new((V::Private, "test_vfunc"), [Ar::const_self()])
                        .with_doc_comments(vec![" My test vfunc!".to_string()])]),
                    TS::field((V::Private, "field_1"), T::ident("i32"))
                        .with_doc_comments(vec![" This is a field doc comment".to_string()]),
                ]),
            )
            .with_doc_comments(vec![" This is a doc comment".to_string()])])
            .with_impls([FB::new(
                "TestType",
                [F::new((V::Private, "test_func"), [Ar::const_self()])
                    .with_doc_comments(vec![" My test func!".to_string()])
                    .with_attributes([A::address(0x123)])],
            )])
            .with_doc_comments(vec![
                " This is a module doc comment".to_string(),
                " The best of its kind".to_string(),
            ]);

        assert_eq!(parse_str_for_tests(text).unwrap().strip_locations(), ast);
    }

    #[test]
    fn can_parse_doc_comments_after_attributes() {
        let text = r#"
#[size(8), align(4)]
extern type SharedPtr<PfxInstanceInterface>;

/// `IPfxInstance` in original game
pub type PfxInstanceInterface {
    vftable {}
}

#[size(0x10)]
/// `CPfxInstance` in original game
pub type PfxInstance {
    vftable {},
    pub instance: SharedPtr<PfxInstanceInterface>,
}
impl PfxInstance {
    #[address(0x6B7C40)]
    pub fn set_game_object(&mut self, game_object: *mut PfxGameObject);
}
    "#;

        // Don't use strip_locations() - it empties doc_comments and converts them to attributes
        let module = parse_str_for_tests(text).unwrap();

        // Check extern type has no doc comments (doc comes after, not before)
        if let ModuleItem::ExternType { doc_comments, .. } = &module.items[0] {
            assert_eq!(
                doc_comments.len(),
                0,
                "Extern type should have no doc comments"
            );
        } else {
            panic!("Expected ExternType");
        }

        // Check first type definition has doc comments
        if let ModuleItem::Definition { definition } = &module.items[1] {
            assert_eq!(definition.name.0, "PfxInstanceInterface");
            assert_eq!(
                definition.doc_comments,
                vec![" `IPfxInstance` in original game"]
            );
        } else {
            panic!("Expected Definition for PfxInstanceInterface");
        }

        // Check second type definition has doc comments (after attributes)
        if let ModuleItem::Definition { definition } = &module.items[2] {
            assert_eq!(definition.name.0, "PfxInstance");
            assert_eq!(
                definition.doc_comments,
                vec![" `CPfxInstance` in original game"]
            );
        } else {
            panic!("Expected Definition for PfxInstance");
        }
    }
}
