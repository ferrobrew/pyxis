; highlights.scm — Tree-sitter highlight queries for the Pyxis DSL.
;
; Capture names follow the tree-sitter standard highlight names so that
; both Zed and (via tree-sitter-textmate) VSCode can consume them.
;
; Note: Pyxis keywords like `pub`, `type`, `enum`, etc. are not emitted as
; separate anonymous tokens because they are consumed inside named rules
; (e.g. `visibility` contains `pub`). We capture the named wrapper nodes
; instead and let editors map them.

; ============================================================================
; Comments
; ============================================================================
(line_comment) @comment
(doc_comment) @comment.documentation
(block_comment) @comment

; ============================================================================
; Literals
; ============================================================================
(integer_literal) @number
(string_literal) @string
(raw_string_literal) @string.special

; ============================================================================
; Attributes
; ============================================================================
(attribute) @attribute
(attribute_list) @attribute

; ============================================================================
; Keywords
; ============================================================================
; Anonymous tokens (string literals in the grammar) are matched directly.
; These capture the keyword tokens themselves, not wrapper nodes.
[
  "type"
  "enum"
  "bitflags"
  "impl"
  "fn"
  "extern"
  "use"
  "backend"
  "vftable"
  "const"
  "mut"
  "as"
] @keyword

; `prologue` / `epilogue` / `uses` are backend slots, captured via their node
(backend_slot) @keyword

; ============================================================================
; Visibility (pub keyword)
; ============================================================================
(visibility) @keyword

; ============================================================================
; Types — references
; ============================================================================
; A `type_identifier` wraps a `path` of one or more `identifier` nodes.
; The last identifier is the type name; intermediate ones are namespace
; segments (handled by the Paths section below).
;
; Builtin primitives: u8, u32, f32, bool, void, AtomicU32, etc.
; These match regardless of whether the type_identifier is a direct field
; (e.g. `field type:`) or nested inside pointer_type/array_type/generic_type.
(type_identifier
  (path
    (identifier) @type.builtin
    (#match? @type.builtin
      "^(u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize|f32|f64|bool|void|char|c_char|c_int|AtomicBool|AtomicU8|AtomicU16|AtomicU32|AtomicU64|AtomicI8|AtomicI16|AtomicI32|AtomicI64|AtomicPtr)$")))

; Non-builtin type references: the leaf identifier of a type_identifier path.
; We match a path whose last child is an identifier (no trailing `::`).
; The `.` after `(identifier) @type` anchors it as the final child of `path`.
; This covers all type positions — field types, pointer pointees, array
; elements, generic arguments, return types, parameter types, impl targets,
; type alias values, enum/bitflags base types.
(type_identifier
  (path
    (identifier) @type .)
  (#not-match? @type
    "^(u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize|f32|f64|bool|void|char|c_char|c_int|AtomicBool|AtomicU8|AtomicU16|AtomicU32|AtomicU64|AtomicI8|AtomicI16|AtomicI32|AtomicI64|AtomicPtr)$"))

; Generic type outer name (e.g. `SharedPtr` in `SharedPtr<Camera>`)
(generic_type
  name: (path
    (identifier) @type .)
  (#not-match? @type
    "^(u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize|f32|f64|bool|void|char|c_char|c_int|AtomicBool|AtomicU8|AtomicU16|AtomicU32|AtomicU64|AtomicI8|AtomicI16|AtomicI32|AtomicI64|AtomicPtr)$"))

; ============================================================================
; Type definition names (the identifier after `type`/`enum`/`bitflags`)
; ============================================================================
(type_definition
  name: (identifier) @type)
(enum_definition
  name: (identifier) @type)
(bitflags_definition
  name: (identifier) @type)
(type_alias
  name: (identifier) @type)
(extern_type
  name: (identifier) @type)

; Enum variants and bitflag names
(enum_variant
  name: (identifier) @constant)
(bitflag
  name: (identifier) @constant)

; Type parameter names (generics)
(type_parameter
  name: (identifier) @type.parameter)

; ============================================================================
; Functions
; ============================================================================
(function_signature
  name: (identifier) @function)

; ============================================================================
; Variables / fields / parameters
; ============================================================================
(field
  name: (identifier) @variable)
(named_argument
  name: (identifier) @variable.parameter)

; ============================================================================
; Paths — intermediate segments are namespaces
; ============================================================================
(path
  (identifier) @namespace
  "::"
  (identifier))
