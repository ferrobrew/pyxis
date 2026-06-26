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
; Visibility (pub keyword)
; ============================================================================
(visibility) @keyword

; ============================================================================
; Types — builtin primitives
; ============================================================================
(type_identifier
  (path
    (identifier) @type.builtin
    (#match? @type.builtin
      "^(u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize|f32|f64|bool|void|char|c_char|c_int|AtomicBool|AtomicU8|AtomicU16|AtomicU32|AtomicU64|AtomicI8|AtomicI16|AtomicI32|AtomicI64|AtomicPtr)$")))

; Type definition names (after `type`/`enum`/`bitflags`)
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

; Non-builtin type identifiers in field/argument/return positions
(field
  type: (type_identifier
    (path
      (identifier) @type))
  (#not-match? @type
    "^(u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize|f32|f64|bool|void|char|c_char|c_int|AtomicBool|AtomicU8|AtomicU16|AtomicU32|AtomicU64|AtomicI8|AtomicI16|AtomicI32|AtomicI64|AtomicPtr)$"))

(function_signature
  return_type: (type_identifier
    (path
      (identifier) @type))
  (#not-match? @type
    "^(u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize|f32|f64|bool|void|char|c_char|c_int|AtomicBool|AtomicU8|AtomicU16|AtomicU32|AtomicU64|AtomicI8|AtomicI16|AtomicI32|AtomicI64|AtomicPtr)$"))

(named_argument
  type: (type_identifier
    (path
      (identifier) @type))
  (#not-match? @type
    "^(u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize|f32|f64|bool|void|char|c_char|c_int|AtomicBool|AtomicU8|AtomicU16|AtomicU32|AtomicU64|AtomicI8|AtomicI16|AtomicI32|AtomicI64|AtomicPtr)$"))

; Generic type name (the outer identifier in `Foo<Bar>`)
(generic_type
  name: (path
    (identifier) @type))

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
