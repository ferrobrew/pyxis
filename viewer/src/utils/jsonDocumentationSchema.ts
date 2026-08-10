import { z } from 'zod';

// SYNCHRONISATION NOTICE — read this if you touch any of the three files it
// names:
//
// The zod schema in this file mirrors the `JsonDocumentation` wire shape
// declared in `types/json.ts` (generated from the Rust structs in
// `src/backends/json/schema.rs`, which derive `specta::Type`). Any change to
// one of the three must be reflected in the other two:
//
//   - Rust structs:        src/backends/json/schema.rs
//   - generated types:     types/json.ts  (regenerate with `cargo run -p pyxis-driver -- gen-types`)
//   - zod schema:          this file
//
// There is no automated tie between the Rust source and the zod schema; this
// is a documented convention. See the matching notice in `schema.rs`.

const jsonVisibility = z.enum(['public', 'private']);
const jsonCallingConvention = z.enum([
  'c',
  'cdecl',
  'stdcall',
  'fastcall',
  'thiscall',
  'vectorcall',
  'system',
]);
const jsonSpliceKind = z.enum(['prologue', 'epilogue']);
const jsonDocLinkTargetKind = z.enum(['item', 'module']);
const jsonItemCategory = z.enum(['defined', 'predefined', 'extern']);

const jsonSourceLocation = z.object({
  file_index: z.number(),
  line: z.number(),
});

const jsonCfg: z.ZodType<unknown> = z.lazy(() =>
  z.union([
    z.object({ type: z.literal('ident'), name: z.string() }),
    z.object({ type: z.literal('key_value'), key: z.string(), value: z.string() }),
    z.object({ type: z.literal('any'), predicates: z.array(jsonCfg) }),
    z.object({ type: z.literal('all'), predicates: z.array(jsonCfg) }),
    z.object({ type: z.literal('not'), predicate: jsonCfg }),
  ])
);

const jsonDocLink = z.object({
  text: z.string(),
  target_kind: jsonDocLinkTargetKind,
  path: z.string(),
  anchor: z.string().nullable().optional(),
});

const jsonFunctionArgument = z.object({
  name: z.string().nullable(),
  type_ref: z.lazy(() => jsonType),
});

const jsonType: z.ZodType<unknown> = z.lazy(() =>
  z.union([
    z.object({ type: z.literal('raw'), path: z.string() }),
    z.object({ type: z.literal('generic'), base: z.string(), args: z.array(jsonType) }),
    z.object({ type: z.literal('type_parameter'), name: z.string() }),
    z.object({ type: z.literal('const_pointer'), inner: jsonType }),
    z.object({ type: z.literal('mut_pointer'), inner: jsonType }),
    z.object({ type: z.literal('array'), inner: jsonType, size: z.number() }),
    z.object({
      type: z.literal('function'),
      calling_convention: jsonCallingConvention,
      arguments: z.array(jsonFunctionArgument),
      return_type: jsonType.nullable(),
    }),
  ])
);

const jsonFunctionBody = z.union([
  z.object({ type: z.literal('address'), address: z.number() }),
  z.object({ type: z.literal('field'), field: z.string(), function_name: z.string() }),
  z.object({ type: z.literal('vftable'), function_name: z.string() }),
  z.object({ type: z.literal('external') }),
]);

const jsonArgument = z.union([
  z.object({ type: z.literal('const_self') }),
  z.object({ type: z.literal('mut_self') }),
  z.object({ type: z.literal('field'), name: z.string(), type_ref: z.lazy(() => jsonType) }),
]);

const jsonFunction = z.object({
  visibility: jsonVisibility,
  name: z.string(),
  doc: z.string().nullable(),
  doc_links: z.array(jsonDocLink).optional(),
  body: jsonFunctionBody,
  arguments: z.array(jsonArgument),
  return_type: z.lazy(() => jsonType.nullable()),
  calling_convention: jsonCallingConvention,
  method_type_parameters: z.array(z.string()).optional(),
  cfg: jsonCfg.nullable().optional(),
  source: jsonSourceLocation.nullable(),
});

const jsonConstField = z.object({
  name: z.string(),
  value: z.lazy(() => jsonConstValue),
});

const jsonConstValue: z.ZodType<unknown> = z.lazy(() =>
  z.union([
    z.object({ kind: z.literal('int'), value: z.number() }),
    z.object({ kind: z.literal('float'), value: z.number() }),
    z.object({ kind: z.literal('string'), value: z.string() }),
    z.object({ kind: z.literal('c_string'), value: z.string() }),
    z.object({ kind: z.literal('enum_value'), path: z.string() }),
    z.object({ kind: z.literal('struct'), fields: z.array(jsonConstField) }),
    z.object({ kind: z.literal('array'), elements: z.array(jsonConstValue) }),
    z.object({ kind: z.literal('const_ref'), path: z.string() }),
  ])
);

const jsonRegion = z.object({
  visibility: jsonVisibility,
  name: z.string().nullable(),
  doc: z.string().nullable(),
  doc_links: z.array(jsonDocLink).optional(),
  type_ref: z.lazy(() => jsonType),
  offset: z.number(),
  size: z.number(),
  alignment: z.number(),
  is_base: z.boolean(),
  source: jsonSourceLocation.nullable(),
});

const jsonTypeVftable = z.object({
  functions: z.array(jsonFunction),
});

const jsonBitflag = z.object({
  name: z.string(),
  value: z.number(),
  doc: z.string().nullable().optional(),
  doc_links: z.array(jsonDocLink).optional(),
  source: jsonSourceLocation.nullable(),
});

const jsonEnumVariant = z.object({
  name: z.string(),
  value: z.number(),
  doc: z.string().nullable().optional(),
  doc_links: z.array(jsonDocLink).optional(),
  source: jsonSourceLocation.nullable(),
});

const jsonConstantDefinition = z.object({
  doc: z.string().nullable(),
  doc_links: z.array(jsonDocLink).optional(),
  value_type: z.lazy(() => jsonType),
  value: jsonConstValue,
});

const jsonExternValueDefinition = z.object({
  doc: z.string().nullable(),
  doc_links: z.array(jsonDocLink).optional(),
  value_type: z.lazy(() => jsonType),
  address: z.number(),
});

const jsonTypeAliasDefinition = z.object({
  doc: z.string().nullable(),
  doc_links: z.array(jsonDocLink).optional(),
  target: z.lazy(() => jsonType),
});

const jsonTypeDefinition = z.object({
  doc: z.string().nullable(),
  doc_links: z.array(jsonDocLink).optional(),
  fields: z.array(jsonRegion),
  associated_functions: z.array(jsonFunction),
  vftable: jsonTypeVftable.nullable(),
  singleton: z.number().nullable(),
  copyable: z.boolean(),
  cloneable: z.boolean(),
  defaultable: z.boolean(),
  packed: z.boolean(),
  pinned: z.boolean(),
  nested_items: z.array(z.string()).optional(),
});

const jsonUnionDefinition = z.object({
  doc: z.string().nullable(),
  doc_links: z.array(jsonDocLink).optional(),
  fields: z.array(jsonRegion),
  size: z.number(),
  alignment: z.number(),
  copyable: z.boolean(),
  cloneable: z.boolean(),
  defaultable: z.boolean(),
  packed: z.boolean(),
  pinned: z.boolean(),
  nested_items: z.array(z.string()).optional(),
});

const jsonEnumDefinition = z.object({
  doc: z.string().nullable(),
  doc_links: z.array(jsonDocLink).optional(),
  underlying_type: z.lazy(() => jsonType),
  variants: z.array(jsonEnumVariant),
  associated_functions: z.array(jsonFunction),
  singleton: z.number().nullable(),
  copyable: z.boolean(),
  cloneable: z.boolean(),
  default: z.number().nullable(),
  pinned: z.boolean(),
  nested_items: z.array(z.string()).optional(),
});

const jsonBitflagsDefinition = z.object({
  doc: z.string().nullable(),
  doc_links: z.array(jsonDocLink).optional(),
  underlying_type: z.lazy(() => jsonType),
  flags: z.array(jsonBitflag),
  singleton: z.number().nullable(),
  copyable: z.boolean(),
  cloneable: z.boolean(),
  default: z.number().nullable(),
  pinned: z.boolean(),
  nested_items: z.array(z.string()).optional(),
});

const jsonItemKind = z.discriminatedUnion('type', [
  jsonTypeDefinition.extend({ type: z.literal('type') }),
  jsonEnumDefinition.extend({ type: z.literal('enum') }),
  jsonBitflagsDefinition.extend({ type: z.literal('bitflags') }),
  jsonUnionDefinition.extend({ type: z.literal('union') }),
  jsonTypeAliasDefinition.extend({ type: z.literal('type_alias') }),
  jsonConstantDefinition.extend({ type: z.literal('constant') }),
  jsonExternValueDefinition.extend({ type: z.literal('extern_value') }),
]);

const jsonItem = z.object({
  path: z.string(),
  visibility: jsonVisibility,
  type_parameters: z.array(z.string()).optional(),
  size: z.number(),
  alignment: z.number(),
  category: jsonItemCategory,
  cpp_name: z.string().nullable().optional(),
  cpp_header: z.string().nullable().optional(),
  rust_name: z.string().nullable().optional(),
  kind: jsonItemKind,
  cfg: jsonCfg.nullable().optional(),
  source: jsonSourceLocation.nullable(),
});

const jsonReexport = z.object({
  name: z.string(),
  path: z.string(),
});

const jsonSplice = z.object({
  kind: jsonSpliceKind,
  cfg: jsonCfg.nullable().optional(),
  definition: z.boolean(),
  for_type: z.string().nullable().optional(),
  text: z.string(),
});

const jsonModule: z.ZodType<unknown> = z.lazy(() =>
  z.object({
    doc: z.string().nullable(),
    doc_links: z.array(jsonDocLink).optional(),
    items: z.array(z.string()),
    reexports: z.array(jsonReexport).optional(),
    submodules: z.record(z.string(), jsonModule),
    functions: z.array(jsonFunction),
    splices: z.array(jsonSplice),
    source: jsonSourceLocation.nullable().optional(),
  })
);

export const jsonDocumentationSchema = z.object({
  schema_version: z.number().optional(),
  pyxis_version: z.string().optional(),
  pointer_size: z.number(),
  project_name: z.string(),
  items: z.record(z.string(), jsonItem),
  modules: z.record(z.string(), jsonModule),
  source_paths: z.array(z.string()),
});

/// Parse untrusted `JsonDocumentation` JSON text. Returns the validated
/// document, or `null` (with a message) when the input is not a valid
/// documentation payload. This is the single boundary every local-upload and
/// remote-fetch path passes through before data enters typed state.
export function parseJsonDocumentation(text: string):
  | {
      ok: true;
      document: unknown;
    }
  | { ok: false; error: string } {
  let value: unknown;
  try {
    value = JSON.parse(text);
  } catch (err) {
    return { ok: false, error: `invalid JSON: ${(err as Error).message}` };
  }
  const result = jsonDocumentationSchema.safeParse(value);
  if (!result.success) {
    return {
      ok: false,
      error: `document does not match the expected schema: ${result.error.issues[0]?.message ?? 'unknown error'}`,
    };
  }
  return { ok: true, document: result.data };
}
