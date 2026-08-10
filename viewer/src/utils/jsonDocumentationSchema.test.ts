// Unit tests for the `JsonDocumentation` boundary validator — the zod schema
// that guards untrusted JSON before it enters typed state (see
// `jsonDocumentationSchema.ts` for the sync notice with `types/json.ts` and
// `src/backends/json/schema.rs`).

import { describe, expect, it } from 'vitest';
import { jsonDocumentationSchema, parseJsonDocumentation } from './jsonDocumentationSchema';

/// A minimal but structurally complete positive document covering every item
/// kind and the nested module/function/splice shapes.
const VALID_DOCUMENT = {
  schema_version: 13,
  pyxis_version: '0.1.0',
  pointer_size: 8,
  project_name: 'fixture',
  items: {
    'module::Thing': {
      path: 'module::Thing',
      visibility: 'public',
      size: 8,
      alignment: 8,
      category: 'defined',
      kind: {
        type: 'type',
        doc: null,
        fields: [
          {
            visibility: 'public',
            name: 'value',
            doc: null,
            type_ref: { type: 'raw', path: 'u64' },
            offset: 0,
            size: 8,
            alignment: 8,
            is_base: false,
            source: { file_index: 1, line: 2 },
          },
        ],
        associated_functions: [],
        vftable: null,
        singleton: null,
        copyable: true,
        cloneable: true,
        defaultable: false,
        packed: false,
        pinned: false,
      },
      source: { file_index: 1, line: 1 },
    },
    'module::Kind': {
      path: 'module::Kind',
      visibility: 'public',
      size: 4,
      alignment: 4,
      category: 'defined',
      kind: {
        type: 'enum',
        doc: null,
        underlying_type: { type: 'raw', path: 'u32' },
        variants: [{ name: 'One', value: 1, source: { file_index: 1, line: 3 } }],
        associated_functions: [],
        singleton: null,
        copyable: true,
        cloneable: true,
        default: null,
        pinned: false,
      },
      source: { file_index: 1, line: 3 },
    },
  },
  modules: {
    module: {
      doc: null,
      items: ['module::Thing', 'module::Kind'],
      submodules: {},
      functions: [],
      splices: [],
      source: { file_index: 1, line: 1 },
    },
  },
  source_paths: ['module.pyxis'],
} as const;

describe('parseJsonDocumentation', () => {
  it('accepts a structurally complete document', () => {
    const result = parseJsonDocumentation(JSON.stringify(VALID_DOCUMENT));
    expect(result.ok).toBe(true);
  });

  it('accepts documents missing the optional version fields (pre-v2)', () => {
    const withoutVersions = {
      pointer_size: VALID_DOCUMENT.pointer_size,
      project_name: VALID_DOCUMENT.project_name,
      items: VALID_DOCUMENT.items,
      modules: VALID_DOCUMENT.modules,
      source_paths: VALID_DOCUMENT.source_paths,
    };
    const result = parseJsonDocumentation(JSON.stringify(withoutVersions));
    expect(result.ok).toBe(true);
  });

  it('rejects non-JSON input (degenerate: empty string)', () => {
    const result = parseJsonDocumentation('');
    expect(result.ok).toBe(false);
  });

  it('rejects a JSON array at the top level (degenerate: wrong root shape)', () => {
    const result = parseJsonDocumentation('[]');
    expect(result.ok).toBe(false);
  });

  it('rejects JSON that is not an object', () => {
    const result = parseJsonDocumentation('42');
    expect(result.ok).toBe(false);
  });

  it('rejects a document with a missing required field', () => {
    const withoutPointer = {
      schema_version: VALID_DOCUMENT.schema_version,
      pyxis_version: VALID_DOCUMENT.pyxis_version,
      project_name: VALID_DOCUMENT.project_name,
      items: VALID_DOCUMENT.items,
      modules: VALID_DOCUMENT.modules,
      source_paths: VALID_DOCUMENT.source_paths,
    };
    const result = parseJsonDocumentation(JSON.stringify(withoutPointer));
    expect(result.ok).toBe(false);
  });

  it('rejects an item whose kind discriminator is unknown', () => {
    const doc = {
      ...VALID_DOCUMENT,
      items: {
        'module::Bad': {
          ...VALID_DOCUMENT.items['module::Thing'],
          kind: { type: 'not_a_real_kind' },
        },
      },
    };
    const result = parseJsonDocumentation(JSON.stringify(doc));
    expect(result.ok).toBe(false);
  });

  it('rejects a type field with a wrong-typed value', () => {
    const doc = {
      ...VALID_DOCUMENT,
      items: {
        'module::Bad': {
          ...VALID_DOCUMENT.items['module::Thing'],
          size: 'not-a-number',
        },
      },
    };
    const result = parseJsonDocumentation(JSON.stringify(doc));
    expect(result.ok).toBe(false);
  });
});

describe('jsonDocumentationSchema', () => {
  it('is the schema used by the validator', () => {
    // Guards against the validator and schema drifting apart.
    expect(jsonDocumentationSchema.safeParse(VALID_DOCUMENT).success).toBe(true);
  });
});
