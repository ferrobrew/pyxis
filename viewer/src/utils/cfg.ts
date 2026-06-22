import type { JsonCfg } from '@pyxis/types';

/**
 * Render a `#[cfg(...)]` predicate AST back to its source-like form, e.g.
 * `backend = "cpp"`, `all(backend = "cpp", not(test))`.
 */
export function formatCfg(cfg: JsonCfg): string {
  switch (cfg.type) {
    case 'ident':
      return cfg.name;
    case 'key_value':
      return `${cfg.key} = "${cfg.value}"`;
    case 'any':
      return `any(${cfg.predicates.map(formatCfg).join(', ')})`;
    case 'all':
      return `all(${cfg.predicates.map(formatCfg).join(', ')})`;
    case 'not':
      return `not(${formatCfg(cfg.predicate)})`;
  }
}
