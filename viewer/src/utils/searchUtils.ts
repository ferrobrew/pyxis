import type { JsonDocumentation, JsonModule } from '@pyxis/types';
import type { ItemType } from './colors';

export type SearchKind = ItemType | 'module';

export interface SearchResult {
  /** The matched identifier, shown prominently. */
  name: string;
  /** Owning context (module or parent type), shown dimmed for disambiguation. */
  detail: string;
  kind: SearchKind;
  score: number;
  /** Where to navigate: an item or module page, optionally with an anchor. */
  target: { kind: 'module' | 'item'; path: string; anchor?: string };
}

function scoreName(name: string, query: string): number {
  const n = name.toLowerCase();
  if (n === query) return 100;
  if (n.startsWith(query)) return 80;
  if (n.includes(query)) return 60;
  return 0;
}

function parentPath(path: string): string {
  const idx = path.lastIndexOf('::');
  return idx === -1 ? '' : path.slice(0, idx);
}

/**
 * Search through documentation for items, their members (fields, functions,
 * variants, flags), modules, and module-level functions/externs.
 */
export function searchDocumentation(doc: JsonDocumentation, query: string): SearchResult[] {
  if (!query.trim()) return [];

  const q = query.toLowerCase();
  const results: SearchResult[] = [];

  const push = (
    name: string,
    detail: string,
    kind: SearchKind,
    score: number,
    target: SearchResult['target']
  ) => {
    if (score > 0) results.push({ name, detail, kind, score, target });
  };

  // Items and their members.
  for (const [path, item] of Object.entries(doc.items)) {
    const name = path.split('::').pop() || path;
    const itemTarget = { kind: 'item' as const, path };

    let itemScore = scoreName(name, q);
    if (itemScore === 0 && path.toLowerCase().includes(q)) itemScore = 40;
    if (itemScore === 0 && item.kind.doc && item.kind.doc.toLowerCase().includes(q)) itemScore = 20;
    push(name, parentPath(path), item.kind.type as SearchKind, itemScore, itemTarget);

    const kind = item.kind;
    if (kind.type === 'type') {
      for (const f of kind.fields) {
        if (f.name) {
          push(f.name, path, 'field', scoreName(f.name, q), {
            ...itemTarget,
            anchor: `field-${f.name}`,
          });
        }
      }
      for (const fn of kind.vftable?.functions ?? []) {
        push(fn.name, path, 'function', scoreName(fn.name, q), {
          ...itemTarget,
          anchor: `vfunc-${fn.name}`,
        });
      }
      for (const fn of kind.associated_functions) {
        push(fn.name, path, 'function', scoreName(fn.name, q), {
          ...itemTarget,
          anchor: `func-${fn.name}`,
        });
      }
    } else if (kind.type === 'enum') {
      for (const v of kind.variants) {
        push(v.name, path, 'enum-variant', scoreName(v.name, q), {
          ...itemTarget,
          anchor: `variant-${v.name}`,
        });
      }
      for (const fn of kind.associated_functions) {
        push(fn.name, path, 'function', scoreName(fn.name, q), {
          ...itemTarget,
          anchor: `func-${fn.name}`,
        });
      }
    } else if (kind.type === 'bitflags') {
      for (const fl of kind.flags) {
        push(fl.name, path, 'bitflags', scoreName(fl.name, q), {
          ...itemTarget,
          anchor: `flag-${fl.name}`,
        });
      }
    }
  }

  // Modules, their functions and externs, recursively.
  const walkModule = (module: JsonModule, modulePath: string) => {
    const target = { kind: 'module' as const, path: modulePath };
    for (const fn of module.functions) {
      push(fn.name, modulePath, 'function', scoreName(fn.name, q), {
        ...target,
        anchor: `func-${fn.name}`,
      });
    }
    for (const ext of module.extern_values) {
      push(ext.name, modulePath, 'extern', scoreName(ext.name, q), {
        ...target,
        anchor: `extval-${ext.name}`,
      });
    }
    for (const [subName, sub] of Object.entries(module.submodules)) {
      const subPath = modulePath ? `${modulePath}::${subName}` : subName;
      push(subName, modulePath, 'module', scoreName(subName, q), { kind: 'module', path: subPath });
      walkModule(sub, subPath);
    }
  };

  for (const [name, module] of Object.entries(doc.modules)) {
    push(name, '', 'module', scoreName(name, q), { kind: 'module', path: name });
    walkModule(module, name);
  }

  // Highest score first; then prefer shorter names and alphabetical order.
  results.sort(
    (a, b) => b.score - a.score || a.name.length - b.name.length || a.name.localeCompare(b.name)
  );

  return results;
}
