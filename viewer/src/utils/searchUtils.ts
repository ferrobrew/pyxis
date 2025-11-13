import type { JsonDocumentation, JsonItem } from '@pyxis/types';
import { getItemName } from './pathUtils';

export interface SearchResult {
  path: string;
  item: JsonItem;
  type: 'type' | 'enum' | 'bitflags' | 'function' | 'module';
  score: number;
}

/**
 * Search through documentation for items matching the query
 */
export function searchDocumentation(doc: JsonDocumentation, query: string): SearchResult[] {
  if (!query.trim()) return [];

  const results: SearchResult[] = [];
  const lowerQuery = query.toLowerCase();

  // Search items
  for (const [path, item] of Object.entries(doc.items)) {
    const itemName = getItemName(path).toLowerCase();
    const pathLower = path.toLowerCase();

    let score = 0;

    // Exact match on name
    if (itemName === lowerQuery) {
      score = 100;
    }
    // Name starts with query
    else if (itemName.startsWith(lowerQuery)) {
      score = 80;
    }
    // Name contains query
    else if (itemName.includes(lowerQuery)) {
      score = 60;
    }
    // Path contains query
    else if (pathLower.includes(lowerQuery)) {
      score = 40;
    }
    // Doc contains query
    else if (item.kind.doc && item.kind.doc.toLowerCase().includes(lowerQuery)) {
      score = 20;
    }

    if (score > 0) {
      results.push({
        path,
        item,
        type: item.kind.type as 'type' | 'enum' | 'bitflags',
        score,
      });
    }
  }

  // Search functions in modules
  const searchFunctionsInModule = (module: any, modulePath: string) => {
    if (module.functions) {
      for (const func of module.functions) {
        const funcName = func.name.toLowerCase();
        let score = 0;

        if (funcName === lowerQuery) {
          score = 100;
        } else if (funcName.startsWith(lowerQuery)) {
          score = 80;
        } else if (funcName.includes(lowerQuery)) {
          score = 60;
        } else if (func.doc && func.doc.toLowerCase().includes(lowerQuery)) {
          score = 20;
        }

        if (score > 0) {
          results.push({
            path: modulePath ? `${modulePath}::${func.name}` : func.name,
            item: func as any,
            type: 'function',
            score,
          });
        }
      }
    }

    if (module.submodules) {
      for (const [name, submodule] of Object.entries(module.submodules)) {
        const subPath = modulePath ? `${modulePath}::${name}` : name;
        searchFunctionsInModule(submodule, subPath);
      }
    }
  };

  for (const [name, module] of Object.entries(doc.modules)) {
    searchFunctionsInModule(module, name);

    // Also match module names
    const moduleName = name.toLowerCase();
    if (moduleName.includes(lowerQuery)) {
      results.push({
        path: name,
        item: module as any,
        type: 'module',
        score: moduleName === lowerQuery ? 100 : moduleName.startsWith(lowerQuery) ? 80 : 60,
      });
    }
  }

  // Sort by score descending
  results.sort((a, b) => b.score - a.score);

  return results;
}
