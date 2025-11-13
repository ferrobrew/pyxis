import type { JsonDocumentation } from '@pyxis/types';

/**
 * Get all predefined type paths from documentation
 */
export function getPredefinedTypes(documentation: JsonDocumentation | null): Set<string> {
  if (!documentation) return new Set();

  const predefinedTypes = new Set<string>();
  for (const [path, item] of Object.entries(documentation.items)) {
    if (item.category === 'predefined') {
      predefinedTypes.add(path);
    }
  }
  return predefinedTypes;
}

/**
 * Get the relative path from a current module path to a target path
 * If the target is in a parent or sibling module, use the absolute path
 */
export function getRelativePath(
  currentModulePath: string,
  targetPath: string,
  predefinedTypes: Set<string>
): string {
  // Split paths into segments
  const currentSegments = currentModulePath.split('::').filter(Boolean);
  const targetSegments = targetPath.split('::').filter(Boolean);

  // If target is a predefined type, return as is
  if (targetSegments.length === 1 && predefinedTypes.has(targetPath)) {
    return targetPath;
  }

  // Find common prefix
  let commonPrefixLength = 0;
  while (
    commonPrefixLength < currentSegments.length &&
    commonPrefixLength < targetSegments.length &&
    currentSegments[commonPrefixLength] === targetSegments[commonPrefixLength]
  ) {
    commonPrefixLength++;
  }

  // If they share the same module (parent), use relative path
  if (commonPrefixLength === currentSegments.length) {
    // Target is in the same module or a submodule
    return targetSegments.slice(commonPrefixLength).join('::');
  } else if (commonPrefixLength === currentSegments.length - 1 && commonPrefixLength > 0) {
    // Target is a sibling in the same parent module
    return targetSegments.slice(commonPrefixLength).join('::');
  }

  // Otherwise, use the full absolute path
  return targetPath;
}

/**
 * Get the module path from an item path
 */
export function getModulePath(itemPath: string): string {
  const segments = itemPath.split('::');
  return segments.slice(0, -1).join('::');
}

/**
 * Get the item name from an item path
 */
export function getItemName(itemPath: string): string {
  const segments = itemPath.split('::');
  return segments[segments.length - 1];
}

/**
 * Navigate through nested modules to find a specific module
 */
export function findModule(
  modules: Record<string, unknown>,
  path: string
): Record<string, unknown> | null {
  if (!path) return null;

  const segments = path.split('::').filter(Boolean);
  let current: Record<string, unknown> = modules;

  for (let i = 0; i < segments.length; i++) {
    const segment = segments[i];

    if (i === 0) {
      // First segment: navigate to top-level module
      if (!current[segment]) return null;
      current = current[segment] as Record<string, unknown>;
    } else {
      // Subsequent segments: navigate through submodules
      if (!current.submodules || typeof current.submodules !== 'object') return null;
      const submodules = current.submodules as Record<string, unknown>;
      if (!submodules[segment]) return null;
      current = submodules[segment] as Record<string, unknown>;
    }
  }

  return current;
}

/**
 * Get all module paths from the nested module structure
 */
export function getAllModulePaths(modules: Record<string, unknown>, prefix = ''): string[] {
  const paths: string[] = [];

  for (const [name, module] of Object.entries(modules)) {
    const fullPath = prefix ? `${prefix}::${name}` : name;
    paths.push(fullPath);

    if (module && typeof module === 'object' && 'submodules' in module) {
      const modRecord = module as Record<string, unknown>;
      paths.push(...getAllModulePaths(modRecord.submodules as Record<string, unknown>, fullPath));
    }
  }

  return paths;
}

/**
 * Find the longest valid ancestor path for a given path.
 * For items, checks both item existence and module existence.
 * For modules, checks module existence.
 * Returns the longest valid ancestor path, or null if none exists (shouldn't happen as root is always valid).
 */
export function findLongestValidAncestor(
  path: string,
  documentation: { items: Record<string, unknown>; modules: Record<string, unknown> },
  isItem: boolean
): string | null {
  const segments = path.split('::').filter(Boolean);

  // Try paths from longest to shortest
  for (let i = segments.length; i >= 0; i--) {
    const ancestorPath = segments.slice(0, i).join('::');

    if (isItem && i === segments.length) {
      // For items, first check if the full item path exists
      if (documentation.items[path]) {
        return path; // Return the full item path
      }
      // If the full item doesn't exist, continue to check module ancestors
    }

    // Check if the module path exists (for both items and modules)
    if (ancestorPath === '') {
      // Root is always valid
      return '';
    }

    const module = findModule(documentation.modules, ancestorPath);
    if (module) {
      return ancestorPath;
    }
  }

  // Should never reach here as root is always valid
  return '';
}
