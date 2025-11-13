/**
 * Get the relative path from a current module path to a target path
 * If the target is in a parent or sibling module, use the absolute path
 */
export function getRelativePath(currentModulePath: string, targetPath: string): string {
  // Split paths into segments
  const currentSegments = currentModulePath.split('::').filter(Boolean);
  const targetSegments = targetPath.split('::').filter(Boolean);

  // If target is a primitive type, return as is
  if (targetSegments.length === 1 && isPrimitiveType(targetPath)) {
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
 * Check if a type name is a primitive type
 */
export function isPrimitiveType(typeName: string): boolean {
  const primitives = [
    'bool',
    'i8',
    'i16',
    'i32',
    'i64',
    'i128',
    'isize',
    'u8',
    'u16',
    'u32',
    'u64',
    'u128',
    'usize',
    'f32',
    'f64',
    'char',
    'str',
    'void',
  ];
  return primitives.includes(typeName);
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

  for (const segment of segments) {
    if (!current[segment]) return null;
    current = current[segment] as Record<string, unknown>;
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
