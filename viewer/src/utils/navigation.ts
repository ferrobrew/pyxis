// Global source map: ID -> full path
const sourceMap = new Map<string, string>();

/**
 * Register a source path and return its ID
 */
export function registerSource(path: string): string {
  const id = extractSourceId(path);
  sourceMap.set(id, path);
  return id;
}

/**
 * Extract a simple ID from a GitHub path
 * e.g., "docs/JustCause3_Steam_1227440/output.json" -> "JustCause3_Steam_1227440"
 */
export function extractSourceId(path: string): string {
  // Extract the directory name from the path
  // "docs/JustCause3_Steam_1227440/output.json" -> "JustCause3_Steam_1227440"
  const parts = path.split('/');
  if (parts.length >= 2) {
    return parts[parts.length - 2]; // Get the directory name before the filename
  }
  // Fallback: use the path without extension
  return path.replace(/\.json$/, '').replace(/\//g, '_');
}

/**
 * Get the URL-safe source identifier
 * For local projects, returns "local"
 * For GitHub projects, returns a simple ID extracted from the path
 */
export function getSourceIdentifier(selectedSource: string): string {
  if (selectedSource === 'local') {
    return 'local';
  }
  // For GitHub sources, extract a simple ID
  return extractSourceId(selectedSource);
}

/**
 * Decode the source identifier from URL back to full path
 * Uses the global source map to look up the full path from the ID
 */
export function decodeSourceIdentifier(sourceParam: string): string {
  if (sourceParam === 'local') {
    return 'local';
  }
  // Look up the full path from the ID
  return sourceMap.get(sourceParam) || sourceParam;
}

/**
 * Build a navigation path with source
 */
export function buildPathWithSource(path: string, source: string): string {
  const sourceId = getSourceIdentifier(source);
  return `/${path}/${sourceId}`;
}

/**
 * Build module navigation URL
 * Note: HashRouter automatically adds the #, so we don't include it here
 */
export function buildModuleUrl(modulePath: string, source: string): string {
  const sourceId = getSourceIdentifier(source);
  const encodedPath = modulePath ? encodeURIComponent(modulePath) : '';
  // For empty paths, we still need to include something, so use empty string encoded
  return `/${sourceId}/module/${encodedPath}`;
}

/**
 * Build item navigation URL
 * Note: HashRouter automatically adds the #, so we don't include it here
 */
export function buildItemUrl(itemPath: string, source: string): string {
  const sourceId = getSourceIdentifier(source);
  return `/${sourceId}/item/${encodeURIComponent(itemPath)}`;
}

/**
 * Build root URL
 * Note: HashRouter automatically adds the #, so we don't include it here
 */
export function buildRootUrl(): string {
  return '/';
}
