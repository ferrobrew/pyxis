import { useDocumentation } from '../contexts/DocumentationContext';
import type { JsonSourceLocation } from '@pyxis/types';

// Base URL for pyxis-defs repository
const PYXIS_DEFS_REPO = 'https://github.com/ferrobrew/pyxis-defs';

interface SourceLinkProps {
  source: JsonSourceLocation;
}

/**
 * Resolves the source file path from the file index.
 * Returns the path from source_paths array if available.
 */
function useSourcePath(fileIndex: number): string | null {
  const { documentation } = useDocumentation();
  if (!documentation?.source_paths || fileIndex >= documentation.source_paths.length) {
    return null;
  }
  return documentation.source_paths[fileIndex];
}

/**
 * Determines if the current source is from pyxis-defs (a remote GitHub source).
 */
function useIsPyxisDefsSource(): boolean {
  const { selectedSource } = useDocumentation();
  // Remote sources have paths like "docs/JustCause3_Steam_1227440/output.json"
  // Local sources are "local"
  return selectedSource !== 'local' && selectedSource.startsWith('docs/');
}

/**
 * Builds a GitHub URL for a pyxis-defs source file.
 * The URL format is: https://github.com/ferrobrew/pyxis-defs/blob/main/{path}#L{line}
 */
function buildGitHubUrl(sourcePath: string, line: number, selectedSource: string): string {
  // The sourcePath is relative to the project directory (e.g., "game.pyxis")
  // We need to construct the full path: projects/{project_path}/{sourcePath}
  // selectedSource looks like "docs/JustCause3_Steam_1227440/output.json"
  // The project name has _ where the original path had / (see build_json.py)
  const match = selectedSource.match(/^docs\/([^/]+)\//);
  if (!match) {
    return '#';
  }
  // Convert underscores back to slashes for the actual GitHub path
  const projectPath = match[1].replace(/_/g, '/');
  return `${PYXIS_DEFS_REPO}/blob/main/projects/${projectPath}/${sourcePath}#L${line}`;
}

interface SourceNameProps {
  source: JsonSourceLocation;
  children: React.ReactNode;
  className?: string;
}

/**
 * Wraps a name/label to make it a clickable source link.
 * For pyxis-defs sources, renders as a GitHub link.
 * For local sources, renders children with a dotted underline indicating source info.
 */
export function SourceName({ source, children, className = '' }: SourceNameProps) {
  const { selectedSource } = useDocumentation();
  const sourcePath = useSourcePath(source.file_index);
  const isPyxisDefs = useIsPyxisDefsSource();

  if (!sourcePath) {
    return <>{children}</>;
  }

  const tooltip = `${sourcePath}:${source.line}`;

  if (isPyxisDefs) {
    const githubUrl = buildGitHubUrl(sourcePath, source.line, selectedSource);
    return (
      <a
        href={githubUrl}
        target="_blank"
        rel="noopener noreferrer"
        className={`hover:underline text-blue-600 dark:text-blue-400 ${className}`}
        title={tooltip}
      >
        {children}
      </a>
    );
  }

  // For local sources, show with dotted underline to indicate source info available
  return (
    <span
      className={`border-b border-dotted border-gray-400 dark:border-slate-500 ${className}`}
      title={tooltip}
    >
      {children}
    </span>
  );
}

export function SourceLink({ source }: SourceLinkProps) {
  const { selectedSource } = useDocumentation();
  const sourcePath = useSourcePath(source.file_index);
  const isPyxisDefs = useIsPyxisDefsSource();

  if (!sourcePath) {
    return null;
  }

  const tooltip = `${sourcePath}:${source.line}`;

  if (isPyxisDefs) {
    const githubUrl = buildGitHubUrl(sourcePath, source.line, selectedSource);
    return (
      <a
        href={githubUrl}
        target="_blank"
        rel="noopener noreferrer"
        className="text-blue-600 dark:text-blue-400 hover:underline"
        title={tooltip}
      >
        Source
      </a>
    );
  }

  // For local sources, just display without a link
  return (
    <span className="text-gray-500 dark:text-slate-500" title={tooltip}>
      Source
    </span>
  );
}
