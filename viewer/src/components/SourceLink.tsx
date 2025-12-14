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
  // We need to construct the full path: projects/{project_name}/{sourcePath}
  // selectedSource looks like "docs/JustCause3_Steam_1227440/output.json"
  const match = selectedSource.match(/^docs\/([^/]+)\//);
  if (!match) {
    return '#';
  }
  const projectName = match[1];
  return `${PYXIS_DEFS_REPO}/blob/main/projects/${projectName}/${sourcePath}#L${line}`;
}

export function SourceLink({ source }: SourceLinkProps) {
  const { selectedSource } = useDocumentation();
  const sourcePath = useSourcePath(source.file_index);
  const isPyxisDefs = useIsPyxisDefsSource();

  if (!sourcePath) {
    return null;
  }

  const displayText = `${sourcePath}:${source.line}`;

  if (isPyxisDefs) {
    const githubUrl = buildGitHubUrl(sourcePath, source.line, selectedSource);
    return (
      <a
        href={githubUrl}
        target="_blank"
        rel="noopener noreferrer"
        className="text-sm text-blue-600 dark:text-blue-400 hover:underline font-mono"
        title="View source on GitHub"
      >
        {displayText}
      </a>
    );
  }

  // For local sources, just display the path without a link
  return (
    <span className="text-sm text-gray-600 dark:text-slate-400 font-mono" title="Source location">
      {displayText}
    </span>
  );
}
