import { useEffect } from 'react';
import { useParams, Link, useNavigate } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { findModule, findLongestValidAncestor } from '../utils/pathUtils';
import { buildModuleUrl, buildItemUrl, buildRootUrl } from '../utils/navigation';
import { Collapsible } from './Collapsible';
import { TypeRef } from './TypeRef';
import { FunctionDisplay } from './FunctionDisplay';
import { CodeBlock } from './CodeBlock';
import { Breadcrumbs } from './Breadcrumbs';
import { Markdown } from './Markdown';
import type {
  JsonExternValue,
  JsonBackend,
  JsonBackendSplice,
  JsonItem,
  JsonFunction,
} from '@pyxis/types';

interface ModuleData {
  doc?: string | null;
  items?: string[];
  submodules?: { [key: string]: unknown };
  extern_values?: JsonExternValue[];
  functions?: JsonFunction[];
  backends?: { [key: string]: unknown };
}

// Extern value display component
function ExternValueItem({ extern: ext }: { extern: JsonExternValue }) {
  const isPrivate = ext.visibility === 'private';
  const containerClasses = isPrivate
    ? 'p-2 opacity-60 border-b border-gray-200 dark:border-slate-700 last:border-b-0'
    : 'p-2 border-b border-gray-200 dark:border-slate-700 last:border-b-0';
  const nameClasses = isPrivate
    ? 'font-semibold text-gray-500 dark:text-slate-600'
    : 'font-semibold text-gray-900 dark:text-slate-200';

  return (
    <div id={`extval-${ext.name}`} className={containerClasses}>
      <div className="font-mono text-sm">
        <span className="text-violet-600 dark:text-slate-500">extern </span>
        <span className={nameClasses}>{ext.name}</span>
        <span className="text-gray-600 dark:text-slate-400">: </span>
        <TypeRef type={ext.type_ref} />
      </div>
      <div className="mt-2 text-xs text-gray-500 dark:text-slate-500">
        Address: 0x{ext.address.toString(16)}
      </div>
    </div>
  );
}

// Backend prologue/epilogue section. The cpp backend can populate two
// payloads per slot — `header` (lands in the `.hpp`) and `definition`
// (lands in the `.cpp`). Other backends only ever populate `header`,
// so they render as a single block; cpp renders both with labels.
type SpliceSlot = 'prologue' | 'epilogue';

interface BackendSpliceSectionProps {
  backends: { [key: string]: unknown };
  slot: SpliceSlot;
}

// Trim leading/trailing blank lines and remove the largest common
// leading-whitespace prefix shared by every non-blank line. Splices
// are stored as raw-string literals in the source (often
// `r#"<newline>    body<newline>"#`), so without this the rendered
// block carries gratuitous outer padding.
function normalizeSpliceText(text: string): string {
  const lines = text.split('\n');
  let start = 0;
  let end = lines.length;
  while (start < end && lines[start].trim() === '') start++;
  while (end > start && lines[end - 1].trim() === '') end--;
  const trimmed = lines.slice(start, end);
  if (trimmed.length === 0) return '';

  let minIndent = Infinity;
  for (const line of trimmed) {
    if (line.trim() === '') continue;
    const match = line.match(/^[ \t]*/);
    const indent = match ? match[0].length : 0;
    if (indent < minIndent) minIndent = indent;
  }
  if (!isFinite(minIndent) || minIndent === 0) return trimmed.join('\n');

  return trimmed.map((line) => line.slice(minIndent)).join('\n');
}

function joinSplicePart(splices: JsonBackendSplice[], part: 'header' | 'definition'): string {
  return splices
    .map((s) => s[part])
    .filter((v): v is string => v != null)
    .map(normalizeSpliceText)
    .filter((v) => v.length > 0)
    .join('\n\n');
}

function BackendSpliceSection({ backends, slot }: BackendSpliceSectionProps) {
  if (!backends || Object.keys(backends).length === 0) return null;

  const title = slot === 'prologue' ? 'Backend Prologue' : 'Backend Epilogue';
  const slotLabel = slot === 'prologue' ? 'Prologue' : 'Epilogue';
  const sectionMargin = slot === 'prologue' ? 'mb-6' : 'my-6';

  const hasAny = Object.values(backends).some((configs) =>
    (configs as JsonBackend[]).some((config) => {
      const splice = config[slot];
      return splice && (splice.header || splice.definition);
    })
  );
  if (!hasAny) return null;

  return (
    <div className={sectionMargin}>
      <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">{title}</h2>
      {Object.entries(backends).map(([backendName, configs]) => {
        const splices = (configs as JsonBackend[])
          .map((config) => config[slot])
          .filter((s): s is JsonBackendSplice => s != null);

        const joinedHeader = joinSplicePart(splices, 'header');
        const joinedDefinition = joinSplicePart(splices, 'definition');
        if (!joinedHeader && !joinedDefinition) return null;

        // Label the parts whenever a `definition` payload exists - that's
        // the only signal-bearing case (cpp routes definitions to the
        // `.cpp` source, headers to the `.hpp`). Header-only blocks
        // (rust, json, header-only cpp) don't need a label.
        const showLabels = !!joinedDefinition;
        const parts = [
          joinedHeader ? { label: 'header', code: joinedHeader } : null,
          joinedDefinition ? { label: 'definition', code: joinedDefinition } : null,
        ].filter((p): p is { label: string; code: string } => p !== null);

        return (
          <div key={backendName} className="mb-4">
            <h3 className="text-lg font-medium mb-2 text-gray-800 dark:text-slate-300">
              {backendName}
            </h3>
            <Collapsible title={slotLabel}>
              {parts.map((part, idx) => (
                <div
                  key={part.label}
                  className={idx > 0 ? 'border-t border-gray-300 dark:border-slate-800' : ''}
                >
                  {showLabels && (
                    <div className="px-3 py-1 text-[10px] uppercase tracking-wider font-semibold text-gray-500 dark:text-slate-400 bg-gray-100 dark:bg-slate-800 border-b border-gray-300 dark:border-slate-800">
                      {part.label}
                    </div>
                  )}
                  <CodeBlock code={part.code} language={backendName} />
                </div>
              ))}
            </Collapsible>
          </div>
        );
      })}
    </div>
  );
}

// Item list component
interface ItemListProps {
  items: Array<{ path: string; item: JsonItem }>;
}

function ItemList({ items }: ItemListProps) {
  const { selectedSource } = useDocumentation();
  if (items.length === 0) return null;

  return (
    <div>
      <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">Types</h2>
      <div className="bg-gray-50 dark:bg-slate-800 rounded-md overflow-hidden">
        {items.map(({ path, item }: { path: string; item: JsonItem }) => {
          if (!item) return null;
          const name = path.split('::').pop();
          return (
            <Link
              key={path}
              to={buildItemUrl(path, selectedSource)}
              className="block p-2 hover:bg-gray-100 dark:hover:bg-slate-700 transition-colors border-b border-gray-200 dark:border-slate-700 last:border-b-0"
            >
              <div className="flex items-center justify-between">
                <div>
                  <div className="font-mono text-sm font-semibold text-blue-600 dark:text-blue-400">
                    {name}
                  </div>
                  <div className="text-xs text-gray-500 dark:text-slate-500 mt-1">
                    {item.kind.type} • {item.size} bytes • align {item.alignment}
                  </div>
                </div>
                {item.kind.doc && (
                  <div className="text-sm text-gray-600 dark:text-slate-400 ml-4 max-w-md truncate">
                    {item.kind.doc}
                  </div>
                )}
              </div>
            </Link>
          );
        })}
      </div>
    </div>
  );
}

// Submodule list component
interface SubmoduleListProps {
  submodules: { [key: string]: unknown };
  parentPath: string;
}

interface SubmoduleData {
  doc?: string | null;
}

function SubmoduleList({ submodules, parentPath }: SubmoduleListProps) {
  const { selectedSource } = useDocumentation();
  if (!submodules || Object.keys(submodules).length === 0) return null;

  return (
    <div className="mt-6">
      <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">Submodules</h2>
      <div className="bg-gray-50 dark:bg-slate-800 rounded-md overflow-hidden">
        {Object.entries(submodules).map(([name, submodule]) => {
          const subPath = parentPath ? `${parentPath}::${name}` : name;
          const data = submodule as SubmoduleData;
          return (
            <Link
              key={name}
              to={buildModuleUrl(subPath, selectedSource)}
              className="block p-2 border-b border-gray-200 dark:border-slate-700 last:border-b-0 hover:bg-gray-100 dark:hover:bg-slate-700 transition-colors"
            >
              <div className="font-mono text-sm font-semibold text-blue-600 dark:text-blue-400">
                {name}
              </div>
              {data.doc && (
                <div className="text-sm text-gray-600 dark:text-slate-400 mt-1">{data.doc}</div>
              )}
            </Link>
          );
        })}
      </div>
    </div>
  );
}

// Main ModuleView component
export function ModuleView() {
  const { modulePath = '' } = useParams();
  const { documentation, selectedSource } = useDocumentation();
  const navigate = useNavigate();

  useEffect(() => {
    if (documentation && modulePath !== undefined) {
      const decodedPath = decodeURIComponent(modulePath);
      // Empty path represents root, which is always valid
      if (decodedPath === '') {
        return;
      }
      const moduleRaw = findModule(documentation.modules, decodedPath);

      if (!moduleRaw) {
        // Module doesn't exist, find the longest valid ancestor
        const ancestorPath = findLongestValidAncestor(decodedPath, documentation, false);

        if (ancestorPath === null || ancestorPath === '') {
          // No valid ancestor found, navigate to root with current source
          navigate(buildRootUrl(selectedSource), { replace: true });
        } else {
          // Navigate to the ancestor module
          navigate(buildModuleUrl(ancestorPath, selectedSource), { replace: true });
        }
      }
    }
  }, [documentation, modulePath, navigate, selectedSource]);

  if (!documentation) {
    return (
      <div className="p-8">
        <div className="text-gray-500 dark:text-slate-400">
          Please load a documentation file to begin.
        </div>
      </div>
    );
  }

  const decodedPath = decodeURIComponent(modulePath);
  const moduleRaw = findModule(documentation.modules, decodedPath);

  if (!moduleRaw) {
    // Return null while redirecting (unless it's root, which is handled above)
    if (decodedPath === '') {
      // This shouldn't happen, but handle it gracefully
      return null;
    }
    return null;
  }

  const module = moduleRaw as ModuleData;

  const items =
    module.items?.map((itemPath: string) => ({
      path: itemPath,
      item: documentation.items[itemPath],
    })) || [];

  return (
    <div className="p-4 md:p-6 lg:p-8 max-w-6xl">
      <div className="mb-4">
        <Breadcrumbs path={decodedPath} />
      </div>

      {module.doc && (
        <div className="mb-6 p-4 bg-blue-50 dark:bg-blue-900/20 border-l-4 border-blue-500 rounded text-gray-700 dark:text-slate-400">
          <Markdown>{module.doc}</Markdown>
        </div>
      )}

      {/* Backend Prologues */}
      <BackendSpliceSection backends={module.backends || {}} slot="prologue" />

      {/* Extern Values */}
      {module.extern_values && module.extern_values.length > 0 && (
        <div className="mb-6">
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">
            Extern Values
          </h2>
          <div className="bg-gray-50 dark:bg-slate-800 rounded-md overflow-hidden">
            {module.extern_values.map((ext: JsonExternValue, idx: number) => (
              <ExternValueItem key={idx} extern={ext} />
            ))}
          </div>
        </div>
      )}

      {/* Functions */}
      {module.functions && module.functions.length > 0 && (
        <div className="mb-6">
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">
            Functions
          </h2>
          <div className="bg-gray-50 dark:bg-slate-800 rounded-md overflow-hidden">
            {module.functions.map((func: JsonFunction, idx: number) => (
              <FunctionDisplay
                key={idx}
                id={`func-${func.name}`}
                func={func}
                modulePath={decodedPath}
              />
            ))}
          </div>
        </div>
      )}

      {/* Items (Types/Enums/Bitflags) */}
      <ItemList items={items} />

      {/* Submodules */}
      <SubmoduleList submodules={module.submodules || {}} parentPath={decodedPath} />

      {/* Backend Epilogues */}
      <BackendSpliceSection backends={module.backends || {}} slot="epilogue" />
    </div>
  );
}
