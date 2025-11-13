import { useEffect } from 'react';
import { useParams, Link, useNavigate } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { findModule, findLongestValidAncestor } from '../utils/pathUtils';
import {
  buildModuleUrl,
  buildItemUrl,
  buildRootUrl,
  decodeSourceIdentifier,
} from '../utils/navigation';
import { Collapsible } from './Collapsible';
import { TypeRef } from './TypeRef';
import { FunctionDisplay } from './FunctionDisplay';
import { CodeBlock } from './CodeBlock';
import type { JsonExternValue, JsonBackend, JsonItem, JsonFunction } from '@pyxis/types';

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
    ? 'mb-4 p-4 bg-gray-50 dark:bg-slate-800 rounded-md opacity-60'
    : 'mb-4 p-4 bg-gray-50 dark:bg-slate-800 rounded-md';
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

// Backend prologue section
interface BackendSectionProps {
  backends: { [key: string]: unknown };
}

function PrologueSection({ backends }: BackendSectionProps) {
  if (!backends || Object.keys(backends).length === 0) return null;

  const hasPrologues = Object.values(backends).some((configs) =>
    (configs as JsonBackend[]).some((config: JsonBackend) => config.prologue)
  );

  if (!hasPrologues) return null;

  return (
    <div className="mb-6">
      <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">
        Backend Prologue
      </h2>
      {Object.entries(backends).map(([backendName, configs]) => {
        // Join all prologues for this backend
        const joinedPrologue = (configs as JsonBackend[])
          .map((config: JsonBackend) => config.prologue)
          .filter((p) => p != null)
          .join('\n\n');

        if (!joinedPrologue) return null;

        return (
          <div key={backendName} className="mb-4">
            <h3 className="text-lg font-medium mb-2 text-gray-800 dark:text-slate-300">
              {backendName}
            </h3>
            <Collapsible title="Prologue">
              <CodeBlock code={joinedPrologue} language={backendName} />
            </Collapsible>
          </div>
        );
      })}
    </div>
  );
}

// Backend epilogue section
function EpilogueSection({ backends }: BackendSectionProps) {
  if (!backends || Object.keys(backends).length === 0) return null;

  const hasEpilogues = Object.values(backends).some((configs) =>
    (configs as JsonBackend[]).some((config: JsonBackend) => config.epilogue)
  );

  if (!hasEpilogues) return null;

  return (
    <div className="mb-6">
      <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">
        Backend Epilogue
      </h2>
      {Object.entries(backends).map(([backendName, configs]) => {
        // Join all epilogues for this backend
        const joinedEpilogue = (configs as JsonBackend[])
          .map((config: JsonBackend) => config.epilogue)
          .filter((e) => e != null)
          .join('\n\n');

        if (!joinedEpilogue) return null;

        return (
          <div key={backendName} className="mb-4">
            <h3 className="text-lg font-medium mb-2 text-gray-800 dark:text-slate-300">
              {backendName}
            </h3>
            <Collapsible title="Epilogue">
              <CodeBlock code={joinedEpilogue} language={backendName} />
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
      <div className="space-y-2">
        {Object.entries(submodules).map(([name, submodule]) => {
          const subPath = parentPath ? `${parentPath}::${name}` : name;
          const data = submodule as SubmoduleData;
          return (
            <Link
              key={name}
              to={buildModuleUrl(subPath, selectedSource)}
              className="block p-4 bg-gray-50 dark:bg-slate-800 rounded-md hover:bg-gray-100 dark:hover:bg-slate-700 transition-colors"
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
  const { modulePath = '', source = 'local' } = useParams();
  const { documentation, selectedSource, setSelectedSource } = useDocumentation();
  const navigate = useNavigate();

  // Sync source from URL to context (only if source is not 'local')
  useEffect(() => {
    if (source === 'local') {
      // If URL says local, ensure context matches
      if (selectedSource !== 'local') {
        setSelectedSource('local');
      }
    } else {
      // For remote sources, decode and sync
      const decodedSource = decodeSourceIdentifier(source);
      if (decodedSource !== selectedSource && decodedSource !== source) {
        setSelectedSource(decodedSource);
      }
    }
  }, [source, selectedSource, setSelectedSource]);

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
          // No valid ancestor found, navigate to root
          navigate(buildRootUrl(), { replace: true });
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

  // Generate breadcrumbs
  const pathSegments = decodedPath ? decodedPath.split('::') : [];
  const breadcrumbs = pathSegments.map((segment, idx) => {
    const partialPath = pathSegments.slice(0, idx + 1).join('::');
    return { name: segment, path: partialPath };
  });

  return (
    <div className="p-8 max-w-6xl">
      {/* Breadcrumbs */}
      {breadcrumbs.length > 0 && (
        <nav className="mb-4 flex items-center text-sm text-gray-600 dark:text-slate-400">
          {breadcrumbs.map((crumb, idx) => (
            <span key={crumb.path} className="flex items-center">
              {idx > 0 && (
                <svg className="w-4 h-4 mx-2" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    strokeWidth={2}
                    d="M9 5l7 7-7 7"
                  />
                </svg>
              )}
              {idx === breadcrumbs.length - 1 ? (
                <span className="font-semibold text-gray-900 dark:text-slate-200">
                  {crumb.name}
                </span>
              ) : (
                <Link
                  to={buildModuleUrl(crumb.path, selectedSource)}
                  className="hover:text-blue-600 dark:hover:text-blue-400"
                >
                  {crumb.name}
                </Link>
              )}
            </span>
          ))}
        </nav>
      )}

      <h1 className="text-3xl font-bold mb-2 text-gray-900 dark:text-slate-200">
        {decodedPath || 'Root'}
      </h1>

      {module.doc && (
        <div className="mb-6 p-4 bg-blue-50 dark:bg-blue-900/20 border-l-4 border-blue-500 rounded">
          <p className="text-gray-700 dark:text-slate-400">{module.doc}</p>
        </div>
      )}

      {/* Backend Prologues */}
      <PrologueSection backends={module.backends || {}} />

      {/* Extern Values */}
      {module.extern_values && module.extern_values.length > 0 && (
        <div className="mb-6">
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">
            Extern Values
          </h2>
          {module.extern_values.map((ext: JsonExternValue, idx: number) => (
            <ExternValueItem key={idx} extern={ext} />
          ))}
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
      <EpilogueSection backends={module.backends || {}} />
    </div>
  );
}
