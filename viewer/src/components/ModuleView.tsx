import { useParams, Link } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { findModule } from '../utils/pathUtils';
import { Collapsible } from './Collapsible';
import { TypeRef } from './TypeRef';
import { FunctionDisplay } from './FunctionDisplay';
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
  return (
    <div className="mb-4 p-4 bg-gray-50 dark:bg-gray-800 rounded-md">
      <div className="font-mono text-sm">
        <span className="text-purple-600 dark:text-purple-400">extern </span>
        <span className="font-semibold text-gray-900 dark:text-gray-100">{ext.name}</span>
        <span className="text-gray-600 dark:text-gray-400">: </span>
        <TypeRef type={ext.type_ref} />
      </div>
      <div className="mt-2 text-xs text-gray-500 dark:text-gray-500">
        Address: 0x{ext.address.toString(16)}
      </div>
    </div>
  );
}

// Backend configuration section
interface BackendSectionProps {
  backends: { [key: string]: unknown };
}

function BackendSection({ backends }: BackendSectionProps) {
  if (!backends || Object.keys(backends).length === 0) return null;

  return (
    <div className="mb-6">
      <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-gray-100">
        Backend Configuration
      </h2>
      {Object.entries(backends).map(([backendName, configs]) => (
        <div key={backendName} className="mb-4">
          <h3 className="text-lg font-medium mb-2 text-gray-800 dark:text-gray-200">
            {backendName}
          </h3>
          {(configs as JsonBackend[]).map((config: JsonBackend, idx: number) => (
            <div key={idx} className="space-y-2">
              {config.prologue && (
                <Collapsible title="Prologue">
                  <pre className="text-sm font-mono bg-gray-100 dark:bg-gray-900 p-3 rounded overflow-x-auto">
                    {config.prologue}
                  </pre>
                </Collapsible>
              )}
              {config.epilogue && (
                <Collapsible title="Epilogue">
                  <pre className="text-sm font-mono bg-gray-100 dark:bg-gray-900 p-3 rounded overflow-x-auto">
                    {config.epilogue}
                  </pre>
                </Collapsible>
              )}
            </div>
          ))}
        </div>
      ))}
    </div>
  );
}

// Item list component
interface ItemListProps {
  items: Array<{ path: string; item: JsonItem }>;
}

function ItemList({ items }: ItemListProps) {
  if (items.length === 0) return null;

  return (
    <div>
      <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-gray-100">Types</h2>
      <div className="space-y-2">
        {items.map(({ path, item }: { path: string; item: JsonItem }) => {
          if (!item) return null;
          const name = path.split('::').pop();
          return (
            <Link
              key={path}
              to={`/item/${encodeURIComponent(path)}`}
              className="block p-4 bg-gray-50 dark:bg-gray-800 rounded-md hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors"
            >
              <div className="flex items-center justify-between">
                <div>
                  <div className="font-mono text-sm font-semibold text-blue-600 dark:text-blue-400">
                    {name}
                  </div>
                  <div className="text-xs text-gray-500 dark:text-gray-500 mt-1">
                    {item.kind.type} • {item.size} bytes • align {item.alignment}
                  </div>
                </div>
                {item.kind.doc && (
                  <div className="text-sm text-gray-600 dark:text-gray-400 ml-4 max-w-md truncate">
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
  if (!submodules || Object.keys(submodules).length === 0) return null;

  return (
    <div className="mt-6">
      <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-gray-100">Submodules</h2>
      <div className="space-y-2">
        {Object.entries(submodules).map(([name, submodule]) => {
          const subPath = parentPath ? `${parentPath}::${name}` : name;
          const data = submodule as SubmoduleData;
          return (
            <Link
              key={name}
              to={`/module/${encodeURIComponent(subPath)}`}
              className="block p-4 bg-gray-50 dark:bg-gray-800 rounded-md hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors"
            >
              <div className="font-mono text-sm font-semibold text-blue-600 dark:text-blue-400">
                {name}
              </div>
              {data.doc && (
                <div className="text-sm text-gray-600 dark:text-gray-400 mt-1">{data.doc}</div>
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
  const { documentation } = useDocumentation();

  if (!documentation) {
    return (
      <div className="p-8">
        <div className="text-gray-500 dark:text-gray-400">
          Please load a documentation file to begin.
        </div>
      </div>
    );
  }

  const decodedPath = decodeURIComponent(modulePath);
  const moduleRaw = findModule(documentation.modules, decodedPath);

  if (!moduleRaw) {
    return (
      <div className="p-8">
        <div className="text-red-500">Module not found: {decodedPath}</div>
      </div>
    );
  }

  const module = moduleRaw as ModuleData;

  const items =
    module.items?.map((itemPath: string) => ({
      path: itemPath,
      item: documentation.items[itemPath],
    })) || [];

  return (
    <div className="p-8 max-w-6xl">
      <h1 className="text-3xl font-bold mb-2 text-gray-900 dark:text-gray-100">
        {decodedPath || 'Root'}
      </h1>

      {module.doc && (
        <div className="mb-6 p-4 bg-blue-50 dark:bg-blue-900/20 border-l-4 border-blue-500 rounded">
          <p className="text-gray-700 dark:text-gray-300">{module.doc}</p>
        </div>
      )}

      {/* Backend Configurations */}
      <BackendSection backends={module.backends || {}} />

      {/* Extern Values */}
      {module.extern_values && module.extern_values.length > 0 && (
        <div className="mb-6">
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-gray-100">
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
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-gray-100">Functions</h2>
          {module.functions.map((func: JsonFunction, idx: number) => (
            <FunctionDisplay key={idx} func={func} modulePath={decodedPath} />
          ))}
        </div>
      )}

      {/* Items (Types/Enums/Bitflags) */}
      <ItemList items={items} />

      {/* Submodules */}
      <SubmoduleList submodules={module.submodules || {}} parentPath={decodedPath} />
    </div>
  );
}
