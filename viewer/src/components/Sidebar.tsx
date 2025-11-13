import { useState } from 'react';
import { Link, useLocation } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import type { JsonModule } from '@pyxis/types';

interface ModuleTreeProps {
  name: string;
  module: JsonModule;
  path: string;
  level: number;
}

// Icon components for different item types
function TypeIcon() {
  return (
    <svg className="w-3.5 h-3.5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
      <path
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={2}
        d="M4 6h16M4 12h16M4 18h16"
      />
    </svg>
  );
}

function EnumIcon() {
  return (
    <svg className="w-3.5 h-3.5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
      <path
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={2}
        d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2"
      />
    </svg>
  );
}

function BitflagsIcon() {
  return (
    <svg className="w-3.5 h-3.5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
      <path
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={2}
        d="M3 21v-4m0 0V5a2 2 0 012-2h6.5l1 1H21l-3 6 3 6h-8.5l-1-1H5a2 2 0 00-2 2zm9-13.5V9"
      />
    </svg>
  );
}

function FunctionIcon() {
  return (
    <svg className="w-3.5 h-3.5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
      <path
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={2}
        d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4"
      />
    </svg>
  );
}

function GlobalIcon() {
  return (
    <svg className="w-3.5 h-3.5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
      <path
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={2}
        d="M3.055 11H5a2 2 0 012 2v1a2 2 0 002 2 2 2 0 012 2v2.945M8 3.935V5.5A2.5 2.5 0 0010.5 8h.5a2 2 0 012 2 2 2 0 104 0 2 2 0 012-2h1.064M15 20.488V18a2 2 0 012-2h3.064M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
      />
    </svg>
  );
}

function ModuleTree({ name, module, path, level }: ModuleTreeProps) {
  const [isOpen, setIsOpen] = useState(level < 2); // Auto-expand first two levels
  const location = useLocation();
  const { documentation } = useDocumentation();
  const currentPath = decodeURIComponent(location.pathname.split('/').pop() || '');
  const isActive = currentPath === path;

  const hasSubmodules = module.submodules && Object.keys(module.submodules).length > 0;
  const hasContent =
    hasSubmodules ||
    module.items.length > 0 ||
    module.functions.length > 0 ||
    module.extern_values.length > 0;

  return (
    <div>
      <div
        className={`flex items-center gap-1 py-1 px-2 rounded ${isActive ? 'bg-blue-100 dark:bg-purple-900/30' : ''}`}
      >
        {hasContent && (
          <button
            onClick={() => setIsOpen(!isOpen)}
            className="p-0.5 hover:bg-gray-200 dark:hover:bg-purple-950 rounded"
          >
            <svg
              className={`w-4 h-4 transition-transform ${isOpen ? 'rotate-90' : ''}`}
              fill="none"
              viewBox="0 0 24 24"
              stroke="currentColor"
            >
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5l7 7-7 7" />
            </svg>
          </button>
        )}
        {!hasContent && <div className="w-5" />}
        <Link
          to={`/module/${encodeURIComponent(path)}`}
          className="flex-1 text-sm hover:text-blue-600 dark:hover:text-blue-400"
          style={{ paddingLeft: `${level * 0.5}rem` }}
        >
          {name}
        </Link>
      </div>

      {isOpen && (
        <div className="ml-2">
          {/* Items (types, enums, bitflags) */}
          {module.items.length > 0 &&
            documentation &&
            module.items.map((itemPath) => {
              const item = documentation.items[itemPath];
              if (!item) return null;

              const itemName = itemPath.split('::').pop() || itemPath;
              const isItemActive = currentPath === itemPath;

              let Icon = TypeIcon;
              if (item.kind.type === 'enum') Icon = EnumIcon;
              else if (item.kind.type === 'bitflags') Icon = BitflagsIcon;

              return (
                <Link
                  key={itemPath}
                  to={`/item/${encodeURIComponent(itemPath)}`}
                  className={`flex items-center gap-2 py-1 px-2 text-sm hover:text-blue-600 dark:hover:text-blue-400 rounded ${
                    isItemActive ? 'bg-blue-100 dark:bg-purple-900/30' : ''
                  }`}
                  style={{ paddingLeft: `${(level + 1) * 0.5 + 1.25}rem` }}
                >
                  <Icon />
                  <span className="truncate">{itemName}</span>
                </Link>
              );
            })}

          {/* Functions */}
          {module.functions.length > 0 &&
            module.functions.map((func, idx) => {
              const funcKey = `${path}::${func.name}`;
              const isFuncActive = currentPath === funcKey;

              return (
                <div
                  key={`func-${idx}`}
                  className={`flex items-center gap-2 py-1 px-2 text-sm rounded ${
                    isFuncActive ? 'bg-blue-100 dark:bg-purple-900/30' : ''
                  }`}
                  style={{ paddingLeft: `${(level + 1) * 0.5 + 1.25}rem` }}
                >
                  <FunctionIcon />
                  <span className="truncate text-gray-600 dark:text-slate-400">{func.name}</span>
                </div>
              );
            })}

          {/* Extern values */}
          {module.extern_values.length > 0 &&
            module.extern_values.map((extVal, idx) => {
              const extKey = `${path}::${extVal.name}`;
              const isExtActive = currentPath === extKey;

              return (
                <div
                  key={`ext-${idx}`}
                  className={`flex items-center gap-2 py-1 px-2 text-sm rounded ${
                    isExtActive ? 'bg-blue-100 dark:bg-purple-900/30' : ''
                  }`}
                  style={{ paddingLeft: `${(level + 1) * 0.5 + 1.25}rem` }}
                >
                  <GlobalIcon />
                  <span className="truncate text-gray-600 dark:text-slate-400">{extVal.name}</span>
                </div>
              );
            })}

          {/* Submodules */}
          {hasSubmodules &&
            Object.entries(module.submodules).map(([subName, subModule]) => (
              <ModuleTree
                key={subName}
                name={subName}
                module={subModule as JsonModule}
                path={path ? `${path}::${subName}` : subName}
                level={level + 1}
              />
            ))}
        </div>
      )}
    </div>
  );
}

export function Sidebar() {
  const { documentation } = useDocumentation();

  if (!documentation) {
    return (
      <aside className="w-64 border-r bg-gray-50 dark:bg-slate-950 border-gray-200 dark:border-slate-800 p-4">
        <div className="text-sm text-gray-500 dark:text-slate-400">No documentation loaded</div>
      </aside>
    );
  }

  return (
    <aside className="w-64 border-r bg-gray-50 dark:bg-slate-950 border-gray-200 dark:border-slate-800 overflow-y-auto">
      <div className="p-4">
        <h2 className="text-lg font-semibold mb-4 text-gray-900 dark:text-slate-200">Modules</h2>
        <nav>
          {Object.entries(documentation.modules).map(([name, module]) => (
            <ModuleTree
              key={name}
              name={name}
              module={module as JsonModule}
              path={name}
              level={0}
            />
          ))}
        </nav>
      </div>
    </aside>
  );
}
