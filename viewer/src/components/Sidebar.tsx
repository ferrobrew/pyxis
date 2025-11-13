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

function ModuleTree({ name, module, path, level }: ModuleTreeProps) {
  const [isOpen, setIsOpen] = useState(level < 2); // Auto-expand first two levels
  const location = useLocation();
  const currentPath = decodeURIComponent(location.pathname.split('/').pop() || '');
  const isActive = currentPath === path;

  const hasSubmodules = module.submodules && Object.keys(module.submodules).length > 0;

  return (
    <div>
      <div
        className={`flex items-center gap-1 py-1 px-2 rounded ${isActive ? 'bg-blue-100 dark:bg-purple-800/50' : ''}`}
      >
        {hasSubmodules && (
          <button
            onClick={() => setIsOpen(!isOpen)}
            className="p-0.5 hover:bg-gray-200 dark:hover:bg-purple-800 rounded"
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
        {!hasSubmodules && <div className="w-5" />}
        <Link
          to={`/module/${encodeURIComponent(path)}`}
          className="flex-1 text-sm hover:text-blue-600 dark:hover:text-blue-400"
          style={{ paddingLeft: `${level * 0.5}rem` }}
        >
          {name}
        </Link>
      </div>

      {isOpen && hasSubmodules && (
        <div className="ml-2">
          {Object.entries(module.submodules).map(([subName, subModule]) => (
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
      <aside className="w-64 border-r bg-gray-50 dark:bg-purple-950 border-gray-200 dark:border-purple-800 p-4">
        <div className="text-sm text-gray-500 dark:text-purple-300">No documentation loaded</div>
      </aside>
    );
  }

  return (
    <aside className="w-64 border-r bg-gray-50 dark:bg-purple-950 border-gray-200 dark:border-purple-800 overflow-y-auto">
      <div className="p-4">
        <h2 className="text-lg font-semibold mb-4 text-gray-900 dark:text-purple-50">Modules</h2>
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
