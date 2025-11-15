import { useState, useEffect, useRef } from 'react';
import { Link, useLocation } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import type { JsonModule } from '@pyxis/types';
import { buildModuleUrl, buildItemUrl } from '../utils/navigation';
import { getItemTypeColor, getItemTypeHoverColor, type ItemType } from '../utils/colors';

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

function FieldIcon() {
  return (
    <svg className="w-3.5 h-3.5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
      <path
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={2}
        d="M7 7h.01M7 3h5c.512 0 1.024.195 1.414.586l7 7a2 2 0 010 2.828l-7 7a2 2 0 01-2.828 0l-7-7A1.994 1.994 0 013 12V7a4 4 0 014-4z"
      />
    </svg>
  );
}

// Item tree component for types/enums/bitflags with their members
interface ItemTreeProps {
  itemPath: string;
  level: number;
}

function ItemTree({ itemPath, level }: ItemTreeProps) {
  const { documentation, selectedSource } = useDocumentation();
  const location = useLocation();
  const [isItemOpen, setIsItemOpen] = useState(false);

  if (!documentation) return null;

  const item = documentation.items[itemPath];
  if (!item) return null;

  const itemName = itemPath.split('::').pop() || itemPath;
  const currentPath = decodeURIComponent(location.pathname.split('/').pop() || '');
  const isItemActive = currentPath === itemPath;

  let Icon = TypeIcon;
  let itemType: ItemType = 'type';
  if (item.kind.type === 'enum') {
    Icon = EnumIcon;
    itemType = 'enum';
  } else if (item.kind.type === 'bitflags') {
    Icon = BitflagsIcon;
    itemType = 'bitflags';
  }

  // Get public members for this item
  const publicFields =
    item.kind.type === 'type'
      ? item.kind.fields.filter((f) => f.visibility === 'public' && f.name)
      : [];
  const publicVirtualFunctions =
    item.kind.type === 'type' && item.kind.vftable
      ? item.kind.vftable.functions.filter((f) => f.visibility === 'public')
      : [];
  const publicAssociatedFunctions =
    item.kind.type === 'type' || item.kind.type === 'enum'
      ? item.kind.associated_functions.filter((f) => f.visibility === 'public')
      : [];
  const variants = item.kind.type === 'enum' ? item.kind.variants : [];
  const flags = item.kind.type === 'bitflags' ? item.kind.flags : [];

  const hasMembers =
    publicFields.length > 0 ||
    publicVirtualFunctions.length > 0 ||
    publicAssociatedFunctions.length > 0 ||
    variants.length > 0 ||
    flags.length > 0;

  const handleItemClick = () => {
    if (hasMembers) {
      setIsItemOpen(true);
    }
  };

  return (
    <div>
      <div
        className={`flex items-center gap-1 py-1 px-2 rounded ${
          isItemActive ? 'bg-blue-100 dark:bg-slate-700/30' : ''
        }`}
        style={{ paddingLeft: `${(level + 1) * 0.5 + 1.25}rem` }}
      >
        {hasMembers && (
          <button
            onClick={(e) => {
              e.stopPropagation();
              setIsItemOpen(!isItemOpen);
            }}
            className="p-0.5 hover:bg-gray-200 dark:hover:bg-slate-800 rounded flex-shrink-0"
          >
            <svg
              className={`w-3 h-3 transition-transform ${isItemOpen ? 'rotate-90' : ''}`}
              fill="none"
              viewBox="0 0 24 24"
              stroke="currentColor"
            >
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5l7 7-7 7" />
            </svg>
          </button>
        )}
        {!hasMembers && <div className="w-3.5" />}
        <Link
          to={buildItemUrl(itemPath, selectedSource)}
          onClick={handleItemClick}
          className={`flex items-center gap-2 flex-1 text-sm min-w-0 ${getItemTypeColor(itemType)} ${getItemTypeHoverColor(itemType)}`}
        >
          <Icon />
          <span className="truncate">{itemName}</span>
        </Link>
      </div>

      {/* Item members */}
      {isItemOpen && hasMembers && (
        <div className="ml-4">
          {/* Fields */}
          {publicFields.map((field) => (
            <Link
              key={`${itemPath}-field-${field.name}`}
              to={`${buildItemUrl(itemPath, selectedSource)}##field-${field.name}`}
              className="flex items-center gap-2 py-1 px-2 text-xs text-gray-600 dark:text-slate-400 hover:text-blue-600 dark:hover:text-blue-400 rounded"
              style={{ paddingLeft: `${(level + 2) * 0.5 + 1.25}rem` }}
            >
              <FieldIcon />
              <span className="truncate">{field.name}</span>
            </Link>
          ))}

          {/* Virtual Functions */}
          {publicVirtualFunctions.map((func) => (
            <Link
              key={`${itemPath}-vfunc-${func.name}`}
              to={`${buildItemUrl(itemPath, selectedSource)}##vfunc-${func.name}`}
              className={`flex items-center gap-2 py-1 px-2 text-xs rounded ${getItemTypeColor('function')} ${getItemTypeHoverColor('function')}`}
              style={{ paddingLeft: `${(level + 2) * 0.5 + 1.25}rem` }}
            >
              <FunctionIcon />
              <span className="truncate">{func.name}</span>
            </Link>
          ))}

          {/* Associated Functions */}
          {publicAssociatedFunctions.map((func) => (
            <Link
              key={`${itemPath}-func-${func.name}`}
              to={`${buildItemUrl(itemPath, selectedSource)}##func-${func.name}`}
              className={`flex items-center gap-2 py-1 px-2 text-xs rounded ${getItemTypeColor('function')} ${getItemTypeHoverColor('function')}`}
              style={{ paddingLeft: `${(level + 2) * 0.5 + 1.25}rem` }}
            >
              <FunctionIcon />
              <span className="truncate">{func.name}</span>
            </Link>
          ))}

          {/* Enum Variants */}
          {variants.map((variant) => (
            <Link
              key={`${itemPath}-variant-${variant.name}`}
              to={`${buildItemUrl(itemPath, selectedSource)}##variant-${variant.name}`}
              className={`flex items-center gap-2 py-1 px-2 text-xs rounded ${getItemTypeColor('enum-variant')} ${getItemTypeHoverColor('enum-variant')}`}
              style={{ paddingLeft: `${(level + 2) * 0.5 + 1.25}rem` }}
            >
              <EnumIcon />
              <span className="truncate">{variant.name}</span>
            </Link>
          ))}

          {/* Bitflags */}
          {flags.map((flag) => (
            <Link
              key={`${itemPath}-flag-${flag.name}`}
              to={`${buildItemUrl(itemPath, selectedSource)}##flag-${flag.name}`}
              className={`flex items-center gap-2 py-1 px-2 text-xs rounded ${getItemTypeColor('bitflags')} ${getItemTypeHoverColor('bitflags')}`}
              style={{ paddingLeft: `${(level + 2) * 0.5 + 1.25}rem` }}
            >
              <BitflagsIcon />
              <span className="truncate">{flag.name}</span>
            </Link>
          ))}
        </div>
      )}
    </div>
  );
}

function ModuleTree({ name, module, path, level }: ModuleTreeProps) {
  const [isOpen, setIsOpen] = useState(level < 2); // Auto-expand first two levels
  const location = useLocation();
  const { documentation, selectedSource } = useDocumentation();
  const currentPath = decodeURIComponent(location.pathname.split('/').pop() || '');
  const isActive = currentPath === path;

  const hasSubmodules = module.submodules && Object.keys(module.submodules).length > 0;
  const hasContent =
    hasSubmodules ||
    module.items.length > 0 ||
    module.functions.length > 0 ||
    module.extern_values.length > 0;

  const handleModuleClick = () => {
    if (hasContent) {
      setIsOpen(true);
    }
  };

  return (
    <div>
      <div
        className={`flex items-center gap-1 py-1 px-2 rounded ${isActive ? 'bg-blue-100 dark:bg-slate-700/30' : ''}`}
      >
        {hasContent && (
          <button
            onClick={(e) => {
              e.stopPropagation();
              setIsOpen(!isOpen);
            }}
            className="p-0.5 hover:bg-gray-200 dark:hover:bg-slate-800 rounded"
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
          to={buildModuleUrl(path, selectedSource)}
          onClick={handleModuleClick}
          className={`flex-1 text-sm ${getItemTypeColor('module')} ${getItemTypeHoverColor('module')}`}
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
            module.items.map((itemPath) => (
              <ItemTree key={itemPath} itemPath={itemPath} level={level} />
            ))}

          {/* Functions */}
          {module.functions.length > 0 &&
            module.functions.map((func, idx) => {
              const funcKey = `${path}::${func.name}`;
              const isFuncActive = currentPath === funcKey;

              return (
                <div
                  key={`func-${idx}`}
                  className={`flex items-center gap-2 py-1 px-2 text-sm rounded ${
                    isFuncActive ? 'bg-blue-100 dark:bg-slate-700/30' : ''
                  }`}
                  style={{ paddingLeft: `${(level + 1) * 0.5 + 1.25}rem` }}
                >
                  <FunctionIcon />
                  <span className={`truncate ${getItemTypeColor('function')}`}>{func.name}</span>
                </div>
              );
            })}

          {/* Extern values */}
          {module.extern_values.length > 0 &&
            module.extern_values.map((extVal, idx) => {
              const extKey = `${path}::${extVal.name}`;
              const isExtActive = currentPath === extKey;

              return (
                <Link
                  key={`ext-${idx}`}
                  to={`${buildModuleUrl(path, selectedSource)}##extval-${extVal.name}`}
                  className={`flex items-center gap-2 py-1 px-2 text-sm rounded ${getItemTypeColor('extern')} ${getItemTypeHoverColor('extern')} ${
                    isExtActive ? 'bg-blue-100 dark:bg-slate-700/30' : ''
                  }`}
                  style={{ paddingLeft: `${(level + 1) * 0.5 + 1.25}rem` }}
                >
                  <GlobalIcon />
                  <span className="truncate">{extVal.name}</span>
                </Link>
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
  const [sidebarWidth, setSidebarWidth] = useState(() => {
    const saved = localStorage.getItem('sidebarWidth');
    return saved ? parseInt(saved, 10) : 256; // Default 256px (w-64)
  });
  const [isResizing, setIsResizing] = useState(false);
  const sidebarRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const handleMouseMove = (e: MouseEvent) => {
      if (!isResizing) return;
      const newWidth = e.clientX;
      const minWidth = 200;
      const maxWidth = 800;
      const clampedWidth = Math.max(minWidth, Math.min(maxWidth, newWidth));
      setSidebarWidth(clampedWidth);
    };

    const handleMouseUp = () => {
      setIsResizing(false);
    };

    if (isResizing) {
      document.addEventListener('mousemove', handleMouseMove);
      document.addEventListener('mouseup', handleMouseUp);
      document.body.style.cursor = 'col-resize';
      document.body.style.userSelect = 'none';
    }

    return () => {
      document.removeEventListener('mousemove', handleMouseMove);
      document.removeEventListener('mouseup', handleMouseUp);
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
    };
  }, [isResizing]);

  useEffect(() => {
    localStorage.setItem('sidebarWidth', sidebarWidth.toString());
  }, [sidebarWidth]);

  const handleMouseDown = () => {
    setIsResizing(true);
  };

  if (!documentation) {
    return (
      <div className="flex relative" style={{ width: `${sidebarWidth}px` }}>
        <aside className="flex-1 border-r bg-gray-50 dark:bg-slate-950 border-gray-200 dark:border-slate-800 p-4">
          <div className="text-sm text-gray-500 dark:text-slate-400">No documentation loaded</div>
        </aside>
        <div
          onMouseDown={handleMouseDown}
          className="w-1 bg-gray-200 dark:bg-slate-700 hover:bg-blue-500 dark:hover:bg-blue-600 cursor-col-resize transition-colors"
        />
      </div>
    );
  }

  return (
    <div className="flex relative" ref={sidebarRef} style={{ width: `${sidebarWidth}px` }}>
      <aside className="flex-1 border-r bg-gray-50 dark:bg-slate-950 border-gray-200 dark:border-slate-800 overflow-y-auto">
        <div className="p-2">
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
      <div
        onMouseDown={handleMouseDown}
        className="w-1 bg-gray-200 dark:bg-slate-700 hover:bg-blue-500 dark:hover:bg-blue-600 cursor-col-resize transition-colors"
      />
    </div>
  );
}
