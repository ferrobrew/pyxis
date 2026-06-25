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

// Shared row layout; indentation comes from nested <TreeChildren> wrappers, not
// from per-row padding, so every level steps by the same amount.
const ROW = 'flex items-center gap-1.5 rounded px-1.5 py-1';

// A fixed-width disclosure control so rows with and without children align.
function ChevronSlot({ open = false, onToggle }: { open?: boolean; onToggle?: () => void }) {
  if (!onToggle) return <span className="w-4 flex-shrink-0" aria-hidden="true" />;
  return (
    <button
      type="button"
      onClick={(e) => {
        e.stopPropagation();
        e.preventDefault();
        onToggle();
      }}
      className="flex h-4 w-4 flex-shrink-0 items-center justify-center rounded text-fg-subtle hover:text-fg"
      aria-label={open ? 'Collapse' : 'Expand'}
    >
      <svg
        className={`h-3 w-3 transition-transform ${open ? 'rotate-90' : ''}`}
        fill="none"
        viewBox="0 0 24 24"
        stroke="currentColor"
      >
        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5l7 7-7 7" />
      </svg>
    </button>
  );
}

// Indents one level of children and draws a guide line under the parent.
function TreeChildren({ children }: { children: React.ReactNode }) {
  return <div className="ml-[0.85rem] border-l border-edge pl-1">{children}</div>;
}

function FolderIcon() {
  return (
    <svg
      className="h-3.5 w-3.5 flex-shrink-0"
      fill="none"
      viewBox="0 0 24 24"
      stroke="currentColor"
    >
      <path
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth={2}
        d="M3 7a2 2 0 012-2h3.6a2 2 0 011.4.6L11.4 7H19a2 2 0 012 2v8a2 2 0 01-2 2H5a2 2 0 01-2-2V7z"
      />
    </svg>
  );
}

// A member row (field, function, variant, flag) inside an expanded item. The
// active one (matched by the URL anchor) is highlighted and takes the ref so it
// can be scrolled into view.
function MemberRow({
  to,
  icon,
  label,
  colorClasses,
  active,
  activeRef,
}: {
  to: string;
  icon: React.ReactNode;
  label: string;
  colorClasses: string;
  active: boolean;
  activeRef: React.Ref<HTMLAnchorElement>;
}) {
  return (
    <Link
      ref={active ? activeRef : undefined}
      to={to}
      className={`${ROW} text-xs ${active ? 'bg-accent-soft text-fg' : colorClasses}`}
    >
      <ChevronSlot />
      {icon}
      <span className="truncate">{label}</span>
    </Link>
  );
}

// Item tree component for types/enums/bitflags with their members
interface ItemTreeProps {
  itemPath: string;
}

function ItemTree({ itemPath }: ItemTreeProps) {
  const { documentation, selectedSource } = useDocumentation();
  const location = useLocation();
  const [isItemOpen, setIsItemOpen] = useState(false);
  const rowRef = useRef<HTMLDivElement>(null);
  const memberRef = useRef<HTMLAnchorElement>(null);
  const currentPath = decodeURIComponent(location.pathname.split('/').pop() || '');
  const isItemActive = currentPath === itemPath;
  // The member anchor the URL points at (e.g. `field-m_Foo`), if we're on this
  // item's page. Used to highlight + reveal that member in the tree.
  const activeAnchor = isItemActive ? location.hash.replace(/^#+/, '') : '';

  // Keep the active item scrolled into view within the sidebar.
  // `selectedSource` is a dependency so the scroll re-fires when switching
  // projects (the component stays mounted but the tree rebuilds).
  useEffect(() => {
    if (isItemActive) {
      requestAnimationFrame(() => rowRef.current?.scrollIntoView({ block: 'nearest' }));
    }
  }, [isItemActive, selectedSource]);

  // When the URL targets a member, expand this item...
  useEffect(() => {
    if (activeAnchor) setIsItemOpen(true);
  }, [activeAnchor]);

  // ...then (once its rows are mounted) scroll the active member into view.
  useEffect(() => {
    if (activeAnchor && isItemOpen) memberRef.current?.scrollIntoView({ block: 'nearest' });
  }, [activeAnchor, isItemOpen]);

  if (!documentation) return null;

  const item = documentation.items[itemPath];
  if (!item) return null;

  const itemName = itemPath.split('::').pop() || itemPath;

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
      <div ref={rowRef} className={`${ROW} ${isItemActive ? 'bg-accent-soft text-fg' : ''}`}>
        <ChevronSlot
          open={isItemOpen}
          onToggle={hasMembers ? () => setIsItemOpen(!isItemOpen) : undefined}
        />
        <Link
          to={buildItemUrl(itemPath, selectedSource)}
          onClick={handleItemClick}
          className={`flex min-w-0 flex-1 items-center gap-2 text-sm ${getItemTypeColor(itemType)} ${getItemTypeHoverColor(itemType)}`}
        >
          <Icon />
          <span className="truncate">
            {itemName}
            {item.type_parameters && item.type_parameters.length > 0 && (
              <span className="text-kind-enum">&lt;{item.type_parameters.join(', ')}&gt;</span>
            )}
          </span>
        </Link>
      </div>

      {isItemOpen && hasMembers && (
        <TreeChildren>
          {publicFields.map((field) => (
            <MemberRow
              key={`${itemPath}-field-${field.name}`}
              to={`${buildItemUrl(itemPath, selectedSource)}##field-${field.name}`}
              icon={<FieldIcon />}
              label={field.name!}
              colorClasses="text-fg-muted hover:text-accent"
              active={activeAnchor === `field-${field.name}`}
              activeRef={memberRef}
            />
          ))}

          {publicVirtualFunctions.map((func) => (
            <MemberRow
              key={`${itemPath}-vfunc-${func.name}`}
              to={`${buildItemUrl(itemPath, selectedSource)}##vfunc-${func.name}`}
              icon={<FunctionIcon />}
              label={func.name}
              colorClasses={`${getItemTypeColor('function')} ${getItemTypeHoverColor('function')}`}
              active={activeAnchor === `vfunc-${func.name}`}
              activeRef={memberRef}
            />
          ))}

          {publicAssociatedFunctions.map((func) => (
            <MemberRow
              key={`${itemPath}-func-${func.name}`}
              to={`${buildItemUrl(itemPath, selectedSource)}##func-${func.name}`}
              icon={<FunctionIcon />}
              label={func.name}
              colorClasses={`${getItemTypeColor('function')} ${getItemTypeHoverColor('function')}`}
              active={activeAnchor === `func-${func.name}`}
              activeRef={memberRef}
            />
          ))}

          {variants.map((variant) => (
            <MemberRow
              key={`${itemPath}-variant-${variant.name}`}
              to={`${buildItemUrl(itemPath, selectedSource)}##variant-${variant.name}`}
              icon={<EnumIcon />}
              label={variant.name}
              colorClasses={`${getItemTypeColor('enum-variant')} ${getItemTypeHoverColor('enum-variant')}`}
              active={activeAnchor === `variant-${variant.name}`}
              activeRef={memberRef}
            />
          ))}

          {flags.map((flag) => (
            <MemberRow
              key={`${itemPath}-flag-${flag.name}`}
              to={`${buildItemUrl(itemPath, selectedSource)}##flag-${flag.name}`}
              icon={<BitflagsIcon />}
              label={flag.name}
              colorClasses={`${getItemTypeColor('bitflags')} ${getItemTypeHoverColor('bitflags')}`}
              active={activeAnchor === `flag-${flag.name}`}
              activeRef={memberRef}
            />
          ))}
        </TreeChildren>
      )}
    </div>
  );
}

function ModuleTree({ name, module, path, level }: ModuleTreeProps) {
  const [isOpen, setIsOpen] = useState(level < 2); // Auto-expand first two levels
  const location = useLocation();
  const { documentation, selectedSource } = useDocumentation();
  const rowRef = useRef<HTMLDivElement>(null);
  const memberRef = useRef<HTMLAnchorElement>(null);
  const currentPath = decodeURIComponent(location.pathname.split('/').pop() || '');
  const isActive = currentPath === path;
  // True when the active item/module lives somewhere inside this subtree.
  const isAncestor = isActive || currentPath.startsWith(`${path}::`);
  // The function/extern anchor the URL points at, if we're on this module page.
  const activeAnchor = isActive ? location.hash.replace(/^#+/, '') : '';

  // Expand the path down to the active node, and keep it scrolled into view.
  useEffect(() => {
    if (isAncestor) setIsOpen(true);
  }, [isAncestor]);
  useEffect(() => {
    if (isActive) {
      requestAnimationFrame(() => rowRef.current?.scrollIntoView({ block: 'nearest' }));
    }
  }, [isActive, selectedSource]);
  useEffect(() => {
    if (activeAnchor && isOpen) memberRef.current?.scrollIntoView({ block: 'nearest' });
  }, [activeAnchor, isOpen]);

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
      <div ref={rowRef} className={`${ROW} ${isActive ? 'bg-accent-soft text-fg' : ''}`}>
        <ChevronSlot open={isOpen} onToggle={hasContent ? () => setIsOpen(!isOpen) : undefined} />
        <Link
          to={buildModuleUrl(path, selectedSource)}
          onClick={handleModuleClick}
          className={`flex min-w-0 flex-1 items-center gap-2 text-sm ${getItemTypeColor('module')} ${getItemTypeHoverColor('module')}`}
        >
          <FolderIcon />
          <span className="truncate">{name}</span>
        </Link>
      </div>

      {isOpen && (
        <TreeChildren>
          {module.items.length > 0 &&
            documentation &&
            module.items.map((itemPath) => <ItemTree key={itemPath} itemPath={itemPath} />)}

          {module.functions.length > 0 &&
            module.functions.map((func, idx) => {
              const active = activeAnchor === `func-${func.name}`;
              return (
                <Link
                  key={`func-${idx}`}
                  ref={active ? memberRef : undefined}
                  to={`${buildModuleUrl(path, selectedSource)}##func-${func.name}`}
                  className={`${ROW} text-sm ${active ? 'bg-accent-soft text-fg' : `${getItemTypeColor('function')} ${getItemTypeHoverColor('function')}`}`}
                >
                  <ChevronSlot />
                  <FunctionIcon />
                  <span className="truncate">{func.name}</span>
                </Link>
              );
            })}

          {module.extern_values.length > 0 &&
            module.extern_values.map((extVal, idx) => {
              const active = activeAnchor === `extval-${extVal.name}`;
              return (
                <Link
                  key={`ext-${idx}`}
                  ref={active ? memberRef : undefined}
                  to={`${buildModuleUrl(path, selectedSource)}##extval-${extVal.name}`}
                  className={`${ROW} text-sm ${active ? 'bg-accent-soft text-fg' : `${getItemTypeColor('extern')} ${getItemTypeHoverColor('extern')}`}`}
                >
                  <ChevronSlot />
                  <GlobalIcon />
                  <span className="truncate">{extVal.name}</span>
                </Link>
              );
            })}

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
        </TreeChildren>
      )}
    </div>
  );
}

interface SidebarProps {
  onClose: () => void;
}

export function Sidebar({ onClose }: SidebarProps) {
  const { documentation } = useDocumentation();
  const [sidebarWidth, setSidebarWidth] = useState(() => {
    const saved = localStorage.getItem('sidebarWidth');
    return saved ? parseInt(saved, 10) : 256; // Default 256px (w-64)
  });
  const [isResizing, setIsResizing] = useState(false);
  const [isDesktop, setIsDesktop] = useState(() => window.innerWidth >= 1024);
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

  // Handle responsive width changes
  useEffect(() => {
    const handleResize = () => {
      setIsDesktop(window.innerWidth >= 1024);
    };

    window.addEventListener('resize', handleResize);
    return () => window.removeEventListener('resize', handleResize);
  }, []);

  const handleMouseDown = () => {
    setIsResizing(true);
  };

  if (!documentation) {
    return (
      <div className="hidden lg:flex relative" style={{ width: `${sidebarWidth}px` }}>
        <aside className="flex-1 bg-sidebar p-4">
          <div className="text-sm text-fg-subtle">No documentation loaded</div>
        </aside>
        <div
          onMouseDown={handleMouseDown}
          className="hidden lg:block w-1 bg-edge hover:bg-accent cursor-col-resize transition-colors"
        />
      </div>
    );
  }

  return (
    <div
      className="flex relative"
      ref={sidebarRef}
      style={{
        width: isDesktop ? `${sidebarWidth}px` : '100%',
      }}
    >
      <aside className="flex-1 bg-sidebar overflow-y-auto">
        <div
          className="p-2"
          onClick={(e) => {
            // Close sidebar on mobile when clicking nav links
            if (!isDesktop && (e.target as HTMLElement).closest('a')) {
              onClose();
            }
          }}
        >
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

      {/* Desktop resize handle */}
      {isDesktop && (
        <div
          onMouseDown={handleMouseDown}
          className="w-1 bg-edge hover:bg-accent cursor-col-resize transition-colors"
        />
      )}
    </div>
  );
}
