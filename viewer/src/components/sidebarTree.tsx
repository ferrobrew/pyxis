// The sidebar's tree rendering: navigation rows for items, modules, and their
// members. Extracted from Sidebar.tsx to keep every file under the ~400-line
// threshold.

import { useState, useRef, useEffect } from 'react';
import { Link, useLocation } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import type { JsonModule } from '@pyxis/types';
import { buildModuleUrl, buildItemUrl } from '../utils/navigation';
import { getItemTypeColor, getItemTypeHoverColor, type ItemType } from '../utils/colors';
import { cn } from '../utils/styles';
import {
  TypeIcon,
  EnumIcon,
  BitflagsIcon,
  UnionIcon,
  FunctionIcon,
  GlobalIcon,
  FieldIcon,
  FolderIcon,
} from './sidebarIcons';
export const ROW = 'flex items-center gap-1.5 rounded px-1.5 py-1';

export function ChevronSlot({ open = false, onToggle }: { open?: boolean; onToggle?: () => void }) {
  if (!onToggle) return <span className="w-4 shrink-0" aria-hidden="true" />;
  return (
    <button
      type="button"
      onClick={(e) => {
        e.stopPropagation();
        e.preventDefault();
        onToggle();
      }}
      className="
        flex size-4 shrink-0 items-center justify-center rounded-sm
        text-fg-subtle
        hover:text-fg
      "
      aria-label={open ? 'Collapse' : 'Expand'}
    >
      <svg
        className={cn('size-3 transition-transform', open ? 'rotate-90' : '')}
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
export function TreeChildren({ children }: { children: React.ReactNode }) {
  return <div className="ml-[0.85rem] border-l border-edge pl-1">{children}</div>;
}
// A member row (field, function, variant, flag) inside an expanded item in the
// sidebar tree. The active one (matched by the URL anchor) is highlighted and
// takes the ref so it can be scrolled into view.
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
      className={cn(ROW, 'text-xs', active ? 'bg-accent-soft text-fg' : colorClasses)}
    >
      <ChevronSlot />
      {icon}
      <span className="truncate">{label}</span>
    </Link>
  );
}

// Item tree component for types/enums/bitflags with their members
type ItemTreeProps = {
  itemPath: string;
};

export function ItemTree({ itemPath }: ItemTreeProps) {
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
  // When the URL targets a member, expand this item. Done during render
  // (previous-value tracking) rather than in an effect, per the react-hooks
  // set-state-in-effect rule.
  const [prevActiveAnchor, setPrevActiveAnchor] = useState(activeAnchor);
  if (prevActiveAnchor !== activeAnchor) {
    setPrevActiveAnchor(activeAnchor);
    if (activeAnchor) setIsItemOpen(true);
  }

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
  } else if (item.kind.type === 'union') {
    Icon = UnionIcon;
    itemType = 'union';
  } else if (item.kind.type === 'constant') {
    itemType = 'constant';
  } else if (item.kind.type === 'extern_value') {
    Icon = GlobalIcon;
    itemType = 'extern';
  }

  // Get public members for this item. Union members are listed the same way a
  // type's fields are — they're the item's navigable children either way.
  const publicFields =
    item.kind.type === 'type' || item.kind.type === 'union'
      ? item.kind.fields.filter(
          (f): f is typeof f & { name: string } => f.visibility === 'public' && f.name != null
        )
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
  const nestedItems =
    item.kind.type === 'type' ||
    item.kind.type === 'enum' ||
    item.kind.type === 'bitflags' ||
    item.kind.type === 'union'
      ? (item.kind.nested_items ?? [])
      : [];

  const hasMembers =
    publicFields.length > 0 ||
    publicVirtualFunctions.length > 0 ||
    publicAssociatedFunctions.length > 0 ||
    variants.length > 0 ||
    flags.length > 0 ||
    nestedItems.length > 0;

  const handleItemClick = () => {
    if (hasMembers) {
      setIsItemOpen(true);
    }
  };

  return (
    <div>
      <div
        ref={rowRef}
        className={cn(
          ROW,
          isItemActive
            ? `
        bg-accent-soft text-fg
      `
            : ''
        )}
      >
        <ChevronSlot
          open={isItemOpen}
          onToggle={hasMembers ? () => setIsItemOpen(!isItemOpen) : undefined}
        />
        <Link
          to={buildItemUrl(itemPath, selectedSource)}
          onClick={handleItemClick}
          className={cn(
            'flex min-w-0 flex-1 items-center gap-2 text-sm',
            getItemTypeColor(itemType),
            getItemTypeHoverColor(itemType)
          )}
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
              label={field.name}
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

          {nestedItems.map((nestedPath) => (
            <ItemTree key={`nested-${nestedPath}`} itemPath={nestedPath} />
          ))}
        </TreeChildren>
      )}
    </div>
  );
}

type ModuleTreeProps = {
  name: string;
  module: JsonModule;
  path: string;
  level: number;
};

export function ModuleTree({ name, module, path, level }: ModuleTreeProps) {
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
  // The expansion is adjusted during render (previous-value tracking) rather
  // than in an effect, per the react-hooks set-state-in-effect rule.
  const [prevIsAncestor, setPrevIsAncestor] = useState(isAncestor);
  if (prevIsAncestor !== isAncestor) {
    setPrevIsAncestor(isAncestor);
    if (isAncestor) setIsOpen(true);
  }
  useEffect(() => {
    if (isActive) {
      requestAnimationFrame(() => rowRef.current?.scrollIntoView({ block: 'nearest' }));
    }
  }, [isActive, selectedSource]);
  useEffect(() => {
    if (activeAnchor && isOpen) memberRef.current?.scrollIntoView({ block: 'nearest' });
  }, [activeAnchor, isOpen]);

  const hasSubmodules = module.submodules && Object.keys(module.submodules).length > 0;
  const hasContent = hasSubmodules || module.items.length > 0 || module.functions.length > 0;

  const handleModuleClick = () => {
    if (hasContent) {
      setIsOpen(true);
    }
  };

  return (
    <div>
      <div
        ref={rowRef}
        className={cn(
          ROW,
          isActive
            ? `
        bg-accent-soft text-fg
      `
            : ''
        )}
      >
        <ChevronSlot open={isOpen} onToggle={hasContent ? () => setIsOpen(!isOpen) : undefined} />
        <Link
          to={buildModuleUrl(path, selectedSource)}
          onClick={handleModuleClick}
          className={cn(
            'flex min-w-0 flex-1 items-center gap-2 text-sm',
            getItemTypeColor('module'),
            getItemTypeHoverColor('module')
          )}
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
                  className={cn(
                    ROW,
                    'text-sm',
                    active
                      ? 'bg-accent-soft text-fg'
                      : [getItemTypeColor('function'), getItemTypeHoverColor('function')]
                  )}
                >
                  <ChevronSlot />
                  <FunctionIcon />
                  <span className="truncate">{func.name}</span>
                </Link>
              );
            })}

          {hasSubmodules &&
            Object.entries(module.submodules).map(([subName, subModule]) => (
              <ModuleTree
                key={subName}
                name={subName}
                module={subModule}
                path={path ? `${path}::${subName}` : subName}
                level={level + 1}
              />
            ))}
        </TreeChildren>
      )}
    </div>
  );
}
