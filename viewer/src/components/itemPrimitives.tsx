// Small presentational primitives shared by the item definition views and the
// main item page. Extracted from ItemView.tsx to keep every file under the
// ~400-line threshold.

import type { JsonDocLink, JsonItem } from '@pyxis/types';

import { cn } from '../utils/styles';
import { Markdown } from './Markdown';
import { AnchorLink } from './Actions';
import type { ItemType } from '../utils/colors';

export function itemTypeOfKind(kind: JsonItem['kind']['type']): ItemType {
  switch (kind) {
    case 'enum':
      return 'enum';
    case 'bitflags':
      return 'bitflags';
    case 'union':
      return 'union';
    case 'type_alias':
      return 'type_alias';
    case 'constant':
      return 'constant';
    case 'extern_value':
      return 'extern';
    default:
      return 'type';
  }
}

// Quiet, typographic doc block. Spacing is owned by the header group, so this
// carries no margin of its own.
export function DocBlock({ doc, docLinks }: { doc: string; docLinks?: JsonDocLink[] }) {
  return (
    <div className="text-fg-muted">
      <Markdown docLinks={docLinks}>{doc}</Markdown>
    </div>
  );
}

export function SectionHeader({
  anchor,
  children,
}: {
  anchor?: string;
  children: React.ReactNode;
}) {
  return (
    <h2
      className="
      group mb-4 flex items-center gap-2 border-b border-edge pb-1.5 text-lg
      font-semibold text-fg
    "
    >
      {children}
      {anchor && (
        <AnchorLink
          targetId={anchor}
          className="
        opacity-0
        group-hover:opacity-100
      "
        />
      )}
    </h2>
  );
}

// Shared table chrome
const TH = 'px-4 py-2 text-left text-xs font-semibold uppercase tracking-wide text-fg-muted';
const TD = 'px-4 py-2 text-sm';

export { TH, TD };

export function Table({ children }: { children: React.ReactNode }) {
  return (
    <div className="overflow-x-auto rounded-md border border-edge">
      <table className="w-full border-collapse">{children}</table>
    </div>
  );
}

export type FieldViewMode = 'flat' | 'nested' | 'source';

export const FIELD_VIEW_MODES: { mode: FieldViewMode; label: string }[] = [
  { mode: 'flat', label: 'Flat' },
  { mode: 'nested', label: 'Nested' },
  { mode: 'source', label: 'Source' },
];

// Union members aren't laid out in sequence, so the nested/absolute-offset
// view has nothing to say about them.
export const UNION_VIEW_MODES = FIELD_VIEW_MODES.filter((m) => m.mode !== 'nested');

export function ViewModeToggle({
  mode,
  onModeChange,
  modes = FIELD_VIEW_MODES,
}: {
  mode: FieldViewMode;
  onModeChange: (mode: FieldViewMode) => void;
  modes?: { mode: FieldViewMode; label: string }[];
}) {
  const base =
    'px-3 py-1 text-xs font-medium transition-colors focus:outline-none focus:ring-2 focus:ring-accent';
  const active = 'bg-accent text-white';
  const inactive = 'bg-surface text-fg-muted hover:bg-surface-2 hover:text-fg';

  return (
    <div className="inline-flex overflow-hidden rounded-md border border-edge">
      {modes.map(({ mode: m, label }) => (
        <button
          key={m}
          onClick={() => onModeChange(m)}
          className={cn(base, mode === m ? active : inactive)}
          aria-pressed={mode === m}
        >
          {label}
        </button>
      ))}
    </div>
  );
}

// A panel that wraps a list of function rows.
export function FunctionList({ children }: { children: React.ReactNode }) {
  return (
    <div
      className="
    overflow-hidden rounded-md border border-edge bg-surface
  "
    >
      {children}
    </div>
  );
}
