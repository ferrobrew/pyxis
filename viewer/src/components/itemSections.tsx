// Item-kind definition views (type/enum/bitflags/union bodies and the nested
// items list). Extracted from ItemView.tsx to keep every file under the
// ~400-line threshold.

import type {
  JsonBitflagsDefinition,
  JsonEnumDefinition,
  JsonItem,
  JsonTypeDefinition,
  JsonUnionDefinition,
} from '@pyxis/types';

import { Link } from 'react-router-dom';
import { useState } from 'react';
import { useDocumentation } from '../contexts/DocumentationContext';
import { buildItemUrl } from '../utils/navigation';
import { cn } from '../utils/styles';
import { getItemTypeColor } from '../utils/colors';
import { SmallBadge } from './Badge';
import { Markdown } from './Markdown';
import { FieldTable } from './FieldTable';
import { NestedFieldView } from './NestedFieldView';
import { FieldSourceView } from './FieldSourceView';
import { AnchorLink } from './Actions';
import { FunctionDisplay } from './FunctionDisplay';
import { SourceName } from './SourceLink';
import {
  itemTypeOfKind,
  SectionHeader,
  Table,
  ViewModeToggle,
  FunctionList,
  TH,
  TD,
  UNION_VIEW_MODES,
  type FieldViewMode,
} from './itemPrimitives';

export function NestedItemsList({
  nestedItems,
}: {
  nestedItems: { path: string; item: JsonItem | undefined }[];
}) {
  const { selectedSource } = useDocumentation();
  const validItems = nestedItems.filter(
    (ni): ni is { path: string; item: JsonItem } => ni.item != null
  );

  if (validItems.length === 0) return null;

  return (
    <div id="nested-items" className="mb-8">
      <SectionHeader anchor="nested-items">Nested Items</SectionHeader>
      <div className="overflow-hidden rounded-md border border-edge bg-surface">
        {validItems.map(({ path, item }) => {
          const name = path.split('::').pop() || path;
          const itemType = itemTypeOfKind(item.kind.type);
          return (
            <Link
              key={path}
              to={buildItemUrl(path, selectedSource)}
              className="
                flex items-center gap-2 border-b border-edge px-4 py-2 text-sm
                last:border-b-0
                hover:bg-accent-soft
              "
            >
              <span className={cn(getItemTypeColor(itemType))}>{name}</span>
              <span className="text-xs text-fg-subtle">{itemType}</span>
            </Link>
          );
        })}
      </div>
    </div>
  );
}

export function TypeView({ def, modulePath }: { def: JsonTypeDefinition; modulePath: string }) {
  const [fieldViewMode, setFieldViewMode] = useState<FieldViewMode>('flat');
  const { documentation } = useDocumentation();

  const nestedItems = (def.nested_items ?? []).map((path) => ({
    path,
    item: documentation?.items[path],
  }));

  return (
    <div>
      <NestedItemsList nestedItems={nestedItems} />

      {def.fields.length > 0 && (
        <div id="fields" className="mb-8">
          <div
            className="
            group mb-4 flex items-center justify-between border-b border-edge
            pb-1.5
          "
          >
            <h2
              className="
              flex items-center gap-2 text-lg font-semibold text-fg
            "
            >
              Fields
              <AnchorLink
                targetId="fields"
                className="
                opacity-0
                group-hover:opacity-100
              "
              />
            </h2>
            <ViewModeToggle mode={fieldViewMode} onModeChange={setFieldViewMode} />
          </div>
          {fieldViewMode === 'flat' && <FieldTable fields={def.fields} modulePath={modulePath} />}
          {fieldViewMode === 'nested' && (
            <NestedFieldView fields={def.fields} modulePath={modulePath} />
          )}
          {fieldViewMode === 'source' && (
            <FieldSourceView fields={def.fields} modulePath={modulePath} />
          )}
        </div>
      )}

      {def.vftable && def.vftable.functions.length > 0 && (
        <div id="virtual-functions" className="mb-8">
          <SectionHeader anchor="virtual-functions">Virtual Functions</SectionHeader>
          <FunctionList>
            {def.vftable.functions.map((func, idx) => (
              <FunctionDisplay
                key={idx}
                id={`vfunc-${func.name}`}
                func={func}
                modulePath={modulePath}
              />
            ))}
          </FunctionList>
        </div>
      )}

      {def.associated_functions.length > 0 && (
        <div id="associated-functions" className="mb-8">
          <SectionHeader anchor="associated-functions">Associated Functions</SectionHeader>
          <FunctionList>
            {def.associated_functions.map((func, idx) => (
              <FunctionDisplay
                key={idx}
                id={`func-${func.name}`}
                func={func}
                modulePath={modulePath}
              />
            ))}
          </FunctionList>
        </div>
      )}
    </div>
  );
}

// Enum view component
export function EnumView({ def, modulePath }: { def: JsonEnumDefinition; modulePath: string }) {
  const { documentation } = useDocumentation();
  const nestedItems = (def.nested_items ?? []).map((path) => ({
    path,
    item: documentation?.items[path],
  }));

  return (
    <div>
      <NestedItemsList nestedItems={nestedItems} />
      <div id="variants" className="mb-8">
        <SectionHeader anchor="variants">Variants</SectionHeader>
        <Table>
          <thead className="bg-surface">
            <tr className="border-b border-edge">
              <th className={TH}>Name</th>
              <th className={TH}>Value</th>
            </tr>
          </thead>
          <tbody>
            {def.variants.map((variant, idx) => (
              <tr
                key={idx}
                id={`variant-${variant.name}`}
                className="
                  border-b border-edge
                  last:border-0
                "
              >
                <td className={cn(TD, 'font-mono text-fg')}>
                  {variant.source ? (
                    <SourceName source={variant.source}>{variant.name}</SourceName>
                  ) : (
                    variant.name
                  )}
                  {def.default === idx && (
                    <SmallBadge variant="purple" className="ml-2">
                      default
                    </SmallBadge>
                  )}
                  {variant.doc && (
                    <div className="mt-1 font-sans text-xs text-fg-muted">
                      <Markdown docLinks={variant.doc_links}>{variant.doc}</Markdown>
                    </div>
                  )}
                </td>
                <td className={cn(TD, 'font-mono text-fg-muted')}>{variant.value}</td>
              </tr>
            ))}
          </tbody>
        </Table>
      </div>

      {def.associated_functions.length > 0 && (
        <div id="associated-functions" className="mb-8">
          <SectionHeader anchor="associated-functions">Associated Functions</SectionHeader>
          <FunctionList>
            {def.associated_functions.map((func, idx) => (
              <FunctionDisplay
                key={idx}
                id={`func-${func.name}`}
                func={func}
                modulePath={modulePath}
              />
            ))}
          </FunctionList>
        </div>
      )}
    </div>
  );
}

// Bitflags view component
export function BitflagsView({ def }: { def: JsonBitflagsDefinition }) {
  const { documentation } = useDocumentation();
  const nestedItems = (def.nested_items ?? []).map((path) => ({
    path,
    item: documentation?.items[path],
  }));

  return (
    <div>
      <NestedItemsList nestedItems={nestedItems} />
      <div id="flags" className="mb-8">
        <SectionHeader anchor="flags">Flags</SectionHeader>
        <Table>
          <thead className="bg-surface">
            <tr className="border-b border-edge">
              <th className={TH}>Name</th>
              <th className={TH}>Value (Dec)</th>
              <th className={TH}>Value (Hex)</th>
              <th className={TH}>Value (Bin)</th>
            </tr>
          </thead>
          <tbody>
            {def.flags.map((flag, idx) => (
              <tr
                key={idx}
                id={`flag-${flag.name}`}
                className="
                border-b border-edge
                last:border-0
              "
              >
                <td className={cn(TD, 'font-mono text-fg')}>
                  {flag.source ? (
                    <SourceName source={flag.source}>{flag.name}</SourceName>
                  ) : (
                    flag.name
                  )}
                  {def.default === idx && (
                    <SmallBadge variant="purple" className="ml-2">
                      default
                    </SmallBadge>
                  )}
                  {flag.doc && (
                    <div className="mt-1 font-sans text-xs text-fg-muted">
                      <Markdown docLinks={flag.doc_links}>{flag.doc}</Markdown>
                    </div>
                  )}
                </td>
                <td className={cn(TD, 'font-mono text-fg-muted')}>{flag.value}</td>
                <td className={cn(TD, 'font-mono text-fg-muted')}>0x{flag.value.toString(16)}</td>
                <td className={cn(TD, 'font-mono text-fg-muted')}>
                  0b{flag.value.toString(2).padStart(8, '0')}
                </td>
              </tr>
            ))}
          </tbody>
        </Table>
      </div>
    </div>
  );
}

// Union view component. A union's members are competing readings of the same
// bytes: every one starts at offset 0, and only one applies at a time — which
// one is a property of the surrounding data, not of the union. The offset
// column is therefore dropped, and the note below says why once rather than
// repeating `0x0` down a column.
export function UnionView({ def, modulePath }: { def: JsonUnionDefinition; modulePath: string }) {
  const [memberViewMode, setMemberViewMode] = useState<FieldViewMode>('flat');
  const { documentation } = useDocumentation();

  const nestedItems = (def.nested_items ?? []).map((path) => ({
    path,
    item: documentation?.items[path],
  }));

  return (
    <div>
      <NestedItemsList nestedItems={nestedItems} />

      {def.fields.length > 0 && (
        <div id="members" className="mb-8">
          <div
            className="
            group mb-4 flex items-center justify-between border-b border-edge
            pb-1.5
          "
          >
            <h2
              className="
              flex items-center gap-2 text-lg font-semibold text-fg
            "
            >
              Members
              <AnchorLink
                targetId="members"
                className="
                opacity-0
                group-hover:opacity-100
              "
              />
            </h2>
            <ViewModeToggle
              mode={memberViewMode}
              onModeChange={setMemberViewMode}
              modes={UNION_VIEW_MODES}
            />
          </div>
          <p className="mb-3 text-sm text-fg-muted">
            All {def.fields.length} members overlap at offset{' '}
            <span className="font-mono text-fg">0x0</span> — they are alternative readings of the
            same {def.size} bytes, and only one applies at a time.
          </p>
          {memberViewMode === 'flat' && (
            <FieldTable fields={def.fields} modulePath={modulePath} showOffsets={false} />
          )}
          {memberViewMode === 'source' && (
            <FieldSourceView fields={def.fields} modulePath={modulePath} layout="overlaid" />
          )}
        </div>
      )}
    </div>
  );
}

// Main ItemView component
