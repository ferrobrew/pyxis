import { useEffect, useState } from 'react';
import { Link, useParams, useNavigate } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { getModulePath, findLongestValidAncestor, findModule } from '../utils/pathUtils';
import { buildModuleUrl, buildItemUrl, buildRootUrl } from '../utils/navigation';
import { getItemTypeColor, type ItemType } from '../utils/colors';
import { TypeRef } from './TypeRef';
import { SmallBadge } from './Badge';
import { ItemAttributes } from './Attributes';
import { FunctionDisplay } from './FunctionDisplay';
import { FieldTable } from './FieldTable';
import { NestedFieldView } from './NestedFieldView';
import { FieldSourceView } from './FieldSourceView';
import { Breadcrumbs } from './Breadcrumbs';
import { SourceLink, SourceName } from './SourceLink';
import { Markdown } from './Markdown';
import { AnchorLink, CopyButton } from './Actions';
import { OnThisPage, type TocEntry } from './OnThisPage';
import { TypeBackendCode } from './BackendSpliceSection';
import type {
  JsonTypeDefinition,
  JsonEnumDefinition,
  JsonBitflagsDefinition,
  JsonDocLink,
  JsonBackend,
  JsonItem,
} from '@pyxis/types';

// Keyword shown in the signature header for each item kind, mirroring how the
// declaration is written in pyxis-defs (`pub type Foo`, `pub enum Bar: u32`).
const KIND_KEYWORD: Record<string, string> = {
  type: 'type',
  enum: 'enum',
  bitflags: 'bitflags',
  type_alias: 'type',
  constant: 'const',
};

// Quiet, typographic doc block. Spacing is owned by the header group, so this
// carries no margin of its own.
function DocBlock({ doc, docLinks }: { doc: string; docLinks?: JsonDocLink[] }) {
  return (
    <div className="text-fg-muted">
      <Markdown docLinks={docLinks}>{doc}</Markdown>
    </div>
  );
}

function SectionHeader({ anchor, children }: { anchor?: string; children: React.ReactNode }) {
  return (
    <h2 className="group mb-4 flex items-center gap-2 border-b border-edge pb-1.5 text-lg font-semibold text-fg">
      {children}
      {anchor && <AnchorLink targetId={anchor} className="opacity-0 group-hover:opacity-100" />}
    </h2>
  );
}

// Shared table chrome
const TH = 'px-4 py-2 text-left text-xs font-semibold uppercase tracking-wide text-fg-muted';
const TD = 'px-4 py-2 text-sm';

function Table({ children }: { children: React.ReactNode }) {
  return (
    <div className="overflow-x-auto rounded-md border border-edge">
      <table className="w-full border-collapse">{children}</table>
    </div>
  );
}

type FieldViewMode = 'flat' | 'nested' | 'source';

const FIELD_VIEW_MODES: { mode: FieldViewMode; label: string }[] = [
  { mode: 'flat', label: 'Flat' },
  { mode: 'nested', label: 'Nested' },
  { mode: 'source', label: 'Source' },
];

function ViewModeToggle({
  mode,
  onModeChange,
}: {
  mode: FieldViewMode;
  onModeChange: (mode: FieldViewMode) => void;
}) {
  const base =
    'px-3 py-1 text-xs font-medium transition-colors focus:outline-none focus:ring-2 focus:ring-accent';
  const active = 'bg-accent text-white';
  const inactive = 'bg-surface text-fg-muted hover:bg-surface-2 hover:text-fg';

  return (
    <div className="inline-flex overflow-hidden rounded-md border border-edge">
      {FIELD_VIEW_MODES.map(({ mode: m, label }) => (
        <button
          key={m}
          onClick={() => onModeChange(m)}
          className={`${base} ${mode === m ? active : inactive}`}
          aria-pressed={mode === m}
        >
          {label}
        </button>
      ))}
    </div>
  );
}

// A panel that wraps a list of function rows.
function FunctionList({ children }: { children: React.ReactNode }) {
  return <div className="overflow-hidden rounded-md border border-edge bg-surface">{children}</div>;
}

// Type view component
// Render an enum-value reference (e.g., `Color::Red`) as clickable links.
// `Color` links to the enum type, `Red` is shown as the variant name.
function EnumValueRef({ path, modulePath }: { path: string; modulePath: string }) {
  const { selectedSource, documentation } = useDocumentation();
  const segments = path.split('::');
  if (segments.length < 2) {
    return <span className="text-fg">{path}</span>;
  }
  const enumName = segments[0];
  const variantName = segments.slice(1).join('::');

  // Resolve the enum path relative to the current module
  // Try: modulePath::enumName, parent::enumName, then just enumName
  const candidatePaths = [
    `${modulePath}::${enumName}`,
    enumName,
    modulePath.split('::').slice(0, -1).concat([enumName]).join('::'),
  ];
  const resolvedEnumPath = candidatePaths.find((p) => documentation?.items[p]);

  return (
    <span className="font-mono text-fg">
      {resolvedEnumPath ? (
        <Link
          to={buildItemUrl(resolvedEnumPath, selectedSource)}
          className="text-kind-module hover:underline"
        >
          {enumName}
        </Link>
      ) : (
        <span>{enumName}</span>
      )}
      <span className="text-fg-muted">::</span>
      {resolvedEnumPath ? (
        <Link
          to={`${buildItemUrl(resolvedEnumPath, selectedSource)}#variant-${variantName}`}
          className="text-kind-enum-variant hover:underline"
        >
          {variantName}
        </Link>
      ) : (
        <span>{variantName}</span>
      )}
    </span>
  );
}

function NestedItemsList({ nestedItems }: { nestedItems: { path: string; item: JsonItem | undefined }[] }) {
  const { selectedSource } = useDocumentation();
  const validItems = nestedItems.filter((ni) => ni.item != null);

  if (validItems.length === 0) return null;

  return (
    <div id="nested-items" className="mb-8">
      <SectionHeader anchor="nested-items">Nested Items</SectionHeader>
      <div className="overflow-hidden rounded-md border border-edge bg-surface">
        {validItems.map(({ path, item }) => {
          const name = path.split('::').pop() || path;
          let itemType: ItemType = 'type';
          if (item!.kind.type === 'enum') itemType = 'enum';
          else if (item!.kind.type === 'bitflags') itemType = 'bitflags';
          else if (item!.kind.type === 'type_alias') itemType = 'type_alias';
          else if (item!.kind.type === 'constant') itemType = 'constant';
          return (
            <Link
              key={path}
              to={buildItemUrl(path, selectedSource)}
              className="flex items-center gap-2 border-b border-edge px-4 py-2 text-sm last:border-b-0 hover:bg-accent-soft"
            >
              <span className={`${getItemTypeColor(itemType)}`}>{name}</span>
              <span className="text-fg-subtle text-xs">{itemType}</span>
            </Link>
          );
        })}
      </div>
    </div>
  );
}

function TypeView({ def, modulePath }: { def: JsonTypeDefinition; modulePath: string }) {
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
          <div className="group mb-4 flex items-center justify-between border-b border-edge pb-1.5">
            <h2 className="flex items-center gap-2 text-lg font-semibold text-fg">
              Fields
              <AnchorLink targetId="fields" className="opacity-0 group-hover:opacity-100" />
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
function EnumView({ def, modulePath }: { def: JsonEnumDefinition; modulePath: string }) {
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
                className="border-b border-edge last:border-0"
              >
                <td className={`${TD} font-mono text-fg`}>
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
                <td className={`${TD} font-mono text-fg-muted`}>{variant.value}</td>
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
function BitflagsView({ def }: { def: JsonBitflagsDefinition }) {
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
              <tr key={idx} id={`flag-${flag.name}`} className="border-b border-edge last:border-0">
                <td className={`${TD} font-mono text-fg`}>
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
                <td className={`${TD} font-mono text-fg-muted`}>{flag.value}</td>
                <td className={`${TD} font-mono text-fg-muted`}>0x{flag.value.toString(16)}</td>
                <td className={`${TD} font-mono text-fg-muted`}>
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

// Main ItemView component
export function ItemView() {
  const { itemPath = '' } = useParams();
  const { documentation, selectedSource } = useDocumentation();
  const navigate = useNavigate();

  useEffect(() => {
    if (documentation && itemPath) {
      const decodedPath = decodeURIComponent(itemPath);
      const item = documentation.items[decodedPath];

      if (!item) {
        // Item doesn't exist, find the longest valid ancestor
        const ancestorPath = findLongestValidAncestor(decodedPath, documentation, true);

        if (ancestorPath === null || ancestorPath === '') {
          // No valid ancestor found, navigate to root with current source
          navigate(buildRootUrl(selectedSource), { replace: true });
        } else if (ancestorPath === decodedPath) {
          // The item itself exists (shouldn't happen here, but handle it)
          return;
        } else {
          // Check if ancestor is an item or a module
          if (documentation.items[ancestorPath]) {
            // Ancestor is an item, navigate to it
            navigate(buildItemUrl(ancestorPath, selectedSource), { replace: true });
          } else {
            // Ancestor is a module, navigate to it
            navigate(buildModuleUrl(ancestorPath, selectedSource), { replace: true });
          }
        }
      }
    }
  }, [documentation, itemPath, navigate, selectedSource]);

  if (!documentation) {
    return (
      <div className="p-8">
        <div className="text-fg-subtle">Please load a documentation file to begin.</div>
      </div>
    );
  }

  const decodedPath = decodeURIComponent(itemPath);
  const item = documentation.items[decodedPath];

  if (!item) {
    // Return null while redirecting
    return null;
  }

  const modulePath = getModulePath(decodedPath);

  // Backend splices tagged `for <Type>` are stored on the enclosing module;
  // pull them here so the type page can render its own prologue/epilogue.
  const moduleBackends =
    (findModule(documentation.modules, modulePath) as { backends?: { [key: string]: unknown } } | null)
      ?.backends ?? {};
  const hasBackendProvided = Object.values(moduleBackends).some((configs) =>
    (configs as JsonBackend[]).some((c) =>
      (['prologue', 'epilogue'] as const).some((slot) => {
        const s = c[slot];
        return s && s.for_type === decodedPath && (s.header || s.definition);
      })
    )
  );

  // Determine item type for color coding
  let itemType: ItemType = 'type';
  if (item.kind.type === 'enum') itemType = 'enum';
  else if (item.kind.type === 'bitflags') itemType = 'bitflags';
  else if (item.kind.type === 'type_alias') itemType = 'type_alias';
  else if (item.kind.type === 'constant') itemType = 'constant';

  const name = decodedPath.split('::').pop() || decodedPath;
  const isExtern = item.category === 'extern';
  const keyword = isExtern ? 'extern type' : (KIND_KEYWORD[item.kind.type] ?? item.kind.type);
  const isPublic = item.visibility === 'public';
  const typeParams = item.type_parameters ?? [];
  const underlying =
    item.kind.type === 'enum' || item.kind.type === 'bitflags' ? item.kind.underlying_type : null;
  const aliasTarget = item.kind.type === 'type_alias' ? item.kind.target : null;
  const constValue = item.kind.type === 'constant' ? item.kind.value : null;
  const constValueType = item.kind.type === 'constant' ? item.kind.value_type : null;
  const singleton = item.kind.type !== 'type_alias' && item.kind.type !== 'constant' ? item.kind.singleton : null;

  const toc: TocEntry[] = [];
  const k = item.kind;
  if (k.type === 'type') {
    if ((k.nested_items ?? []).length > 0) toc.push({ id: 'nested-items', label: 'Nested Items' });
    if (k.fields.length > 0) toc.push({ id: 'fields', label: 'Fields' });
    if (k.vftable && k.vftable.functions.length > 0)
      toc.push({ id: 'virtual-functions', label: 'Virtual Functions' });
    if (k.associated_functions.length > 0)
      toc.push({ id: 'associated-functions', label: 'Associated Functions' });
  } else if (k.type === 'enum') {
    if ((k.nested_items ?? []).length > 0) toc.push({ id: 'nested-items', label: 'Nested Items' });
    toc.push({ id: 'variants', label: 'Variants' });
    if (k.associated_functions.length > 0)
      toc.push({ id: 'associated-functions', label: 'Associated Functions' });
  } else if (k.type === 'bitflags') {
    if ((k.nested_items ?? []).length > 0) toc.push({ id: 'nested-items', label: 'Nested Items' });
    toc.push({ id: 'flags', label: 'Flags' });
  }
  if (hasBackendProvided) toc.push({ id: 'backend-provided', label: 'Backend-provided' });

  return (
    <div className="mx-auto flex max-w-6xl gap-8 px-4 py-6 md:px-8 lg:px-10">
      <article className="min-w-0 flex-1">
        <div className="mb-4">
          <Breadcrumbs path={decodedPath} isItem={true} itemType={itemType} />
        </div>

        {/* Source-faithful declaration header: attributes, signature, the source
            link, then the doc comment, kept together with a single margin. */}
        <div className="group mb-4">
          <ItemAttributes item={item} />
          <div className="flex flex-wrap items-baseline justify-between gap-x-4 gap-y-1">
            <div className="flex items-start gap-2">
              <h1 className="font-mono text-2xl font-semibold tracking-tight">
                {isPublic && <span className="text-fg-muted">pub </span>}
                <span className={getItemTypeColor(itemType)}>{keyword}</span>{' '}
                <span className="text-fg">{name}</span>
                {typeParams.length > 0 && (
                  <span className="text-kind-enum">&lt;{typeParams.join(', ')}&gt;</span>
                )}
                {underlying && (
                  <>
                    <span className="text-fg-muted">: </span>
                    <TypeRef type={underlying} currentModule={modulePath} />
                  </>
                )}
                {aliasTarget && (
                  <>
                    <span className="text-fg-muted"> = </span>
                    <TypeRef type={aliasTarget} currentModule={modulePath} />
                    <span className="text-fg-muted">;</span>
                  </>
                )}
                {constValueType && (
                  <>
                    <span className="text-fg-muted">: </span>
                    <TypeRef type={constValueType} currentModule={modulePath} />
                    <span className="text-fg-muted"> = </span>
                    {constValue?.kind === 'int' && (
                      <span className="text-fg">{constValue.value}</span>
                    )}
                    {constValue?.kind === 'float' && (
                      <span className="text-fg">{constValue.value}</span>
                    )}
                    {constValue?.kind === 'string' && (
                      <span className="text-fg">"{constValue.value}"</span>
                    )}
                    {constValue?.kind === 'enum_value' && (
                      <EnumValueRef path={constValue.path} modulePath={modulePath} />
                    )}
                    <span className="text-fg-muted">;</span>
                  </>
                )}
              </h1>
              {singleton != null && (
                <CopyButton
                  value={`0x${singleton.toString(16)}`}
                  title="Copy singleton address"
                  label="copy addr"
                  className="mt-1 opacity-0 group-hover:opacity-100"
                />
              )}
            </div>
            {item.source && <SourceLink source={item.source} />}
          </div>
          {item.kind.doc && (
            <div className="mt-2">
              <DocBlock doc={item.kind.doc} docLinks={item.kind.doc_links} />
            </div>
          )}
        </div>

        {item.kind.type === 'type' && <TypeView def={item.kind} modulePath={modulePath} />}
        {item.kind.type === 'enum' && <EnumView def={item.kind} modulePath={modulePath} />}
        {item.kind.type === 'bitflags' && <BitflagsView def={item.kind} />}
        {hasBackendProvided && (
          <TypeBackendCode backends={moduleBackends} itemPath={decodedPath} />
        )}
      </article>

      <OnThisPage entries={toc} />
    </div>
  );
}
