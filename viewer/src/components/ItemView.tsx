import { useEffect, useState } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { getModulePath, findLongestValidAncestor } from '../utils/pathUtils';
import { buildModuleUrl, buildItemUrl, buildRootUrl } from '../utils/navigation';
import { getItemTypeColor, type ItemType } from '../utils/colors';
import { TypeRef } from './TypeRef';
import { Badge, SmallBadge } from './Badge';
import { FunctionDisplay } from './FunctionDisplay';
import { FieldTable } from './FieldTable';
import { NestedFieldView } from './NestedFieldView';
import { Breadcrumbs } from './Breadcrumbs';
import { SourceLink, SourceName } from './SourceLink';
import { formatCfg } from '../utils/cfg';
import { Markdown } from './Markdown';
import type {
  JsonTypeDefinition,
  JsonEnumDefinition,
  JsonBitflagsDefinition,
  JsonTypeAliasDefinition,
} from '@pyxis/types';

// Rust-flavoured keyword shown in the signature header for each item kind.
const KIND_KEYWORD: Record<string, string> = {
  type: 'struct',
  enum: 'enum',
  bitflags: 'bitflags',
  type_alias: 'type',
};

// Quiet, typographic doc block — a left rule rather than a loud tinted box.
function DocBlock({ doc }: { doc: string }) {
  return (
    <div className="mb-6 border-l-2 border-edge-strong pl-4 text-fg-muted">
      <Markdown>{doc}</Markdown>
    </div>
  );
}

function SectionHeader({ id, children }: { id?: string; children: React.ReactNode }) {
  return (
    <h2 id={id} className="mb-4 border-b border-edge pb-1.5 text-lg font-semibold text-fg">
      {children}
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

type FieldViewMode = 'flat' | 'nested';

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
      <button
        onClick={() => onModeChange('flat')}
        className={`${base} ${mode === 'flat' ? active : inactive}`}
        aria-pressed={mode === 'flat'}
      >
        Flat
      </button>
      <button
        onClick={() => onModeChange('nested')}
        className={`${base} ${mode === 'nested' ? active : inactive}`}
        aria-pressed={mode === 'nested'}
      >
        Nested
      </button>
    </div>
  );
}

// A panel that wraps a list of function rows.
function FunctionList({ children }: { children: React.ReactNode }) {
  return <div className="overflow-hidden rounded-md border border-edge bg-surface">{children}</div>;
}

// Type view component
function TypeView({ def, modulePath }: { def: JsonTypeDefinition; modulePath: string }) {
  const [fieldViewMode, setFieldViewMode] = useState<FieldViewMode>('flat');

  return (
    <div>
      {def.doc && <DocBlock doc={def.doc} />}

      {def.fields.length > 0 && (
        <div id="fields" className="mb-8">
          <div className="mb-4 flex items-center justify-between border-b border-edge pb-1.5">
            <h2 className="text-lg font-semibold text-fg">Fields</h2>
            <ViewModeToggle mode={fieldViewMode} onModeChange={setFieldViewMode} />
          </div>
          {fieldViewMode === 'flat' ? (
            <FieldTable fields={def.fields} modulePath={modulePath} />
          ) : (
            <NestedFieldView fields={def.fields} modulePath={modulePath} />
          )}
        </div>
      )}

      {def.vftable && def.vftable.functions.length > 0 && (
        <div id="virtual-functions" className="mb-8">
          <SectionHeader>Virtual Functions</SectionHeader>
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
          <SectionHeader>Associated Functions</SectionHeader>
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
  return (
    <div>
      {def.doc && <DocBlock doc={def.doc} />}

      <div className="mb-6 text-sm text-fg-muted">
        Underlying type: <TypeRef type={def.underlying_type} currentModule={modulePath} />
      </div>

      <div id="variants" className="mb-8">
        <SectionHeader>Variants</SectionHeader>
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
                </td>
                <td className={`${TD} font-mono text-fg-muted`}>{variant.value}</td>
              </tr>
            ))}
          </tbody>
        </Table>
      </div>

      {def.associated_functions.length > 0 && (
        <div id="associated-functions" className="mb-8">
          <SectionHeader>Associated Functions</SectionHeader>
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
function BitflagsView({ def, modulePath }: { def: JsonBitflagsDefinition; modulePath: string }) {
  return (
    <div>
      {def.doc && <DocBlock doc={def.doc} />}

      <div className="mb-6 text-sm text-fg-muted">
        Underlying type: <TypeRef type={def.underlying_type} currentModule={modulePath} />
      </div>

      <div id="flags" className="mb-8">
        <SectionHeader>Flags</SectionHeader>
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

// Type alias view component
function TypeAliasView({ def, modulePath }: { def: JsonTypeAliasDefinition; modulePath: string }) {
  return (
    <div>
      {def.doc && <DocBlock doc={def.doc} />}

      <div className="mb-8">
        <SectionHeader>Target Type</SectionHeader>
        <div className="rounded-md border border-edge bg-surface p-4">
          <span className="font-mono text-base">
            <TypeRef type={def.target} currentModule={modulePath} />
          </span>
        </div>
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

  // Determine item type for color coding
  let itemType: ItemType = 'type';
  if (item.kind.type === 'enum') itemType = 'enum';
  else if (item.kind.type === 'bitflags') itemType = 'bitflags';
  else if (item.kind.type === 'type_alias') itemType = 'type_alias';

  const name = decodedPath.split('::').pop() || decodedPath;
  const keyword = KIND_KEYWORD[item.kind.type] ?? item.kind.type;

  // Extract trait badges from kind
  const kind = item.kind;
  const copyable = kind.type !== 'type_alias' && kind.copyable;
  const cloneable = kind.type !== 'type_alias' && kind.cloneable;
  const defaultable = kind.type === 'type' && kind.defaultable;
  const packed = kind.type === 'type' && kind.packed;
  const singleton = kind.type !== 'type_alias' ? kind.singleton : null;

  return (
    <div className="mx-auto max-w-5xl px-4 py-6 md:px-8 lg:px-10">
      <div className="mb-4 flex items-center gap-3">
        <Breadcrumbs path={decodedPath} isItem={true} itemType={itemType} />
        {item.source && <SourceLink source={item.source} />}
      </div>

      {/* rustdoc-style signature header */}
      <h1 className="mb-3 font-mono text-2xl font-semibold tracking-tight">
        <span className={getItemTypeColor(itemType)}>{keyword}</span>{' '}
        <span className="text-fg">{name}</span>
        {item.type_parameters && item.type_parameters.length > 0 && (
          <span className="text-kind-enum">&lt;{item.type_parameters.join(', ')}&gt;</span>
        )}
      </h1>

      {/* Quiet metadata row */}
      <div className="mb-8 flex flex-wrap items-center gap-1.5">
        <Badge variant="cyan">size {item.size}</Badge>
        <Badge variant="teal">align {item.alignment}</Badge>
        <Badge variant="orange">{item.category}</Badge>
        <Badge variant="gray">{item.visibility}</Badge>
        {copyable && <Badge variant="green">copy</Badge>}
        {cloneable && <Badge variant="indigo">clone</Badge>}
        {defaultable && <Badge variant="purple">default</Badge>}
        {packed && <Badge variant="yellow">packed</Badge>}
        {singleton !== null && singleton !== undefined && (
          <Badge variant="red">singleton: 0x{singleton.toString(16)}</Badge>
        )}
        {item.cfg && <Badge variant="pink">cfg({formatCfg(item.cfg)})</Badge>}
      </div>

      {(item.rust_name || item.cpp_name) && (
        <div className="mb-8">
          <SectionHeader>Backend bindings</SectionHeader>
          <Table>
            <thead className="bg-surface">
              <tr className="border-b border-edge">
                <th className={TH}>Backend</th>
                <th className={TH}>Maps to</th>
                <th className={TH}>Header</th>
              </tr>
            </thead>
            <tbody>
              {item.rust_name && (
                <tr className="border-b border-edge last:border-0">
                  <td className={`${TD} text-fg`}>Rust</td>
                  <td className={`${TD} font-mono text-fg-muted`}>{item.rust_name}</td>
                  <td className={`${TD} text-fg-subtle`}>—</td>
                </tr>
              )}
              {item.cpp_name && (
                <tr className="border-b border-edge last:border-0">
                  <td className={`${TD} text-fg`}>C++</td>
                  <td className={`${TD} font-mono text-fg-muted`}>{item.cpp_name}</td>
                  <td className={`${TD} font-mono text-fg-muted`}>{item.cpp_header ?? '—'}</td>
                </tr>
              )}
            </tbody>
          </Table>
        </div>
      )}

      {item.kind.type === 'type' && <TypeView def={item.kind} modulePath={modulePath} />}
      {item.kind.type === 'enum' && <EnumView def={item.kind} modulePath={modulePath} />}
      {item.kind.type === 'bitflags' && <BitflagsView def={item.kind} modulePath={modulePath} />}
      {item.kind.type === 'type_alias' && <TypeAliasView def={item.kind} modulePath={modulePath} />}
    </div>
  );
}
