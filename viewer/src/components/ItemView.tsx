import { useEffect, useState } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { getModulePath, findLongestValidAncestor } from '../utils/pathUtils';
import { buildModuleUrl, buildItemUrl, buildRootUrl } from '../utils/navigation';
import { TypeRef } from './TypeRef';
import { Badge, SmallBadge } from './Badge';
import { FunctionDisplay } from './FunctionDisplay';
import { FieldTable } from './FieldTable';
import { NestedFieldView } from './NestedFieldView';
import { Breadcrumbs } from './Breadcrumbs';
import type { JsonTypeDefinition, JsonEnumDefinition, JsonBitflagsDefinition, JsonTypeAliasDefinition } from '@pyxis/types';

// Documentation display component for code blocks
function DocBlock({ doc }: { doc: string }) {
  return (
    <div className="mb-6 p-4 bg-blue-50 dark:bg-blue-900/20 border-l-4 border-blue-500 rounded">
      <p className="text-gray-700 dark:text-slate-400">{doc}</p>
    </div>
  );
}

// Metadata badges for types/enums/bitflags
interface MetadataBadgesProps {
  copyable?: boolean;
  cloneable?: boolean;
  defaultable?: boolean;
  packed?: boolean;
  singleton?: number | null;
  defaultIndex?: number | null;
}

function MetadataBadges({
  copyable,
  cloneable,
  defaultable,
  packed,
  singleton,
}: MetadataBadgesProps) {
  return (
    <div className="mb-6 flex flex-wrap gap-2">
      {copyable && <Badge variant="green">Copy</Badge>}
      {cloneable && <Badge variant="blue">Clone</Badge>}
      {defaultable && <Badge variant="purple">Default</Badge>}
      {packed && <Badge variant="orange">Packed</Badge>}
      {singleton !== null && singleton !== undefined && (
        <Badge variant="gray">Singleton: 0x{singleton.toString(16)}</Badge>
      )}
    </div>
  );
}

type FieldViewMode = 'flat' | 'nested';

// View mode toggle button component
function ViewModeToggle({
  mode,
  onModeChange,
}: {
  mode: FieldViewMode;
  onModeChange: (mode: FieldViewMode) => void;
}) {
  const baseClasses =
    'px-3 py-1 text-sm font-medium transition-colors focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-1';
  const activeClasses = 'bg-blue-600 text-white';
  const inactiveClasses =
    'bg-gray-100 text-gray-700 hover:bg-gray-200 dark:bg-slate-700 dark:text-slate-300 dark:hover:bg-slate-600';

  return (
    <div className="inline-flex rounded-md overflow-hidden border border-gray-300 dark:border-slate-600">
      <button
        onClick={() => onModeChange('flat')}
        className={`${baseClasses} ${mode === 'flat' ? activeClasses : inactiveClasses}`}
        aria-pressed={mode === 'flat'}
      >
        Flat
      </button>
      <button
        onClick={() => onModeChange('nested')}
        className={`${baseClasses} ${mode === 'nested' ? activeClasses : inactiveClasses}`}
        aria-pressed={mode === 'nested'}
      >
        Nested
      </button>
    </div>
  );
}

// Type view component
function TypeView({ def, modulePath }: { def: JsonTypeDefinition; modulePath: string }) {
  const [fieldViewMode, setFieldViewMode] = useState<FieldViewMode>('flat');

  return (
    <div>
      {def.doc && <DocBlock doc={def.doc} />}

      <MetadataBadges
        copyable={def.copyable}
        cloneable={def.cloneable}
        defaultable={def.defaultable}
        packed={def.packed}
        singleton={def.singleton}
      />

      {def.fields.length > 0 && (
        <div id="fields" className="mb-6">
          <div className="flex items-center justify-between mb-3">
            <h2 className="text-xl font-semibold text-gray-900 dark:text-slate-200">Fields</h2>
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
        <div id="virtual-functions" className="mb-6">
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">
            Virtual Functions
          </h2>
          <div className="bg-gray-50 dark:bg-slate-800 rounded-md overflow-hidden">
            {def.vftable.functions.map((func, idx) => (
              <FunctionDisplay
                key={idx}
                id={`vfunc-${func.name}`}
                func={func}
                modulePath={modulePath}
              />
            ))}
          </div>
        </div>
      )}

      {def.associated_functions.length > 0 && (
        <div id="associated-functions" className="mb-6">
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">
            Associated Functions
          </h2>
          <div className="bg-gray-50 dark:bg-slate-800 rounded-md overflow-hidden">
            {def.associated_functions.map((func, idx) => (
              <FunctionDisplay
                key={idx}
                id={`func-${func.name}`}
                func={func}
                modulePath={modulePath}
              />
            ))}
          </div>
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

      <div className="mb-6">
        <div className="text-sm text-gray-600 dark:text-slate-400 mb-2">
          Underlying type: <TypeRef type={def.underlying_type} currentModule={modulePath} />
        </div>
        <MetadataBadges
          copyable={def.copyable}
          cloneable={def.cloneable}
          singleton={def.singleton}
        />
      </div>

      <div id="variants" className="mb-6">
        <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">Variants</h2>
        <div className="overflow-x-auto">
          <table className="w-full border border-gray-200 dark:border-slate-800 rounded-md">
            <thead className="bg-gray-100 dark:bg-slate-800">
              <tr>
                <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
                  Name
                </th>
                <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
                  Value
                </th>
              </tr>
            </thead>
            <tbody>
              {def.variants.map((variant, idx) => (
                <tr
                  key={idx}
                  id={`variant-${variant.name}`}
                  className="border-b border-gray-200 dark:border-slate-800"
                >
                  <td className="px-4 py-2 font-mono text-sm text-gray-900 dark:text-slate-200">
                    {variant.name}
                    {def.default === idx && <SmallBadge variant="purple">default</SmallBadge>}
                  </td>
                  <td className="px-4 py-2 text-sm text-gray-600 dark:text-slate-400 font-mono">
                    {variant.value}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>

      {def.associated_functions.length > 0 && (
        <div id="associated-functions" className="mb-6">
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">
            Associated Functions
          </h2>
          <div className="bg-gray-50 dark:bg-slate-800 rounded-md overflow-hidden">
            {def.associated_functions.map((func, idx) => (
              <FunctionDisplay
                key={idx}
                id={`func-${func.name}`}
                func={func}
                modulePath={modulePath}
              />
            ))}
          </div>
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

      <div className="mb-6">
        <div className="text-sm text-gray-600 dark:text-slate-400 mb-2">
          Underlying type: <TypeRef type={def.underlying_type} currentModule={modulePath} />
        </div>
        <MetadataBadges
          copyable={def.copyable}
          cloneable={def.cloneable}
          singleton={def.singleton}
        />
      </div>

      <div id="flags" className="mb-6">
        <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">Flags</h2>
        <div className="overflow-x-auto">
          <table className="w-full border border-gray-200 dark:border-slate-800 rounded-md">
            <thead className="bg-gray-100 dark:bg-slate-800">
              <tr>
                <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
                  Name
                </th>
                <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
                  Value (Dec)
                </th>
                <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
                  Value (Hex)
                </th>
                <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
                  Value (Bin)
                </th>
              </tr>
            </thead>
            <tbody>
              {def.flags.map((flag, idx) => (
                <tr
                  key={idx}
                  id={`flag-${flag.name}`}
                  className="border-b border-gray-200 dark:border-slate-800"
                >
                  <td className="px-4 py-2 font-mono text-sm text-gray-900 dark:text-slate-200">
                    {flag.name}
                    {def.default === idx && <SmallBadge variant="purple">default</SmallBadge>}
                  </td>
                  <td className="px-4 py-2 text-sm text-gray-600 dark:text-slate-400 font-mono">
                    {flag.value}
                  </td>
                  <td className="px-4 py-2 text-sm text-gray-600 dark:text-slate-400 font-mono">
                    0x{flag.value.toString(16)}
                  </td>
                  <td className="px-4 py-2 text-sm text-gray-600 dark:text-slate-400 font-mono">
                    0b{flag.value.toString(2).padStart(8, '0')}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
}

// Type alias view component
function TypeAliasView({ def, modulePath }: { def: JsonTypeAliasDefinition; modulePath: string }) {
  return (
    <div>
      {def.doc && <DocBlock doc={def.doc} />}

      <div className="mb-6">
        <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">Target Type</h2>
        <div className="p-4 bg-gray-50 dark:bg-slate-800 rounded-md">
          <span className="font-mono text-lg">
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
        <div className="text-gray-500 dark:text-slate-400">
          Please load a documentation file to begin.
        </div>
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
  let itemType: 'type' | 'enum' | 'bitflags' | 'type_alias' = 'type';
  if (item.kind.type === 'enum') itemType = 'enum';
  else if (item.kind.type === 'bitflags') itemType = 'bitflags';
  else if (item.kind.type === 'type_alias') itemType = 'type_alias';

  return (
    <div className="p-4 md:p-6 lg:p-8 max-w-6xl">
      <Breadcrumbs path={decodedPath} isItem={true} itemType={itemType} />
      <div className="mb-4 flex flex-wrap items-center gap-2 md:gap-3 text-sm text-gray-600 dark:text-slate-400">
        <span className="px-2 py-1 bg-gray-100 dark:bg-slate-800 rounded capitalize">
          {item.kind.type}
        </span>
        <span>Size: {item.size} bytes</span>
        <span>Alignment: {item.alignment} bytes</span>
        <span className="capitalize">{item.category}</span>
        <span className="capitalize">{item.visibility}</span>
      </div>

      {item.kind.type === 'type' && <TypeView def={item.kind} modulePath={modulePath} />}
      {item.kind.type === 'enum' && <EnumView def={item.kind} modulePath={modulePath} />}
      {item.kind.type === 'bitflags' && <BitflagsView def={item.kind} modulePath={modulePath} />}
      {item.kind.type === 'type_alias' && <TypeAliasView def={item.kind} modulePath={modulePath} />}
    </div>
  );
}
