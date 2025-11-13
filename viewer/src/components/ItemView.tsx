import { useEffect } from 'react';
import { useParams, Link, useNavigate } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { getModulePath, getItemName, findLongestValidAncestor } from '../utils/pathUtils';
import {
  buildModuleUrl,
  buildItemUrl,
  buildRootUrl,
  decodeSourceIdentifier,
} from '../utils/navigation';
import { TypeRef } from './TypeRef';
import { Badge, SmallBadge } from './Badge';
import { FunctionDisplay } from './FunctionDisplay';
import { FieldTable } from './FieldTable';
import type { JsonTypeDefinition, JsonEnumDefinition, JsonBitflagsDefinition } from '@pyxis/types';

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

// Type view component
function TypeView({ def, modulePath }: { def: JsonTypeDefinition; modulePath: string }) {
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
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-slate-200">Fields</h2>
          <FieldTable fields={def.fields} modulePath={modulePath} />
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

// Main ItemView component
export function ItemView() {
  const { itemPath = '', source = 'local' } = useParams();
  const { documentation, selectedSource, setSelectedSource } = useDocumentation();
  const navigate = useNavigate();

  // Sync source from URL to context (only if source is not 'local')
  useEffect(() => {
    if (source === 'local') {
      // If URL says local, ensure context matches
      if (selectedSource !== 'local') {
        setSelectedSource('local');
      }
    } else {
      // For remote sources, decode and sync
      const decodedSource = decodeSourceIdentifier(source);
      if (decodedSource !== selectedSource && decodedSource !== source) {
        setSelectedSource(decodedSource);
      }
    }
  }, [source, selectedSource, setSelectedSource]);

  useEffect(() => {
    if (documentation && itemPath) {
      const decodedPath = decodeURIComponent(itemPath);
      const item = documentation.items[decodedPath];

      if (!item) {
        // Item doesn't exist, find the longest valid ancestor
        const ancestorPath = findLongestValidAncestor(decodedPath, documentation, true);

        if (ancestorPath === null || ancestorPath === '') {
          // No valid ancestor found, navigate to root
          navigate(buildRootUrl(), { replace: true });
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
  const itemName = getItemName(decodedPath);

  return (
    <div className="p-8 max-w-6xl">
      <div className="mb-4">
        <div className="text-sm text-gray-500 dark:text-slate-400 mb-2">
          <Link to={buildModuleUrl(modulePath, selectedSource)} className="hover:underline">
            {modulePath || 'root'}
          </Link>
        </div>
        <h1 className="text-3xl font-bold mb-2 text-gray-900 dark:text-slate-200">{itemName}</h1>
        <div className="flex items-center gap-3 text-sm text-gray-600 dark:text-slate-400">
          <span className="px-2 py-1 bg-gray-100 dark:bg-slate-800 rounded capitalize">
            {item.kind.type}
          </span>
          <span>Size: {item.size} bytes</span>
          <span>Alignment: {item.alignment} bytes</span>
          <span className="capitalize">{item.category}</span>
          <span className="capitalize">{item.visibility}</span>
        </div>
      </div>

      {item.kind.type === 'type' && <TypeView def={item.kind} modulePath={modulePath} />}
      {item.kind.type === 'enum' && <EnumView def={item.kind} modulePath={modulePath} />}
      {item.kind.type === 'bitflags' && <BitflagsView def={item.kind} modulePath={modulePath} />}
    </div>
  );
}
