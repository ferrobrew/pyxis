import { useParams, Link } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { getModulePath, getItemName } from '../utils/pathUtils';
import { TypeRef } from './TypeRef';
import type { JsonTypeDefinition, JsonEnumDefinition, JsonBitflagsDefinition, JsonFunction, JsonRegion } from '@pyxis/types';

function FieldRow({ field, modulePath }: { field: JsonRegion; modulePath: string }) {
  return (
    <tr className="border-b border-gray-200 dark:border-gray-700">
      <td className="px-4 py-2 font-mono text-sm text-gray-900 dark:text-gray-100">
        {field.name || '<anonymous>'}
      </td>
      <td className="px-4 py-2 font-mono text-sm">
        <TypeRef type={field.type_ref} currentModule={modulePath} />
      </td>
      <td className="px-4 py-2 text-sm text-gray-600 dark:text-gray-400">
        0x{field.offset.toString(16)}
      </td>
      <td className="px-4 py-2 text-sm text-gray-600 dark:text-gray-400">
        {field.size}
      </td>
      <td className="px-4 py-2 text-sm text-gray-600 dark:text-gray-400">
        {field.alignment}
      </td>
      <td className="px-4 py-2 text-sm">
        {field.is_base && (
          <span className="px-2 py-1 bg-purple-100 dark:bg-purple-900 text-purple-800 dark:text-purple-200 rounded text-xs">
            base
          </span>
        )}
        {field.doc && (
          <div className="text-gray-600 dark:text-gray-400 mt-1">{field.doc}</div>
        )}
      </td>
    </tr>
  );
}

function FunctionRow({ func, modulePath }: { func: JsonFunction; modulePath: string }) {
  return (
    <div className="mb-4 p-4 bg-gray-50 dark:bg-gray-800 rounded-md">
      <div className="flex items-start gap-2">
        <span className="text-purple-600 dark:text-purple-400 font-mono text-sm">fn</span>
        <div className="flex-1">
          <div className="font-mono text-sm">
            <span className="font-semibold text-gray-900 dark:text-gray-100">{func.name}</span>
            <span className="text-gray-600 dark:text-gray-400">(</span>
            {func.arguments.map((arg, i) => (
              <span key={i}>
                {i > 0 && <span className="text-gray-600 dark:text-gray-400">, </span>}
                {arg.type === 'const_self' && (
                  <span className="text-orange-600 dark:text-orange-400">&amp;self</span>
                )}
                {arg.type === 'mut_self' && (
                  <span className="text-orange-600 dark:text-orange-400">&amp;mut self</span>
                )}
                {arg.type === 'field' && (
                  <>
                    <span className="text-gray-800 dark:text-gray-200">{arg.name}</span>
                    <span className="text-gray-600 dark:text-gray-400">: </span>
                    <TypeRef type={arg.type_ref} currentModule={modulePath} />
                  </>
                )}
              </span>
            ))}
            <span className="text-gray-600 dark:text-gray-400">)</span>
            {func.return_type && (
              <>
                <span className="text-gray-600 dark:text-gray-400"> -&gt; </span>
                <TypeRef type={func.return_type} currentModule={modulePath} />
              </>
            )}
          </div>
          {func.doc && (
            <div className="mt-2 text-sm text-gray-600 dark:text-gray-400">
              {func.doc}
            </div>
          )}
          <div className="mt-2 text-xs text-gray-500 dark:text-gray-500">
            {func.body.type === 'address' && `Address: 0x${func.body.address.toString(16)}`}
            {func.body.type === 'field' && `Field: ${func.body.field}`}
            {func.body.type === 'vftable' && `VFTable: ${func.body.function_name}`}
            {func.calling_convention !== 'c' && ` • ${func.calling_convention}`}
          </div>
        </div>
      </div>
    </div>
  );
}

function TypeView({ def, modulePath }: { def: JsonTypeDefinition; modulePath: string }) {
  return (
    <div>
      {def.doc && (
        <div className="mb-6 p-4 bg-blue-50 dark:bg-blue-900/20 border-l-4 border-blue-500 rounded">
          <p className="text-gray-700 dark:text-gray-300">{def.doc}</p>
        </div>
      )}

      <div className="mb-6 flex flex-wrap gap-2">
        {def.copyable && (
          <span className="px-3 py-1 bg-green-100 dark:bg-green-900 text-green-800 dark:text-green-200 rounded-md text-sm">
            Copy
          </span>
        )}
        {def.cloneable && (
          <span className="px-3 py-1 bg-blue-100 dark:bg-blue-900 text-blue-800 dark:text-blue-200 rounded-md text-sm">
            Clone
          </span>
        )}
        {def.defaultable && (
          <span className="px-3 py-1 bg-purple-100 dark:bg-purple-900 text-purple-800 dark:text-purple-200 rounded-md text-sm">
            Default
          </span>
        )}
        {def.packed && (
          <span className="px-3 py-1 bg-orange-100 dark:bg-orange-900 text-orange-800 dark:text-orange-200 rounded-md text-sm">
            Packed
          </span>
        )}
        {def.singleton !== null && (
          <span className="px-3 py-1 bg-gray-100 dark:bg-gray-800 text-gray-800 dark:text-gray-200 rounded-md text-sm font-mono">
            Singleton: 0x{def.singleton.toString(16)}
          </span>
        )}
      </div>

      {def.fields.length > 0 && (
        <div className="mb-6">
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-gray-100">Fields</h2>
          <div className="overflow-x-auto">
            <table className="w-full border border-gray-200 dark:border-gray-700 rounded-md">
              <thead className="bg-gray-100 dark:bg-gray-800">
                <tr>
                  <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-gray-100">Name</th>
                  <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-gray-100">Type</th>
                  <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-gray-100">Offset</th>
                  <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-gray-100">Size</th>
                  <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-gray-100">Align</th>
                  <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-gray-100">Notes</th>
                </tr>
              </thead>
              <tbody>
                {def.fields.map((field, idx) => (
                  <FieldRow key={idx} field={field} modulePath={modulePath} />
                ))}
              </tbody>
            </table>
          </div>
        </div>
      )}

      {def.vftable && def.vftable.functions.length > 0 && (
        <div className="mb-6">
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-gray-100">Virtual Functions</h2>
          {def.vftable.functions.map((func, idx) => (
            <FunctionRow key={idx} func={func} modulePath={modulePath} />
          ))}
        </div>
      )}

      {def.associated_functions.length > 0 && (
        <div className="mb-6">
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-gray-100">Associated Functions</h2>
          {def.associated_functions.map((func, idx) => (
            <FunctionRow key={idx} func={func} modulePath={modulePath} />
          ))}
        </div>
      )}
    </div>
  );
}

function EnumView({ def, modulePath }: { def: JsonEnumDefinition; modulePath: string }) {
  return (
    <div>
      {def.doc && (
        <div className="mb-6 p-4 bg-blue-50 dark:bg-blue-900/20 border-l-4 border-blue-500 rounded">
          <p className="text-gray-700 dark:text-gray-300">{def.doc}</p>
        </div>
      )}

      <div className="mb-6">
        <div className="text-sm text-gray-600 dark:text-gray-400 mb-2">
          Underlying type:{' '}
          <TypeRef type={def.underlying_type} currentModule={modulePath} />
        </div>
        <div className="flex flex-wrap gap-2">
          {def.copyable && (
            <span className="px-3 py-1 bg-green-100 dark:bg-green-900 text-green-800 dark:text-green-200 rounded-md text-sm">
              Copy
            </span>
          )}
          {def.cloneable && (
            <span className="px-3 py-1 bg-blue-100 dark:bg-blue-900 text-blue-800 dark:text-blue-200 rounded-md text-sm">
              Clone
            </span>
          )}
          {def.singleton !== null && (
            <span className="px-3 py-1 bg-gray-100 dark:bg-gray-800 text-gray-800 dark:text-gray-200 rounded-md text-sm font-mono">
              Singleton: 0x{def.singleton.toString(16)}
            </span>
          )}
        </div>
      </div>

      <div className="mb-6">
        <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-gray-100">Variants</h2>
        <div className="overflow-x-auto">
          <table className="w-full border border-gray-200 dark:border-gray-700 rounded-md">
            <thead className="bg-gray-100 dark:bg-gray-800">
              <tr>
                <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-gray-100">Name</th>
                <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-gray-100">Value</th>
              </tr>
            </thead>
            <tbody>
              {def.variants.map((variant, idx) => (
                <tr key={idx} className="border-b border-gray-200 dark:border-gray-700">
                  <td className="px-4 py-2 font-mono text-sm text-gray-900 dark:text-gray-100">
                    {variant.name}
                    {def.default === idx && (
                      <span className="ml-2 px-2 py-1 bg-purple-100 dark:bg-purple-900 text-purple-800 dark:text-purple-200 rounded text-xs">
                        default
                      </span>
                    )}
                  </td>
                  <td className="px-4 py-2 text-sm text-gray-600 dark:text-gray-400 font-mono">
                    {variant.value}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>

      {def.associated_functions.length > 0 && (
        <div className="mb-6">
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-gray-100">Associated Functions</h2>
          {def.associated_functions.map((func, idx) => (
            <FunctionRow key={idx} func={func} modulePath={modulePath} />
          ))}
        </div>
      )}
    </div>
  );
}

function BitflagsView({ def, modulePath }: { def: JsonBitflagsDefinition; modulePath: string }) {
  return (
    <div>
      {def.doc && (
        <div className="mb-6 p-4 bg-blue-50 dark:bg-blue-900/20 border-l-4 border-blue-500 rounded">
          <p className="text-gray-700 dark:text-gray-300">{def.doc}</p>
        </div>
      )}

      <div className="mb-6">
        <div className="text-sm text-gray-600 dark:text-gray-400 mb-2">
          Underlying type:{' '}
          <TypeRef type={def.underlying_type} currentModule={modulePath} />
        </div>
        <div className="flex flex-wrap gap-2">
          {def.copyable && (
            <span className="px-3 py-1 bg-green-100 dark:bg-green-900 text-green-800 dark:text-green-200 rounded-md text-sm">
              Copy
            </span>
          )}
          {def.cloneable && (
            <span className="px-3 py-1 bg-blue-100 dark:bg-blue-900 text-blue-800 dark:text-blue-200 rounded-md text-sm">
              Clone
            </span>
          )}
          {def.singleton !== null && (
            <span className="px-3 py-1 bg-gray-100 dark:bg-gray-800 text-gray-800 dark:text-gray-200 rounded-md text-sm font-mono">
              Singleton: 0x{def.singleton.toString(16)}
            </span>
          )}
        </div>
      </div>

      <div className="mb-6">
        <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-gray-100">Flags</h2>
        <div className="overflow-x-auto">
          <table className="w-full border border-gray-200 dark:border-gray-700 rounded-md">
            <thead className="bg-gray-100 dark:bg-gray-800">
              <tr>
                <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-gray-100">Name</th>
                <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-gray-100">Value (Dec)</th>
                <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-gray-100">Value (Hex)</th>
                <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-gray-100">Value (Bin)</th>
              </tr>
            </thead>
            <tbody>
              {def.flags.map((flag, idx) => (
                <tr key={idx} className="border-b border-gray-200 dark:border-gray-700">
                  <td className="px-4 py-2 font-mono text-sm text-gray-900 dark:text-gray-100">
                    {flag.name}
                    {def.default === idx && (
                      <span className="ml-2 px-2 py-1 bg-purple-100 dark:bg-purple-900 text-purple-800 dark:text-purple-200 rounded text-xs">
                        default
                      </span>
                    )}
                  </td>
                  <td className="px-4 py-2 text-sm text-gray-600 dark:text-gray-400 font-mono">
                    {flag.value}
                  </td>
                  <td className="px-4 py-2 text-sm text-gray-600 dark:text-gray-400 font-mono">
                    0x{flag.value.toString(16)}
                  </td>
                  <td className="px-4 py-2 text-sm text-gray-600 dark:text-gray-400 font-mono">
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

export function ItemView() {
  const { itemPath = '' } = useParams();
  const { documentation } = useDocumentation();

  if (!documentation) {
    return (
      <div className="p-8">
        <div className="text-gray-500 dark:text-gray-400">
          Please load a documentation file to begin.
        </div>
      </div>
    );
  }

  const decodedPath = decodeURIComponent(itemPath);
  const item = documentation.items[decodedPath];

  if (!item) {
    return (
      <div className="p-8">
        <div className="text-red-500">Item not found: {decodedPath}</div>
      </div>
    );
  }

  const modulePath = getModulePath(decodedPath);
  const itemName = getItemName(decodedPath);

  return (
    <div className="p-8 max-w-6xl">
      <div className="mb-4">
        <div className="text-sm text-gray-500 dark:text-gray-400 mb-2">
          <Link to={`/module/${encodeURIComponent(modulePath)}`} className="hover:underline">
            {modulePath || 'root'}
          </Link>
        </div>
        <h1 className="text-3xl font-bold mb-2 text-gray-900 dark:text-gray-100">
          {itemName}
        </h1>
        <div className="flex items-center gap-3 text-sm text-gray-600 dark:text-gray-400">
          <span className="px-2 py-1 bg-gray-100 dark:bg-gray-800 rounded capitalize">
            {item.kind.type}
          </span>
          <span>Size: {item.size} bytes</span>
          <span>Alignment: {item.alignment} bytes</span>
          <span className="capitalize">{item.category}</span>
          <span className="capitalize">{item.visibility}</span>
        </div>
      </div>

      {item.kind.type === 'type' && (
        <TypeView def={item.kind} modulePath={modulePath} />
      )}
      {item.kind.type === 'enum' && (
        <EnumView def={item.kind} modulePath={modulePath} />
      )}
      {item.kind.type === 'bitflags' && (
        <BitflagsView def={item.kind} modulePath={modulePath} />
      )}
    </div>
  );
}
