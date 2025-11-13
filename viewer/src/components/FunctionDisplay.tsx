import type { JsonFunction } from '@pyxis/types';
import { TypeRef } from './TypeRef';

interface FunctionDisplayProps {
  func: JsonFunction;
  modulePath: string;
  id?: string;
}

export function FunctionDisplay({ func, modulePath, id }: FunctionDisplayProps) {
  const isPrivate = func.visibility === 'private';
  const containerClasses = isPrivate
    ? 'mb-4 p-4 bg-gray-50 dark:bg-slate-800 rounded-md opacity-60'
    : 'mb-4 p-4 bg-gray-50 dark:bg-slate-800 rounded-md';
  const nameClasses = isPrivate
    ? 'font-semibold text-gray-500 dark:text-slate-600'
    : 'font-semibold text-gray-900 dark:text-slate-200';

  return (
    <div id={id} className={containerClasses}>
      <div className="flex items-start gap-2">
        <span className="text-violet-600 dark:text-slate-500 font-mono text-sm">fn</span>
        <div className="flex-1">
          <div className="font-mono text-sm">
            <span className={nameClasses}>{func.name}</span>
            <span className="text-gray-600 dark:text-slate-400">(</span>
            {func.arguments.map((arg, i) => (
              <span key={i}>
                {i > 0 && <span className="text-gray-600 dark:text-slate-400">, </span>}
                {arg.type === 'const_self' && (
                  <span className="text-orange-600 dark:text-orange-400">&amp;self</span>
                )}
                {arg.type === 'mut_self' && (
                  <span className="text-orange-600 dark:text-orange-400">&amp;mut self</span>
                )}
                {arg.type === 'field' && (
                  <>
                    <span className="text-gray-800 dark:text-slate-300">{arg.name}</span>
                    <span className="text-gray-600 dark:text-slate-400">: </span>
                    <TypeRef type={arg.type_ref} currentModule={modulePath} />
                  </>
                )}
              </span>
            ))}
            <span className="text-gray-600 dark:text-slate-400">)</span>
            {func.return_type && (
              <>
                <span className="text-gray-600 dark:text-slate-400"> -&gt; </span>
                <TypeRef type={func.return_type} currentModule={modulePath} />
              </>
            )}
          </div>
          {func.doc && (
            <div className="mt-2 text-sm text-gray-600 dark:text-slate-400">{func.doc}</div>
          )}
          <div className="mt-2 text-xs text-gray-500 dark:text-slate-500">
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
