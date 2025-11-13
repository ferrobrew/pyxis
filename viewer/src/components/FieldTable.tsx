import type { JsonRegion } from '@pyxis/types';
import { TypeRef } from './TypeRef';
import { SmallBadge } from './Badge';

interface FieldTableProps {
  fields: JsonRegion[];
  modulePath: string;
}

export function FieldTable({ fields, modulePath }: FieldTableProps) {
  return (
    <div className="overflow-x-auto">
      <table className="w-full border border-gray-200 dark:border-slate-800 rounded-md">
        <thead className="bg-gray-100 dark:bg-purple-950">
          <tr>
            <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
              Name
            </th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
              Type
            </th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
              Offset
            </th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
              Size
            </th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
              Align
            </th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
              Notes
            </th>
          </tr>
        </thead>
        <tbody>
          {fields.map((field, idx) => {
            const isPrivate = field.visibility === 'private';
            const nameClasses = isPrivate
              ? 'px-4 py-2 font-mono text-sm text-gray-500 dark:text-slate-600'
              : 'px-4 py-2 font-mono text-sm text-gray-900 dark:text-slate-200';
            const typeClasses = isPrivate ? 'opacity-60' : '';

            return (
              <tr key={idx} className="border-b border-gray-200 dark:border-slate-800">
                <td className={nameClasses}>{field.name || '<anonymous>'}</td>
                <td className={`px-4 py-2 font-mono text-sm ${typeClasses}`}>
                  <TypeRef type={field.type_ref} currentModule={modulePath} />
                </td>
                <td className="px-4 py-2 text-sm text-gray-600 dark:text-slate-400">
                  0x{field.offset.toString(16)}
                </td>
                <td className="px-4 py-2 text-sm text-gray-600 dark:text-slate-400">
                  {field.size}
                </td>
                <td className="px-4 py-2 text-sm text-gray-600 dark:text-slate-400">
                  {field.alignment}
                </td>
                <td className="px-4 py-2 text-sm">
                  {field.is_base && <SmallBadge variant="purple">base</SmallBadge>}
                  {field.doc && (
                    <div className="text-gray-600 dark:text-slate-400 mt-1">{field.doc}</div>
                  )}
                </td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}
