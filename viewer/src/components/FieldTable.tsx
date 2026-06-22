import type { JsonRegion } from '@pyxis/types';
import { TypeRef } from './TypeRef';
import { SmallBadge } from './Badge';
import { SourceName } from './SourceLink';
import { Markdown } from './Markdown';

interface FieldTableProps {
  fields: JsonRegion[];
  modulePath: string;
}

export function FieldTable({ fields, modulePath }: FieldTableProps) {
  return (
    <div className="overflow-x-auto">
      <table className="w-full border border-edge rounded-md">
        <thead className="bg-surface">
          <tr>
            <th className="px-4 py-2 text-left text-sm font-semibold text-fg">Offset</th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-fg">Name</th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-fg">Type</th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-fg">Size</th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-fg">Align</th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-fg">Notes</th>
          </tr>
        </thead>
        <tbody>
          {fields.map((field, idx) => {
            const isPrivate = field.visibility === 'private';
            const nameClasses = isPrivate
              ? 'px-4 py-2 font-mono text-sm text-fg-subtle'
              : 'px-4 py-2 font-mono text-sm text-fg';
            const typeClasses = isPrivate ? 'text-fg-subtle' : '';

            return (
              <tr
                key={idx}
                id={field.name ? `field-${field.name}` : undefined}
                className="border-b border-edge"
              >
                <td className="px-4 py-2 text-sm text-fg-muted font-mono">
                  0x{field.offset.toString(16).toUpperCase()}
                </td>
                <td className={nameClasses}>
                  {field.source ? (
                    <SourceName source={field.source}>{field.name || '<anonymous>'}</SourceName>
                  ) : (
                    field.name || '<anonymous>'
                  )}
                </td>
                <td className={`whitespace-nowrap px-4 py-2 font-mono text-sm ${typeClasses}`}>
                  <TypeRef type={field.type_ref} currentModule={modulePath} />
                </td>
                <td className="px-4 py-2 text-sm text-fg-muted">{field.size}</td>
                <td className="px-4 py-2 text-sm text-fg-muted">{field.alignment}</td>
                <td className="whitespace-nowrap px-4 py-2 text-sm">
                  {field.is_base && <SmallBadge variant="violet">base</SmallBadge>}
                  {field.doc && (
                    <div className="text-fg-muted mt-1">
                      <Markdown>{field.doc}</Markdown>
                    </div>
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
