import type { JsonRegion } from '@pyxis/types';
import { TypeRef } from './TypeRef';
import { SmallBadge } from './Badge';
import { SourceName } from './SourceLink';
import { Markdown } from './Markdown';
import { cn } from '../utils/styles';

type FieldTableProps = {
  fields: JsonRegion[];
  modulePath: string;
  /**
   * Whether the fields are laid out one after another. Union members are not:
   * they all start at offset 0, so the offset column carries no information
   * and is dropped in favour of a note that says so once.
   */
  showOffsets?: boolean;
};

export function FieldTable({ fields, modulePath, showOffsets = true }: FieldTableProps) {
  return (
    <div className="overflow-x-auto">
      <table className="w-full rounded-md border border-edge">
        <thead className="bg-surface">
          <tr>
            {showOffsets && (
              <th className="px-4 py-2 text-left text-sm font-semibold text-fg">Offset</th>
            )}
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
                {showOffsets && (
                  <td className="px-4 py-2 font-mono text-sm text-fg-muted">
                    0x{field.offset.toString(16).toUpperCase()}
                  </td>
                )}
                <td className={nameClasses}>
                  {field.source ? (
                    <SourceName source={field.source}>{field.name || '<anonymous>'}</SourceName>
                  ) : (
                    field.name || '<anonymous>'
                  )}
                </td>
                <td
                  className={cn(
                    `
                  px-4 py-2 font-mono text-sm whitespace-nowrap
                `,
                    typeClasses
                  )}
                >
                  <TypeRef type={field.type_ref} currentModule={modulePath} />
                </td>
                <td className="px-4 py-2 text-sm text-fg-muted">{field.size}</td>
                <td className="px-4 py-2 text-sm text-fg-muted">{field.alignment}</td>
                <td className="px-4 py-2 text-sm whitespace-nowrap">
                  {field.is_base && <SmallBadge variant="violet">base</SmallBadge>}
                  {field.doc && (
                    <div className="mt-1 text-fg-muted">
                      <Markdown docLinks={field.doc_links}>{field.doc}</Markdown>
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
