import type { JsonRegion } from '@pyxis/types';
import { TypeRef } from './TypeRef';
import { SourceName } from './SourceLink';
import { formatHexAddress } from '../utils/format';

interface FieldSourceViewProps {
  fields: JsonRegion[];
  modulePath: string;
}

// Emit an `#[address(...)]` attribute only where a field doesn't sit
// immediately after the previous one (i.e. where the source needs to jump),
// plus `base` for base-class regions. Computed up front so render stays pure.
function computeFieldAttrs(fields: JsonRegion[]): string[][] {
  let expected = 0;
  return fields.map((field) => {
    const attrs: string[] = [];
    if (field.offset !== expected) attrs.push(`address(${formatHexAddress(field.offset)})`);
    if (field.is_base) attrs.push('base');
    expected = field.offset + field.size;
    return attrs;
  });
}

// Renders the struct body the way it reads in pyxis-defs: `pub name: Type,`
// lines, mirroring how the definitions are actually written.
export function FieldSourceView({ fields, modulePath }: FieldSourceViewProps) {
  const fieldAttrs = computeFieldAttrs(fields);

  return (
    <div className="overflow-x-auto rounded-md border border-edge bg-inset p-4 font-mono text-sm leading-relaxed">
      {fields.map((field, idx) => {
        const attrs = fieldAttrs[idx];
        const isPrivate = field.visibility === 'private';
        const name = field.name || '_';

        return (
          <div key={idx} id={field.name ? `field-${field.name}` : undefined}>
            {attrs.length > 0 && <div className="text-fg-subtle">#[{attrs.join(', ')}]</div>}
            <div>
              {!isPrivate && <span className="text-fg-muted">pub </span>}
              <span className={isPrivate ? 'text-fg-subtle' : 'text-fg'}>
                {field.source ? <SourceName source={field.source}>{name}</SourceName> : name}
              </span>
              <span className="text-fg-muted">: </span>
              <TypeRef type={field.type_ref} currentModule={modulePath} />
              <span className="text-fg-muted">,</span>
            </div>
          </div>
        );
      })}
    </div>
  );
}
