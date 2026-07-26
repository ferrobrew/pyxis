import type { JsonRegion } from '@pyxis/types';
import { TypeRef } from './TypeRef';
import { SourceName } from './SourceLink';
import { formatHexAddress } from '../utils/format';

interface FieldSourceViewProps {
  fields: JsonRegion[];
  modulePath: string;
  /**
   * How the fields sit in memory. `sequential` (a type body) means each field
   * follows the last, so a gap has to be reconstructed as `#[address(...)]`.
   * `overlaid` (a union body) means every member starts at offset 0 by
   * definition — there is nothing to reconstruct, and comparing offsets
   * against a running counter would stamp a bogus `#[address(0x0)]` on every
   * member after the first.
   */
  layout?: 'sequential' | 'overlaid';
}

// Emit an `#[address(...)]` attribute only where a field doesn't sit
// immediately after the previous one (i.e. where the source needs to jump),
// plus `base` for base-class regions. Computed up front so render stays pure.
function computeFieldAttrs(fields: JsonRegion[], layout: 'sequential' | 'overlaid'): string[][] {
  let expected = 0;
  return fields.map((field) => {
    const attrs: string[] = [];
    if (layout === 'sequential') {
      if (field.offset !== expected) attrs.push(`address(${formatHexAddress(field.offset)})`);
      expected = field.offset + field.size;
    }
    if (field.is_base) attrs.push('base');
    return attrs;
  });
}

// Renders the struct/union body the way it reads in pyxis-defs: `pub name: Type,`
// lines, mirroring how the definitions are actually written.
export function FieldSourceView({
  fields,
  modulePath,
  layout = 'sequential',
}: FieldSourceViewProps) {
  const fieldAttrs = computeFieldAttrs(fields, layout);

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
