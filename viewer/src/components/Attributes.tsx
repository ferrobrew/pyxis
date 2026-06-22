import type { AttrGroups } from '../utils/attributes';
import { WRAP_COLUMN } from '../utils/typeString';

function AttrGroup({ attrs }: { attrs: string[] }) {
  const oneLine = `#[${attrs.join(', ')}]`;
  if (oneLine.length <= WRAP_COLUMN) {
    return <div>{oneLine}</div>;
  }
  // rustfmt-style: open bracket, one attribute per indented line with a
  // trailing comma, then the closing bracket on its own line.
  return (
    <div>
      <div>#[</div>
      {attrs.map((attr, i) => (
        <div key={i} className="pl-4">
          {attr},
        </div>
      ))}
      <div>]</div>
    </div>
  );
}

// Renders attribute groups as `#[...]` lines in a quiet, monospace tone — the
// shared presentation for functions, extern values, and item declaration
// headers. Long groups wrap one attribute per line.
export function Attributes({ groups, className = '' }: { groups: AttrGroups; className?: string }) {
  if (groups.length === 0) return null;
  return (
    <div className={`font-mono text-sm text-fg-subtle ${className}`}>
      {groups.map((group, i) => (
        <AttrGroup key={i} attrs={group} />
      ))}
    </div>
  );
}
