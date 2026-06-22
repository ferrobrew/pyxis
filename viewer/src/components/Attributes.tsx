import type { AttrGroups } from '../utils/attributes';

// Renders attribute groups as stacked `#[...]` lines in a quiet, monospace
// tone — the shared presentation for functions, extern values, and item
// declaration headers.
export function Attributes({ groups, className = '' }: { groups: AttrGroups; className?: string }) {
  if (groups.length === 0) return null;
  return (
    <div className={`font-mono text-sm text-fg-subtle ${className}`}>
      {groups.map((group, i) => (
        <div key={i}>#[{group.join(', ')}]</div>
      ))}
    </div>
  );
}
