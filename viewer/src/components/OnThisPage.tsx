import { useEffect, useState } from 'react';

export interface TocEntry {
  id: string;
  label: string;
}

// rustdoc-style "On this page" rail with scroll-spy. Hidden below xl, where
// there isn't room beside the content column.
export function OnThisPage({ entries }: { entries: TocEntry[] }) {
  const [active, setActive] = useState<string | null>(entries[0]?.id ?? null);
  const key = entries.map((e) => e.id).join('|');

  useEffect(() => {
    const root = document.querySelector('main');
    const ids = key ? key.split('|') : [];
    const els = ids
      .map((id) => document.getElementById(id))
      .filter((el): el is HTMLElement => el !== null);
    if (els.length === 0) return;

    const visible = new Set<string>();
    const observer = new IntersectionObserver(
      (records) => {
        for (const r of records) {
          if (r.isIntersecting) visible.add(r.target.id);
          else visible.delete(r.target.id);
        }
        const first = ids.find((id) => visible.has(id));
        if (first) setActive(first);
      },
      { root, rootMargin: '0px 0px -70% 0px', threshold: 0 }
    );
    els.forEach((el) => observer.observe(el));
    return () => observer.disconnect();
  }, [key]);

  if (entries.length < 2) return null;

  return (
    <nav className="sticky top-0 hidden max-h-screen w-48 flex-shrink-0 self-start overflow-y-auto py-6 xl:block">
      <div className="mb-3 font-mono text-xs font-semibold uppercase tracking-wider text-fg-subtle">
        On this page
      </div>
      <ul className="border-l border-edge">
        {entries.map((e) => (
          <li key={e.id}>
            <button
              type="button"
              onClick={() =>
                document
                  .getElementById(e.id)
                  ?.scrollIntoView({ behavior: 'smooth', block: 'start' })
              }
              className={`-ml-px block border-l-2 py-1 pl-3 text-left text-sm transition-colors ${
                active === e.id
                  ? 'border-accent text-accent'
                  : 'border-transparent text-fg-muted hover:border-edge-strong hover:text-fg'
              }`}
            >
              {e.label}
            </button>
          </li>
        ))}
      </ul>
    </nav>
  );
}
