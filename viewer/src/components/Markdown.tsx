import { useMemo } from 'react';
import { Link } from 'react-router-dom';
import ReactMarkdown, { type Components } from 'react-markdown';
import remarkGfm from 'remark-gfm';
import type { JsonDocLink } from '@pyxis/types';
import { useDocumentation } from '../contexts/DocumentationContext';
import { buildItemUrl, buildModuleUrl } from '../utils/navigation';

// Element styling for rendered doc-comment Markdown. Text colour and size are
// inherited from the surrounding container, so docs blend into their context;
// only structure (code, links, lists, emphasis) is styled here.
const baseComponents: Components = {
  p: ({ children }) => <p className="mb-2 last:mb-0">{children}</p>,
  ul: ({ children }) => <ul className="mb-2 list-inside list-disc last:mb-0">{children}</ul>,
  ol: ({ children }) => <ol className="mb-2 list-inside list-decimal last:mb-0">{children}</ol>,
  li: ({ children }) => <li className="ml-1">{children}</li>,
  // Inline code gets a subtle pill; fenced blocks (which carry a language
  // class and are wrapped in <pre>) just stay monospace to avoid double bg.
  code: ({ className, children }) =>
    className ? (
      <code className={`${className} font-mono`}>{children}</code>
    ) : (
      <code className="rounded bg-inset px-1 py-0.5 font-mono text-[0.9em]">{children}</code>
    ),
  pre: ({ children }) => (
    <pre className="mb-2 overflow-x-auto rounded bg-inset p-3 text-sm last:mb-0">{children}</pre>
  ),
  h1: ({ children }) => <h4 className="mb-1 mt-2 font-semibold first:mt-0">{children}</h4>,
  h2: ({ children }) => <h4 className="mb-1 mt-2 font-semibold first:mt-0">{children}</h4>,
  h3: ({ children }) => <h4 className="mb-1 mt-2 font-semibold first:mt-0">{children}</h4>,
};

// The router destination for a resolved intra-doc link: the item or module
// page, plus a `##member` anchor when it points at a member.
function docLinkTo(link: JsonDocLink, source: string): string {
  const base =
    link.target_kind === 'item'
      ? buildItemUrl(link.path, source)
      : buildModuleUrl(link.path, source);
  return link.anchor ? `${base}##${link.anchor}` : base;
}

function escapeRegExp(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

// rustdoc shortcut links (``[`Foo`]``) aren't valid Markdown links on their own,
// so rewrite the resolved ones into inline links (``[`Foo`](Foo)``) whose href is
// the link text — the same href the inline form (``[label](Foo)``) already uses,
// which the `a` renderer then maps back to a route.
function normalizeShortcutLinks(markdown: string, links: JsonDocLink[]): string {
  let out = markdown;
  for (const link of links) {
    const pattern = new RegExp('\\[`' + escapeRegExp(link.text) + '`\\](?!\\()', 'g');
    out = out.replace(pattern, `[\`${link.text}\`](${link.text})`);
  }
  return out;
}

/** Render a doc comment (or any prose string) as GitHub-flavored Markdown. */
export function Markdown({ children, docLinks }: { children: string; docLinks?: JsonDocLink[] }) {
  const { selectedSource } = useDocumentation();

  const linkMap = useMemo(() => {
    const map = new Map<string, JsonDocLink>();
    for (const link of docLinks ?? []) map.set(link.text, link);
    return map;
  }, [docLinks]);

  const source = normalizeShortcutLinks(children, docLinks ?? []);

  const components: Components = useMemo(
    () => ({
      ...baseComponents,
      a: ({ href, children }) => {
        const link = href ? linkMap.get(href) : undefined;
        if (link) {
          return (
            <Link to={docLinkTo(link, selectedSource)} className="text-accent hover:underline">
              {children}
            </Link>
          );
        }
        return (
          <a
            href={href}
            target="_blank"
            rel="noopener noreferrer"
            className="text-accent hover:underline"
          >
            {children}
          </a>
        );
      },
    }),
    [linkMap, selectedSource]
  );

  return (
    <ReactMarkdown remarkPlugins={[remarkGfm]} components={components} urlTransform={(url) => url}>
      {source}
    </ReactMarkdown>
  );
}
