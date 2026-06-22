import ReactMarkdown, { type Components } from 'react-markdown';
import remarkGfm from 'remark-gfm';

// Element styling for rendered doc-comment Markdown. Text colour and size are
// inherited from the surrounding container, so docs blend into their context;
// only structure (code, links, lists, emphasis) is styled here.
const components: Components = {
  p: ({ children }) => <p className="mb-2 last:mb-0">{children}</p>,
  a: ({ href, children }) => (
    <a
      href={href}
      target="_blank"
      rel="noopener noreferrer"
      className="text-accent hover:underline"
    >
      {children}
    </a>
  ),
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

/** Render a doc comment (or any prose string) as GitHub-flavored Markdown. */
export function Markdown({ children }: { children: string }) {
  return (
    <ReactMarkdown remarkPlugins={[remarkGfm]} components={components}>
      {children}
    </ReactMarkdown>
  );
}
