import { useState, useRef, useEffect, useMemo } from 'react';
import { cn, HEADER_INPUT_HEIGHT } from '../utils/styles';
import { useNavigate } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { searchDocumentation, type SearchResult, type SearchKind } from '../utils/searchUtils';
import { buildModuleUrl, buildItemUrl } from '../utils/navigation';
import { getItemTypeColor } from '../utils/colors';
import { KindIcon } from './KindIcon';

const KIND_LABEL: Record<SearchKind, string> = {
  module: 'module',
  type: 'type',
  enum: 'enum',
  'enum-variant': 'variant',
  bitflags: 'bitflags',
  union: 'union',
  type_alias: 'alias',
  constant: 'const',
  function: 'fn',
  extern: 'extern',
  field: 'field',
};

// Bold the matched substring within a result name.
function highlightMatch(name: string, query: string) {
  const i = name.toLowerCase().indexOf(query.toLowerCase());
  if (!query || i === -1) return name;
  return (
    <>
      {name.slice(0, i)}
      <span className="font-semibold text-accent">{name.slice(i, i + query.length)}</span>
      {name.slice(i + query.length)}
    </>
  );
}

// The inner content of a result row, shared by the desktop and mobile lists.
function ResultContent({ result, query }: { result: SearchResult; query: string }) {
  return (
    <div className="flex items-center gap-2">
      <KindIcon
        kind={result.kind}
        className={cn('size-4 shrink-0', getItemTypeColor(result.kind))}
      />
      <span className={cn('font-mono text-sm', getItemTypeColor(result.kind))}>
        {highlightMatch(result.name, query)}
      </span>
      {result.detail && (
        <span
          className="
          min-w-0 flex-1 truncate font-mono text-xs text-fg-subtle
        "
        >
          {result.detail}
        </span>
      )}
      <span className="ml-auto shrink-0 text-xs text-fg-subtle">{KIND_LABEL[result.kind]}</span>
    </div>
  );
}

export function SearchBar() {
  const { documentation, selectedSource } = useDocumentation();
  const [query, setQuery] = useState('');
  const [isOpen, setIsOpen] = useState(false);
  const [selectedIndex, setSelectedIndex] = useState(0);
  const [isMobileSearchOpen, setIsMobileSearchOpen] = useState(false);
  const navigate = useNavigate();
  const searchRef = useRef<HTMLDivElement>(null);
  const mobileInputRef = useRef<HTMLInputElement>(null);

  const results = useMemo(() => {
    if (!documentation || !query.trim()) {
      return [];
    }
    return searchDocumentation(documentation, query).slice(0, 10); // Limit to 10 results
  }, [documentation, query]);

  // Auto-open on new results and reset the highlighted row when the result set
  // changes. Adjusted during render (previous-value tracking) rather than in
  // an effect, per the react-hooks set-state-in-effect rule.
  const [prevResults, setPrevResults] = useState(results);
  if (prevResults !== results) {
    setPrevResults(results);
    setSelectedIndex(0);
    if (results.length > 0) {
      setIsOpen(true);
    }
  }

  useEffect(() => {
    const handleClickOutside = (event: MouseEvent | TouchEvent) => {
      if (searchRef.current && !searchRef.current.contains(event.target as Node)) {
        setIsOpen(false);
      }
    };

    document.addEventListener('mousedown', handleClickOutside);
    document.addEventListener('touchstart', handleClickOutside);
    return () => {
      document.removeEventListener('mousedown', handleClickOutside);
      document.removeEventListener('touchstart', handleClickOutside);
    };
  }, []);

  useEffect(() => {
    if (isMobileSearchOpen && mobileInputRef.current) {
      mobileInputRef.current.focus();
    }
  }, [isMobileSearchOpen]);

  const handleSelect = (result: SearchResult) => {
    const base =
      result.target.kind === 'module'
        ? buildModuleUrl(result.target.path, selectedSource)
        : buildItemUrl(result.target.path, selectedSource);
    navigate(result.target.anchor ? `${base}##${result.target.anchor}` : base);
    setQuery('');
    setIsOpen(false);
    setIsMobileSearchOpen(false);
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (!isOpen) return;

    switch (e.key) {
      case 'ArrowDown':
        e.preventDefault();
        setSelectedIndex((prev) => (prev + 1) % results.length);
        break;
      case 'ArrowUp':
        e.preventDefault();
        setSelectedIndex((prev) => (prev - 1 + results.length) % results.length);
        break;
      case 'Enter':
        e.preventDefault();
        if (results[selectedIndex]) {
          handleSelect(results[selectedIndex]);
        }
        break;
      case 'Escape':
        setIsOpen(false);
        break;
    }
  };

  return (
    <>
      {/* Mobile: Search button */}
      <button
        onClick={() => setIsMobileSearchOpen(true)}
        className="
          shrink-0 rounded-md border border-edge bg-surface p-2
          transition-colors
          hover:bg-surface-2
          lg:hidden
        "
        aria-label="Search"
      >
        <svg className="size-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"
          />
        </svg>
      </button>

      {/* Desktop: Full search bar */}
      <div
        ref={searchRef}
        className="
        relative hidden
        lg:flex lg:w-full lg:min-w-0 lg:flex-1
      "
      >
        <div className="relative w-full">
          <input
            type="text"
            value={query}
            onChange={(e) => setQuery(e.target.value)}
            onKeyDown={handleKeyDown}
            onFocus={() => results.length > 0 && setIsOpen(true)}
            placeholder="Search documentation..."
            className="
              size-full rounded-md border border-edge bg-surface px-4 py-2
              text-sm
              focus:ring-2 focus:ring-accent focus:outline-none
            "
          />
          <svg
            className="
              pointer-events-none absolute top-1/2 right-3 size-5
              -translate-y-1/2 text-fg-subtle
            "
            fill="none"
            viewBox="0 0 24 24"
            stroke="currentColor"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"
            />
          </svg>
        </div>

        {isOpen && results.length > 0 && (
          <div
            className="
            absolute top-full z-50 mt-1 max-h-96 w-full overflow-y-auto
            rounded-md border border-edge bg-surface shadow-lg
          "
          >
            {results.map((result, index) => (
              <button
                key={`${result.kind}:${result.target.path}:${result.target.anchor ?? ''}:${result.name}`}
                onClick={() => handleSelect(result)}
                className={cn(
                  `
                  w-full px-4 py-2 text-left
                  hover:bg-surface-2
                `,
                  index === selectedIndex ? `bg-surface-2` : ''
                )}
              >
                <ResultContent result={result} query={query} />
              </button>
            ))}
          </div>
        )}
      </div>

      {/* Mobile: Full-screen search modal */}
      {isMobileSearchOpen && (
        <div
          className="
          fixed inset-0 z-50 flex flex-col overflow-hidden bg-canvas
          lg:hidden
        "
        >
          <div className="flex items-stretch gap-2 border-b border-edge p-3">
            <button
              onClick={() => {
                setIsMobileSearchOpen(false);
                setQuery('');
                setIsOpen(false);
              }}
              className="
                flex shrink-0 items-center justify-center rounded-md border
                border-edge bg-surface px-2 transition-colors
                hover:bg-surface-2
              "
              aria-label="Close search"
            >
              <svg className="size-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M6 18L18 6M6 6l12 12"
                />
              </svg>
            </button>
            <div className="relative flex-1">
              <input
                ref={mobileInputRef}
                type="text"
                value={query}
                onChange={(e) => setQuery(e.target.value)}
                onKeyDown={handleKeyDown}
                placeholder="Search documentation..."
                className={cn(
                  'w-full',
                  HEADER_INPUT_HEIGHT,
                  `
                  rounded-md border border-edge bg-surface px-4 text-[16px]
                  focus:ring-2 focus:ring-accent focus:outline-none
                `
                )}
              />
              <svg
                className="
                  absolute top-1/2 right-3 size-5 -translate-y-1/2
                  text-fg-subtle
                "
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"
                />
              </svg>
            </div>
          </div>

          <div className="flex-1 overflow-y-auto">
            {results.length > 0 ? (
              results.map((result, index) => (
                <button
                  key={`${result.kind}:${result.target.path}:${result.target.anchor ?? ''}:${result.name}`}
                  onClick={() => handleSelect(result)}
                  className={cn(
                    `
                    w-full border-b border-edge px-4 py-3 text-left
                    hover:bg-surface-2
                  `,
                    index === selectedIndex ? `bg-surface-2` : ''
                  )}
                >
                  <ResultContent result={result} query={query} />
                </button>
              ))
            ) : query.trim() ? (
              <div className="p-4 text-sm text-fg-subtle">No results found</div>
            ) : (
              <div className="p-4 text-sm text-fg-subtle">Start typing to search...</div>
            )}
          </div>
        </div>
      )}
    </>
  );
}
