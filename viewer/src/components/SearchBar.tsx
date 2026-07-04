import { useState, useRef, useEffect, useMemo } from 'react';
import { HEADER_INPUT_HEIGHT } from '../utils/styles';
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
        className={`h-4 w-4 flex-shrink-0 ${getItemTypeColor(result.kind)}`}
      />
      <span className={`font-mono text-sm ${getItemTypeColor(result.kind)}`}>
        {highlightMatch(result.name, query)}
      </span>
      {result.detail && (
        <span className="min-w-0 flex-1 truncate font-mono text-xs text-fg-subtle">
          {result.detail}
        </span>
      )}
      <span className="ml-auto flex-shrink-0 text-xs text-fg-subtle">
        {KIND_LABEL[result.kind]}
      </span>
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

  useEffect(() => {
    setIsOpen(results.length > 0 && query.trim().length > 0);
    setSelectedIndex(0);
  }, [results, query]);

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
        className="lg:hidden p-2 rounded-md border border-edge bg-surface hover:bg-surface-2 transition-colors flex-shrink-0"
        aria-label="Search"
      >
        <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"
          />
        </svg>
      </button>

      {/* Desktop: Full search bar */}
      <div ref={searchRef} className="hidden lg:flex relative lg:w-full lg:flex-1 lg:min-w-0">
        <div className="relative w-full">
          <input
            type="text"
            value={query}
            onChange={(e) => setQuery(e.target.value)}
            onKeyDown={handleKeyDown}
            onFocus={() => results.length > 0 && setIsOpen(true)}
            placeholder="Search documentation..."
            className="w-full h-full px-4 py-2 text-sm border rounded-md bg-surface border-edge focus:outline-none focus:ring-2 focus:ring-accent"
          />
          <svg
            className="absolute right-3 top-1/2 -translate-y-1/2 w-5 h-5 text-fg-subtle pointer-events-none"
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
          <div className="absolute top-full mt-1 w-full bg-surface border border-edge rounded-md shadow-lg max-h-96 overflow-y-auto z-50">
            {results.map((result, index) => (
              <button
                key={`${result.kind}:${result.target.path}:${result.target.anchor ?? ''}:${result.name}`}
                onClick={() => handleSelect(result)}
                className={`w-full px-4 py-2 text-left hover:bg-surface-2 ${
                  index === selectedIndex ? 'bg-surface-2' : ''
                }`}
              >
                <ResultContent result={result} query={query} />
              </button>
            ))}
          </div>
        )}
      </div>

      {/* Mobile: Full-screen search modal */}
      {isMobileSearchOpen && (
        <div className="lg:hidden fixed inset-0 z-50 bg-canvas flex flex-col overflow-hidden">
          <div className="flex items-stretch gap-2 p-3 border-b border-edge">
            <button
              onClick={() => {
                setIsMobileSearchOpen(false);
                setQuery('');
                setIsOpen(false);
              }}
              className="px-2 rounded-md border border-edge bg-surface hover:bg-surface-2 transition-colors flex-shrink-0 flex items-center justify-center"
              aria-label="Close search"
            >
              <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
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
                className={`w-full ${HEADER_INPUT_HEIGHT} px-4 text-[16px] border rounded-md bg-surface border-edge focus:outline-none focus:ring-2 focus:ring-accent`}
              />
              <svg
                className="absolute right-3 top-1/2 -translate-y-1/2 w-5 h-5 text-fg-subtle"
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
                  className={`w-full border-b border-edge px-4 py-3 text-left hover:bg-surface-2 ${
                    index === selectedIndex ? 'bg-surface-2' : ''
                  }`}
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
