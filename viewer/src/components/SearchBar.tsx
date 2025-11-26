import { useState, useRef, useEffect, useMemo } from 'react';
import { useNavigate } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { searchDocumentation, type SearchResult } from '../utils/searchUtils';
import { buildModuleUrl, buildItemUrl } from '../utils/navigation';

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
    if (result.type === 'module') {
      navigate(buildModuleUrl(result.path, selectedSource));
    } else if (result.type === 'function') {
      const modulePath = result.path.split('::').slice(0, -1).join('::');
      navigate(`${buildModuleUrl(modulePath, selectedSource)}#${result.path.split('::').pop()}`);
    } else {
      navigate(buildItemUrl(result.path, selectedSource));
    }
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
        className="lg:hidden p-2 rounded-md border border-gray-300 dark:border-slate-700 bg-white dark:bg-slate-800 hover:bg-gray-50 dark:hover:bg-slate-700 transition-colors flex-shrink-0"
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
      <div ref={searchRef} className="hidden lg:flex relative w-full flex-1 min-w-0">
        <div className="relative w-full">
          <input
            type="text"
            value={query}
            onChange={(e) => setQuery(e.target.value)}
            onKeyDown={handleKeyDown}
            onFocus={() => results.length > 0 && setIsOpen(true)}
            placeholder="Search documentation..."
            className="w-full h-full px-4 py-2 text-sm border rounded-md bg-white dark:bg-slate-800 border-gray-300 dark:border-slate-800 focus:outline-none focus:ring-2 focus:ring-blue-500 dark:focus:ring-blue-400"
          />
          <svg
            className="absolute right-3 top-1/2 -translate-y-1/2 w-5 h-5 text-gray-400 pointer-events-none"
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
          <div className="absolute top-full mt-1 w-full bg-white dark:bg-slate-800 border border-gray-300 dark:border-slate-800 rounded-md shadow-lg max-h-96 overflow-y-auto z-50">
            {results.map((result, index) => (
              <button
                key={result.path}
                onClick={() => handleSelect(result)}
                className={`w-full px-4 py-2 text-left hover:bg-gray-100 dark:hover:bg-slate-700 ${
                  index === selectedIndex ? 'bg-gray-100 dark:bg-slate-700' : ''
                }`}
              >
                <div className="flex items-center justify-between">
                  <div>
                    <div className="font-mono text-sm text-gray-900 dark:text-slate-200">
                      {result.path}
                    </div>
                    <div className="text-xs text-gray-500 dark:text-slate-500 capitalize">
                      {result.type}
                    </div>
                  </div>
                </div>
              </button>
            ))}
          </div>
        )}
      </div>

      {/* Mobile: Full-screen search modal */}
      {isMobileSearchOpen && (
        <div className="lg:hidden fixed inset-0 z-50 bg-white dark:bg-slate-950 flex flex-col">
          <div className="flex items-center gap-2 p-3 border-b border-gray-200 dark:border-slate-800">
            <button
              onClick={() => {
                setIsMobileSearchOpen(false);
                setQuery('');
                setIsOpen(false);
              }}
              className="p-2 rounded-md border border-gray-300 dark:border-slate-700 bg-white dark:bg-slate-800 hover:bg-gray-50 dark:hover:bg-slate-700 transition-colors flex-shrink-0"
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
                className="w-full px-4 py-2 text-sm border rounded-md bg-white dark:bg-slate-800 border-gray-300 dark:border-slate-800 focus:outline-none focus:ring-2 focus:ring-blue-500 dark:focus:ring-blue-400"
              />
              <svg
                className="absolute right-3 top-2.5 w-5 h-5 text-gray-400"
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
                  key={result.path}
                  onClick={() => handleSelect(result)}
                  className={`w-full px-4 py-3 text-left border-b border-gray-200 dark:border-slate-800 hover:bg-gray-100 dark:hover:bg-slate-900 ${
                    index === selectedIndex ? 'bg-gray-100 dark:bg-slate-900' : ''
                  }`}
                >
                  <div className="font-mono text-sm text-gray-900 dark:text-slate-200">
                    {result.path}
                  </div>
                  <div className="text-xs text-gray-500 dark:text-slate-500 capitalize mt-1">
                    {result.type}
                  </div>
                </button>
              ))
            ) : query.trim() ? (
              <div className="p-4 text-sm text-gray-500 dark:text-slate-400">
                No results found
              </div>
            ) : (
              <div className="p-4 text-sm text-gray-500 dark:text-slate-400">
                Start typing to search...
              </div>
            )}
          </div>
        </div>
      )}
    </>
  );
}
