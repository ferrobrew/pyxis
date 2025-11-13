import { useState, useRef, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { searchDocumentation, type SearchResult } from '../utils/searchUtils';

export function SearchBar() {
  const { documentation } = useDocumentation();
  const [query, setQuery] = useState('');
  const [results, setResults] = useState<SearchResult[]>([]);
  const [isOpen, setIsOpen] = useState(false);
  const [selectedIndex, setSelectedIndex] = useState(0);
  const navigate = useNavigate();
  const searchRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!documentation || !query.trim()) {
      setResults([]);
      setIsOpen(false);
      return;
    }

    const searchResults = searchDocumentation(documentation, query);
    setResults(searchResults.slice(0, 10)); // Limit to 10 results
    setIsOpen(searchResults.length > 0);
    setSelectedIndex(0);
  }, [query, documentation]);

  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (searchRef.current && !searchRef.current.contains(event.target as Node)) {
        setIsOpen(false);
      }
    };

    document.addEventListener('mousedown', handleClickOutside);
    return () => document.removeEventListener('mousedown', handleClickOutside);
  }, []);

  const handleSelect = (result: SearchResult) => {
    if (result.type === 'module') {
      navigate(`/module/${encodeURIComponent(result.path)}`);
    } else if (result.type === 'function') {
      const modulePath = result.path.split('::').slice(0, -1).join('::');
      navigate(`/module/${encodeURIComponent(modulePath)}#${result.path.split('::').pop()}`);
    } else {
      navigate(`/item/${encodeURIComponent(result.path)}`);
    }
    setQuery('');
    setIsOpen(false);
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (!isOpen) return;

    switch (e.key) {
      case 'ArrowDown':
        e.preventDefault();
        setSelectedIndex(prev => (prev + 1) % results.length);
        break;
      case 'ArrowUp':
        e.preventDefault();
        setSelectedIndex(prev => (prev - 1 + results.length) % results.length);
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
    <div ref={searchRef} className="relative w-96">
      <div className="relative">
        <input
          type="text"
          value={query}
          onChange={(e) => setQuery(e.target.value)}
          onKeyDown={handleKeyDown}
          onFocus={() => results.length > 0 && setIsOpen(true)}
          placeholder="Search documentation..."
          className="w-full px-4 py-2 text-sm border rounded-md bg-white dark:bg-gray-800 border-gray-300 dark:border-gray-700 focus:outline-none focus:ring-2 focus:ring-blue-500 dark:focus:ring-blue-400"
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

      {isOpen && results.length > 0 && (
        <div className="absolute top-full mt-1 w-full bg-white dark:bg-gray-800 border border-gray-300 dark:border-gray-700 rounded-md shadow-lg max-h-96 overflow-y-auto">
          {results.map((result, index) => (
            <button
              key={result.path}
              onClick={() => handleSelect(result)}
              className={`w-full px-4 py-2 text-left hover:bg-gray-100 dark:hover:bg-gray-700 ${
                index === selectedIndex ? 'bg-gray-100 dark:bg-gray-700' : ''
              }`}
            >
              <div className="flex items-center justify-between">
                <div>
                  <div className="font-mono text-sm text-gray-900 dark:text-gray-100">
                    {result.path}
                  </div>
                  <div className="text-xs text-gray-500 dark:text-gray-400 capitalize">
                    {result.type}
                  </div>
                </div>
              </div>
            </button>
          ))}
        </div>
      )}
    </div>
  );
}
