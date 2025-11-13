import { useRef, useState, useEffect } from 'react';
import { useDocumentation } from '../contexts/DocumentationContext';
import type { JsonDocumentation } from '@pyxis/types';

const INDEX_URL =
  'https://raw.githubusercontent.com/ferrobrew/pyxis-defs/refs/heads/main/docs/index.json';
const BASE_URL = 'https://raw.githubusercontent.com/ferrobrew/pyxis-defs/refs/heads/main/';

interface DocEntry {
  name: string;
  path: string;
  last_modified_iso8601: string;
}

interface DocsIndex {
  generated_iso8601: string;
  docs: DocEntry[];
}

export function FileUpload() {
  const { setDocumentation, setFileName, fileName } = useDocumentation();
  const fileInputRef = useRef<HTMLInputElement>(null);
  const [availableDocs, setAvailableDocs] = useState<DocEntry[]>([]);
  const [selectedSource, setSelectedSource] = useState<string>('local');
  const [isLoading, setIsLoading] = useState(false);

  // Fetch the index on mount
  useEffect(() => {
    const fetchIndex = async () => {
      try {
        const response = await fetch(INDEX_URL);
        if (!response.ok) {
          console.error('Failed to fetch documentation index');
          return;
        }
        const index: DocsIndex = await response.json();
        setAvailableDocs(index.docs);
      } catch (error) {
        console.error('Error fetching documentation index:', error);
      }
    };

    fetchIndex();
  }, []);

  const handleFileUpload = async (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (!file) return;

    try {
      const text = await file.text();
      const json = JSON.parse(text) as JsonDocumentation;
      setDocumentation(json);
      setFileName(file.name);
    } catch (error) {
      console.error('Error parsing JSON:', error);
      alert('Error parsing JSON file. Please ensure it is a valid Pyxis documentation file.');
    }
  };

  const handleButtonClick = () => {
    fileInputRef.current?.click();
  };

  const handleSourceChange = async (event: React.ChangeEvent<HTMLSelectElement>) => {
    const value = event.target.value;
    setSelectedSource(value);

    if (value === 'local') {
      // Do nothing, user will click browse button
      return;
    }

    // Load from GitHub
    const docEntry = availableDocs.find((doc) => doc.path === value);
    if (!docEntry) return;

    setIsLoading(true);
    try {
      const url = BASE_URL + docEntry.path;
      const response = await fetch(url);
      if (!response.ok) {
        throw new Error(`Failed to fetch: ${response.statusText}`);
      }
      const json = (await response.json()) as JsonDocumentation;
      setDocumentation(json);
      setFileName(docEntry.name);
    } catch (error) {
      console.error('Error loading documentation from GitHub:', error);
      alert('Error loading documentation from GitHub. Please try again or use a local file.');
    } finally {
      setIsLoading(false);
    }
  };

  const formatLastModified = (isoDate: string): string => {
    try {
      const date = new Date(isoDate);
      return date.toLocaleString();
    } catch {
      return isoDate;
    }
  };

  return (
    <div className="flex items-center gap-3">
      <select
        value={selectedSource}
        onChange={handleSourceChange}
        disabled={isLoading}
        className="px-3 py-2 text-sm border border-gray-300 dark:border-slate-700 rounded-md bg-white dark:bg-slate-800 text-gray-900 dark:text-slate-100 focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:opacity-50"
      >
        <option value="local">Local</option>
        {availableDocs.map((doc) => (
          <option key={doc.path} value={doc.path}>
            {doc.name} (Updated: {formatLastModified(doc.last_modified_iso8601)})
          </option>
        ))}
      </select>

      {selectedSource === 'local' && (
        <>
          <input
            ref={fileInputRef}
            type="file"
            accept=".json"
            onChange={handleFileUpload}
            className="hidden"
          />
          <button
            onClick={handleButtonClick}
            className="px-4 py-2 text-sm font-medium bg-blue-600 text-white rounded-md hover:bg-blue-700 dark:bg-blue-500 dark:hover:bg-blue-600 transition-colors"
          >
            Browse
          </button>
        </>
      )}

      {isLoading && <span className="text-sm text-gray-600 dark:text-slate-400">Loading...</span>}
      {!isLoading && fileName && (
        <span className="text-sm text-gray-600 dark:text-slate-400">{fileName}</span>
      )}
    </div>
  );
}
