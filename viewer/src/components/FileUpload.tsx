import { useRef, useState, useEffect } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import type { JsonDocumentation } from '@pyxis/types';
import { CustomDropdown } from './CustomDropdown';
import {
  buildModuleUrl,
  registerSource,
  decodeSourceIdentifier,
  extractSourceId,
} from '../utils/navigation';

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
  const {
    setDocumentation,
    setFileName,
    selectedSource,
    setSelectedSource,
    documentation,
    fileName,
  } = useDocumentation();
  const fileInputRef = useRef<HTMLInputElement>(null);
  const navigate = useNavigate();
  const location = useLocation();
  const [availableDocs, setAvailableDocs] = useState<DocEntry[]>([]);
  const [isLoading, setIsLoading] = useState(false);
  const isManualChangeRef = useRef(false);

  // Sync source from URL when route changes (but not when on root - root means Local)
  // Only sync if we're not currently loading or manually changing (to avoid conflicts during project switching)
  useEffect(() => {
    if (isLoading || isManualChangeRef.current) return; // Don't sync while loading or manually changing

    // If we're on root without source, only set to local if we don't have documentation
    if (location.pathname === '/') {
      // Don't change source if we already have documentation loaded
      if (!documentation && selectedSource !== 'local') {
        setSelectedSource('local');
      }
      return;
    }

    const pathMatch = location.pathname.match(/^\/([^/]+)\/(module|item)\//);
    if (pathMatch && availableDocs.length > 0) {
      const sourceId = pathMatch[1];
      if (sourceId === 'local') {
        if (selectedSource !== 'local') {
          setSelectedSource('local');
        }
      } else {
        // Try to decode the source ID to full path
        const decodedSource = decodeSourceIdentifier(sourceId);
        // If decoding worked (got a full path), use it
        if (decodedSource !== sourceId && decodedSource !== selectedSource) {
          setSelectedSource(decodedSource);
        } else {
          // If decoding didn't work, try to find the doc by matching the ID
          const matchingDoc = availableDocs.find((doc) => extractSourceId(doc.path) === sourceId);
          if (matchingDoc && matchingDoc.path !== selectedSource) {
            setSelectedSource(matchingDoc.path);
          }
        }
      }
    }
  }, [
    location.pathname,
    availableDocs,
    selectedSource,
    setSelectedSource,
    documentation,
    isLoading,
  ]);

  // Load documentation when selectedSource changes to a remote source and we don't have docs loaded
  useEffect(() => {
    if (isLoading || !availableDocs.length) return;
    if (selectedSource === 'local') return;

    // Check if we already have documentation for this source
    const docEntry = availableDocs.find((doc) => doc.path === selectedSource);
    if (!docEntry) return;

    // Don't load if we already have this documentation loaded
    if (documentation && fileName === docEntry.name) return;

    // Don't load if we have a local file loaded (check if fileName doesn't match any remote doc)
    if (documentation && fileName) {
      const isLocalFile = !availableDocs.some((doc) => doc.name === fileName);
      if (isLocalFile) return; // We have a local file loaded, don't overwrite it
    }

    // Load the documentation
    setIsLoading(true);
    fetch(BASE_URL + docEntry.path)
      .then((response) => {
        if (!response.ok) {
          throw new Error(`Failed to fetch: ${response.statusText}`);
        }
        return response.json();
      })
      .then((json: JsonDocumentation) => {
        setDocumentation(json);
        setFileName(docEntry.name);
      })
      .catch((error) => {
        console.error('Error loading documentation from GitHub:', error);
      })
      .finally(() => {
        setIsLoading(false);
      });
  }, [selectedSource, availableDocs, documentation, fileName, isLoading]);

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
        // Register all source paths for ID lookup
        index.docs.forEach((doc) => {
          registerSource(doc.path);
        });
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
      setSelectedSource('local'); // Ensure selectedSource is set to 'local' after upload

      // Navigate to first module with source in URL
      const firstModule = Object.keys(json.modules)[0];
      if (firstModule) {
        navigate(buildModuleUrl(firstModule, 'local'));
      } else {
        navigate('/');
      }
    } catch (error) {
      console.error('Error parsing JSON:', error);
      alert('Error parsing JSON file. Please ensure it is a valid Pyxis documentation file.');
    }

    // Reset the input so the same file can be selected again
    if (fileInputRef.current) {
      fileInputRef.current.value = '';
    }
  };

  const handleButtonClick = () => {
    fileInputRef.current?.click();
  };

  const handleSourceChange = async (value: string) => {
    if (value === 'another-local') {
      // Trigger file input for another local project
      fileInputRef.current?.click();
      return;
    }

    // Mark that we're making a manual change to prevent sync effect from interfering
    isManualChangeRef.current = true;

    if (value === 'local') {
      // Only reset if we're switching from a different source
      if (selectedSource !== 'local') {
        setDocumentation(null);
        setFileName(null);
        // Navigate to root when switching to Local
        navigate('/');
      }
      setSelectedSource(value);
      // Allow sync effect to run again after a brief delay
      setTimeout(() => {
        isManualChangeRef.current = false;
      }, 100);
      return;
    }

    // Load from GitHub
    const docEntry = availableDocs.find((doc) => doc.path === value);
    if (!docEntry) {
      isManualChangeRef.current = false;
      return;
    }

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

      // Set selectedSource AFTER loading documentation and BEFORE navigation
      // This ensures the dropdown matches the new project
      setSelectedSource(value);

      // Navigate to first module with source in URL
      const firstModule = Object.keys(json.modules)[0];
      if (firstModule) {
        navigate(buildModuleUrl(firstModule, value));
      } else {
        navigate('/');
      }
    } catch (error) {
      console.error('Error loading documentation from GitHub:', error);
      alert('Error loading documentation from GitHub. Please try again or use a local file.');
      // Don't update selectedSource on error
    } finally {
      setIsLoading(false);
      // Allow sync effect to run again after navigation completes
      setTimeout(() => {
        isManualChangeRef.current = false;
      }, 100);
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

  const dropdownOptions = [
    {
      value: 'local',
      label:
        documentation && selectedSource === 'local'
          ? `Local (${documentation.project_name})`
          : 'Local',
    },
    ...(documentation && selectedSource === 'local'
      ? [{ value: 'another-local', label: 'Another Local Project' }]
      : []),
    ...availableDocs.map((doc) => ({
      value: doc.path,
      label: doc.name,
      datetime: `Updated: ${formatLastModified(doc.last_modified_iso8601)}`,
    })),
  ];

  return (
    <div className="flex items-center gap-3">
      <CustomDropdown
        value={selectedSource}
        onChange={handleSourceChange}
        options={dropdownOptions}
        disabled={isLoading}
      />

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
    </div>
  );
}
