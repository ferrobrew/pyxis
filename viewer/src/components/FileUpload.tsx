import { useRef } from 'react';
import { useDocumentation } from '../contexts/DocumentationContext';
import type { JsonDocumentation } from '@pyxis/types';

export function FileUpload() {
  const { setDocumentation, setFileName, fileName } = useDocumentation();
  const fileInputRef = useRef<HTMLInputElement>(null);

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

  return (
    <div className="flex items-center gap-3">
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
        {fileName ? 'Change File' : 'Load Documentation'}
      </button>
      {fileName && <span className="text-sm text-gray-600 dark:text-gray-400">{fileName}</span>}
      {/* Placeholder for future dropdown */}
      <div className="text-gray-400 dark:text-gray-600 text-sm">
        {/* Future: monorepo dropdown will go here */}
      </div>
    </div>
  );
}
