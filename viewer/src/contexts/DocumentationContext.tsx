import { createContext, useContext, useState, useMemo, type ReactNode } from 'react';
import type { JsonDocumentation } from '@pyxis/types';
import { getPredefinedTypes } from '../utils/pathUtils';

interface DocumentationContextType {
  documentation: JsonDocumentation | null;
  setDocumentation: (doc: JsonDocumentation | null) => void;
  fileName: string | null;
  setFileName: (name: string | null) => void;
  selectedSource: string;
  setSelectedSource: (source: string) => void;
  predefinedTypes: Set<string>;
}

const DocumentationContext = createContext<DocumentationContextType | undefined>(undefined);

export function DocumentationProvider({ children }: { children: ReactNode }) {
  const [documentation, setDocumentation] = useState<JsonDocumentation | null>(null);
  const [fileName, setFileName] = useState<string | null>(null);
  const [selectedSource, setSelectedSource] = useState<string>('local');

  const predefinedTypes = useMemo(() => getPredefinedTypes(documentation), [documentation]);

  return (
    <DocumentationContext.Provider
      value={{
        documentation,
        setDocumentation,
        fileName,
        setFileName,
        selectedSource,
        setSelectedSource,
        predefinedTypes,
      }}
    >
      {children}
    </DocumentationContext.Provider>
  );
}

// eslint-disable-next-line react-refresh/only-export-components
export function useDocumentation() {
  const context = useContext(DocumentationContext);
  if (!context) {
    throw new Error('useDocumentation must be used within DocumentationProvider');
  }
  return context;
}
