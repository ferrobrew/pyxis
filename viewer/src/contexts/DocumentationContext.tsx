import { createContext, useContext, useState, useMemo, type ReactNode } from 'react';
import type { JsonDocumentation } from '@pyxis/types';

interface DocumentationContextType {
  documentation: JsonDocumentation | null;
  setDocumentation: (doc: JsonDocumentation | null) => void;
  fileName: string | null;
  setFileName: (name: string | null) => void;
  predefinedTypes: Set<string>;
}

/**
 * Get all predefined type paths from documentation
 */
export function getPredefinedTypes(documentation: JsonDocumentation | null): Set<string> {
  if (!documentation) return new Set();

  const predefinedTypes = new Set<string>();
  for (const [path, item] of Object.entries(documentation.items)) {
    if (item.category === 'predefined') {
      predefinedTypes.add(path);
    }
  }
  return predefinedTypes;
}

const DocumentationContext = createContext<DocumentationContextType | undefined>(undefined);

export function DocumentationProvider({ children }: { children: ReactNode }) {
  const [documentation, setDocumentation] = useState<JsonDocumentation | null>(null);
  const [fileName, setFileName] = useState<string | null>(null);

  const predefinedTypes = useMemo(() => getPredefinedTypes(documentation), [documentation]);

  return (
    <DocumentationContext.Provider
      value={{ documentation, setDocumentation, fileName, setFileName, predefinedTypes }}
    >
      {children}
    </DocumentationContext.Provider>
  );
}

export function useDocumentation() {
  const context = useContext(DocumentationContext);
  if (!context) {
    throw new Error('useDocumentation must be used within DocumentationProvider');
  }
  return context;
}
