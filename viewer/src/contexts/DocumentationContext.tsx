import { createContext, useContext, useState, type ReactNode } from 'react';
import type { JsonDocumentation } from '@pyxis/types';

interface DocumentationContextType {
  documentation: JsonDocumentation | null;
  setDocumentation: (doc: JsonDocumentation | null) => void;
  fileName: string | null;
  setFileName: (name: string | null) => void;
}

const DocumentationContext = createContext<DocumentationContextType | undefined>(undefined);

export function DocumentationProvider({ children }: { children: ReactNode }) {
  const [documentation, setDocumentation] = useState<JsonDocumentation | null>(null);
  const [fileName, setFileName] = useState<string | null>(null);

  return (
    <DocumentationContext.Provider value={{ documentation, setDocumentation, fileName, setFileName }}>
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
