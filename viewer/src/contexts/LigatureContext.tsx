import { createContext, useContext, useEffect, useState, type ReactNode } from 'react';

interface LigatureContextType {
  ligatures: boolean;
  toggleLigatures: () => void;
}

const LigatureContext = createContext<LigatureContextType | undefined>(undefined);

export function LigatureProvider({ children }: { children: ReactNode }) {
  const [ligatures, setLigatures] = useState<boolean>(() => {
    const stored = localStorage.getItem('ligatures');
    // Default to on — JetBrains Mono is designed with ligatures.
    return stored === null ? true : stored === 'true';
  });

  useEffect(() => {
    document.documentElement.classList.toggle('no-ligatures', !ligatures);
    localStorage.setItem('ligatures', String(ligatures));
  }, [ligatures]);

  const toggleLigatures = () => setLigatures((prev) => !prev);

  return (
    <LigatureContext.Provider value={{ ligatures, toggleLigatures }}>
      {children}
    </LigatureContext.Provider>
  );
}

// eslint-disable-next-line react-refresh/only-export-components
export function useLigatures() {
  const context = useContext(LigatureContext);
  if (!context) {
    throw new Error('useLigatures must be used within LigatureProvider');
  }
  return context;
}
