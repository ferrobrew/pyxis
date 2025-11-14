import { useEffect, useState } from 'react';
import { HashRouter, Routes, Route, Navigate, useLocation } from 'react-router-dom';
import { ThemeProvider } from './contexts/ThemeContext';
import { DocumentationProvider } from './contexts/DocumentationContext';
import { Header } from './components/Header';
import { Sidebar } from './components/Sidebar';
import { ModuleView } from './components/ModuleView';
import { ItemView } from './components/ItemView';
import { WelcomePage } from './components/WelcomePage';
import { useDocumentation } from './contexts/DocumentationContext';

function AppLayout() {
  const location = useLocation();
  const { documentation } = useDocumentation();
  const [isSidebarOpen, setIsSidebarOpen] = useState(false);

  // Handle anchor scrolling
  useEffect(() => {
    if (!documentation) return;

    const hash = window.location.hash;
    const doubleHashIndex = hash.indexOf('##');
    if (doubleHashIndex !== -1) {
      const anchor = hash.substring(doubleHashIndex + 2);
      const timeoutId = setTimeout(() => {
        const element = document.getElementById(anchor);
        const mainElement = document.querySelector('main');
        if (element && mainElement) {
          element.scrollIntoView({ behavior: 'smooth', block: 'center' });
        }
      }, 200);

      return () => clearTimeout(timeoutId);
    }
  }, [location, documentation]);

  return (
    <div className="min-h-screen bg-white dark:bg-slate-950 text-gray-900 dark:text-slate-200">
      <Header
        isSidebarOpen={isSidebarOpen}
        onToggleSidebar={() => setIsSidebarOpen(!isSidebarOpen)}
      />
      <div className="flex h-[calc(100vh-60px)]">
        <Sidebar
          isOpen={isSidebarOpen}
          onClose={() => setIsSidebarOpen(false)}
        />
        <main className="flex-1 overflow-y-auto">
          <Routes>
            <Route path="/" element={<WelcomePage />} />
            <Route path="/:source/module/:modulePath" element={<ModuleView />} />
            <Route path="/:source/item/:itemPath" element={<ItemView />} />
            <Route path="*" element={<Navigate to="/" replace />} />
          </Routes>
        </main>
      </div>
    </div>
  );
}

export default function App() {
  return (
    <ThemeProvider>
      <DocumentationProvider>
        <HashRouter>
          <AppLayout />
        </HashRouter>
      </DocumentationProvider>
    </ThemeProvider>
  );
}
