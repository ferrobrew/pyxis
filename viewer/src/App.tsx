import { HashRouter, Routes, Route, Navigate } from 'react-router-dom';
import { ThemeProvider } from './contexts/ThemeContext';
import { DocumentationProvider } from './contexts/DocumentationContext';
import { Header } from './components/Header';
import { Sidebar } from './components/Sidebar';
import { ModuleView } from './components/ModuleView';
import { ItemView } from './components/ItemView';
import { WelcomePage } from './components/WelcomePage';

function AppLayout() {
  return (
    <div className="min-h-screen bg-white dark:bg-slate-950 text-gray-900 dark:text-slate-200">
      <Header />
      <div className="flex h-[calc(100vh-60px)]">
        <Sidebar />
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
