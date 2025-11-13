import { useDocumentation } from '../contexts/DocumentationContext';
import { useNavigate } from 'react-router-dom';
import { useEffect } from 'react';

export function WelcomePage() {
  const { documentation } = useDocumentation();
  const navigate = useNavigate();

  // If documentation is loaded, navigate to the first module
  useEffect(() => {
    if (documentation) {
      const firstModule = Object.keys(documentation.modules)[0];
      if (firstModule) {
        navigate(`/module/${encodeURIComponent(firstModule)}`);
      }
    }
  }, [documentation, navigate]);

  return (
    <div className="flex items-center justify-center h-full p-8">
      <div className="max-w-2xl text-center">
        <h1 className="text-4xl font-bold mb-4 text-gray-900 dark:text-purple-50">
          Pyxis Documentation Viewer
        </h1>
        <p className="text-lg text-gray-600 dark:text-purple-300 mb-8">
          A rustdoc-style viewer for Pyxis JSON documentation files.
        </p>
        <div className="bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-800 rounded-lg p-6 text-left">
          <h2 className="text-xl font-semibold mb-3 text-gray-900 dark:text-purple-50">
            Getting Started
          </h2>
          <ol className="list-decimal list-inside space-y-2 text-gray-700 dark:text-purple-200">
            <li>Click "Load Documentation" in the top-left corner</li>
            <li>Select a Pyxis JSON documentation file</li>
            <li>Browse modules and types using the sidebar</li>
            <li>Use the search bar to quickly find items</li>
          </ol>
        </div>
        <div className="mt-8 text-sm text-gray-500 dark:text-purple-300">
          <p>
            This viewer displays types, enums, bitflags, functions, and modules from Pyxis-generated
            documentation.
          </p>
        </div>
      </div>
    </div>
  );
}
