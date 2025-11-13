import { useTheme } from '../contexts/ThemeContext';
import { useDocumentation } from '../contexts/DocumentationContext';
import { FileUpload } from './FileUpload';
import { SearchBar } from './SearchBar';

export function Header() {
  const { theme, toggleTheme } = useTheme();
  const { documentation } = useDocumentation();

  return (
    <header className="sticky top-0 z-50 border-b bg-white dark:bg-gray-900 border-gray-200 dark:border-gray-800">
      <div className="flex items-center justify-between px-6 py-3">
        <div className="flex items-center gap-6">
          <FileUpload />
          {documentation && (
            <div className="text-lg font-semibold text-gray-900 dark:text-gray-100">
              {documentation.project_name}
            </div>
          )}
        </div>

        <div className="flex items-center gap-4">
          {documentation && <SearchBar />}
          <button
            onClick={toggleTheme}
            className="p-2 rounded-md hover:bg-gray-100 dark:hover:bg-gray-800 transition-colors"
            aria-label="Toggle theme"
          >
            {theme === 'light' ? (
              <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M20.354 15.354A9 9 0 018.646 3.646 9.003 9.003 0 0012 21a9.003 9.003 0 008.354-5.646z"
                />
              </svg>
            ) : (
              <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z"
                />
              </svg>
            )}
          </button>
        </div>
      </div>
    </header>
  );
}
