import { useTheme } from '../contexts/ThemeContext';
import { useLigatures } from '../contexts/LigatureContext';
import { useDocumentation } from '../contexts/DocumentationContext';
import { FileUpload } from './FileUpload';
import { SearchBar } from './SearchBar';

interface HeaderProps {
  isSidebarOpen: boolean;
  onToggleSidebar: () => void;
}

const iconButton =
  'p-2 rounded-md border border-edge bg-surface text-fg-muted hover:text-fg hover:bg-surface-2 transition-colors flex-shrink-0';

export function Header({ isSidebarOpen, onToggleSidebar }: HeaderProps) {
  const { theme, toggleTheme } = useTheme();
  const { ligatures, toggleLigatures } = useLigatures();
  const { documentation } = useDocumentation();

  return (
    <header className="flex-shrink-0 border-b border-edge bg-surface">
      <div className="flex items-stretch p-3 gap-2">
        {/* Sidebar toggle button (mobile only) */}
        {documentation && (
          <button
            onClick={onToggleSidebar}
            className={`lg:hidden ${iconButton}`}
            aria-label="Toggle sidebar"
          >
            <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d={isSidebarOpen ? 'M6 18L18 6M6 6l12 12' : 'M4 6h16M4 12h16M4 18h16'}
              />
            </svg>
          </button>
        )}

        {/* Wordmark */}
        <a
          href="#/"
          className="hidden lg:flex items-center px-1 select-none font-mono text-lg font-bold tracking-tight text-fg hover:text-fg flex-shrink-0"
          aria-label="pyxisdoc home"
        >
          pyxis<span className="text-accent">doc</span>
        </a>

        {/* FileUpload with constrained width on mobile */}
        <div className="flex-1 min-w-0 lg:flex-initial lg:flex-shrink-0">
          <FileUpload />
        </div>

        {/* Right side: Search + Ligature toggle + Theme toggle */}
        <div className="flex items-stretch gap-2 min-w-0 lg:flex-1">
          {documentation && <SearchBar />}
          <button
            onClick={toggleLigatures}
            className={iconButton}
            aria-label={ligatures ? 'Disable code ligatures' : 'Enable code ligatures'}
            aria-pressed={ligatures}
            title={ligatures ? 'Disable code ligatures' : 'Enable code ligatures'}
          >
            <span
              className="font-mono text-sm font-semibold leading-none"
              style={{ fontVariantLigatures: ligatures ? 'none' : 'normal' }}
            >
              {'=>'}
            </span>
          </button>
          <button onClick={toggleTheme} className={iconButton} aria-label="Toggle theme">
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
