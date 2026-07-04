import { useDocumentation } from '../contexts/DocumentationContext';
import { useNavigate } from 'react-router-dom';
import { useEffect } from 'react';
import { buildModuleUrl } from '../utils/navigation';
import { useDocumentTitle } from '../utils/title';

function Step({ n, children }: { n: number; children: React.ReactNode }) {
  return (
    <li className="flex gap-3">
      <span className="flex-shrink-0 mt-0.5 flex h-5 w-5 items-center justify-center rounded-full border border-edge-strong font-mono text-[11px] text-fg-muted">
        {n}
      </span>
      <span className="text-fg-muted">{children}</span>
    </li>
  );
}

export function WelcomePage() {
  const { documentation, selectedSource } = useDocumentation();
  const navigate = useNavigate();

  useDocumentTitle();

  // If documentation is loaded, navigate to the first module
  useEffect(() => {
    if (documentation) {
      const firstModule = Object.keys(documentation.modules)[0];
      if (firstModule) {
        navigate(buildModuleUrl(firstModule, selectedSource));
      }
    }
  }, [documentation, navigate, selectedSource]);

  return (
    <div className="flex min-h-full items-center justify-center p-6 md:p-10">
      <div className="w-full max-w-xl">
        <p className="font-mono text-xs uppercase tracking-[0.2em] text-fg-subtle">
          Pyxis documentation
        </p>
        <h1 className="mt-2 font-mono text-5xl font-bold tracking-tight text-fg">
          pyxis<span className="text-accent">doc</span>
        </h1>
        <p className="mt-4 text-lg text-fg-muted">
          A rustdoc-style viewer for Pyxis JSON output — browse types, enums, bitflags, functions,
          and modules.
        </p>

        <div className="mt-10">
          <h2 className="font-mono text-xs font-semibold uppercase tracking-[0.15em] text-fg-subtle">
            Getting started
          </h2>
          <ol className="mt-4 space-y-3 text-sm">
            <Step n={1}>
              Select <span className="text-fg">Local</span> from the dropdown and click{' '}
              <span className="text-fg">Browse</span> to load a JSON file
            </Step>
            <Step n={2}>Or pick a project from the dropdown to load it from GitHub</Step>
            <Step n={3}>Navigate modules and types using the sidebar</Step>
            <Step n={4}>Use the search bar to jump straight to an item</Step>
          </ol>
        </div>
      </div>
    </div>
  );
}
