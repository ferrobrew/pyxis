import { Link } from 'react-router-dom';
import { buildModuleUrl, buildRootUrl } from '../utils/navigation';
import { useDocumentation } from '../contexts/DocumentationContext';
import type { ItemType } from '../utils/colors';

interface BreadcrumbsProps {
  path: string;
  isItem?: boolean;
  itemType?: ItemType;
}

function Separator() {
  return (
    <svg className="mx-2 h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5l7 7-7 7" />
    </svg>
  );
}

export function Breadcrumbs({ path }: BreadcrumbsProps) {
  const { selectedSource } = useDocumentation();

  // The current item/module is the page heading, so the trail shows only its
  // module ancestors — the last path segment is always dropped.
  const segments = path ? path.split('::') : [];
  const crumbs = segments.slice(0, -1);

  if (crumbs.length === 0) {
    return (
      <nav className="flex items-center text-sm text-fg-muted">
        <Link to={buildRootUrl(selectedSource)} className="hover:text-accent">
          Root
        </Link>
      </nav>
    );
  }

  return (
    <nav className="flex items-center text-sm text-fg-muted">
      {crumbs.map((segment, idx) => {
        const partialPath = crumbs.slice(0, idx + 1).join('::');
        return (
          <span key={partialPath} className="flex items-center">
            {idx > 0 && <Separator />}
            <Link to={buildModuleUrl(partialPath, selectedSource)} className="hover:text-accent">
              {segment}
            </Link>
          </span>
        );
      })}
    </nav>
  );
}
