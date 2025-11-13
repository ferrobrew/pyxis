import { Link } from 'react-router-dom';
import { buildModuleUrl, buildItemUrl } from '../utils/navigation';
import { useDocumentation } from '../contexts/DocumentationContext';

interface BreadcrumbsProps {
  path: string;
  isItem?: boolean;
}

export function Breadcrumbs({ path, isItem = false }: BreadcrumbsProps) {
  const { selectedSource } = useDocumentation();

  if (!path) return null;

  // For items, get the module path for breadcrumbs
  const pathToUse = isItem ? path.split('::').slice(0, -1).join('::') : path;
  const pathSegments = pathToUse ? pathToUse.split('::') : [];

  if (pathSegments.length === 0) return null;

  const breadcrumbs = pathSegments.map((segment, idx) => {
    const partialPath = pathSegments.slice(0, idx + 1).join('::');
    return { name: segment, path: partialPath };
  });

  return (
    <nav className="mb-4 flex items-center text-sm text-gray-600 dark:text-slate-400">
      {breadcrumbs.map((crumb, idx) => (
        <span key={crumb.path} className="flex items-center">
          {idx > 0 && (
            <svg className="w-4 h-4 mx-2" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M9 5l7 7-7 7"
              />
            </svg>
          )}
          {idx === breadcrumbs.length - 1 ? (
            <span className="font-semibold text-gray-900 dark:text-slate-200">
              {crumb.name}
            </span>
          ) : (
            <Link
              to={buildModuleUrl(crumb.path, selectedSource)}
              className="hover:text-blue-600 dark:hover:text-blue-400"
            >
              {crumb.name}
            </Link>
          )}
        </span>
      ))}
    </nav>
  );
}

