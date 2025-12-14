import { Link } from 'react-router-dom';
import { buildModuleUrl, buildItemUrl } from '../utils/navigation';
import { useDocumentation } from '../contexts/DocumentationContext';
import { getItemTypeColor, getItemTypeHoverColor, type ItemType } from '../utils/colors';

interface BreadcrumbsProps {
  path: string;
  isItem?: boolean;
  itemType?: ItemType;
}

export function Breadcrumbs({ path, isItem = false }: BreadcrumbsProps) {
  const { selectedSource, documentation } = useDocumentation();

  if (!path) {
    // Root module
    return (
      <nav className="mb-4 flex items-center text-sm text-gray-600 dark:text-slate-400">
        <span className="font-semibold text-blue-600 dark:text-blue-400">Root</span>
      </nav>
    );
  }

  const pathSegments = path.split('::');

  // Determine item type if not provided
  let finalItemType: ItemType = 'module';
  if (isItem && documentation) {
    const item = documentation.items[path];
    if (item) {
      if (item.kind.type === 'type') finalItemType = 'type';
      else if (item.kind.type === 'enum') finalItemType = 'enum';
      else if (item.kind.type === 'bitflags') finalItemType = 'bitflags';
      else if (item.kind.type === 'type_alias') finalItemType = 'type_alias';
    }
  } else {
    finalItemType = 'module';
  }

  const typeColor = getItemTypeColor(finalItemType);
  const hoverColor = getItemTypeHoverColor(finalItemType);

  return (
    <nav className="mb-4 flex items-center text-sm text-gray-600 dark:text-slate-400">
      {/* All breadcrumbs including current */}
      {pathSegments.map((segment, idx) => {
        const partialPath = pathSegments.slice(0, idx + 1).join('::');
        const isLast = idx === pathSegments.length - 1;

        return (
          <span key={partialPath} className="flex items-center">
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
            {isLast ? (
              isItem ? (
                <Link
                  to={buildItemUrl(path, selectedSource)}
                  className={`font-semibold ${typeColor} ${hoverColor}`}
                >
                  {segment}
                </Link>
              ) : (
                <span className={`font-semibold ${typeColor}`}>{segment}</span>
              )
            ) : (
              <Link
                to={buildModuleUrl(partialPath, selectedSource)}
                className="hover:text-blue-600 dark:hover:text-blue-400"
              >
                {segment}
              </Link>
            )}
          </span>
        );
      })}
    </nav>
  );
}
