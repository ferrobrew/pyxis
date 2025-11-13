import { Link } from 'react-router-dom';
import type { JsonType, JsonCallingConvention } from '@pyxis/types';
import { getRelativePath } from '../utils/pathUtils';
import { useDocumentation } from '../contexts/DocumentationContext';

interface TypeRefProps {
  type: JsonType;
  currentModule?: string;
}

function getCallingConvention(cc: JsonCallingConvention): string {
  if (cc === 'c') return '';
  return `extern "${cc}" `;
}

export function TypeRef({ type, currentModule = '' }: TypeRefProps) {
  const { predefinedTypes } = useDocumentation();

  const renderType = (t: JsonType): React.ReactNode => {
    switch (t.type) {
      case 'raw': {
        const displayPath = currentModule
          ? getRelativePath(currentModule, t.path, predefinedTypes)
          : t.path;
        const isPredefined = predefinedTypes.has(t.path);

        if (isPredefined) {
          return <span className="text-violet-600 dark:text-slate-500">{displayPath}</span>;
        }

        return (
          <Link
            to={`/item/${encodeURIComponent(t.path)}`}
            className="text-blue-600 dark:text-blue-400 hover:underline"
          >
            {displayPath}
          </Link>
        );
      }

      case 'const_pointer':
        return (
          <>
            <span className="text-gray-600 dark:text-slate-400">*const </span>
            {renderType(t.inner)}
          </>
        );

      case 'mut_pointer':
        return (
          <>
            <span className="text-gray-600 dark:text-slate-400">*mut </span>
            {renderType(t.inner)}
          </>
        );

      case 'array':
        return (
          <>
            <span className="text-gray-600 dark:text-slate-400">[</span>
            {renderType(t.inner)}
            <span className="text-gray-600 dark:text-slate-400">; {t.size}]</span>
          </>
        );

      case 'function': {
        const cc = getCallingConvention(t.calling_convention);
        return (
          <span className="font-mono text-sm">
            {cc && <span className="text-orange-600 dark:text-orange-400">{cc}</span>}
            <span className="text-gray-600 dark:text-slate-400">fn(</span>
            {t.arguments.map((arg, i) => (
              <span key={i}>
                {i > 0 && <span className="text-gray-600 dark:text-slate-400">, </span>}
                <span className="text-gray-800 dark:text-slate-300">{arg.name}</span>
                <span className="text-gray-600 dark:text-slate-400">: </span>
                {renderType(arg.type_ref)}
              </span>
            ))}
            <span className="text-gray-600 dark:text-slate-400">)</span>
            {t.return_type && (
              <>
                <span className="text-gray-600 dark:text-slate-400"> -&gt; </span>
                {renderType(t.return_type)}
              </>
            )}
          </span>
        );
      }

      default:
        return <span className="text-red-500">Unknown type</span>;
    }
  };

  return <span className="font-mono text-sm">{renderType(type)}</span>;
}
