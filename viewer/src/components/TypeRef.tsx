import { Link } from 'react-router-dom';
import type { JsonType, JsonCallingConvention } from '@pyxis/types';
import { getRelativePath, isPrimitiveType } from '../utils/pathUtils';

interface TypeRefProps {
  type: JsonType;
  currentModule?: string;
}

function getCallingConvention(cc: JsonCallingConvention): string {
  if (cc === 'c') return '';
  return `extern "${cc}" `;
}

export function TypeRef({ type, currentModule = '' }: TypeRefProps) {
  const renderType = (t: JsonType): React.ReactNode => {
    switch (t.type) {
      case 'raw': {
        const displayPath = currentModule ? getRelativePath(currentModule, t.path) : t.path;
        const isPrimitive = isPrimitiveType(displayPath);

        if (isPrimitive) {
          return <span className="text-purple-600 dark:text-purple-400">{displayPath}</span>;
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
            <span className="text-gray-600 dark:text-purple-300">*const </span>
            {renderType(t.inner)}
          </>
        );

      case 'mut_pointer':
        return (
          <>
            <span className="text-gray-600 dark:text-purple-300">*mut </span>
            {renderType(t.inner)}
          </>
        );

      case 'array':
        return (
          <>
            <span className="text-gray-600 dark:text-purple-300">[</span>
            {renderType(t.inner)}
            <span className="text-gray-600 dark:text-purple-300">; {t.size}]</span>
          </>
        );

      case 'function': {
        const cc = getCallingConvention(t.calling_convention);
        return (
          <span className="font-mono text-sm">
            {cc && <span className="text-orange-600 dark:text-orange-400">{cc}</span>}
            <span className="text-gray-600 dark:text-purple-300">fn(</span>
            {t.arguments.map((arg, i) => (
              <span key={i}>
                {i > 0 && <span className="text-gray-600 dark:text-purple-300">, </span>}
                <span className="text-gray-800 dark:text-purple-100">{arg.name}</span>
                <span className="text-gray-600 dark:text-purple-300">: </span>
                {renderType(arg.type_ref)}
              </span>
            ))}
            <span className="text-gray-600 dark:text-purple-300">)</span>
            {t.return_type && (
              <>
                <span className="text-gray-600 dark:text-purple-300"> -&gt; </span>
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
