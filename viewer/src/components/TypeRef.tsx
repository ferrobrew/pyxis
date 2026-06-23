import { Link } from 'react-router-dom';
import type { JsonType, JsonCallingConvention } from '@pyxis/types';
import { getRelativePath } from '../utils/pathUtils';
import { useDocumentation } from '../contexts/DocumentationContext';
import { buildItemUrl } from '../utils/navigation';

interface TypeRefProps {
  type: JsonType;
  currentModule?: string;
}

function getCallingConvention(cc: JsonCallingConvention): string {
  if (cc === 'c') return '';
  return `extern "${cc}" `;
}

export function TypeRef({ type, currentModule = '' }: TypeRefProps) {
  const { predefinedTypes, selectedSource } = useDocumentation();

  const renderType = (t: JsonType): React.ReactNode => {
    switch (t.type) {
      case 'raw': {
        const displayPath = currentModule
          ? getRelativePath(currentModule, t.path, predefinedTypes)
          : t.path;
        const isPredefined = predefinedTypes.has(t.path);

        return (
          <Link
            to={buildItemUrl(t.path, selectedSource)}
            className={
              isPredefined ? 'text-kind-type hover:underline' : 'text-kind-module hover:underline'
            }
          >
            {displayPath}
          </Link>
        );
      }

      case 'const_pointer':
        return (
          <>
            <span className="text-fg-muted">*const </span>
            {renderType(t.inner)}
          </>
        );

      case 'mut_pointer':
        return (
          <>
            <span className="text-fg-muted">*mut </span>
            {renderType(t.inner)}
          </>
        );

      case 'array':
        return (
          <>
            <span className="text-fg-muted">[</span>
            {renderType(t.inner)}
            <span className="text-fg-muted">; {t.size}]</span>
          </>
        );

      case 'generic': {
        // Generic type instantiation, e.g., SharedPtr<GameObject>
        const displayBase = currentModule
          ? getRelativePath(currentModule, t.base, predefinedTypes)
          : t.base;
        const isPredefined = predefinedTypes.has(t.base);

        return (
          <>
            <Link
              to={buildItemUrl(t.base, selectedSource)}
              className={
                isPredefined ? 'text-kind-type hover:underline' : 'text-kind-module hover:underline'
              }
            >
              {displayBase}
            </Link>
            <span className="text-fg-muted">&lt;</span>
            {t.args.map((arg, i) => (
              <span key={i}>
                {i > 0 && <span className="text-fg-muted">, </span>}
                {renderType(arg)}
              </span>
            ))}
            <span className="text-fg-muted">&gt;</span>
          </>
        );
      }

      case 'type_parameter':
        // Type parameter reference, e.g., T inside a generic type
        return <span className="text-kind-enum italic">{t.name}</span>;

      case 'function': {
        const cc = getCallingConvention(t.calling_convention);
        return (
          <span className="font-mono">
            {cc && <span className="text-kind-extern">{cc}</span>}
            <span className="text-fg-muted">fn(</span>
            {t.arguments.map((arg, i) => (
              <span key={i}>
                {i > 0 && <span className="text-fg-muted">, </span>}
                <span className="text-fg">{arg.name}</span>
                <span className="text-fg-muted">: </span>
                {renderType(arg.type_ref)}
              </span>
            ))}
            <span className="text-fg-muted">)</span>
            {t.return_type && (
              <>
                <span className="text-fg-muted"> -&gt; </span>
                {renderType(t.return_type)}
              </>
            )}
          </span>
        );
      }

      default:
        return <span className="text-accent">Unknown type</span>;
    }
  };

  return <span className="font-mono">{renderType(type)}</span>;
}
