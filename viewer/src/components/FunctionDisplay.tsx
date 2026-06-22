import { useNavigate, useLocation } from 'react-router-dom';
import type { JsonFunction } from '@pyxis/types';
import { TypeRef } from './TypeRef';
import { SourceName } from './SourceLink';
import { formatCfg } from '../utils/cfg';
import { Markdown } from './Markdown';

interface FunctionDisplayProps {
  func: JsonFunction;
  modulePath: string;
  id?: string;
}

export function FunctionDisplay({ func, modulePath, id }: FunctionDisplayProps) {
  const location = useLocation();
  const navigate = useNavigate();
  const isPrivate = func.visibility === 'private';
  const nameClasses = isPrivate ? 'font-semibold text-fg-subtle' : 'font-semibold text-fg';

  // Metadata shown under the signature, dot-separated. Each applicable piece
  // is pushed in order; the separators are spliced in at render time.
  const metaParts: React.ReactNode[] = [];
  if (func.body.type === 'address') {
    metaParts.push(
      <>
        Address: <span className="font-mono">0x{func.body.address.toString(16)}</span>
      </>
    );
  } else if (func.body.type === 'field') {
    metaParts.push(
      <>
        Field: <span className="font-mono">{func.body.field}</span>
      </>
    );
  } else if (func.body.type === 'vftable') {
    metaParts.push(
      <>
        VFTable: <span className="font-mono">{func.body.function_name}</span>
      </>
    );
  }
  if (func.calling_convention !== 'c') {
    metaParts.push(<span className="font-mono">{func.calling_convention}</span>);
  }
  if (func.cfg) {
    metaParts.push(<span className="font-mono">cfg({formatCfg(func.cfg)})</span>);
  }

  // Construct the link URL with anchor
  // Use double hash: first # is for HashRouter route, second # is for anchor
  const linkUrl = id ? `${location.pathname}##${id}` : location.pathname;

  const handleClick = (e: React.MouseEvent) => {
    // Only navigate if clicking directly on the function display, not on nested links
    if (e.target === e.currentTarget || (e.target as HTMLElement).closest('a') === null) {
      navigate(linkUrl);
    }
  };

  return (
    <div
      id={id}
      onClick={handleClick}
      className="cursor-pointer border-b border-edge p-2 transition-colors last:border-b-0 hover:bg-surface-2"
    >
      <div className="flex items-start gap-2">
        <span className="text-kind-function font-mono text-sm">fn</span>
        <div className="flex-1">
          <div className="font-mono text-sm">
            <span className={nameClasses}>
              {func.source ? <SourceName source={func.source}>{func.name}</SourceName> : func.name}
            </span>
            <span className="text-fg-muted">(</span>
            {func.arguments.map((arg, i) => (
              <span key={i}>
                {i > 0 && <span className="text-fg-muted">, </span>}
                {arg.type === 'const_self' && <span className="text-kind-extern">&amp;self</span>}
                {arg.type === 'mut_self' && <span className="text-kind-extern">&amp;mut self</span>}
                {arg.type === 'field' && (
                  <>
                    <span className="text-fg">{arg.name}</span>
                    <span className="text-fg-muted">: </span>
                    <TypeRef type={arg.type_ref} currentModule={modulePath} />
                  </>
                )}
              </span>
            ))}
            <span className="text-fg-muted">)</span>
            {func.return_type && (
              <>
                <span className="text-fg-muted"> -&gt; </span>
                <TypeRef type={func.return_type} currentModule={modulePath} />
              </>
            )}
          </div>
          {func.doc && (
            <div className="mt-2 text-sm text-fg-muted">
              <Markdown>{func.doc}</Markdown>
            </div>
          )}
          {metaParts.length > 0 && (
            <div className="mt-2 text-xs text-fg-subtle">
              {metaParts.map((part, i) => (
                <span key={i}>
                  {i > 0 && ' • '}
                  {part}
                </span>
              ))}
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
