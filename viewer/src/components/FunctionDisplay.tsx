import { useNavigate, useLocation } from 'react-router-dom';
import type { JsonFunction } from '@pyxis/types';
import { TypeRef } from './TypeRef';
import { SourceName } from './SourceLink';
import { Attributes } from './Attributes';
import { AnchorLink, CopyButton } from './Actions';
import { functionAttributeGroups } from '../utils/attributes';
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
  const typeParams = func.method_type_parameters ?? [];
  const address = func.body.type === 'address' ? `0x${func.body.address.toString(16)}` : null;

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
      className="group relative cursor-pointer border-b border-edge p-3 transition-colors last:border-b-0 hover:bg-surface-2"
    >
      {(address || id) && (
        <div className="absolute right-2 top-2 flex items-center gap-1 opacity-0 transition-opacity group-hover:opacity-100">
          {address && <CopyButton value={address} title="Copy address" />}
          {id && <AnchorLink targetId={id} className="px-1" />}
        </div>
      )}
      <div className="font-mono text-sm leading-relaxed">
        <Attributes groups={functionAttributeGroups(func)} />
        <div>
          {!isPrivate && <span className="text-fg-muted">pub </span>}
          <span className="text-kind-function">fn </span>
          <span className={nameClasses}>
            {func.source ? <SourceName source={func.source}>{func.name}</SourceName> : func.name}
          </span>
          {typeParams.length > 0 && (
            <span className="text-kind-enum">&lt;{typeParams.join(', ')}&gt;</span>
          )}
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
          <span className="text-fg-muted">;</span>
        </div>
      </div>
      {func.doc && (
        <div className="mt-2 text-sm text-fg-muted">
          <Markdown>{func.doc}</Markdown>
        </div>
      )}
    </div>
  );
}
