import { useCallback, useEffect, useState, type ReactNode } from 'react';
import { useLocation, useNavigate } from 'react-router-dom';

// Briefly-latching clipboard helper used by the copy affordances.
function useCopy(timeout = 1200) {
  const [copied, setCopied] = useState(false);
  useEffect(() => {
    if (!copied) return;
    const t = setTimeout(() => setCopied(false), timeout);
    return () => clearTimeout(t);
  }, [copied, timeout]);
  const copy = useCallback((text: string) => {
    void navigator.clipboard?.writeText(text).then(
      () => setCopied(true),
      () => {}
    );
  }, []);
  return { copied, copy };
}

// Shared chrome for inline row actions (copy, permalink). Centralising this
// keeps every affordance visually identical and stops the row click from
// firing when an action is used. An action renders an icon/glyph plus an
// optional small text label.
function ActionButton({
  icon,
  label,
  title,
  onActivate,
  className = '',
}: {
  icon: ReactNode;
  label?: string;
  title: string;
  onActivate: () => void;
  className?: string;
}) {
  return (
    <button
      type="button"
      onClick={(e) => {
        e.stopPropagation();
        e.preventDefault();
        onActivate();
      }}
      title={title}
      aria-label={title}
      className={`inline-flex items-center gap-1 rounded p-1 text-fg-subtle transition-colors hover:bg-surface-2 hover:text-fg ${className}`}
    >
      {icon}
      {label && <span className="text-xs">{label}</span>}
    </button>
  );
}

function CopyIcon({ checked }: { checked: boolean }) {
  return (
    <svg className="h-3.5 w-3.5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
      {checked ? (
        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 13l4 4L19 7" />
      ) : (
        <path
          strokeLinecap="round"
          strokeLinejoin="round"
          strokeWidth={2}
          d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"
        />
      )}
    </svg>
  );
}

export function CopyButton({
  value,
  title = 'Copy',
  label,
  className = '',
}: {
  value: string;
  title?: string;
  label?: string;
  className?: string;
}) {
  const { copied, copy } = useCopy();
  return (
    <ActionButton
      icon={<CopyIcon checked={copied} />}
      label={label && (copied ? 'copied' : label)}
      title={copied ? 'Copied!' : title}
      onActivate={() => copy(value)}
      className={className}
    />
  );
}

// A `#` permalink that points at an on-page anchor. Uses the double-hash
// convention (`#/route##anchor`) the HashRouter app relies on.
export function AnchorLink({
  targetId,
  label,
  className = '',
}: {
  targetId: string;
  label?: string;
  className?: string;
}) {
  const location = useLocation();
  const navigate = useNavigate();
  return (
    <ActionButton
      icon={<span className="font-mono text-sm leading-none">#</span>}
      label={label}
      title="Link to this"
      onActivate={() => {
        navigate(`${location.pathname}##${targetId}`);
        document.getElementById(targetId)?.scrollIntoView({ behavior: 'smooth', block: 'start' });
      }}
      className={className}
    />
  );
}
