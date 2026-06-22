import { useState, type ReactNode } from 'react';

interface CollapsibleProps {
  title: string;
  children: ReactNode;
  defaultOpen?: boolean;
}

export function Collapsible({ title, children, defaultOpen = false }: CollapsibleProps) {
  const [isOpen, setIsOpen] = useState(defaultOpen);

  return (
    <div className="border border-edge rounded-md overflow-hidden">
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="w-full flex items-center justify-between p-2 bg-surface hover:bg-surface-2 transition-colors"
      >
        <span className="font-medium text-sm text-fg">{title}</span>
        <svg
          className={`w-5 h-5 transition-transform ${isOpen ? 'rotate-180' : ''}`}
          fill="none"
          viewBox="0 0 24 24"
          stroke="currentColor"
        >
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 9l-7 7-7-7" />
        </svg>
      </button>
      {isOpen && <div className="border-t border-edge">{children}</div>}
    </div>
  );
}
