import { useState, type ReactNode } from 'react';
import { cn } from '../utils/styles';

type CollapsibleProps = {
  title: string;
  children: ReactNode;
  defaultOpen?: boolean;
};

export function Collapsible({ title, children, defaultOpen = false }: CollapsibleProps) {
  const [isOpen, setIsOpen] = useState(defaultOpen);

  return (
    <div className="overflow-hidden rounded-md border border-edge">
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="
          flex w-full items-center justify-between bg-surface p-2
          transition-colors
          hover:bg-surface-2
        "
      >
        <span className="text-sm font-medium text-fg">{title}</span>
        <svg
          className={cn('size-5 transition-transform', isOpen ? 'rotate-180' : '')}
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
