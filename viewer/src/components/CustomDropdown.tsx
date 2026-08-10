import { useState, useRef, useEffect, useMemo, useCallback } from 'react';
import { cn, HEADER_INPUT_HEIGHT } from '../utils/styles';

type DropdownOption = {
  value: string;
  label: string;
  datetime?: string;
  group?: string;
};

type CustomDropdownProps = {
  value: string;
  onChange: (value: string) => void;
  options: DropdownOption[];
  disabled?: boolean;
};

type RenderGroup = {
  name?: string;
  options: { option: DropdownOption; index: number }[];
};

// Label + optional subtitle, used by option buttons, group headers, and
// the main dropdown button.
function TileContent({ label, subtitle }: { label: string; subtitle?: string }) {
  return (
    <div className="flex min-w-0 flex-col">
      <span className="truncate text-sm text-fg">{label}</span>
      {subtitle && <span className="mt-0.5 truncate text-xs text-fg-subtle">{subtitle}</span>}
    </div>
  );
}

// A clickable option in the dropdown list. Used both for flat options and
// inside expanded group sublists.
function OptionButton({
  option,
  index,
  focused,
  selected,
  onClick,
  onHover,
  indented = false,
}: {
  option: DropdownOption;
  index: number;
  focused: boolean;
  selected: boolean;
  onClick: () => void;
  onHover: () => void;
  indented?: boolean;
}) {
  return (
    <button
      key={option.value}
      type="button"
      data-index={index}
      onClick={onClick}
      onMouseEnter={onHover}
      className={cn(
        `
        w-full px-3 py-2 text-left
        hover:bg-surface-2
        focus:bg-surface-2 focus:outline-none
      `,
        indented ? `pl-6` : '',
        focused ? `bg-surface-2` : '',
        selected
          ? `
        bg-accent-soft
      `
          : ''
      )}
    >
      <TileContent label={option.label} subtitle={option.datetime} />
    </button>
  );
}

// Chevron SVG used by group headers to signal expand/collapse.
function Chevron({ open }: { open: boolean }) {
  return (
    <svg
      className={cn(
        'ml-2 size-3 shrink-0 transition-transform',
        open
          ? `
        rotate-90
      `
          : ''
      )}
      fill="none"
      viewBox="0 0 24 24"
      stroke="currentColor"
    >
      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5l7 7-7 7" />
    </svg>
  );
}

export function CustomDropdown({ value, onChange, options, disabled }: CustomDropdownProps) {
  const [isOpen, setIsOpen] = useState(false);
  const [focusedIndex, setFocusedIndex] = useState(-1);
  const [expandedGroups, setExpandedGroups] = useState<Set<number>>(new Set());
  const dropdownRef = useRef<HTMLDivElement>(null);
  const buttonRef = useRef<HTMLButtonElement>(null);

  const selectedOption = options.find((opt) => opt.value === value) || options[0];

  // Close the dropdown. Resets the transient open state together so the next
  // open starts fresh: focused selection and expanded groups. Doing the reset
  // here (at the transition) instead of in an effect keeps the state changes
  // at the event boundary, which the react-hooks set-state-in-effect rule
  // requires. `useCallback` keeps the identity stable so the click-outside
  // effect's dependency list doesn't churn every render.
  const close = useCallback(() => {
    setIsOpen(false);
    setFocusedIndex(-1);
    setExpandedGroups(new Set());
  }, []);

  // Pre-group options: consecutive options sharing a `group` string form
  // one group. Ungrouped options are each their own singleton group.
  const renderGroups: RenderGroup[] = useMemo(() => {
    const groups: RenderGroup[] = [];
    let current: RenderGroup | null = null;
    options.forEach((option, index) => {
      const name = option.group;
      if (!current || current.name !== name) {
        current = { name, options: [] };
        groups.push(current);
      }
      current.options.push({ option, index });
    });
    return groups;
  }, [options]);

  useEffect(() => {
    const handleClickOutside = (event: MouseEvent | TouchEvent) => {
      if (dropdownRef.current && !dropdownRef.current.contains(event.target as Node)) {
        close();
      }
    };

    if (isOpen) {
      document.addEventListener('mousedown', handleClickOutside);
      document.addEventListener('touchstart', handleClickOutside);
      return () => {
        document.removeEventListener('mousedown', handleClickOutside);
        document.removeEventListener('touchstart', handleClickOutside);
      };
    }
  }, [isOpen, close]);

  useEffect(() => {
    if (isOpen && focusedIndex >= 0 && dropdownRef.current) {
      const optionElement = dropdownRef.current.querySelector(
        `[data-index="${focusedIndex}"]`
      ) as HTMLElement;
      optionElement?.scrollIntoView({ block: 'nearest' });
    }
  }, [focusedIndex, isOpen]);

  const handleKeyDown = (event: React.KeyboardEvent) => {
    if (disabled) return;

    switch (event.key) {
      case 'Enter':
      case ' ':
        if (isOpen && focusedIndex >= 0) {
          event.preventDefault();
          const option = options[focusedIndex];
          onChange(option.value);
          close();
        } else if (!isOpen) {
          event.preventDefault();
          setIsOpen(true);
        }
        break;
      case 'Escape':
        close();
        buttonRef.current?.focus();
        break;
      case 'ArrowDown':
        event.preventDefault();
        if (!isOpen) {
          setIsOpen(true);
        } else {
          setFocusedIndex((prev) => (prev < options.length - 1 ? prev + 1 : prev));
        }
        break;
      case 'ArrowUp':
        event.preventDefault();
        if (isOpen) {
          setFocusedIndex((prev) => (prev > 0 ? prev - 1 : 0));
        }
        break;
    }
  };

  const handleOptionClick = (optionValue: string) => {
    onChange(optionValue);
    close();
  };

  const toggleGroup = (gi: number) => {
    setExpandedGroups((prev) => {
      const next = new Set(prev);
      if (next.has(gi)) next.delete(gi);
      else next.add(gi);
      return next;
    });
  };

  const renderOptionButton = (option: DropdownOption, index: number, indented = false) => (
    <OptionButton
      key={option.value}
      option={option}
      index={index}
      focused={index === focusedIndex}
      selected={option.value === value}
      onClick={() => handleOptionClick(option.value)}
      onHover={() => setFocusedIndex(index)}
      indented={indented}
    />
  );

  return (
    <div
      className="
      relative w-full
      lg:w-100
    "
      ref={dropdownRef}
    >
      <button
        ref={buttonRef}
        type="button"
        onClick={() => !disabled && setIsOpen(!isOpen)}
        onKeyDown={handleKeyDown}
        disabled={disabled}
        className={cn(
          'w-full',
          HEADER_INPUT_HEIGHT,
          `
          flex items-center justify-between rounded-md border border-edge
          bg-surface px-3 text-left text-sm text-fg
          focus:ring-2 focus:ring-accent focus:outline-none
          disabled:opacity-50
        `
        )}
        aria-haspopup="listbox"
        aria-expanded={isOpen}
      >
        <div className="flex min-w-0 flex-1 flex-col items-start">
          <span className="w-full truncate">{selectedOption.label}</span>
          {selectedOption.datetime && (
            <span className="mt-0.5 text-xs text-fg-subtle">{selectedOption.datetime}</span>
          )}
        </div>
        <svg
          className={cn(
            'ml-2 size-4 shrink-0 transition-transform',
            isOpen
              ? `
            rotate-180
          `
              : ''
          )}
          fill="none"
          viewBox="0 0 24 24"
          stroke="currentColor"
        >
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 9l-7 7-7-7" />
        </svg>
      </button>

      {isOpen && (
        <div
          className="
          absolute top-full z-50 mt-1 w-full rounded-md border border-edge
          bg-surface shadow-lg
        "
        >
          {renderGroups.map((group, gi) => {
            // Single-option groups (or ungrouped) render flat.
            if (!group.name || group.options.length === 1) {
              return (
                <div key={gi}>
                  {group.options.map(({ option, index }) => renderOptionButton(option, index))}
                </div>
              );
            }

            // Multi-option group: clickable header with chevron, expands
            // to show subitems inline underneath (indented).
            const hasSelected = group.options.some(({ option }) => option.value === value);
            const isExpanded = expandedGroups.has(gi) || hasSelected;
            const versions = group.options
              .map(({ option }) => {
                const m = option.label.match(/\(([^)]+)\)/);
                return m ? m[1] : option.label;
              })
              .join(' · ');

            return (
              <div key={gi}>
                <button
                  type="button"
                  onClick={() => toggleGroup(gi)}
                  className={cn(
                    `
                    flex w-full items-center justify-between px-3 py-2 text-left
                    hover:bg-surface-2
                  `,
                    hasSelected ? `bg-accent-soft` : ''
                  )}
                >
                  <TileContent label={group.name} subtitle={versions} />
                  <Chevron open={isExpanded} />
                </button>
                {isExpanded && (
                  <div>
                    {group.options.map(({ option, index }) =>
                      renderOptionButton(option, index, true)
                    )}
                  </div>
                )}
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
}
