import { useState, useRef, useEffect, useMemo } from 'react';
import { HEADER_INPUT_HEIGHT } from '../utils/styles';

interface DropdownOption {
  value: string;
  label: string;
  datetime?: string;
  group?: string;
}

interface CustomDropdownProps {
  value: string;
  onChange: (value: string) => void;
  options: DropdownOption[];
  disabled?: boolean;
}

interface RenderGroup {
  name?: string;
  options: { option: DropdownOption; index: number }[];
}

// Label + optional subtitle, used by option buttons, group headers, and
// the main dropdown button.
function TileContent({ label, subtitle }: { label: string; subtitle?: string }) {
  return (
    <div className="flex flex-col min-w-0">
      <span className="text-sm text-fg truncate">{label}</span>
      {subtitle && (
        <span className="text-xs text-fg-subtle mt-0.5 truncate">{subtitle}</span>
      )}
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
      className={`w-full px-3 py-2 text-left hover:bg-surface-2 focus:bg-surface-2 focus:outline-none ${
        indented ? 'pl-6' : ''
      } ${focused ? 'bg-surface-2' : ''} ${selected ? 'bg-accent-soft' : ''}`}
    >
      <TileContent label={option.label} subtitle={option.datetime} />
    </button>
  );
}

// Chevron SVG used by group headers to signal expand/collapse.
function Chevron({ open }: { open: boolean }) {
  return (
    <svg
      className={`w-3 h-3 ml-2 flex-shrink-0 transition-transform ${open ? 'rotate-90' : ''}`}
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
        setIsOpen(false);
        setFocusedIndex(-1);
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
  }, [isOpen]);

  useEffect(() => {
    if (isOpen && focusedIndex >= 0 && dropdownRef.current) {
      const optionElement = dropdownRef.current.querySelector(
        `[data-index="${focusedIndex}"]`
      ) as HTMLElement;
      optionElement?.scrollIntoView({ block: 'nearest' });
    }
  }, [focusedIndex, isOpen]);

  // Reset expanded groups when dropdown closes so it starts fresh next time.
  useEffect(() => {
    if (!isOpen) setExpandedGroups(new Set());
  }, [isOpen]);

  const handleKeyDown = (event: React.KeyboardEvent) => {
    if (disabled) return;

    switch (event.key) {
      case 'Enter':
      case ' ':
        if (isOpen && focusedIndex >= 0) {
          event.preventDefault();
          const option = options[focusedIndex];
          onChange(option.value);
          setIsOpen(false);
          setFocusedIndex(-1);
        } else if (!isOpen) {
          event.preventDefault();
          setIsOpen(true);
        }
        break;
      case 'Escape':
        setIsOpen(false);
        setFocusedIndex(-1);
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
    setIsOpen(false);
    setFocusedIndex(-1);
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
    <div className="relative w-full lg:w-100" ref={dropdownRef}>
      <button
        ref={buttonRef}
        type="button"
        onClick={() => !disabled && setIsOpen(!isOpen)}
        onKeyDown={handleKeyDown}
        disabled={disabled}
        className={`w-full ${HEADER_INPUT_HEIGHT} px-3 text-sm text-left border border-edge rounded-md bg-surface text-fg focus:outline-none focus:ring-2 focus:ring-accent disabled:opacity-50 flex items-center justify-between`}
        aria-haspopup="listbox"
        aria-expanded={isOpen}
      >
        <div className="flex flex-col items-start flex-1 min-w-0">
          <span className="truncate w-full">{selectedOption.label}</span>
          {selectedOption.datetime && (
            <span className="text-xs text-fg-subtle mt-0.5">{selectedOption.datetime}</span>
          )}
        </div>
        <svg
          className={`w-4 h-4 ml-2 flex-shrink-0 transition-transform ${
            isOpen ? 'rotate-180' : ''
          }`}
          fill="none"
          viewBox="0 0 24 24"
          stroke="currentColor"
        >
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 9l-7 7-7-7" />
        </svg>
      </button>

      {isOpen && (
        <div className="absolute top-full mt-1 w-full bg-surface border border-edge rounded-md shadow-lg z-50">
          {renderGroups.map((group, gi) => {
            // Single-option groups (or ungrouped) render flat.
            if (!group.name || group.options.length === 1) {
              return (
                <div key={gi}>
                  {group.options.map(({ option, index }) =>
                    renderOptionButton(option, index)
                  )}
                </div>
              );
            }

            // Multi-option group: clickable header with chevron, expands
            // to show subitems inline underneath (indented).
            const hasSelected = group.options.some(
              ({ option }) => option.value === value,
            );
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
                  className={`w-full px-3 py-2 text-left hover:bg-surface-2 flex items-center justify-between ${
                    hasSelected ? 'bg-accent-soft' : ''
                  }`}
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
