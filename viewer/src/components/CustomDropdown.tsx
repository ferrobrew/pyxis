import { useState, useRef, useEffect } from 'react';

interface DropdownOption {
  value: string;
  label: string;
  datetime?: string;
}

interface CustomDropdownProps {
  value: string;
  onChange: (value: string) => void;
  options: DropdownOption[];
  disabled?: boolean;
}

export function CustomDropdown({ value, onChange, options, disabled }: CustomDropdownProps) {
  const [isOpen, setIsOpen] = useState(false);
  const [focusedIndex, setFocusedIndex] = useState(-1);
  const dropdownRef = useRef<HTMLDivElement>(null);
  const buttonRef = useRef<HTMLButtonElement>(null);

  const selectedOption = options.find((opt) => opt.value === value) || options[0];

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

  return (
    <div className="relative w-full lg:w-100" ref={dropdownRef}>
      <button
        ref={buttonRef}
        type="button"
        onClick={() => !disabled && setIsOpen(!isOpen)}
        onKeyDown={handleKeyDown}
        disabled={disabled}
        className="w-full px-3 py-2 text-sm text-left border border-gray-300 dark:border-slate-700 rounded-md bg-white dark:bg-slate-800 text-gray-900 dark:text-slate-100 focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:opacity-50 flex items-center justify-between"
        aria-haspopup="listbox"
        aria-expanded={isOpen}
      >
        <div className="flex flex-col items-start flex-1 min-w-0">
          <span className="truncate w-full">{selectedOption.label}</span>
          {selectedOption.datetime && (
            <span className="text-xs text-gray-500 dark:text-slate-400 mt-0.5">
              {selectedOption.datetime}
            </span>
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
        <div className="absolute top-full mt-1 w-full bg-white dark:bg-slate-800 border border-gray-300 dark:border-slate-700 rounded-md shadow-lg z-50 max-h-96 overflow-y-auto">
          {options.map((option, index) => (
            <button
              key={option.value}
              type="button"
              data-index={index}
              onClick={() => handleOptionClick(option.value)}
              onMouseEnter={() => setFocusedIndex(index)}
              className={`w-full px-3 py-2 text-left hover:bg-gray-100 dark:hover:bg-slate-700 focus:bg-gray-100 dark:focus:bg-slate-700 focus:outline-none ${
                index === focusedIndex ? 'bg-gray-100 dark:bg-slate-700' : ''
              } ${option.value === value ? 'bg-blue-50 dark:bg-blue-900/20' : ''}`}
            >
              <div className="flex flex-col min-w-0">
                <span className="text-sm text-gray-900 dark:text-slate-100 truncate">
                  {option.label}
                </span>
                {option.datetime && (
                  <span className="text-xs text-gray-500 dark:text-slate-400 mt-0.5 truncate">
                    {option.datetime}
                  </span>
                )}
              </div>
            </button>
          ))}
        </div>
      )}
    </div>
  );
}
