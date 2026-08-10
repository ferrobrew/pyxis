import { clsx, type ClassValue } from 'clsx';
import { twMerge } from 'tailwind-merge';

/// Combine Tailwind classes, with `tailwind-merge` resolving conflicts so a
/// later class overrides an earlier one (e.g. a caller-supplied `className`
/// prop wins over a component default).
export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}

/// Height applied to the header search input; shared so the header and the
/// mobile overlay align.
export const HEADER_INPUT_HEIGHT = 'h-[60px]';
