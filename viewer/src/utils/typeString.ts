import type { JsonType, JsonCallingConvention } from '@pyxis/types';
import { getRelativePath } from './pathUtils';

// Shared line-width budget: past this rendered width, declaration lists
// (function arguments, attribute groups) wrap one item per line, rustfmt-style.
export const WRAP_COLUMN = 120;

function ccText(cc: JsonCallingConvention): string {
  return cc === 'c' ? '' : `extern "${cc}" `;
}

// Render a JsonType to its plain-text source form, matching what <TypeRef />
// displays. Used to measure signature width so we can decide when to wrap
// argument/attribute lists rustfmt-style.
export function typeToString(
  type: JsonType,
  currentModule = '',
  predefined: Set<string> = new Set()
): string {
  const r = (t: JsonType): string => {
    switch (t.type) {
      case 'raw':
        return currentModule ? getRelativePath(currentModule, t.path, predefined) : t.path;
      case 'const_pointer':
        return `*const ${r(t.inner)}`;
      case 'mut_pointer':
        return `*mut ${r(t.inner)}`;
      case 'array':
        return `[${r(t.inner)}; ${t.size}]`;
      case 'generic': {
        const base = currentModule ? getRelativePath(currentModule, t.base, predefined) : t.base;
        return `${base}<${t.args.map(r).join(', ')}>`;
      }
      case 'type_parameter':
        return t.name;
      case 'function': {
        const args = t.arguments.map((a) => `${a.name}: ${r(a.type_ref)}`).join(', ');
        const ret = t.return_type ? ` -> ${r(t.return_type)}` : '';
        return `${ccText(t.calling_convention)}fn(${args})${ret}`;
      }
      default:
        return 'Unknown';
    }
  };
  return r(type);
}
