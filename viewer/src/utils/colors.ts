// Color schemes for different item types
export type ItemType =
  | 'module'
  | 'type'
  | 'enum'
  | 'enum-variant'
  | 'bitflags'
  | 'type_alias'
  | 'function'
  | 'extern'
  | 'field';

export function getItemTypeColor(type: ItemType): string {
  switch (type) {
    case 'module':
      return 'text-blue-600 dark:text-blue-400';
    case 'type':
      return 'text-purple-600 dark:text-purple-400';
    case 'enum':
      return 'text-green-600 dark:text-green-400';
    case 'enum-variant':
      return 'text-green-700 dark:text-green-500';
    case 'bitflags':
      return 'text-orange-600 dark:text-orange-400';
    case 'type_alias':
      return 'text-cyan-600 dark:text-cyan-400';
    case 'function':
      return 'text-indigo-600 dark:text-indigo-400';
    case 'extern':
      return 'text-amber-600 dark:text-amber-400';
    case 'field':
      return 'text-gray-600 dark:text-gray-400';
    default:
      return 'text-gray-600 dark:text-gray-400';
  }
}

export function getItemTypeHoverColor(type: ItemType): string {
  switch (type) {
    case 'module':
      return 'hover:text-blue-500 dark:hover:text-blue-300';
    case 'type':
      return 'hover:text-purple-500 dark:hover:text-purple-300';
    case 'enum':
      return 'hover:text-green-500 dark:hover:text-green-300';
    case 'enum-variant':
      return 'hover:text-green-600 dark:hover:text-green-400';
    case 'bitflags':
      return 'hover:text-orange-500 dark:hover:text-orange-300';
    case 'type_alias':
      return 'hover:text-cyan-500 dark:hover:text-cyan-300';
    case 'function':
      return 'hover:text-indigo-500 dark:hover:text-indigo-300';
    case 'extern':
      return 'hover:text-amber-500 dark:hover:text-amber-300';
    case 'field':
      return 'hover:text-blue-600 dark:hover:text-blue-400';
    default:
      return 'hover:text-gray-500 dark:hover:text-gray-300';
  }
}

export function getItemTypeBgColor(type: ItemType): string {
  switch (type) {
    case 'module':
      return 'bg-blue-100 dark:bg-blue-900/30';
    case 'type':
      return 'bg-purple-100 dark:bg-purple-900/30';
    case 'enum':
      return 'bg-green-100 dark:bg-green-900/30';
    case 'enum-variant':
      return 'bg-green-100 dark:bg-green-900/30';
    case 'bitflags':
      return 'bg-orange-100 dark:bg-orange-900/30';
    case 'type_alias':
      return 'bg-cyan-100 dark:bg-cyan-900/30';
    case 'function':
      return 'bg-indigo-100 dark:bg-indigo-900/30';
    case 'extern':
      return 'bg-amber-100 dark:bg-amber-900/30';
    case 'field':
      return 'bg-gray-100 dark:bg-gray-900/30';
    default:
      return 'bg-gray-100 dark:bg-gray-900/30';
  }
}
