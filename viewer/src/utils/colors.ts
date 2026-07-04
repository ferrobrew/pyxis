// Color schemes for different item types.
// These map onto the warm semantic `kind-*` tokens defined in index.css, so
// they flip between the parchment and sepia themes automatically.
export type ItemType =
  | 'module'
  | 'type'
  | 'enum'
  | 'enum-variant'
  | 'bitflags'
  | 'type_alias'
  | 'constant'
  | 'function'
  | 'extern'
  | 'field';

export function getItemTypeColor(type: ItemType): string {
  switch (type) {
    case 'module':
      return 'text-kind-module';
    case 'type':
      return 'text-kind-type';
    case 'enum':
      return 'text-kind-enum';
    case 'enum-variant':
      return 'text-kind-enum-variant';
    case 'bitflags':
      return 'text-kind-bitflags';
    case 'type_alias':
      return 'text-kind-alias';
    case 'constant':
      return 'text-kind-constant';
    case 'function':
      return 'text-kind-function';
    case 'extern':
      return 'text-kind-extern';
    case 'field':
      return 'text-fg-muted';
    default:
      return 'text-fg-muted';
  }
}

export function getItemTypeHoverColor(type: ItemType): string {
  switch (type) {
    case 'module':
      return 'hover:text-kind-module-hover';
    case 'type':
      return 'hover:text-kind-type-hover';
    case 'enum':
      return 'hover:text-kind-enum-hover';
    case 'enum-variant':
      return 'hover:text-kind-enum-variant-hover';
    case 'bitflags':
      return 'hover:text-kind-bitflags-hover';
    case 'type_alias':
      return 'hover:text-kind-alias-hover';
    case 'constant':
      return 'hover:text-kind-constant-hover';
    case 'function':
      return 'hover:text-kind-function-hover';
    case 'extern':
      return 'hover:text-kind-extern-hover';
    case 'field':
      return 'hover:text-accent';
    default:
      return 'hover:text-fg';
  }
}
