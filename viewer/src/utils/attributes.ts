import type { JsonItem, JsonFunction, JsonExternValue } from '@pyxis/types';
import { formatCfg } from './cfg';
import { formatHexAddress } from './format';

// Attributes are grouped into `#[...]` brackets the way pyxis-defs sources are
// written: structural attributes share one bracket, and `#[cfg(...)]` gets its
// own line. Each inner array is one bracket's comma-separated contents.
export type AttrGroups = string[][];

function clean(groups: AttrGroups): AttrGroups {
  return groups.filter((g) => g.length > 0);
}

export function itemAttributeGroups(item: JsonItem): AttrGroups {
  const primary: string[] = [];
  const kind = item.kind;

  const singleton = kind.type !== 'type_alias' ? kind.singleton : null;
  if (singleton != null) primary.push(`singleton(${formatHexAddress(singleton)})`);

  // Aliases are just `type X = Y;` — no layout/trait attributes.
  if (kind.type !== 'type_alias') {
    primary.push(`size(${formatHexAddress(item.size)})`);
    primary.push(`align(${item.alignment})`);
    if (kind.copyable) primary.push('copyable');
    if (kind.cloneable) primary.push('cloneable');
  }
  if (kind.type === 'type') {
    if (kind.defaultable) primary.push('defaultable');
    if (kind.packed) primary.push('packed');
  }

  // Backend type bindings (mostly for `extern type`s).
  if (item.rust_name) primary.push(`rust_name = "${item.rust_name}"`);
  if (item.cpp_name) primary.push(`cpp_name = "${item.cpp_name}"`);
  if (item.cpp_header) primary.push(`cpp_header = "${item.cpp_header}"`);

  const groups: AttrGroups = [primary];
  if (item.cfg) groups.push([`cfg(${formatCfg(item.cfg)})`]);
  return clean(groups);
}

export function functionAttributeGroups(func: JsonFunction): AttrGroups {
  const primary: string[] = [];
  if (func.calling_convention !== 'c') {
    primary.push(`calling_convention("${func.calling_convention}")`);
  }
  switch (func.body.type) {
    case 'address':
      primary.push(`address(${formatHexAddress(func.body.address)})`);
      break;
    case 'field':
      primary.push(`field(${func.body.field})`);
      break;
    case 'vftable':
      primary.push(`vftable(${func.body.function_name})`);
      break;
    case 'external':
      primary.push('external_body');
      break;
  }

  const groups: AttrGroups = [primary];
  if (func.cfg) groups.push([`cfg(${formatCfg(func.cfg)})`]);
  return clean(groups);
}

export function externAttributeGroups(ext: JsonExternValue): AttrGroups {
  return [[`address(${formatHexAddress(ext.address)})`]];
}
