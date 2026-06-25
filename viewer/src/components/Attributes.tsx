import { Fragment, type ReactNode } from 'react';
import type { JsonItem, JsonFunction, JsonExternValue, JsonCfg } from '@pyxis/types';
import { formatHexAddress } from '../utils/format';
import { WRAP_COLUMN } from '../utils/typeString';

// --- token helpers ---

function Name({ children }: { children: string }) {
  return <span className="text-kind-type">{children}</span>;
}

function Str({ children }: { children: string }) {
  return <span className="text-kind-enum">"{children}"</span>;
}

function Num({ children }: { children: string }) {
  return <span className="text-kind-bitflags">{children}</span>;
}

// --- cfg rendering ---

function renderCfg(cfg: JsonCfg): ReactNode {
  switch (cfg.type) {
    case 'ident':
      return cfg.name;
    case 'key_value':
      return (
        <>
          {cfg.key} = <Str>{cfg.value}</Str>
        </>
      );
    case 'any':
      return <>any({intersperse(cfg.predicates.map(renderCfg))})</>;
    case 'all':
      return <>all({intersperse(cfg.predicates.map(renderCfg))})</>;
    case 'not':
      return <>not({renderCfg(cfg.predicate)})</>;
  }
}

// --- shared #[...] wrapper ---

// Extract plain text from a React node tree, for width-based wrapping.
function nodeText(node: ReactNode): string {
  if (node == null || node === false) return '';
  if (typeof node === 'string') return node;
  if (typeof node === 'number') return String(node);
  if (Array.isArray(node)) return node.map(nodeText).join('');
  if (typeof node === 'object' && 'props' in node) {
    return nodeText((node as { props: { children?: ReactNode } }).props.children);
  }
  return '';
}

function intersperse(nodes: ReactNode[]): ReactNode {
  return nodes.map((node, i) => (
    <Fragment key={i}>
      {i > 0 && ', '}
      {node}
    </Fragment>
  ));
}

function AttrBracket({ attrs }: { attrs: ReactNode[] }) {
  const oneLine = `#[${attrs.map(nodeText).join(', ')}]`;
  if (oneLine.length <= WRAP_COLUMN) {
    return (
      <div>
        #[{intersperse(attrs)}]
      </div>
    );
  }
  // rustfmt-style: open bracket, one attribute per indented line with a
  // trailing comma, then the closing bracket on its own line.
  return (
    <div>
      <div>#[</div>
      {attrs.map((attr, i) => (
        <div key={i} className="pl-4">
          {attr},
        </div>
      ))}
      <div>]</div>
    </div>
  );
}

function AttrContainer({ groups, className = '' }: { groups: ReactNode[][]; className?: string }) {
  if (groups.every((g) => g.length === 0)) return null;
  return (
    <div className={`font-mono text-sm text-fg-subtle ${className}`}>
      {groups.filter((g) => g.length > 0).map((group, i) => (
        <AttrBracket key={i} attrs={group} />
      ))}
    </div>
  );
}

// --- per-entity components ---

export function ItemAttributes({ item, className = '' }: { item: JsonItem; className?: string }) {
  const primary: ReactNode[] = [];
  const kind = item.kind;

  const singleton = kind.type !== 'type_alias' ? kind.singleton : null;
  if (singleton != null) {
    const s = formatHexAddress(singleton);
    primary.push(
      <>
        <Name>singleton</Name>({<Num>{s}</Num>})
      </>,
    );
  }

  // Aliases are just `type X = Y;` — no layout/trait attributes.
  if (kind.type !== 'type_alias') {
    const sz = formatHexAddress(item.size);
    primary.push(
      <>
        <Name>size</Name>({<Num>{sz}</Num>})
      </>,
    );
    primary.push(
      <>
        <Name>align</Name>({<Num>{String(item.alignment)}</Num>})
      </>,
    );
    if (kind.copyable) primary.push(<Name>copyable</Name>);
    if (kind.cloneable) primary.push(<Name>cloneable</Name>);
  }
  if (kind.type === 'type') {
    if (kind.defaultable) primary.push(<Name>defaultable</Name>);
    if (kind.packed) primary.push(<Name>packed</Name>);
  }

  // Backend type bindings (mostly for `extern type`s).
  if (item.rust_name) primary.push(
    <>
      <Name>rust_name</Name> = <Str>{item.rust_name}</Str>
    </>,
  );
  if (item.cpp_name) primary.push(
    <>
      <Name>cpp_name</Name> = <Str>{item.cpp_name}</Str>
    </>,
  );
  if (item.cpp_header) primary.push(
    <>
      <Name>cpp_header</Name> = <Str>{item.cpp_header}</Str>
    </>,
  );

  const groups: ReactNode[][] = [primary];
  if (item.cfg) groups.push([<>
    <Name>cfg</Name>({renderCfg(item.cfg)})
  </>]);
  return <AttrContainer groups={groups} className={className} />;
}

export function FunctionAttributes({ func, className = '' }: { func: JsonFunction; className?: string }) {
  const primary: ReactNode[] = [];
  if (func.calling_convention !== 'c') {
    primary.push(
      <>
        <Name>calling_convention</Name>({<Str>{func.calling_convention}</Str>})
      </>,
    );
  }
  switch (func.body.type) {
    case 'address': {
      const a = formatHexAddress(func.body.address);
      primary.push(
        <>
          <Name>address</Name>({<Num>{a}</Num>})
        </>,
      );
      break;
    }
    case 'field':
      primary.push(
        <>
          <Name>field</Name>({func.body.field})
        </>,
      );
      break;
    case 'vftable':
      primary.push(
        <>
          <Name>vftable</Name>({func.body.function_name})
        </>,
      );
      break;
    case 'external':
      primary.push(<Name>external_body</Name>);
      break;
  }

  const groups: ReactNode[][] = [primary];
  if (func.cfg) groups.push([<>
    <Name>cfg</Name>({renderCfg(func.cfg)})
  </>]);
  return <AttrContainer groups={groups} className={className} />;
}

export function ExternAttributes({ ext, className = '' }: { ext: JsonExternValue; className?: string }) {
  const a = formatHexAddress(ext.address);
  return (
    <AttrContainer
      groups={[[<>
        <Name>address</Name>({<Num>{a}</Num>})
      </>]]}
      className={className}
    />
  );
}
