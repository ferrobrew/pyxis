// Renderers for enum-value references and inline const values (struct fields,
// array elements, const_reference links). Extracted from ItemView.tsx to keep
// every file under the ~400-line threshold.

import type { JsonConstValue } from '@pyxis/types';

import { Link } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { buildItemUrl } from '../utils/navigation';

// Render an enum-value reference (e.g., `Color::Red`) as clickable links.
// `Color` links to the enum type, `Red` is shown as the variant name.
export function EnumValueRef({ path, modulePath }: { path: string; modulePath: string }) {
  const { selectedSource, documentation } = useDocumentation();
  const segments = path.split('::');
  if (segments.length < 2) {
    return <span className="text-fg">{path}</span>;
  }
  const enumName = segments[0];
  const variantName = segments.slice(1).join('::');

  // Resolve the enum path relative to the current module
  // Try: modulePath::enumName, parent::enumName, then just enumName
  const candidatePaths = [
    `${modulePath}::${enumName}`,
    enumName,
    modulePath.split('::').slice(0, -1).concat([enumName]).join('::'),
  ];
  const resolvedEnumPath = candidatePaths.find((p) => documentation?.items[p]);

  return (
    <span className="font-mono text-fg">
      {resolvedEnumPath ? (
        <Link
          to={buildItemUrl(resolvedEnumPath, selectedSource)}
          className="
            text-kind-module
            hover:underline
          "
        >
          {enumName}
        </Link>
      ) : (
        <span>{enumName}</span>
      )}
      <span className="text-fg-muted">::</span>
      {resolvedEnumPath ? (
        <Link
          to={`${buildItemUrl(resolvedEnumPath, selectedSource)}#variant-${variantName}`}
          className="
            text-kind-enum-variant
            hover:underline
          "
        >
          {variantName}
        </Link>
      ) : (
        <span>{variantName}</span>
      )}
    </span>
  );
}

/// Recursively render a `JsonConstValue` inline (for struct fields and array
/// elements). Mirrors the top-level const value rendering but without the
/// trailing semicolon.
export function ConstValueDisplay({
  value,
  modulePath,
}: {
  value: JsonConstValue;
  modulePath: string;
}) {
  switch (value.kind) {
    case 'int':
    case 'float':
      return <span className="text-fg">{value.value}</span>;
    case 'string':
      return <span className="text-fg">"{value.value}"</span>;
    case 'c_string':
      return <span className="text-fg">c"{value.value}"</span>;
    case 'enum_value':
      return <EnumValueRef path={value.path} modulePath={modulePath} />;
    case 'struct':
      return <ConstValueStruct value={value} modulePath={modulePath} />;
    case 'array':
      return <ConstValueArray value={value} modulePath={modulePath} />;
    case 'const_ref':
      return <ConstRef path={value.path} modulePath={modulePath} />;
  }
}

export function ConstValueStruct({
  value,
  modulePath,
}: {
  value: Extract<JsonConstValue, { kind: 'struct' }>;
  modulePath: string;
}) {
  return (
    <span className="font-mono text-fg">
      {'{ '}
      {value.fields.map((f, i) => (
        <span key={i}>
          {i > 0 && <span className="text-fg-muted">, </span>}
          <span className="text-fg-muted">{f.name}: </span>
          <ConstValueDisplay value={f.value} modulePath={modulePath} />
        </span>
      ))}
      {' }'}
    </span>
  );
}

export function ConstValueArray({
  value,
  modulePath,
}: {
  value: Extract<JsonConstValue, { kind: 'array' }>;
  modulePath: string;
}) {
  return (
    <span className="font-mono text-fg">
      [{' '}
      {value.elements.map((e, i) => (
        <span key={i}>
          {i > 0 && <span className="text-fg-muted">, </span>}
          <ConstValueDisplay value={e} modulePath={modulePath} />
        </span>
      ))}
      {']'}
    </span>
  );
}

export function ConstRef({ path, modulePath }: { path: string; modulePath: string }) {
  const { selectedSource, documentation } = useDocumentation();
  // Try to resolve the const path relative to the current module.
  const candidatePaths = [
    `${modulePath}::${path}`,
    path,
    modulePath.split('::').slice(0, -1).concat([path]).join('::'),
  ];
  const resolvedPath = candidatePaths.find((p) => documentation?.items[p]);

  return (
    <span className="font-mono text-fg">
      {resolvedPath ? (
        <Link
          to={buildItemUrl(resolvedPath, selectedSource)}
          className="
            text-kind-module
            hover:underline
          "
        >
          {path}
        </Link>
      ) : (
        <span>{path}</span>
      )}
    </span>
  );
}
