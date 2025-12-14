import { useState, useMemo, useEffect } from 'react';
import type { JsonRegion, JsonType, JsonDocumentation } from '@pyxis/types';
import { useDocumentation } from '../contexts/DocumentationContext';
import { TypeRef } from './TypeRef';
import { SmallBadge } from './Badge';
import { SourceName } from './SourceLink';

interface NestedFieldViewProps {
  fields: JsonRegion[];
  modulePath: string;
}

interface NestedFieldRowProps {
  field: JsonRegion;
  modulePath: string;
  baseOffset: number;
  depth: number;
  expandedPaths: Set<string>;
  onToggleExpand: (path: string) => void;
  fieldPath: string;
}

/**
 * Get the raw type path from a JsonType, if it's a simple raw type.
 * Returns null for pointers, arrays, functions, etc.
 */
function getRawTypePath(type: JsonType): string | null {
  if (type.type === 'raw') {
    return type.path;
  }
  return null;
}

/**
 * Recursively collect all expandable field paths for default expansion.
 */
function collectExpandablePaths(
  fields: JsonRegion[],
  documentation: JsonDocumentation | null,
  basePath: string = '',
  visited: Set<string> = new Set()
): Set<string> {
  const paths = new Set<string>();

  for (let idx = 0; idx < fields.length; idx++) {
    const field = fields[idx];
    const fieldPath = basePath
      ? `${basePath}.${field.name || idx}`
      : field.name || `field-${idx}`;

    const rawTypePath = getRawTypePath(field.type_ref);
    if (!rawTypePath || visited.has(rawTypePath)) continue;

    const nestedItem = documentation?.items[rawTypePath];
    if (nestedItem?.kind.type === 'type' && nestedItem.kind.fields.length > 0) {
      paths.add(fieldPath);

      // Recurse into nested fields, tracking visited types to avoid cycles
      const nextVisited = new Set(visited).add(rawTypePath);
      const nestedPaths = collectExpandablePaths(
        nestedItem.kind.fields,
        documentation,
        fieldPath,
        nextVisited
      );
      nestedPaths.forEach((p) => paths.add(p));
    }
  }

  return paths;
}

/**
 * A single field row that can be expanded to show nested struct fields.
 */
function NestedFieldRow({
  field,
  modulePath,
  baseOffset,
  depth,
  expandedPaths,
  onToggleExpand,
  fieldPath,
}: NestedFieldRowProps) {
  const { documentation } = useDocumentation();

  const absoluteOffset = baseOffset + field.offset;
  const isPrivate = field.visibility === 'private';

  // Check if this field's type can be expanded (is a struct type)
  const rawTypePath = getRawTypePath(field.type_ref);
  const nestedItem = rawTypePath ? documentation?.items[rawTypePath] : null;
  const isExpandable =
    nestedItem?.kind.type === 'type' && nestedItem.kind.fields.length > 0;
  const isExpanded = expandedPaths.has(fieldPath);

  const nameClasses = isPrivate
    ? 'font-mono text-sm text-gray-500 dark:text-slate-600'
    : 'font-mono text-sm text-gray-900 dark:text-slate-200';
  const typeClasses = isPrivate ? 'opacity-60' : '';

  // Indentation based on depth
  const indentStyle = { paddingLeft: `${depth * 1.5}rem` };

  return (
    <>
      <tr
        id={field.name ? `field-${field.name}` : undefined}
        className="border-b border-gray-200 dark:border-slate-800"
      >
        <td
          className="px-4 py-2 text-sm text-gray-600 dark:text-slate-400 font-mono whitespace-nowrap"
          style={indentStyle}
        >
          <span className="inline-flex items-center gap-2">
            {isExpandable && (
              <button
                onClick={() => onToggleExpand(fieldPath)}
                className="w-4 h-4 flex items-center justify-center text-gray-500 hover:text-gray-700 dark:text-slate-500 dark:hover:text-slate-300 focus:outline-none"
                aria-label={isExpanded ? 'Collapse' : 'Expand'}
              >
                <svg
                  className={`w-3 h-3 transition-transform ${isExpanded ? 'rotate-90' : ''}`}
                  fill="currentColor"
                  viewBox="0 0 20 20"
                >
                  <path
                    fillRule="evenodd"
                    d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
                    clipRule="evenodd"
                  />
                </svg>
              </button>
            )}
            {!isExpandable && <span className="w-4" />}
            0x{absoluteOffset.toString(16).toUpperCase()}
          </span>
        </td>
        <td className={`px-4 py-2 ${nameClasses}`}>
          {field.source ? (
            <SourceName source={field.source}>{field.name || '<anonymous>'}</SourceName>
          ) : (
            field.name || '<anonymous>'
          )}
        </td>
        <td className={`px-4 py-2 font-mono text-sm ${typeClasses}`}>
          <TypeRef type={field.type_ref} currentModule={modulePath} />
        </td>
        <td className="px-4 py-2 text-sm text-gray-600 dark:text-slate-400">
          {field.size}
        </td>
        <td className="px-4 py-2 text-sm text-gray-600 dark:text-slate-400">
          {field.alignment}
        </td>
        <td className="px-4 py-2 text-sm">
          {field.is_base && <SmallBadge variant="violet">base</SmallBadge>}
          {field.doc && (
            <div className="text-gray-600 dark:text-slate-400 mt-1">
              {field.doc}
            </div>
          )}
        </td>
      </tr>

      {/* Render nested fields if expanded */}
      {isExpanded && nestedItem?.kind.type === 'type' && (
        <>
          {nestedItem.kind.fields.map((nestedField, idx) => {
            const nestedFieldPath = `${fieldPath}.${nestedField.name || idx}`;
            return (
              <NestedFieldRow
                key={nestedFieldPath}
                field={nestedField}
                modulePath={modulePath}
                baseOffset={absoluteOffset}
                depth={depth + 1}
                expandedPaths={expandedPaths}
                onToggleExpand={onToggleExpand}
                fieldPath={nestedFieldPath}
              />
            );
          })}
        </>
      )}
    </>
  );
}

/**
 * Nested/tree view for struct fields that allows expanding fields
 * to see their internal structure with relative offsets.
 */
export function NestedFieldView({ fields, modulePath }: NestedFieldViewProps) {
  const { documentation } = useDocumentation();

  // Compute all expandable paths for default expansion
  const initialExpandedPaths = useMemo(
    () => collectExpandablePaths(fields, documentation),
    [fields, documentation]
  );

  const [expandedPaths, setExpandedPaths] = useState<Set<string>>(initialExpandedPaths);

  // Reset expanded paths when fields change (e.g., navigating to a different type)
  useEffect(() => {
    setExpandedPaths(initialExpandedPaths);
  }, [initialExpandedPaths]);

  const handleToggleExpand = (path: string) => {
    setExpandedPaths((prev) => {
      const next = new Set(prev);
      if (next.has(path)) {
        next.delete(path);
      } else {
        next.add(path);
      }
      return next;
    });
  };

  return (
    <div className="overflow-x-auto">
      <table className="w-full border border-gray-200 dark:border-slate-800 rounded-md">
        <thead className="bg-gray-100 dark:bg-slate-800">
          <tr>
            <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
              Offset
            </th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
              Name
            </th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
              Type
            </th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
              Size
            </th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
              Align
            </th>
            <th className="px-4 py-2 text-left text-sm font-semibold text-gray-900 dark:text-slate-200">
              Notes
            </th>
          </tr>
        </thead>
        <tbody>
          {fields.map((field, idx) => {
            const fieldPath = field.name || `field-${idx}`;
            return (
              <NestedFieldRow
                key={fieldPath}
                field={field}
                modulePath={modulePath}
                baseOffset={0}
                depth={0}
                expandedPaths={expandedPaths}
                onToggleExpand={handleToggleExpand}
                fieldPath={fieldPath}
              />
            );
          })}
        </tbody>
      </table>
    </div>
  );
}
