import { useEffect } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { getModulePath, findLongestValidAncestor, findModule } from '../utils/pathUtils';
import { useDocumentTitle } from '../utils/title';
import { buildModuleUrl, buildItemUrl, buildRootUrl } from '../utils/navigation';
import { getItemTypeColor } from '../utils/colors';
import { TypeRef } from './TypeRef';
import { ItemAttributes } from './Attributes';
import { Breadcrumbs } from './Breadcrumbs';
import { SourceLink } from './SourceLink';
import { CopyButton } from './Actions';
import { OnThisPage, type TocEntry } from './OnThisPage';
import { TypeBackendCode } from './BackendSpliceSection';
import type { JsonSplice } from '@pyxis/types';
import { itemTypeOfKind, DocBlock } from './itemPrimitives';
import { TypeView, EnumView, BitflagsView, UnionView } from './itemSections';
import { EnumValueRef, ConstValueStruct, ConstValueArray, ConstRef } from './itemValues';

// Keyword shown in the signature header for each item kind, mirroring how the
// declaration is written in pyxis-defs (`pub type Foo`, `pub enum Bar: u32`).
const KIND_KEYWORD: Record<string, string> = {
  type: 'type',
  enum: 'enum',
  bitflags: 'bitflags',
  union: 'union',
  type_alias: 'type',
  constant: 'const',
  extern_value: 'extern',
};

export function ItemView() {
  const { itemPath = '' } = useParams();
  const { documentation, selectedSource } = useDocumentation();
  const navigate = useNavigate();

  useDocumentTitle(decodeURIComponent(itemPath));

  useEffect(() => {
    if (documentation && itemPath) {
      const decodedPath = decodeURIComponent(itemPath);
      const item = documentation.items[decodedPath];

      if (!item) {
        // Item doesn't exist, find the longest valid ancestor
        const ancestorPath = findLongestValidAncestor(decodedPath, documentation, true);

        if (ancestorPath === null || ancestorPath === '') {
          // No valid ancestor found, navigate to root with current source
          navigate(buildRootUrl(selectedSource), { replace: true });
        } else if (ancestorPath === decodedPath) {
          // The item itself exists (shouldn't happen here, but handle it)
          return;
        } else {
          // Check if ancestor is an item or a module
          if (documentation.items[ancestorPath]) {
            // Ancestor is an item, navigate to it
            navigate(buildItemUrl(ancestorPath, selectedSource), { replace: true });
          } else {
            // Ancestor is a module, navigate to it
            navigate(buildModuleUrl(ancestorPath, selectedSource), { replace: true });
          }
        }
      }
    }
  }, [documentation, itemPath, navigate, selectedSource]);

  if (!documentation) {
    return (
      <div className="p-8">
        <div className="text-fg-subtle">Please load a documentation file to begin.</div>
      </div>
    );
  }

  const decodedPath = decodeURIComponent(itemPath);
  const item = documentation.items[decodedPath];

  if (!item) {
    // Return null while redirecting
    return null;
  }

  const modulePath = getModulePath(decodedPath);

  // Backend splices tagged `for <Type>` are stored on the enclosing module;
  // pull them here so the type page can render its own prologue/epilogue.
  const moduleSplices =
    (
      findModule(documentation.modules, modulePath) as {
        splices?: JsonSplice[];
      } | null
    )?.splices ?? [];
  const hasBackendProvided = moduleSplices.some(
    (s) => s.for_type === decodedPath && s.text.trim().length > 0
  );

  // Determine item type for color coding
  const itemType = itemTypeOfKind(item.kind.type);

  const name = decodedPath.split('::').pop() || decodedPath;
  const isExtern = item.category === 'extern';
  // Predefined types (f32, u32, str, ...) are compiler builtins, not
  // user-declared items, so don't dress them up as `pub type f32`.
  const isPredefined = item.category === 'predefined';
  const keyword = isExtern
    ? 'extern type'
    : isPredefined
      ? 'predefined type'
      : (KIND_KEYWORD[item.kind.type] ?? item.kind.type);
  const isPublic = item.visibility === 'public' && !isPredefined;
  const typeParams = item.type_parameters ?? [];
  const underlying =
    item.kind.type === 'enum' || item.kind.type === 'bitflags' ? item.kind.underlying_type : null;
  const aliasTarget = item.kind.type === 'type_alias' ? item.kind.target : null;
  const constValue = item.kind.type === 'constant' ? item.kind.value : null;
  const constValueType = item.kind.type === 'constant' ? item.kind.value_type : null;
  const externValueType = item.kind.type === 'extern_value' ? item.kind.value_type : null;
  const externAddress = item.kind.type === 'extern_value' ? item.kind.address : null;
  // Only the kinds that can be `#[singleton]`-annotated carry an address.
  const singleton =
    item.kind.type === 'type' || item.kind.type === 'enum' || item.kind.type === 'bitflags'
      ? item.kind.singleton
      : null;

  const toc: TocEntry[] = [];
  const k = item.kind;
  if (k.type === 'type') {
    if ((k.nested_items ?? []).length > 0) toc.push({ id: 'nested-items', label: 'Nested Items' });
    if (k.fields.length > 0) toc.push({ id: 'fields', label: 'Fields' });
    if (k.vftable && k.vftable.functions.length > 0)
      toc.push({ id: 'virtual-functions', label: 'Virtual Functions' });
    if (k.associated_functions.length > 0)
      toc.push({ id: 'associated-functions', label: 'Associated Functions' });
  } else if (k.type === 'enum') {
    if ((k.nested_items ?? []).length > 0) toc.push({ id: 'nested-items', label: 'Nested Items' });
    toc.push({ id: 'variants', label: 'Variants' });
    if (k.associated_functions.length > 0)
      toc.push({ id: 'associated-functions', label: 'Associated Functions' });
  } else if (k.type === 'bitflags') {
    if ((k.nested_items ?? []).length > 0) toc.push({ id: 'nested-items', label: 'Nested Items' });
    toc.push({ id: 'flags', label: 'Flags' });
  } else if (k.type === 'union') {
    if ((k.nested_items ?? []).length > 0) toc.push({ id: 'nested-items', label: 'Nested Items' });
    if (k.fields.length > 0) toc.push({ id: 'members', label: 'Members' });
  }
  if (hasBackendProvided) toc.push({ id: 'backend-provided', label: 'Backend-provided' });

  return (
    <div className="mx-auto flex max-w-6xl gap-8 px-4 py-6 md:px-8 lg:px-10">
      <article className="min-w-0 flex-1">
        <div className="mb-4">
          <Breadcrumbs path={decodedPath} isItem={true} itemType={itemType} />
        </div>

        {/* Source-faithful declaration header: attributes, signature, the source
            link, then the doc comment, kept together with a single margin. */}
        <div className="group mb-4">
          <ItemAttributes item={item} />
          <div className="flex flex-wrap items-baseline justify-between gap-x-4 gap-y-1">
            <div className="flex items-start gap-2">
              <h1 className="font-mono text-2xl font-semibold tracking-tight">
                {isPublic && <span className="text-fg-muted">pub </span>}
                <span className={getItemTypeColor(itemType)}>{keyword}</span>{' '}
                <span className="text-fg">{name}</span>
                {typeParams.length > 0 && (
                  <span className="text-kind-enum">&lt;{typeParams.join(', ')}&gt;</span>
                )}
                {underlying && (
                  <>
                    <span className="text-fg-muted">: </span>
                    <TypeRef type={underlying} currentModule={modulePath} />
                  </>
                )}
                {aliasTarget && (
                  <>
                    <span className="text-fg-muted"> = </span>
                    <TypeRef type={aliasTarget} currentModule={modulePath} />
                    <span className="text-fg-muted">;</span>
                  </>
                )}
                {constValueType && (
                  <>
                    <span className="text-fg-muted">: </span>
                    <TypeRef type={constValueType} currentModule={modulePath} />
                    <span className="text-fg-muted"> = </span>
                    {constValue?.kind === 'int' && (
                      <span className="text-fg">{constValue.value}</span>
                    )}
                    {constValue?.kind === 'float' && (
                      <span className="text-fg">{constValue.value}</span>
                    )}
                    {constValue?.kind === 'string' && (
                      <span className="text-fg">"{constValue.value}"</span>
                    )}
                    {constValue?.kind === 'c_string' && (
                      <span className="text-fg">c"{constValue.value}"</span>
                    )}
                    {constValue?.kind === 'enum_value' && (
                      <EnumValueRef path={constValue.path} modulePath={modulePath} />
                    )}
                    {constValue?.kind === 'struct' && (
                      <ConstValueStruct value={constValue} modulePath={modulePath} />
                    )}
                    {constValue?.kind === 'array' && (
                      <ConstValueArray value={constValue} modulePath={modulePath} />
                    )}
                    {constValue?.kind === 'const_ref' && (
                      <ConstRef path={constValue.path} modulePath={modulePath} />
                    )}
                    <span className="text-fg-muted">;</span>
                  </>
                )}
                {externValueType && (
                  <>
                    <span className="text-fg-muted">: </span>
                    <TypeRef type={externValueType} currentModule={modulePath} />
                    <span className="text-fg-muted">;</span>
                  </>
                )}
              </h1>
              {singleton != null && (
                <CopyButton
                  value={`0x${singleton.toString(16)}`}
                  title="Copy singleton address"
                  label="copy addr"
                  className="mt-1 opacity-0 group-hover:opacity-100"
                />
              )}
              {externAddress != null && (
                <CopyButton
                  value={`0x${externAddress.toString(16)}`}
                  title="Copy address"
                  label="copy addr"
                  className="mt-1 opacity-0 group-hover:opacity-100"
                />
              )}
            </div>
            {item.source && <SourceLink source={item.source} />}
          </div>
          {item.kind.doc && (
            <div className="mt-2">
              <DocBlock doc={item.kind.doc} docLinks={item.kind.doc_links} />
            </div>
          )}
        </div>

        {item.kind.type === 'type' && <TypeView def={item.kind} modulePath={modulePath} />}
        {item.kind.type === 'enum' && <EnumView def={item.kind} modulePath={modulePath} />}
        {item.kind.type === 'bitflags' && <BitflagsView def={item.kind} />}
        {item.kind.type === 'union' && <UnionView def={item.kind} modulePath={modulePath} />}
        {hasBackendProvided && <TypeBackendCode splices={moduleSplices} itemPath={decodedPath} />}
      </article>

      <OnThisPage entries={toc} />
    </div>
  );
}
