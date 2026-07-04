import { useEffect } from 'react';
import { useParams, Link, useNavigate } from 'react-router-dom';
import { useDocumentation } from '../contexts/DocumentationContext';
import { findModule, findLongestValidAncestor } from '../utils/pathUtils';
import { useDocumentTitle } from '../utils/title';
import { buildModuleUrl, buildItemUrl, buildRootUrl } from '../utils/navigation';
import { getItemTypeColor, type ItemType } from '../utils/colors';
import { ExternAttributes } from './Attributes';
import { BackendSpliceSection } from './BackendSpliceSection';
import { TypeRef } from './TypeRef';
import { FunctionDisplay } from './FunctionDisplay';
import { Breadcrumbs } from './Breadcrumbs';
import { Markdown } from './Markdown';
import { AnchorLink, CopyButton } from './Actions';
import { SourceLink, SourceName } from './SourceLink';
import { OnThisPage, type TocEntry } from './OnThisPage';
import type {
  JsonExternValue,
  JsonBackend,
  JsonItem,
  JsonFunction,
  JsonSourceLocation,
  JsonDocLink,
} from '@pyxis/types';

interface ModuleData {
  doc?: string | null;
  doc_links?: JsonDocLink[];
  items?: string[];
  submodules?: { [key: string]: unknown };
  extern_values?: JsonExternValue[];
  functions?: JsonFunction[];
  backends?: { [key: string]: unknown };
  source?: JsonSourceLocation | null;
}

function SectionHeader({ anchor, children }: { anchor?: string; children: React.ReactNode }) {
  return (
    <h2 className="group mb-4 flex items-center gap-2 border-b border-edge pb-1.5 text-lg font-semibold text-fg">
      {children}
      {anchor && <AnchorLink targetId={anchor} className="opacity-0 group-hover:opacity-100" />}
    </h2>
  );
}

// A panel that wraps a list of rows.
function Panel({ children }: { children: React.ReactNode }) {
  return <div className="overflow-hidden rounded-md border border-edge bg-surface">{children}</div>;
}

const ROW = 'block p-3 border-b border-edge last:border-0 hover:bg-surface-2 transition-colors';

function kindToItemType(kind: string): ItemType {
  if (kind === 'enum' || kind === 'bitflags' || kind === 'type_alias' || kind === 'constant')
    return kind;
  return 'type';
}

// Extern value display component
function ExternValueItem({ extern: ext }: { extern: JsonExternValue }) {
  const isPrivate = ext.visibility === 'private';
  const nameClasses = isPrivate ? 'font-semibold text-fg-subtle' : 'font-semibold text-fg';

  return (
    <div
      id={`extval-${ext.name}`}
      className="group relative border-b border-edge p-3 last:border-0"
    >
      <div className="absolute right-2 top-2 flex items-center gap-1 opacity-0 transition-opacity group-hover:opacity-100">
        <CopyButton
          value={`0x${ext.address.toString(16)}`}
          title="Copy address"
          label="copy addr"
        />
        <AnchorLink targetId={`extval-${ext.name}`} label="link" />
      </div>
      <div className="font-mono text-sm leading-relaxed">
        <ExternAttributes ext={ext} />
        <div>
          {!isPrivate && <span className="text-fg-muted">pub </span>}
          <span className="text-kind-extern">extern </span>
          <span className={nameClasses}>
            {ext.source ? <SourceName source={ext.source}>{ext.name}</SourceName> : ext.name}
          </span>
          <span className="text-fg-muted">: </span>
          <TypeRef type={ext.type_ref} />
          <span className="text-fg-muted">;</span>
        </div>
      </div>
      {ext.doc && (
        <div className="mt-2 text-sm text-fg-muted">
          <Markdown docLinks={ext.doc_links}>{ext.doc}</Markdown>
        </div>
      )}
    </div>
  );
}

// Item list component
interface ItemListProps {
  items: Array<{ path: string; item: JsonItem }>;
}

function ItemList({ items }: ItemListProps) {
  const { selectedSource } = useDocumentation();
  if (items.length === 0) return null;

  return (
    <div id="types" className="mb-8">
      <SectionHeader anchor="types">Types</SectionHeader>
      <Panel>
        {items.map(({ path, item }: { path: string; item: JsonItem }) => {
          if (!item) return null;
          const name = path.split('::').pop();
          return (
            <Link key={path} to={buildItemUrl(path, selectedSource)} className={ROW}>
              <div className="flex items-center justify-between gap-4">
                <div className="min-w-0">
                  <div
                    className={`font-mono text-sm font-semibold ${getItemTypeColor(
                      kindToItemType(item.kind.type)
                    )}`}
                  >
                    {name}
                  </div>
                  <div className="mt-1 text-xs text-fg-subtle">
                    {item.kind.type} • {item.size} bytes • align {item.alignment}
                  </div>
                </div>
                {item.kind.doc && (
                  <div className="ml-4 max-w-md truncate text-sm text-fg-muted">
                    {item.kind.doc}
                  </div>
                )}
              </div>
            </Link>
          );
        })}
      </Panel>
    </div>
  );
}

// Submodule list component
interface SubmoduleListProps {
  submodules: { [key: string]: unknown };
  parentPath: string;
}

interface SubmoduleData {
  doc?: string | null;
}

function SubmoduleList({ submodules, parentPath }: SubmoduleListProps) {
  const { selectedSource } = useDocumentation();
  if (!submodules || Object.keys(submodules).length === 0) return null;

  return (
    <div id="submodules" className="mb-8">
      <SectionHeader anchor="submodules">Submodules</SectionHeader>
      <Panel>
        {Object.entries(submodules).map(([name, submodule]) => {
          const subPath = parentPath ? `${parentPath}::${name}` : name;
          const data = submodule as SubmoduleData;
          return (
            <Link key={name} to={buildModuleUrl(subPath, selectedSource)} className={ROW}>
              <div className="font-mono text-sm font-semibold text-kind-module">{name}</div>
              {data.doc && <div className="mt-1 text-sm text-fg-muted">{data.doc}</div>}
            </Link>
          );
        })}
      </Panel>
    </div>
  );
}

// Main ModuleView component
export function ModuleView() {
  const { modulePath = '' } = useParams();
  const { documentation, selectedSource } = useDocumentation();
  const navigate = useNavigate();

  useDocumentTitle(decodeURIComponent(modulePath));

  useEffect(() => {
    if (documentation && modulePath !== undefined) {
      const decodedPath = decodeURIComponent(modulePath);
      // Empty path represents root, which is always valid
      if (decodedPath === '') {
        return;
      }
      const moduleRaw = findModule(documentation.modules, decodedPath);

      if (!moduleRaw) {
        // Module doesn't exist, find the longest valid ancestor
        const ancestorPath = findLongestValidAncestor(decodedPath, documentation, false);

        if (ancestorPath === null || ancestorPath === '') {
          // No valid ancestor found, navigate to root with current source
          navigate(buildRootUrl(selectedSource), { replace: true });
        } else {
          // Navigate to the ancestor module
          navigate(buildModuleUrl(ancestorPath, selectedSource), { replace: true });
        }
      }
    }
  }, [documentation, modulePath, navigate, selectedSource]);

  if (!documentation) {
    return (
      <div className="p-8">
        <div className="text-fg-subtle">Please load a documentation file to begin.</div>
      </div>
    );
  }

  const decodedPath = decodeURIComponent(modulePath);
  const moduleRaw = findModule(documentation.modules, decodedPath);

  if (!moduleRaw) {
    // Return null while redirecting (unless it's root, which is handled above)
    if (decodedPath === '') {
      // This shouldn't happen, but handle it gracefully
      return null;
    }
    return null;
  }

  const module = moduleRaw as ModuleData;
  const name = decodedPath.split('::').pop() || decodedPath;

  const items =
    module.items?.map((itemPath: string) => ({
      path: itemPath,
      item: documentation.items[itemPath],
    })) || [];

  const backends = module.backends || {};
  const hasBackendSlot = (slot: 'prologue' | 'epilogue') =>
    Object.values(backends).some((configs) =>
      (configs as JsonBackend[]).some((c) => {
        const s = c[slot];
        // Tagged splices (`for <Type>`) render on the type's page, not here.
        return s && !s.for_type && (s.header || s.definition);
      })
    );

  const toc: TocEntry[] = [];
  if (hasBackendSlot('prologue')) toc.push({ id: 'backend-prologue', label: 'Backend Prologue' });
  if (module.extern_values && module.extern_values.length > 0)
    toc.push({ id: 'extern-values', label: 'Extern Values' });
  if (module.functions && module.functions.length > 0)
    toc.push({ id: 'functions', label: 'Functions' });
  if (items.length > 0) toc.push({ id: 'types', label: 'Types' });
  if (module.submodules && Object.keys(module.submodules).length > 0)
    toc.push({ id: 'submodules', label: 'Submodules' });
  if (hasBackendSlot('epilogue')) toc.push({ id: 'backend-epilogue', label: 'Backend Epilogue' });

  return (
    <div className="mx-auto flex max-w-6xl gap-8 px-4 py-6 md:px-8 lg:px-10">
      <article className="min-w-0 flex-1">
        <div className="mb-4">
          <Breadcrumbs path={decodedPath} />
        </div>

        <div className="mb-6 flex flex-wrap items-baseline justify-between gap-x-4 gap-y-1">
          <h1 className="font-mono text-2xl font-semibold tracking-tight">
            <span className="text-kind-module">mod</span> <span className="text-fg">{name}</span>
          </h1>
          {module.source && <SourceLink source={module.source} />}
        </div>

        {module.doc && (
          <div className="mb-8 border-l-2 border-edge-strong pl-4 text-fg-muted">
            <Markdown docLinks={module.doc_links}>{module.doc}</Markdown>
          </div>
        )}

        {/* Backend Prologues */}
        <BackendSpliceSection backends={backends} slot="prologue" />

        {/* Extern Values */}
        {module.extern_values && module.extern_values.length > 0 && (
          <div id="extern-values" className="mb-8">
            <SectionHeader anchor="extern-values">Extern Values</SectionHeader>
            <Panel>
              {module.extern_values.map((ext: JsonExternValue, idx: number) => (
                <ExternValueItem key={idx} extern={ext} />
              ))}
            </Panel>
          </div>
        )}

        {/* Functions */}
        {module.functions && module.functions.length > 0 && (
          <div id="functions" className="mb-8">
            <SectionHeader anchor="functions">Functions</SectionHeader>
            <Panel>
              {module.functions.map((func: JsonFunction, idx: number) => (
                <FunctionDisplay
                  key={idx}
                  id={`func-${func.name}`}
                  func={func}
                  modulePath={decodedPath}
                />
              ))}
            </Panel>
          </div>
        )}

        {/* Items (Types/Enums/Bitflags) */}
        <ItemList items={items} />

        {/* Submodules */}
        <SubmoduleList submodules={module.submodules || {}} parentPath={decodedPath} />

        {/* Backend Epilogues */}
        <BackendSpliceSection backends={backends} slot="epilogue" />
      </article>

      <OnThisPage entries={toc} />
    </div>
  );
}
