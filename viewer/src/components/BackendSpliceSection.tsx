import { Collapsible } from './Collapsible';
import { CodeBlock } from './CodeBlock';
import { AnchorLink } from './Actions';
import type { JsonBackend, JsonBackendSplice } from '@pyxis/types';

export type SpliceSlot = 'prologue' | 'epilogue';

// Trim leading/trailing blank lines and remove the largest common
// leading-whitespace prefix shared by every non-blank line. Splices are
// stored as raw-string literals in the source (often
// `r#"<newline>    body<newline>"#`), so without this the rendered block
// carries gratuitous outer padding.
export function normalizeSpliceText(text: string): string {
  const lines = text.split('\n');
  let start = 0;
  let end = lines.length;
  while (start < end && lines[start].trim() === '') start++;
  while (end > start && lines[end - 1].trim() === '') end--;
  const trimmed = lines.slice(start, end);
  if (trimmed.length === 0) return '';

  let minIndent = Infinity;
  for (const line of trimmed) {
    if (line.trim() === '') continue;
    const match = line.match(/^[ \t]*/);
    const indent = match ? match[0].length : 0;
    if (indent < minIndent) minIndent = indent;
  }
  if (!isFinite(minIndent) || minIndent === 0) return trimmed.join('\n');

  return trimmed.map((line) => line.slice(minIndent)).join('\n');
}

function joinSplicePart(splices: JsonBackendSplice[], part: 'header' | 'definition'): string {
  return splices
    .map((s) => s[part])
    .filter((v): v is string => v != null)
    .map(normalizeSpliceText)
    .filter((v) => v.length > 0)
    .join('\n\n');
}

function SectionHeader({ anchor, children }: { anchor?: string; children: React.ReactNode }) {
  return (
    <h2 className="group mb-4 flex items-center gap-2 border-b border-edge pb-1.5 text-lg font-semibold text-fg">
      {children}
      {anchor && <AnchorLink targetId={anchor} className="opacity-0 group-hover:opacity-100" />}
    </h2>
  );
}

// One labeled chunk of splice text (header or definition). `label` is only
// shown when a slot carries both a header and a definition payload (cpp-only),
// so header-only blocks render without noise.
interface SplicePart {
  label: string;
  code: string;
}
interface SpliceBlock {
  slot: SpliceSlot;
  parts: SplicePart[];
  showLabels: boolean;
}

// Collect renderable blocks for a single backend, across the requested slots,
// keeping only splices that pass `filter`. Each slot's header + definition
// payloads are normalized and joined; slots with no text are dropped.
function blocksForBackend(
  configs: JsonBackend[],
  slots: SpliceSlot[],
  filter: (s: JsonBackendSplice) => boolean
): SpliceBlock[] {
  const blocks: SpliceBlock[] = [];
  for (const slot of slots) {
    const splices = configs
      .map((c) => c[slot])
      .filter((s): s is JsonBackendSplice => s != null)
      .filter(filter);
    const header = joinSplicePart(splices, 'header');
    const definition = joinSplicePart(splices, 'definition');
    if (!header && !definition) continue;
    const parts: SplicePart[] = [];
    if (header) parts.push({ label: 'header', code: header });
    if (definition) parts.push({ label: 'definition', code: definition });
    blocks.push({ slot, parts, showLabels: !!definition });
  }
  return blocks;
}

function hasAnyMatching(
  backends: { [key: string]: unknown },
  slots: SpliceSlot[],
  filter: (s: JsonBackendSplice) => boolean
): boolean {
  return Object.values(backends).some((configs) =>
    (configs as JsonBackend[]).some((config) =>
      slots.some((slot) => {
        const s = config[slot];
        return s != null && filter(s) && (s.header || s.definition);
      })
    )
  );
}

interface BackendSpliceSectionProps {
  backends: { [key: string]: unknown };
  slot: SpliceSlot;
}

// Module-page section: renders one slot (prologue OR epilogue), excluding
// any splice tagged `for <Type>` (those render on the type's page instead).
// Drop-in replacement for the old ModuleView-local component.
export function BackendSpliceSection({ backends, slot }: BackendSpliceSectionProps) {
  if (!backends || Object.keys(backends).length === 0) return null;

  const title = slot === 'prologue' ? 'Backend Prologue' : 'Backend Epilogue';
  const slotLabel = slot === 'prologue' ? 'Prologue' : 'Epilogue';
  const sectionMargin = slot === 'prologue' ? 'mb-8' : 'my-8';
  const sectionId = slot === 'prologue' ? 'backend-prologue' : 'backend-epilogue';
  // Module page renders only untagged splices; tagged ones belong to types.
  const filter = (s: JsonBackendSplice) => !s.for_type;

  if (!hasAnyMatching(backends, [slot], filter)) return null;

  return (
    <div id={sectionId} className={sectionMargin}>
      <SectionHeader anchor={sectionId}>{title}</SectionHeader>
      {Object.entries(backends).map(([backendName, configs]) => {
        const blocks = blocksForBackend(configs as JsonBackend[], [slot], filter);
        if (blocks.length === 0) return null;
        const block = blocks[0];
        return (
          <div key={backendName} className="mb-4">
            <h3 className="mb-2 font-mono text-sm font-semibold text-fg-muted">{backendName}</h3>
            <Collapsible title={slotLabel}>
              {block.parts.map((part, idx) => (
                <div key={part.label} className={idx > 0 ? 'border-t border-edge' : ''}>
                  {block.showLabels && (
                    <div className="border-b border-edge bg-surface px-3 py-1 text-[10px] font-semibold uppercase tracking-wider text-fg-subtle">
                      {part.label}
                    </div>
                  )}
                  <CodeBlock code={part.code} language={backendName} />
                </div>
              ))}
            </Collapsible>
          </div>
        );
      })}
    </div>
  );
}

interface TypeBackendCodeProps {
  backends: { [key: string]: unknown };
  itemPath: string;
}

// Type-page section: renders a single "Backend-provided code" block collecting
// every splice (prologue and epilogue, across backends) whose `for_type`
// resolves to this item's path. These are splices the defs author tagged
// `backend <name> <slot> for <Type> r#"..."#;` so that backend-supplied
// functionality (trait impls, etc.) shows up on the type's page rather than
// the module page.
export function TypeBackendCode({ backends, itemPath }: TypeBackendCodeProps) {
  if (!backends || Object.keys(backends).length === 0) return null;

  const slots: SpliceSlot[] = ['prologue', 'epilogue'];
  const filter = (s: JsonBackendSplice) => s.for_type === itemPath;

  if (!hasAnyMatching(backends, slots, filter)) return null;

  return (
    <div id="backend-provided" className="mb-8">
      <SectionHeader anchor="backend-provided">Backend-provided code</SectionHeader>
      {Object.entries(backends).map(([backendName, configs]) => {
        const blocks = blocksForBackend(configs as JsonBackend[], slots, filter);
        if (blocks.length === 0) return null;
        const labelSlots = blocks.length > 1;
        return (
          <div key={backendName} className="mb-4">
            <h3 className="mb-2 font-mono text-sm font-semibold text-fg-muted">{backendName}</h3>
            {blocks.map((block) => (
              <div key={block.slot} className="mb-2 last:mb-0">
                {labelSlots && (
                  <div className="mb-1 text-[10px] font-semibold uppercase tracking-wider text-fg-subtle">
                    {block.slot}
                  </div>
                )}
                <Collapsible title={labelSlots ? block.slot : 'Code'}>
                  {block.parts.map((part, idx) => (
                    <div key={part.label} className={idx > 0 ? 'border-t border-edge' : ''}>
                      {block.showLabels && (
                        <div className="border-b border-edge bg-surface px-3 py-1 text-[10px] font-semibold uppercase tracking-wider text-fg-subtle">
                          {part.label}
                        </div>
                      )}
                      <CodeBlock code={part.code} language={backendName} />
                    </div>
                  ))}
                </Collapsible>
              </div>
            ))}
          </div>
        );
      })}
    </div>
  );
}
