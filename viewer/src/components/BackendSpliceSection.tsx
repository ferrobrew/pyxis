import { Collapsible } from './Collapsible';
import { CodeBlock } from './CodeBlock';
import { AnchorLink } from './Actions';
import { renderCfg } from './Attributes';
import type { JsonSplice, JsonCfg } from '@pyxis/types';

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

// Derive a syntax-highlight language from a splice's cfg: a `backend = "x"`
// atom (possibly nested in any/all) picks `x`; otherwise undefined so the
// code renders as plain text. An ungated splice (no cfg) is language-agnostic.
function spliceLanguage(cfg: JsonCfg | null | undefined): string | undefined {
  if (!cfg) return undefined;
  switch (cfg.type) {
    case 'key_value':
      return cfg.key === 'backend' ? cfg.value : undefined;
    case 'any':
    case 'all':
      for (const p of cfg.predicates) {
        const lang = spliceLanguage(p);
        if (lang) return lang;
      }
      return undefined;
    default:
      return undefined;
  }
}

function SectionHeader({ anchor, children }: { anchor?: string; children: React.ReactNode }) {
  return (
    <h2 className="group mb-4 flex items-center gap-2 border-b border-edge pb-1.5 text-lg font-semibold text-fg">
      {children}
      {anchor && <AnchorLink targetId={anchor} className="opacity-0 group-hover:opacity-100" />}
    </h2>
  );
}

// One splice: its `#[cfg(...)]` gate (if any) and a `definition` marker as
// metadata, then the (dedented) code in a collapsible block.
function SpliceItem({ splice }: { splice: JsonSplice }) {
  const code = normalizeSpliceText(splice.text);
  if (!code) return null;
  const language = spliceLanguage(splice.cfg);
  const title = splice.kind === 'prologue' ? 'Prologue' : 'Epilogue';
  return (
    <div className="mb-4">
      <div className="mb-2 flex items-center gap-2 font-mono text-xs text-fg-muted">
        {splice.cfg ? (
          <code className="text-fg-subtle">#[cfg({renderCfg(splice.cfg)})]</code>
        ) : (
          <span className="text-fg-subtle italic">all backends</span>
        )}
        {splice.definition && (
          <span className="rounded bg-surface px-1.5 py-0.5 text-[10px] font-semibold uppercase tracking-wider text-fg-subtle">
            definition
          </span>
        )}
      </div>
      <Collapsible title={title}>
        <CodeBlock code={code} language={language} />
      </Collapsible>
    </div>
  );
}

interface BackendSpliceSectionProps {
  splices: JsonSplice[];
  slot: SpliceSlot;
}

// Module-page section: renders one slot (prologue OR epilogue), excluding any
// splice tagged `for <Type>` (those render on the type's page instead). Each
// splice shows its own cfg gate, so the reader sees which backends it targets.
export function BackendSpliceSection({ splices, slot }: BackendSpliceSectionProps) {
  const matching = (splices ?? []).filter(
    (s) => s.kind === slot && !s.for_type && normalizeSpliceText(s.text).length > 0
  );
  if (matching.length === 0) return null;

  const title = slot === 'prologue' ? 'Backend Prologue' : 'Backend Epilogue';
  const sectionMargin = slot === 'prologue' ? 'mb-8' : 'my-8';
  const sectionId = slot === 'prologue' ? 'backend-prologue' : 'backend-epilogue';

  return (
    <div id={sectionId} className={sectionMargin}>
      <SectionHeader anchor={sectionId}>{title}</SectionHeader>
      {matching.map((s, i) => (
        <SpliceItem key={i} splice={s} />
      ))}
    </div>
  );
}

interface TypeBackendCodeProps {
  splices: JsonSplice[];
  itemPath: string;
}

// Type-page section: renders every splice (prologue and epilogue) whose
// `for_type` resolves to this item's path. These are splices the defs author
// tagged `<slot> for <Type> r#"..."#;` so backend-supplied functionality
// (trait impls, etc.) shows up on the type's page rather than the module page.
export function TypeBackendCode({ splices, itemPath }: TypeBackendCodeProps) {
  const matching = (splices ?? []).filter(
    (s) => s.for_type === itemPath && normalizeSpliceText(s.text).length > 0
  );
  if (matching.length === 0) return null;

  return (
    <div id="backend-provided" className="mb-8">
      <SectionHeader anchor="backend-provided">Backend-provided code</SectionHeader>
      {matching.map((s, i) => (
        <SpliceItem key={i} splice={s} />
      ))}
    </div>
  );
}
