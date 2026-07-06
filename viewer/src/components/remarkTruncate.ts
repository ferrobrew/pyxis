import type { Root, RootContent } from 'mdast';

const ELLIPSIS = '…';

// A minimal structural view of an mdast node: either a parent (has `children`)
// or a literal (has `value`, e.g. `text`/`inlineCode`).
interface AnyNode {
  type: string;
  value?: string;
  children?: AnyNode[];
}

// Clip a string to its first `limit` whitespace-delimited words, dropping any
// trailing whitespace after the last kept word. Leading whitespace is kept so
// spacing between adjacent inline nodes survives.
function clipToWords(value: string, limit: number): string {
  if (limit <= 0) return '';
  const re = /\S+/g;
  let count = 0;
  let end = 0;
  while (re.exec(value) !== null) {
    count += 1;
    end = re.lastIndex;
    if (count >= limit) break;
  }
  return value.slice(0, end);
}

function countWords(value: string): number {
  const words = value.match(/\S+/g);
  return words ? words.length : 0;
}

/**
 * A remark transformer that reduces a doc comment to a single-block summary:
 * it keeps only the first top-level block and clips that block's text to
 * `budget` words, then appends an ellipsis when anything was dropped.
 *
 * Crucially this operates on the parsed mdast, not the source string, so an
 * emphasis/link/code span whose closing marker falls past the budget still
 * renders as formatted output — clipping the source would leave invalid
 * Markdown that the parser drops or literalises.
 */
export function remarkTruncate(budget: number) {
  return (tree: Root): void => {
    let remaining = budget;
    let truncated = false;

    // Collapse to the first top-level block; anything after it is a separate
    // paragraph/list/heading and doesn't belong in a one-line summary.
    if (tree.children.length > 1) {
      tree.children = tree.children.slice(0, 1) as RootContent[];
      truncated = true;
    }
    if (tree.children.length === 0) return;

    const clip = (node: AnyNode): void => {
      if (node.children) {
        const kept: AnyNode[] = [];
        for (const child of node.children) {
          if (remaining <= 0) {
            truncated = true;
            break;
          }
          clip(child);
          kept.push(child);
        }
        node.children = kept;
        return;
      }
      if (typeof node.value === 'string') {
        const words = countWords(node.value);
        if (words > remaining) {
          node.value = clipToWords(node.value, remaining);
          remaining = 0;
          truncated = true;
        } else {
          remaining -= words;
        }
      }
    };

    const first = tree.children[0] as unknown as AnyNode;
    clip(first);

    // Only paragraphs and headings hold phrasing content that an appended text
    // node reads naturally within; for other blocks the drop stands unmarked.
    if (truncated && (first.type === 'paragraph' || first.type === 'heading') && first.children) {
      // Trim trailing whitespace off the last leaf so the ellipsis sits flush
      // against the final word (a node that fit exactly keeps its trailing space).
      const last = first.children[first.children.length - 1];
      if (last && typeof last.value === 'string') {
        last.value = last.value.replace(/\s+$/, '');
      }
      first.children.push({ type: 'text', value: ELLIPSIS });
    }
  };
}
