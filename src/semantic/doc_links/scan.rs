//! Scanning doc comment text for rustdoc-style intra-doc link syntax, shared
//! by the compiler (validation/imports), the Rust backend (link rewriting),
//! and the LSP (navigation/rename).

use super::types::DocLinkPath;

/// Which Markdown syntax produced a scanned intra-doc link.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocLinkSyntax {
    /// An inline link `[label](path::To)`; the destination is the path.
    Inline,
    /// A code-span shortcut `` [`path::To`] ``; the code span is the path.
    CodeShortcut,
    /// A bare bracketed shortcut `[path::To]`; the bracket text is the path.
    /// The compiler ignores these (only backtick/inline forms are validated
    /// and imported), but the LSP surfaces them as navigable links.
    PlainShortcut,
}

/// A rustdoc-style intra-doc link located in a doc comment, with byte offsets
/// into the scanned text. This is the single source of truth for link syntax;
/// the compiler (validation/imports), the Rust backend (link rewriting), and
/// the LSP (navigation/rename) all consume it and filter by [`DocLinkSyntax`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScannedLink {
    pub syntax: DocLinkSyntax,
    /// The resolvable path text (backticks / surrounding whitespace trimmed).
    pub path: String,
    /// Byte range of the whole link (`[Foo]` / `` [`Foo`] `` / `[label](path)`).
    pub link: (usize, usize),
    /// Precise byte range of `path` in the source — the `(...)` destination for
    /// an inline link, or the bracket / code-span content for a shortcut. This
    /// is exactly the span a rename or a backend rewrite substitutes.
    pub path_region: (usize, usize),
    /// Byte range of the bracket label `[...]` interior. Distinguished from
    /// `path_region` so a rename rewrites an inline link's destination but only
    /// an *echoing* label — never arbitrary prose like `[the Foo struct]`.
    pub label_region: (usize, usize),
}

/// Scan every rustdoc-style intra-doc link in `text`, returning each with its
/// syntax and byte offsets.
///
/// Backed by pulldown-cmark so brackets inside code spans (`` `[first, last)` ``)
/// or code blocks can't accidentally consume a real link's closing `]`. Three
/// forms are recognised:
/// - Inline: `[label](target)` — a `Link` tag whose `dest_url` is the target.
/// - Code shortcut: `` [`Path`] `` — emitted as `Text("[")` + `Code` + `Text("]")`.
/// - Plain shortcut: `[Path]` — emitted as `Text("[")` + `Text` + `Text("]")`.
///
/// Only `::`-path targets (per [`is_path`]) are kept; prose and URLs are dropped.
pub fn scan_links(text: &str) -> Vec<ScannedLink> {
    use pulldown_cmark::{Event, Tag, TagEnd};
    let events: Vec<(Event, std::ops::Range<usize>)> = pulldown_cmark::Parser::new(text)
        .into_offset_iter()
        .collect();

    // Locate the precise span of `needle` within `text[range]`, falling back to
    // the whole range's start if (unexpectedly) not found.
    let span_of = |needle: &str, range: &std::ops::Range<usize>| -> (usize, usize) {
        let start = text[range.clone()]
            .find(needle)
            .map(|off| range.start + off)
            .unwrap_or(range.start);
        (start, start + needle.len())
    };

    let mut out = Vec::new();
    let mut i = 0;
    while i < events.len() {
        let range = events[i].1.clone();
        match &events[i].0 {
            // Inline link: [label](target).
            Event::Start(Tag::Link { dest_url, .. }) => {
                let dest = dest_url.as_ref();
                // The label ends at the `]` that precedes `(`.
                let bracket = text[range.clone()].find("](");
                if !dest.is_empty()
                    && is_path(dest)
                    && let Some(rb) = bracket
                {
                    let label_region = (range.start + 1, range.start + rb);
                    let dest_search = (range.start + rb + 2)..range.end;
                    out.push(ScannedLink {
                        syntax: DocLinkSyntax::Inline,
                        path: dest.to_string(),
                        link: (range.start, range.end),
                        path_region: span_of(dest, &dest_search),
                        label_region,
                    });
                } else if dest.is_empty() {
                    // Reference-style link with an empty URL (`[text][]`);
                    // collect its inner content as the path.
                    let mut content = String::new();
                    let mut inner = i + 1;
                    while inner < events.len()
                        && !matches!(events[inner].0, Event::End(TagEnd::Link))
                    {
                        match &events[inner].0 {
                            Event::Text(t) => content.push_str(t),
                            Event::Code(c) => content.push_str(c),
                            _ => {}
                        }
                        inner += 1;
                    }
                    if is_path(&content) {
                        let interior = (range.start + 1, range.end.saturating_sub(1));
                        out.push(ScannedLink {
                            syntax: DocLinkSyntax::CodeShortcut,
                            path: content,
                            link: (range.start, range.end),
                            path_region: interior,
                            label_region: interior,
                        });
                    }
                }
                // Skip the link's inner events so they don't re-trigger below.
                while i < events.len() && !matches!(events[i].0, Event::End(TagEnd::Link)) {
                    i += 1;
                }
            }

            // Code-span shortcut: [`code`] → Text("[") + Code + Text("]").
            Event::Code(content) => {
                let prev_open =
                    i > 0 && matches!(&events[i - 1].0, Event::Text(t) if t.ends_with('['));
                let next_close = i + 1 < events.len()
                    && matches!(&events[i + 1].0, Event::Text(t) if t.starts_with(']'));
                if prev_open && next_close && is_path(content) {
                    // `[` sits immediately before the code span, `]` immediately after.
                    let link = (range.start - 1, range.end + 1);
                    out.push(ScannedLink {
                        syntax: DocLinkSyntax::CodeShortcut,
                        path: content.to_string(),
                        link,
                        path_region: span_of(content, &range),
                        label_region: (link.0 + 1, link.1 - 1),
                    });
                }
            }

            // Plain shortcut: [code] → Text("[") + Text(code) + Text("]").
            Event::Text(t) if t.ends_with('[') => {
                if i + 2 < events.len()
                    && let Event::Text(content) = &events[i + 1].0
                    && let Event::Text(close) = &events[i + 2].0
                    && close.starts_with(']')
                    && is_path(content.trim())
                {
                    let content_range = events[i + 1].1.clone();
                    let path = content.trim();
                    let open_pos = range.end - 1; // position of `[`
                    let close_pos = events[i + 2].1.start; // position of `]`
                    out.push(ScannedLink {
                        syntax: DocLinkSyntax::PlainShortcut,
                        path: path.to_string(),
                        link: (open_pos, close_pos + 1),
                        path_region: span_of(path, &content_range),
                        label_region: (open_pos + 1, close_pos),
                    });
                }
            }

            _ => {}
        }
        i += 1;
    }
    out
}

/// Extract the intra-doc link paths the compiler validates and imports:
/// the inline (`[label](path)`) and code-shortcut (`` [`path`] ``) forms. Bare
/// `[path]` shortcuts are intentionally excluded.
///
/// Returns `(original_text, parsed_path)` for each link — the original path
/// text exactly as written (what a backend substitutes when rewriting) and its
/// parsed [`DocLinkPath`] (what [`DocLinkResolver::resolve`] consumes).
///
/// [`DocLinkResolver::resolve`]: super::resolver::DocLinkResolver::resolve
pub fn extract_links(doc: &[String]) -> Vec<(String, DocLinkPath)> {
    let text = doc.join("\n");
    scan_links(&text)
        .into_iter()
        .filter(|l| l.syntax != DocLinkSyntax::PlainShortcut)
        .map(|l| {
            let parsed = DocLinkPath::parse(&l.path);
            (l.path, parsed)
        })
        .collect()
}

/// Whether `s` is a valid intra-doc link target: a `::`-separated path of
/// identifier segments (e.g. `Type`, `module::Type::method`), as opposed to
/// arbitrary prose or a URL. Shared with editor tooling so the LSP's link
/// scanner recognises the same targets the compiler resolves.
pub fn is_path(s: &str) -> bool {
    !s.is_empty()
        && s.split("::").all(|seg| {
            let mut chars = seg.chars();
            chars
                .next()
                .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
                && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
        })
}
