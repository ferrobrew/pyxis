//! Span conversion: Pyxis spans (1-indexed, byte columns) → LSP positions (0-indexed, UTF-16 columns).

use lsp_types::{Position, Range};

/// Convert a Pyxis span to an LSP range.
///
/// Pyxis spans use 1-indexed lines and byte-offset columns.
/// LSP uses 0-indexed lines and UTF-16 code-unit columns.
pub fn pyxis_span_to_lsp_range(source: &str, span: &pyxis::span::Span) -> Range {
    Range {
        start: pyxis_location_to_lsp_position(source, &span.start),
        end: pyxis_location_to_lsp_position(source, &span.end),
    }
}

/// Convert a Pyxis location (1-indexed line, byte column) to an LSP position (0-indexed line, UTF-16 column).
fn pyxis_location_to_lsp_position(source: &str, loc: &pyxis::span::Location) -> Position {
    let line = (loc.line.saturating_sub(1)) as u32;
    let column = byte_column_to_utf16(source, loc.line, loc.column);
    Position { line, character: column }
}

/// Convert a byte column offset within a specific line to a UTF-16 code-unit offset.
fn byte_column_to_utf16(source: &str, line: usize, byte_column: usize) -> u32 {
    // Get the specific line (1-indexed in Pyxis)
    let line_str = source.lines().nth(line.saturating_sub(1)).unwrap_or("");

    // The column is a 1-indexed byte offset within the line.
    // Convert to 0-indexed byte offset.
    let byte_offset = byte_column.saturating_sub(1);

    // Convert byte offset to UTF-16 code units
    let mut utf16_offset = 0u32;
    let mut current_byte = 0usize;
    for ch in line_str.chars() {
        if current_byte >= byte_offset {
            break;
        }
        current_byte += ch.len_utf8();
        utf16_offset += ch.len_utf16() as u32;
    }
    utf16_offset
}

/// Ensure a diagnostic range is non-empty so editors render an inline squiggle.
///
/// Zero-width ranges (common for parse/lexer errors at EOF or an unexpected
/// character) are valid LSP but many editors — Zed included — won't draw an
/// underline for them, showing the diagnostic only in the problems list. This
/// widens an empty range to cover one character: the character at the position,
/// else the one before it, else the end of the previous line.
pub fn widen_empty_range(source: &str, range: Range) -> Range {
    if range.start != range.end {
        return range;
    }
    let lines: Vec<&str> = source.lines().collect();
    let utf16_len = |s: &str| s.chars().map(|c| c.len_utf16()).sum::<usize>() as u32;
    let line_idx = range.start.line as usize;

    if let Some(line) = lines.get(line_idx) {
        let len = utf16_len(line);
        if range.start.character < len {
            // Underline the character at the position.
            let mut end = range.end;
            end.character = range.start.character + 1;
            return Range { start: range.start, end };
        }
        if range.start.character > 0 {
            // At/after end of a non-empty line: underline the preceding character.
            let mut start = range.start;
            start.character -= 1;
            return Range { start, end: range.end };
        }
    }
    // Empty position (e.g. EOF on a blank line): underline the end of the
    // previous line instead.
    if line_idx > 0 {
        if let Some(prev) = lines.get(line_idx - 1) {
            let len = utf16_len(prev);
            let line = (line_idx - 1) as u32;
            return Range {
                start: Position { line, character: len.saturating_sub(1) },
                end: Position { line, character: len },
            };
        }
    }
    // Last resort: widen the end by one column.
    let mut end = range.end;
    end.character += 1;
    Range { start: range.start, end }
}

/// Find the Pyxis location (1-indexed line, byte column) at an LSP position (0-indexed line, UTF-16 column).
pub fn lsp_position_to_pyxis_location(source: &str, position: Position) -> pyxis::span::Location {
    let line = (position.line as usize) + 1; // 0-indexed → 1-indexed

    // Get the line
    let line_str = source.lines().nth(position.line as usize).unwrap_or("");

    // Convert UTF-16 column to byte column
    let mut byte_offset = 0usize;
    let mut utf16_count = 0u32;
    for ch in line_str.chars() {
        if utf16_count >= position.character {
            break;
        }
        utf16_count += ch.len_utf16() as u32;
        byte_offset += ch.len_utf8();
    }

    let column = byte_offset + 1; // 0-indexed → 1-indexed

    pyxis::span::Location::new(line, column)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ascii_conversion() {
        let source = "pub type Foo {\n    pub x: u32,\n}";
        let span = pyxis::span::Span::new(
            pyxis::span::Location::new(1, 1),
            pyxis::span::Location::new(1, 4),
        );
        let range = pyxis_span_to_lsp_range(source, &span);
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.line, 0);
        assert_eq!(range.end.character, 3);
    }

    #[test]
    fn test_unicode_conversion() {
        // Line with a multi-byte char: "café" has 5 bytes but 4 UTF-16 code units
        let source = "/// café — test\npub type Foo {}";
        // Span covering "café" (line 1, columns 5-9 byte offset)
        let span = pyxis::span::Span::new(
            pyxis::span::Location::new(1, 5),
            pyxis::span::Location::new(1, 9),
        );
        let range = pyxis_span_to_lsp_range(source, &span);
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 4); // "/// " is 4 chars
        assert_eq!(range.end.line, 0);
        // "café" is 4 UTF-16 code units, but é is 2 bytes = 1 UTF-16 code unit
        assert_eq!(range.end.character, 8); // 4 + 4 = 8
    }

    #[test]
    fn test_widen_empty_range() {
        let source = "pub type Foo {\n    pub x: u32,\n}\n";
        // Non-empty ranges are left untouched.
        let r = Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 3 } };
        assert_eq!(super::widen_empty_range(source, r), r);

        // Empty range mid-line → widened to the character at the position.
        let r = Range { start: Position { line: 1, character: 11 }, end: Position { line: 1, character: 11 } };
        let w = super::widen_empty_range(source, r);
        assert_eq!(w.start, Position { line: 1, character: 11 });
        assert_eq!(w.end, Position { line: 1, character: 12 });

        // Empty range on an empty line (EOF) → backs up to the previous line's end.
        let eof_source = "pub type Foo {\n    pub x: u32,\n";
        let r = Range { start: Position { line: 2, character: 0 }, end: Position { line: 2, character: 0 } };
        let w = super::widen_empty_range(eof_source, r);
        assert_ne!(w.start, w.end, "EOF diagnostic range must be non-empty");
        assert_eq!(w.start.line, 1, "should back up to the previous line");
    }

    #[test]
    fn test_reverse_conversion() {
        let source = "pub type Foo {\n    pub x: u32,\n}";
        let pos = Position { line: 0, character: 4 };
        let loc = lsp_position_to_pyxis_location(source, pos);
        assert_eq!(loc.line, 1);
        assert_eq!(loc.column, 5);
    }
}
