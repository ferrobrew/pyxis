mod equals_ignoring_location;
pub use equals_ignoring_location::*;

#[cfg(test)]
mod strip_locations;
#[cfg(test)]
pub use strip_locations::*;

/// A location in source code (line and column, both 1-indexed)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed, byte offset within the line)
    pub column: usize,
}
impl Location {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}
impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// A span representing a range in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// Start location (inclusive)
    pub start: Location,
    /// End location (exclusive)
    pub end: Location,
}
impl Span {
    pub fn new(start: Location, end: Location) -> Self {
        Self { start, end }
    }

    /// Create a synthetic span (for generated or missing content)
    pub fn synthetic() -> Self {
        Self {
            start: Location::new(0, 0),
            end: Location::new(0, 0),
        }
    }

    /// Merge two spans into one that covers both
    pub fn merge(&self, other: &Span) -> Span {
        let start = self.start.min(other.start);
        let end = self.end.max(other.end);
        Span { start, end }
    }
}
impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

/// A lightweight identifier for a source file.
///
/// This is an index into a [`FileStore`](crate::source_store::FileStore), which
/// maps file IDs to filenames and provides access to source content.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

impl FileId {
    /// File ID for internal/generated items that don't have a source file.
    pub const INTERNAL: FileId = FileId(0);

    /// File ID for test items.
    #[cfg(test)]
    pub const TEST: FileId = FileId(1);

    /// Create a new FileId from a raw index.
    /// This should only be used by FileStore.
    pub(crate) fn new(index: u32) -> Self {
        FileId(index)
    }

    /// Get the raw index of this FileId.
    pub(crate) fn index(self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Display for FileId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            0 => write!(f, "<internal>"),
            #[cfg(test)]
            1 => write!(f, "<test>"),
            n => write!(f, "<file:{n}>"),
        }
    }
}

/// Location of an item in source code (file ID + span)
/// Every grammar and semantic item should have an ItemLocation for error reporting
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemLocation {
    /// Source file containing the item
    pub file_id: FileId,
    /// Span of the item in the source file
    pub span: Span,
}
#[cfg(test)]
impl StripLocations for ItemLocation {
    fn strip_locations(&self) -> Self {
        ItemLocation::test()
    }
}
impl ItemLocation {
    pub fn new(file_id: FileId, span: Span) -> Self {
        Self { file_id, span }
    }

    /// Used only for internal items that don't have a source file
    pub fn internal() -> Self {
        Self {
            file_id: FileId::INTERNAL,
            span: Span::synthetic(),
        }
    }

    #[cfg(test)]
    /// Create a test location for generated/test content
    pub fn test() -> Self {
        Self {
            file_id: FileId::TEST,
            span: Span::synthetic(),
        }
    }
}

impl std::fmt::Display for ItemLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.span.start)
    }
}

/// Trait for types that have an associated source location
pub trait HasLocation {
    fn location(&self) -> &ItemLocation;
}

// Helper functions for span manipulation and ariadne integration

/// Convert a Span to a byte offset in the source string
pub fn span_to_offset(source: &str, span: &Span) -> usize {
    source
        .lines()
        .take(span.start.line.saturating_sub(1))
        .map(|line| line.len() + 1)
        .sum::<usize>()
        + span.start.column.saturating_sub(1)
}

/// Calculate the length of a span in bytes
pub fn span_length(source: &str, span: &Span) -> usize {
    if span.start.line == span.end.line {
        span.end.column.saturating_sub(span.start.column)
    } else {
        let first_line_len = source
            .lines()
            .nth(span.start.line.saturating_sub(1))
            .map(|line| line.len())
            .unwrap_or(0)
            .saturating_sub(span.start.column.saturating_sub(1));
        let middle_lines_len: usize = source
            .lines()
            .skip(span.start.line)
            .take(
                span.end
                    .line
                    .saturating_sub(span.start.line)
                    .saturating_sub(1),
            )
            .map(|line| line.len() + 1)
            .sum();
        let last_line_len = span.end.column.saturating_sub(1);
        first_line_len + middle_lines_len + last_line_len
    }
}
