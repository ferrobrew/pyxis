use std::sync::Arc;

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

/// A value with an associated span
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Located<T> {
    pub value: T,
    pub location: ItemLocation,
}

impl<T> Located<T> {
    pub fn new(value: T, location: ItemLocation) -> Self {
        Self { value, location }
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// Location of an item in source code (filename + span)
/// Every grammar and semantic item should have an ItemLocation for error reporting
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemLocation {
    /// Source file containing the item
    pub filename: Arc<str>,
    /// Span of the item in the source file
    pub span: Span,
}

impl ItemLocation {
    pub fn new(filename: impl Into<Arc<str>>, span: Span) -> Self {
        Self {
            filename: filename.into(),
            span,
        }
    }

    /// Used only for internal items that don't have a source file
    pub fn internal() -> Self {
        Self {
            filename: "<internal>".into(),
            span: Span::synthetic(),
        }
    }

    #[cfg(test)]
    /// Create a test location for generated/test content
    pub fn test() -> Self {
        Self {
            filename: "<test>".into(),
            span: Span::synthetic(),
        }
    }
}

impl std::fmt::Display for ItemLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.filename, self.span)
    }
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

/// Trait for recursively stripping span/location information from types
/// Used in tests to compare semantic structures without worrying about exact source positions
#[cfg(test)]
pub trait StripLocations {
    /// Strip all span and location information, returning a copy without position data
    fn strip_locations(&self) -> Self;
}

// Default implementations for common types
#[cfg(test)]
impl<T: StripLocations> StripLocations for Option<T> {
    fn strip_locations(&self) -> Self {
        self.as_ref().map(|v| v.strip_locations())
    }
}

#[cfg(test)]
impl<T: StripLocations> StripLocations for Vec<T> {
    fn strip_locations(&self) -> Self {
        self.iter().map(|v| v.strip_locations()).collect()
    }
}

#[cfg(test)]
impl<T: StripLocations> StripLocations for Box<T> {
    fn strip_locations(&self) -> Self {
        Box::new((**self).strip_locations())
    }
}

#[cfg(test)]
impl<T: Clone> StripLocations for Located<T> {
    fn strip_locations(&self) -> Self {
        // Return a synthetic version with the same value but no real span
        Located::new(self.value.clone(), ItemLocation::test())
    }
}

// Primitive types don't have spans, so just clone
#[cfg(test)]
impl StripLocations for String {
    fn strip_locations(&self) -> Self {
        self.clone()
    }
}

#[cfg(test)]
impl StripLocations for bool {
    fn strip_locations(&self) -> Self {
        *self
    }
}

#[cfg(test)]
impl StripLocations for usize {
    fn strip_locations(&self) -> Self {
        *self
    }
}

#[cfg(test)]
impl StripLocations for u8 {
    fn strip_locations(&self) -> Self {
        *self
    }
}

#[cfg(test)]
impl StripLocations for u16 {
    fn strip_locations(&self) -> Self {
        *self
    }
}

#[cfg(test)]
impl StripLocations for u32 {
    fn strip_locations(&self) -> Self {
        *self
    }
}

#[cfg(test)]
impl StripLocations for u64 {
    fn strip_locations(&self) -> Self {
        *self
    }
}

#[cfg(test)]
impl StripLocations for i32 {
    fn strip_locations(&self) -> Self {
        *self
    }
}

#[cfg(test)]
impl StripLocations for i64 {
    fn strip_locations(&self) -> Self {
        *self
    }
}
