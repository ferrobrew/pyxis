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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    /// Map the value while preserving the span
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            value: f(self.value),
            span: self.span,
        }
    }

    /// Get a reference to the value
    pub fn value_ref(&self) -> &T {
        &self.value
    }

    /// Get a mutable reference to the value
    pub fn value_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// Label for additional span highlighting in error reports
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErrorLabel {
    /// The span to highlight
    pub span: Span,
    /// Message to show for this label
    pub message: String,
    /// Label color (for terminal output)
    pub color: ErrorLabelColor,
}

/// Color for error labels in terminal output
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorLabelColor {
    Red,
    Yellow,
    Blue,
    Cyan,
}

impl ErrorLabel {
    pub fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            color: ErrorLabelColor::Red,
        }
    }

    pub fn with_color(mut self, color: ErrorLabelColor) -> Self {
        self.color = color;
        self
    }
}

/// Context information for error reporting (filename and span)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErrorContext {
    /// Primary source file where the error occurred
    pub filename: Option<Box<str>>,
    /// Primary span to highlight
    pub span: Option<Span>,
    /// Help text suggesting how to fix the error
    pub help: Option<String>,
    /// Additional notes providing context
    pub notes: Vec<String>,
    /// Additional labeled spans to show related locations
    pub labels: Vec<ErrorLabel>,
}

impl ErrorContext {
    pub fn new() -> Self {
        Self {
            filename: None,
            span: None,
            help: None,
            notes: Vec::new(),
            labels: Vec::new(),
        }
    }

    pub fn with_filename(mut self, filename: impl Into<String>) -> Self {
        self.filename = Some(filename.into().into_boxed_str());
        self
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn with_label(mut self, label: ErrorLabel) -> Self {
        self.labels.push(label);
        self
    }

    pub fn with_labels(mut self, labels: impl IntoIterator<Item = ErrorLabel>) -> Self {
        self.labels.extend(labels);
        self
    }
}

impl Default for ErrorContext {
    fn default() -> Self {
        Self::new()
    }
}
