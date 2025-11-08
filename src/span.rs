use std::fmt;
use std::ops::Range;

/// Represents a span of text in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    /// Create a new span from start and end byte positions
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }

    /// Create a span that covers from the start of `self` to the end of `other`
    pub fn to(&self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }

    /// Get the length of the span in bytes
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Check if the span is empty
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Convert to a Range for indexing
    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Span {
            start: range.start,
            end: range.end,
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

/// A value with an associated source span
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Create a new spanned value
    pub fn new(node: T, span: Span) -> Self {
        Spanned { node, span }
    }

    /// Map the inner value while preserving the span
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }

    /// Convert into the inner value, discarding the span
    pub fn into_inner(self) -> T {
        self.node
    }
}

impl<T> AsRef<T> for Spanned<T> {
    fn as_ref(&self) -> &T {
        &self.node
    }
}

impl<T> AsMut<T> for Spanned<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.node
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            // In alternate mode, show the span
            f.debug_struct("Spanned")
                .field("node", &self.node)
                .field("span", &self.span)
                .finish()
        } else {
            // In normal mode, just show the node (cleaner debug output)
            self.node.fmt(f)
        }
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node
    }
}

// Implement Display for Spanned<T> where T: Display
impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.node.fmt(f)
    }
}

/// Trait for structural equality comparison that ignores spans
pub trait StructuralEq {
    fn structural_eq(&self, other: &Self) -> bool;
}

// Implement for Spanned<T> - the key impl that unwraps and ignores span
impl<T: StructuralEq> StructuralEq for Spanned<T> {
    fn structural_eq(&self, other: &Self) -> bool {
        self.node.structural_eq(&other.node)
    }
}

// Implement for common containers
impl<T: StructuralEq> StructuralEq for Vec<T> {
    fn structural_eq(&self, other: &Self) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(a, b)| a.structural_eq(b))
    }
}

impl<T: StructuralEq> StructuralEq for Option<T> {
    fn structural_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Some(a), Some(b)) => a.structural_eq(b),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<T: StructuralEq> StructuralEq for Box<T> {
    fn structural_eq(&self, other: &Self) -> bool {
        (**self).structural_eq(&**other)
    }
}

// Implement for primitives - delegate to PartialEq
impl StructuralEq for String {
    fn structural_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl StructuralEq for str {
    fn structural_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl StructuralEq for usize {
    fn structural_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl StructuralEq for isize {
    fn structural_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl StructuralEq for bool {
    fn structural_eq(&self, other: &Self) -> bool {
        self == other
    }
}

// Helper macro for tests
#[macro_export]
macro_rules! assert_ast_eq {
    ($left:expr, $right:expr) => {
        assert!(
            $crate::span::StructuralEq::structural_eq(&$left, &$right),
            "ASTs not structurally equal:\nleft: {:#?}\nright: {:#?}",
            $left,
            $right
        )
    };
    ($left:expr, $right:expr, $($arg:tt)+) => {
        assert!(
            $crate::span::StructuralEq::structural_eq(&$left, &$right),
            $($arg)+
        )
    };
}
