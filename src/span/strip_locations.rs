/// Trait for recursively stripping span/location information from types
/// Used in tests to compare semantic structures without worrying about exact source positions
pub trait StripLocations {
    /// Strip all span and location information, returning a copy without position data
    fn strip_locations(&self) -> Self;
}

// Default implementations for common types
impl<T: StripLocations> StripLocations for Option<T> {
    fn strip_locations(&self) -> Self {
        self.as_ref().map(|v| v.strip_locations())
    }
}
impl<T: StripLocations> StripLocations for Vec<T> {
    fn strip_locations(&self) -> Self {
        self.iter().map(|v| v.strip_locations()).collect()
    }
}
impl<T: StripLocations> StripLocations for Box<T> {
    fn strip_locations(&self) -> Self {
        Box::new((**self).strip_locations())
    }
}
impl<T1: StripLocations, T2: StripLocations> StripLocations for (T1, T2) {
    fn strip_locations(&self) -> Self {
        (self.0.strip_locations(), self.1.strip_locations())
    }
}
impl<T1: StripLocations, T2: StripLocations, T3: StripLocations> StripLocations for (T1, T2, T3) {
    fn strip_locations(&self) -> Self {
        (
            self.0.strip_locations(),
            self.1.strip_locations(),
            self.2.strip_locations(),
        )
    }
}
impl<T1: StripLocations, T2: StripLocations, T3: StripLocations, T4: StripLocations> StripLocations
    for (T1, T2, T3, T4)
{
    fn strip_locations(&self) -> Self {
        (
            self.0.strip_locations(),
            self.1.strip_locations(),
            self.2.strip_locations(),
            self.3.strip_locations(),
        )
    }
}

// Primitive types don't have spans, so just clone
mod primitives {
    use super::StripLocations;

    impl StripLocations for String {
        fn strip_locations(&self) -> Self {
            self.clone()
        }
    }
    impl StripLocations for bool {
        fn strip_locations(&self) -> Self {
            *self
        }
    }
    impl StripLocations for u8 {
        fn strip_locations(&self) -> Self {
            *self
        }
    }
    impl StripLocations for u16 {
        fn strip_locations(&self) -> Self {
            *self
        }
    }
    impl StripLocations for u32 {
        fn strip_locations(&self) -> Self {
            *self
        }
    }
    impl StripLocations for u64 {
        fn strip_locations(&self) -> Self {
            *self
        }
    }
    impl StripLocations for i32 {
        fn strip_locations(&self) -> Self {
            *self
        }
    }
    impl StripLocations for i64 {
        fn strip_locations(&self) -> Self {
            *self
        }
    }
    impl StripLocations for isize {
        fn strip_locations(&self) -> Self {
            *self
        }
    }
    impl StripLocations for usize {
        fn strip_locations(&self) -> Self {
            *self
        }
    }
    impl StripLocations for f32 {
        fn strip_locations(&self) -> Self {
            *self
        }
    }
    impl StripLocations for f64 {
        fn strip_locations(&self) -> Self {
            *self
        }
    }
}
