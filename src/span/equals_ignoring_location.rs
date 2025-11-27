pub trait EqualsIgnoringLocations {
    fn equals_ignoring_locations(&self, other: &Self) -> bool;
}

// Default implementations for common types
impl<T: EqualsIgnoringLocations> EqualsIgnoringLocations for Option<T> {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        match (self, other) {
            (Some(v), Some(v2)) => v.equals_ignoring_locations(v2),
            (None, None) => true,
            _ => false,
        }
    }
}
impl<T: EqualsIgnoringLocations> EqualsIgnoringLocations for Vec<T> {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self.iter()
            .zip(other.iter())
            .all(|(v, v2)| v.equals_ignoring_locations(v2))
    }
}
impl<T: EqualsIgnoringLocations> EqualsIgnoringLocations for Box<T> {
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        (**self).equals_ignoring_locations(&**other)
    }
}
impl<T1: EqualsIgnoringLocations, T2: EqualsIgnoringLocations> EqualsIgnoringLocations
    for (T1, T2)
{
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self.0.equals_ignoring_locations(&other.0) && self.1.equals_ignoring_locations(&other.1)
    }
}
impl<T1: EqualsIgnoringLocations, T2: EqualsIgnoringLocations, T3: EqualsIgnoringLocations>
    EqualsIgnoringLocations for (T1, T2, T3)
{
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self.0.equals_ignoring_locations(&other.0)
            && self.1.equals_ignoring_locations(&other.1)
            && self.2.equals_ignoring_locations(&other.2)
    }
}
impl<
    T1: EqualsIgnoringLocations,
    T2: EqualsIgnoringLocations,
    T3: EqualsIgnoringLocations,
    T4: EqualsIgnoringLocations,
> EqualsIgnoringLocations for (T1, T2, T3, T4)
{
    fn equals_ignoring_locations(&self, other: &Self) -> bool {
        self.0.equals_ignoring_locations(&other.0)
            && self.1.equals_ignoring_locations(&other.1)
            && self.2.equals_ignoring_locations(&other.2)
            && self.3.equals_ignoring_locations(&other.3)
    }
}

// Primitive types don't have spans, so just clone
mod primitives {
    use super::EqualsIgnoringLocations;

    impl EqualsIgnoringLocations for String {
        fn equals_ignoring_locations(&self, other: &Self) -> bool {
            *self == *other
        }
    }
    impl EqualsIgnoringLocations for bool {
        fn equals_ignoring_locations(&self, other: &Self) -> bool {
            *self == *other
        }
    }
    impl EqualsIgnoringLocations for u8 {
        fn equals_ignoring_locations(&self, other: &Self) -> bool {
            *self == *other
        }
    }
    impl EqualsIgnoringLocations for u16 {
        fn equals_ignoring_locations(&self, other: &Self) -> bool {
            *self == *other
        }
    }
    impl EqualsIgnoringLocations for u32 {
        fn equals_ignoring_locations(&self, other: &Self) -> bool {
            *self == *other
        }
    }
    impl EqualsIgnoringLocations for u64 {
        fn equals_ignoring_locations(&self, other: &Self) -> bool {
            *self == *other
        }
    }
    impl EqualsIgnoringLocations for i32 {
        fn equals_ignoring_locations(&self, other: &Self) -> bool {
            *self == *other
        }
    }
    impl EqualsIgnoringLocations for i64 {
        fn equals_ignoring_locations(&self, other: &Self) -> bool {
            *self == *other
        }
    }
    impl EqualsIgnoringLocations for isize {
        fn equals_ignoring_locations(&self, other: &Self) -> bool {
            *self == *other
        }
    }
    impl EqualsIgnoringLocations for usize {
        fn equals_ignoring_locations(&self, other: &Self) -> bool {
            *self == *other
        }
    }
    impl EqualsIgnoringLocations for f32 {
        fn equals_ignoring_locations(&self, other: &Self) -> bool {
            *self == *other
        }
    }
    impl EqualsIgnoringLocations for f64 {
        fn equals_ignoring_locations(&self, other: &Self) -> bool {
            *self == *other
        }
    }
}
