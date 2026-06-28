//! Salsa database definition.

/// The Salsa database trait — implemented by [`PyxisDatabaseImpl`].
#[salsa::db]
pub trait Db: salsa::Database {}

/// The concrete Salsa database implementation used by both the batch
/// compiler and the LSP server.
#[salsa::db]
#[derive(Default)]
pub struct PyxisDatabaseImpl {
    storage: salsa::Storage<Self>,
}

impl PyxisDatabaseImpl {
    /// Construct a database that forwards every salsa event to `logger`. Used by
    /// tests to assert incrementality — i.e. which queries actually re-execute
    /// after an edit.
    pub fn with_event_logger(logger: Box<dyn Fn(salsa::Event) + Send + Sync + 'static>) -> Self {
        Self {
            storage: salsa::Storage::new(Some(logger)),
        }
    }
}

#[salsa::db]
impl salsa::Database for PyxisDatabaseImpl {}

#[salsa::db]
impl Db for PyxisDatabaseImpl {}
