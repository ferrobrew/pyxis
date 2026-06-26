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

#[salsa::db]
impl salsa::Database for PyxisDatabaseImpl {}

#[salsa::db]
impl Db for PyxisDatabaseImpl {}
