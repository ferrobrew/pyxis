use std::path::Path;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub project: Project,
}
impl Config {
    pub fn load(path: &Path) -> anyhow::Result<Self> {
        Ok(toml::from_str(&std::fs::read_to_string(path)?)?)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Project {
    pub pointer_size: usize,
}
