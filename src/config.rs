use std::path::Path;

use serde::{Deserialize, Serialize};

/// Configuration loading errors
#[derive(Debug)]
pub enum ConfigError {
    Io(std::io::Error),
    Parse(toml::de::Error),
    Other(String),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigError::Io(err) => write!(f, "Failed to read config file: {}", err),
            ConfigError::Parse(err) => write!(f, "Failed to parse config file: {}", err),
            ConfigError::Other(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for ConfigError {}

impl From<std::io::Error> for ConfigError {
    fn from(err: std::io::Error) -> Self {
        ConfigError::Io(err)
    }
}

impl From<toml::de::Error> for ConfigError {
    fn from(err: toml::de::Error) -> Self {
        ConfigError::Parse(err)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub project: Project,
}
impl Config {
    pub fn load(path: &Path) -> Result<Self, ConfigError> {
        Ok(toml::from_str(&std::fs::read_to_string(path)?)?)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Project {
    pub name: String,
    pub pointer_size: usize,
}
