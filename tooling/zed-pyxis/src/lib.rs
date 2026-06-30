use zed_extension_api as zed;

struct PyxisExtension;

impl zed::Extension for PyxisExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> zed::Result<zed::Command> {
        // Try to find pyxis-lsp on PATH
        if let Some(path) = worktree.which("pyxis-lsp") {
            return Ok(zed::Command {
                command: path,
                args: vec![],
                env: vec![],
            });
        }

        Err(format!(
            "pyxis-lsp not found on PATH. Build it with `cargo build --release -p pyxis-lsp` \
             and add the target directory to your PATH."
        ))
    }
}

zed::register_extension!(PyxisExtension);
