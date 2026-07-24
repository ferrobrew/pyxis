use std::str::FromStr;

use lsp_server::Notification;
use lsp_types::{Diagnostic, PublishDiagnosticsParams, Uri};

pub(super) fn make_publish_diagnostics(uri: Uri, diagnostics: Vec<Diagnostic>) -> Notification {
    Notification::new(
        "textDocument/publishDiagnostics".into(),
        PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: None,
        },
    )
}

pub(super) fn uri_to_filename(uri: &Uri) -> String {
    // Strip the file:// prefix and return a simple relative path
    let s = uri.as_str();
    let path = s.strip_prefix("file://").unwrap_or(s);
    // For test URIs like "file:///test.pyxis", return just "test.pyxis"
    path.rsplit('/').next().unwrap_or(path).to_string()
}

/// Canonicalize a path for dedup comparison, falling back to the path itself
/// when it can't be canonicalized (e.g. synthetic in-memory test paths that
/// don't exist on disk).
pub(super) fn canonical_path(path: &std::path::Path) -> std::path::PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

/// Convert a `file://` URI to a filesystem path.
pub(super) fn uri_to_file_path(uri: &Uri) -> Option<std::path::PathBuf> {
    let s = uri.as_str();
    let path_str = s.strip_prefix("file://")?;
    // On Unix, the path is already absolute (e.g. /home/user/project)
    // On Windows, file:// URIs use /C:/... format
    let path_str =
        if path_str.starts_with('/') && path_str.len() > 2 && path_str.as_bytes()[2] == b':' {
            // Windows: /C:/... → C:/...
            &path_str[1..]
        } else {
            path_str
        };
    // Decode percent-encoding for spaces etc.
    let decoded = percent_decode(path_str);
    let path = std::path::PathBuf::from(decoded);
    if path.exists() || path.parent().is_some() {
        Some(path)
    } else {
        None
    }
}

/// Convert a filesystem path to a `file://` URI.
pub(super) fn file_path_to_uri(path: &std::path::Path) -> Option<Uri> {
    let absolute = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let s = absolute.to_string_lossy();
    let encoded = percent_encode(&s);
    let uri_str = format!("file://{}", encoded);
    Uri::from_str(&uri_str).ok()
}

/// The `file://` URI for a path. Never panics: `percent_encode` guarantees
/// ASCII output, so the formatted string always parses as a `Uri`. The
/// fallback covers synthetic in-memory test paths that don't canonicalize.
pub(super) fn file_uri(path: &std::path::Path) -> Uri {
    if let Some(uri) = file_path_to_uri(path) {
        return uri;
    }
    let encoded = percent_encode(&path.display().to_string());
    let uri_str = format!("file:///{}", encoded.trim_start_matches('/'));
    Uri::from_str(&uri_str)
        .unwrap_or_else(|_| Uri::from_str("file:///").expect("`file:///` is a valid URI"))
}

/// Percent-decoding for file paths. Decodes `%XX` escapes to raw bytes and
/// reassembles them as UTF-8 — accumulating bytes (not chars) so a multi-byte
/// sequence like `%C3%A9` round-trips to `é` rather than mojibake.
pub(super) fn percent_decode(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out: Vec<u8> = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%'
            && i + 2 < bytes.len()
            && let Ok(decoded) = std::str::from_utf8(&bytes[i + 1..i + 3])
                .ok()
                .and_then(|hex| u8::from_str_radix(hex, 16).ok())
                .ok_or(())
        {
            out.push(decoded);
            i += 3;
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }
    String::from_utf8_lossy(&out).into_owned()
}

/// Percent-encoding for file paths. Always produces ASCII output: besides the
/// reserved/unsafe ASCII characters, every control byte and every non-ASCII
/// (UTF-8 continuation/lead) byte is encoded. This is essential — a `Uri` must
/// be ASCII, and emitting `byte as char` for a multi-byte UTF-8 sequence would
/// both corrupt the path and produce a string `Uri::from_str` rejects.
pub(super) fn percent_encode(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for byte in s.bytes() {
        match byte {
            b' ' | b'<' | b'>' | b'#' | b'%' | b'"' | b'{' | b'}' | b'|' | b'\\' | b'^' | b'['
            | b']' | b'`' => result.push_str(&format!("%{byte:02X}")),
            // Control bytes (incl. DEL) and all non-ASCII bytes.
            0x00..=0x1F | 0x7F..=0xFF => result.push_str(&format!("%{byte:02X}")),
            _ => result.push(byte as char),
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn percent_encode_is_ascii_and_encodes_non_ascii() {
        let encoded = percent_encode("/tmp/café/日本語.pyxis");
        assert!(
            encoded.is_ascii(),
            "encoded output must be ASCII: {encoded}"
        );
        assert!(
            encoded.contains("%C3%A9"),
            "é should be percent-encoded UTF-8"
        );
        // Round-trips back to the original bytes.
        assert_eq!(percent_decode(&encoded), "/tmp/café/日本語.pyxis");
    }

    #[test]
    fn file_uri_does_not_panic_on_non_ascii_paths() {
        // Regression: file_uri previously panicked (Uri::from_str(...).unwrap())
        // for any path with a non-ASCII component.
        let uri = file_uri(std::path::Path::new("/tmp/工程/café.pyxis"));
        assert!(uri.as_str().starts_with("file:///"));
        assert!(uri.as_str().is_ascii());
    }
}
