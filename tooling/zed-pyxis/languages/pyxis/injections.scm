; Highlight backend `prologue`/`epilogue` code splices using the backend's own
; language, so e.g. `backend cpp { epilogue r#"..."#; }` gets C++ highlighting
; and `backend rust { ... }` gets Rust highlighting.
;
; The backend name node selects the injected language (dynamic capture), so this
; works for any backend whose name matches a known grammar — no per-backend
; patterns to maintain as more backends are added.
;
; We inject into `raw_string_content` (the grammar's inner node) rather than the
; whole `raw_string_literal`, which excludes the `r#"`/`"#` delimiters. This is
; required for Rust: `r#"..."#` is Rust's own raw-string syntax, so injecting it
; over the delimiters would swallow the whole splice as a single string literal.

(backend_block
  name: (backend_name) @injection.language
  (backend_splice
    body: (raw_string_literal
      (raw_string_content) @injection.content)))
