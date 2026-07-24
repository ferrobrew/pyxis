# Dev-shell extras for pyxis — tools `python test.py` uses beyond the Rust
# toolchain (which contributors typically manage via rustup).
#
# Currently that's doxygen (plus graphviz for its dot graphs), for the C++
# doc-link check: test.py runs doxygen over the emitted C++ corpus and fails
# on unresolved `@ref`s. Without doxygen the check is skipped with a warning.
{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  packages = with pkgs; [
    doxygen
    graphviz
  ];
}
