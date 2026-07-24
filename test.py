#!/usr/bin/env python3
"""
Cross-platform test script for Pyxis.
Runs tests with different pointer sizes, codegen tests, clippy, and fmt.
"""

import os
import shutil
import sys
import subprocess
import tempfile


def run_command(cmd, env=None, cwd=None, shell=False):
    """Run a command and exit if it fails."""
    print(f"\n{'=' * 60}")
    cwd_info = f" (in {cwd})" if cwd else ""
    print(f"Running: {' '.join(cmd)}{cwd_info}")
    print(f"{'=' * 60}\n")

    result = subprocess.run(cmd, env=env, cwd=cwd, shell=shell)
    if result.returncode != 0:
        print(f"\n[FAIL] Command failed with exit code {result.returncode}")
        sys.exit(result.returncode)
    print("\n[PASS] Command succeeded")
    return result


def check_cpp_docs():
    """Run doxygen over codegen_tests/output/cpp and fail on any warning.

    The emitted headers carry doxygen-markdown doc links (`[label](@ref
    ns::Target)`); an unresolvable `@ref` surfaces as a doxygen warning, so
    warnings are treated as errors. `EXTRACT_ALL` keeps undocumented items
    from warning — only genuine doc problems remain.
    """
    print(f"\n{'=' * 60}")
    print("Running: doxygen (C++ doc-link check on codegen_tests/output/cpp)")
    print(f"{'=' * 60}\n")

    cpp_dir = os.path.abspath(os.path.join("codegen_tests", "output", "cpp"))
    with tempfile.TemporaryDirectory(prefix="pyxis-doxygen-") as tmp:
        warn_log = os.path.join(tmp, "warnings.log")
        doxyfile = os.path.join(tmp, "Doxyfile")
        with open(doxyfile, "w") as f:
            f.write(
                "\n".join(
                    [
                        "PROJECT_NAME = pyxis-codegen-tests",
                        f"OUTPUT_DIRECTORY = {tmp}",
                        f"INPUT = {os.path.join(cpp_dir, 'include')}",
                        "FILE_PATTERNS = *.hpp",
                        "RECURSIVE = YES",
                        "EXTRACT_ALL = YES",
                        "GENERATE_HTML = YES",
                        "GENERATE_LATEX = NO",
                        "QUIET = YES",
                        "WARNINGS = YES",
                        "WARN_IF_UNDOCUMENTED = NO",
                        "WARN_IF_DOC_ERROR = YES",
                        f"WARN_LOGFILE = {warn_log}",
                        # Only ref resolution matters here — never generate
                        # graphs, even if the environment has graphviz (some
                        # doxygen builds auto-enable dot and then fail on
                        # their own map files, e.g. Ubuntu's on CI runners).
                        "HAVE_DOT = NO",
                        "CLASS_GRAPH = NO",
                    ]
                )
                + "\n"
            )
        result = subprocess.run(["doxygen", doxyfile])
        warnings = ""
        if os.path.exists(warn_log):
            with open(warn_log) as f:
                warnings = f.read().strip()
        if result.returncode != 0 or warnings:
            if warnings:
                print(warnings)
            print("\n[FAIL] doxygen reported problems in the emitted C++ docs")
            sys.exit(1)
        print("\n[PASS] Command succeeded")


def main():
    # Get command line arguments for cargo test
    test_args = sys.argv[1:]

    # Create environment with current env
    env_4 = os.environ.copy()
    env_4["PYXIS_TEST_POINTER_SIZE"] = "4"

    env_8 = os.environ.copy()
    env_8["PYXIS_TEST_POINTER_SIZE"] = "8"

    # Run cargo test with pointer size 4
    run_command(["cargo", "test"] + test_args, env=env_4)

    # Run cargo test with pointer size 8
    run_command(["cargo", "test"] + test_args, env=env_8)

    # Run codegen tests
    run_command(["cargo", "run", "--example", "codegen_tests"])

    # Check codegen_tests formatting (fail if any changes needed)
    run_command(["cargo", "run", "-q", "-p", "pyxis-driver", "--", "fmt", "--check"], cwd="codegen_tests/input")

    # Run clippy (with RUSTFLAGS to treat warnings as errors, matching CI)
    clippy_env = os.environ.copy()
    clippy_env["RUSTFLAGS"] = "-Dwarnings"
    run_command(["cargo", "clippy", "--all", "--all-targets", "--all-features"], env=clippy_env)

    # The `json` and `cpp` backends are feature-gated, but shared definition
    # files name backends a given consumer may not compile in (issue #104), so
    # parsing, validation, and cfg-evaluation must be warning-free and pass
    # tests in *every* feature combination — not just the `--all-features`
    # unification above (which the workspace `cargo test` and clippy already
    # cover). The driver pulls in both features via workspace unification, so we
    # isolate the core `pyxis` crate with `-p` to exercise each combo honestly.
    for combo in (["--no-default-features"], ["--features", "json"], ["--features", "cpp"]):
        run_command(
            ["cargo", "clippy", "-p", "pyxis", "--all-targets", *combo],
            env=clippy_env,
        )
        run_command(["cargo", "test", "-p", "pyxis", *combo])

    # Run fmt check (nightly — rustfmt.toml uses the unstable imports_granularity)
    run_command(["cargo", "+nightly", "fmt", "--all", "--", "--check"])

    # Run cargo doc on codegen_tests (catches unresolved doc link references
    # in generated Rust output). Treat warnings as errors so broken doc links
    # or other rustdoc issues fail the test suite.
    doc_env = os.environ.copy()
    doc_env["RUSTDOCFLAGS"] = "-Dwarnings"
    run_command(["cargo", "doc", "--no-deps", "-p", "codegen_tests"], env=doc_env)

    # Run doxygen over the emitted C++ corpus (catches unresolved `@ref`s in
    # the rewritten doc links — the C++ analogue of the cargo doc gate above).
    # The output is generated into a temp dir and discarded; only the warnings
    # matter. Skipped with a warning when doxygen isn't installed (it's in
    # shell.nix, and CI installs it).
    if shutil.which("doxygen"):
        check_cpp_docs()
    else:
        print(f"\n{'=' * 60}")
        print("[SKIP] doxygen not found on PATH — C++ doc-link check skipped.")
        print("       Install doxygen (see shell.nix) to run it locally.")
        print(f"{'=' * 60}\n")

    # Lint the viewer (tsc + eslint + prettier). This assumes the workspace
    # deps are already installed — CI runs `npm ci` before test.py, and local
    # devs are expected to `npm install` themselves. `shell=True` on Windows so
    # the `npm` shim resolves.
    run_command(
        ["npm", "run", "lint"],
        cwd="viewer",
        shell=sys.platform == "win32",
    )

    print(f"\n{'=' * 60}")
    print("All checks passed!")
    print(f"{'=' * 60}\n")


if __name__ == "__main__":
    main()
