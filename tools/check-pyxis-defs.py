#!/usr/bin/env python3
"""
Check pyxis-defs builds against the local pyxis checkout.

This is a thin environment-setup wrapper around pyxis-defs' own `build.py`.
It:

1. Finds the pyxis-defs repo (sibling directory, --pyxis-defs, or env var).
2. Sets up the C++ toolchain environment (xwin toolchains, clang-cl via
   nix-shell if not directly on PATH).
3. Ensures Rust Windows targets are installed.
4. Delegates to `build.py --path <pyxis>/driver --check-builds rust,cpp`,
   which installs pyxis from the local checkout and compile-checks every
   project's generated output.

Usage:
    python tools/check-pyxis-defs.py [--pyxis-defs <path>] [--backends rust,cpp]

    --pyxis-defs <path>   Path to the pyxis-defs repo (default: ../pyxis-defs
                          or PYXIS_DEFS_PATH env var).
    --backends <list>     Comma-separated backends to check (default: rust,cpp).
    --output-dir <dir>    Persist generated output and build artifacts to this
                          directory instead of a temp dir (for inspection).

Prerequisites (C++ checks on Linux):
    - xwin: `cargo install xwin && xwin --arch x86,x86_64 --accept-license splat --output ~/.xwin`
    - clang-cl, lld-link, llvm-lib (LLVM >= 16, or via nix-shell)

Prerequisites (Rust checks):
    - Windows targets: `rustup target add i686-pc-windows-msvc x86_64-pc-windows-msvc`
"""

import os
import sys
import subprocess
import argparse
import platform
from pathlib import Path


def find_pyxis_defs(path: str | None) -> Path:
    """Locate the pyxis-defs repo."""
    if path:
        p = Path(path).resolve()
        if not (p / "build.py").exists():
            print(f"Error: no build.py found in {p}", file=sys.stderr)
            sys.exit(1)
        return p

    env_path = os.environ.get("PYXIS_DEFS_PATH")
    if env_path:
        p = Path(env_path).resolve()
        if (p / "build.py").exists():
            return p

    # Try sibling directory: ../pyxis-defs
    repo_root = Path(__file__).resolve().parent.parent
    sibling = repo_root.parent / "pyxis-defs"
    if (sibling / "build.py").exists():
        return sibling

    print(
        "Error: could not find pyxis-defs. Pass --pyxis-defs <path> or set PYXIS_DEFS_PATH.",
        file=sys.stderr,
    )
    sys.exit(1)


def find_xwin_root() -> str | None:
    """Find the xwin SDK root."""
    if os.environ.get("XWIN_ROOT"):
        return os.environ["XWIN_ROOT"]
    default = Path.home() / ".xwin"
    if default.exists():
        return str(default)
    return None


def find_toolchain(arch: str, pyxis_repo: Path) -> str | None:
    """Find the CMake toolchain file for the given architecture."""
    toolchain = os.environ.get(f"PYXIS_CHECK_CMAKE_TOOLCHAIN_{arch}")
    if toolchain:
        return toolchain

    xwin_root = find_xwin_root()
    if xwin_root is None:
        return None

    toolchain_file = pyxis_repo / "tools" / "cmake-toolchains" / f"xwin-{arch.lower()}.cmake"
    if toolchain_file.exists():
        return str(toolchain_file)

    return None


def find_clang_cl() -> list[str] | None:
    """Find clang-cl, possibly via nix-shell. Returns a prefix command list
    (possibly empty) that puts clang-cl on PATH, or None if not found."""
    # Direct?
    result = subprocess.run(["which", "clang-cl"], capture_output=True, text=True)
    if result.returncode == 0 and result.stdout.strip():
        return []

    # Try nix-shell. Need both clang (for clang-cl) and llvm (for llvm-lib,
    # lld-link) and lld (for lld-link on some setups).
    for pkgs in [
        "llvmPackages_21.clang llvmPackages_21.llvm lld",
        "llvmPackages_20.clang llvmPackages_20.llvm lld",
        "llvmPackages_19.clang llvmPackages_19.llvm lld",
        "llvmPackages_18.clang llvmPackages_18.llvm lld",
        "llvmPackages_17.clang llvmPackages_17.llvm lld",
        "llvmPackages_16.clang llvmPackages_16.llvm lld",
    ]:
        result = subprocess.run(
            ["nix-shell", "-p"] + pkgs.split() + ["--run", "which clang-cl llvm-lib lld-link"],
            capture_output=True, text=True,
        )
        if result.returncode == 0 and result.stdout.strip():
            return ["nix-shell", "-p"] + pkgs.split() + ["--run"]

    return None


def main():
    parser = argparse.ArgumentParser(
        description="Check pyxis-defs builds against the local pyxis checkout",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--pyxis-defs",
        type=str,
        default=None,
        help="Path to the pyxis-defs repo (default: ../pyxis-defs or PYXIS_DEFS_PATH env var)",
    )
    parser.add_argument(
        "--backends",
        type=str,
        default="rust,cpp",
        help="Comma-separated backends to check (default: rust,cpp)",
    )
    parser.add_argument(
        "--output-dir",
        type=str,
        default=None,
        metavar="DIR",
        help="Persist generated output and build artifacts to this directory "
        "instead of a temp dir (passed through to build.py --output-dir)",
    )
    args = parser.parse_args()

    pyxis_repo = Path(__file__).resolve().parent.parent
    pyxis_defs = find_pyxis_defs(args.pyxis_defs)

    print(f"pyxis repo:    {pyxis_repo}")
    print(f"pyxis-defs:    {pyxis_defs}")

    # Step 1: Set up environment for C++ checks
    env = os.environ.copy()

    backends = [b.strip() for b in args.backends.split(",") if b.strip()]
    do_rust = "rust" in backends
    do_cpp = "cpp" in backends

    # The command to run build.py — may be wrapped in nix-shell if clang-cl
    # isn't directly on PATH (needed for C++ checks).
    build_py_cmd_prefix: list[str] = []

    if do_cpp:
        # On non-Linux hosts (native Windows), MSVC or clang-cl may already
        # be on PATH — no xwin toolchain needed. build.py's check_cpp_build
        # handles this: if no PYXIS_CHECK_CMAKE_TOOLCHAIN_* is set and the
        # host is Windows, it uses the native MSVC generator directly.
        is_linux = platform.system() == "Linux"

        if is_linux:
            # Linux: need xwin + clang-cl toolchain
            toolchain_x86 = find_toolchain("X86", pyxis_repo)
            toolchain_x64 = find_toolchain("X64", pyxis_repo)

            if toolchain_x86:
                env["PYXIS_CHECK_CMAKE_TOOLCHAIN_X86"] = toolchain_x86
                print(f"X86 toolchain: {toolchain_x86}")
            else:
                print("Warning: no X86 CMake toolchain found (C++ checks for 32-bit projects will be skipped)")
                print("  Set PYXIS_CHECK_CMAKE_TOOLCHAIN_X86 or install xwin at ~/.xwin")

            if toolchain_x64:
                env["PYXIS_CHECK_CMAKE_TOOLCHAIN_X64"] = toolchain_x64
                print(f"X64 toolchain: {toolchain_x64}")

            xwin_root = find_xwin_root()
            if xwin_root:
                env["XWIN_ROOT"] = xwin_root
                print(f"XWIN_ROOT:     {xwin_root}")

            clang_prefix = find_clang_cl()
            if clang_prefix is None:
                print("Warning: clang-cl not found. C++ checks will likely fail.")
                print("  Install LLVM (>= 16) or use nix-shell -p llvmPackages_21.clang llvmPackages_21.llvm lld")
            elif clang_prefix:
                build_py_cmd_prefix = clang_prefix
                print(f"clang-cl via:  {' '.join(clang_prefix)}")
        else:
            # Non-Linux (Windows/macOS): assume the host compiler (MSVC on
            # Windows, clang on macOS) is on PATH. No xwin toolchain needed.
            # build.py will use the native CMake generator.
            print("Host is not Linux — assuming native C++ compiler is on PATH")
            print("  (set PYXIS_CHECK_CMAKE_TOOLCHAIN_X86/X64 to override)")

    if do_rust:
        # Ensure Windows targets are installed
        print("\n=== Checking Rust Windows targets ===")
        for target in ["i686-pc-windows-msvc", "x86_64-pc-windows-msvc"]:
            result = subprocess.run(
                ["rustup", "target", "list", "--installed"],
                capture_output=True, text=True,
            )
            if target not in result.stdout:
                print(f"Installing {target}...")
                subprocess.run(["rustup", "target", "add", target], check=True)
            else:
                print(f"  {target}: already installed")

    # Step 2: Delegate to build.py, which handles pyxis installation and
    # compile-checking every project. We pass --path so build.py installs
    # pyxis from the local checkout.
    backends_str = ",".join(backends)
    print(f"\n=== Running build.py --check-builds {backends_str} ===")

    build_py_args = [
        str(pyxis_defs / "build.py"),
        "--path",
        str(pyxis_repo / "driver"),
        "--check-builds",
        backends_str,
    ]
    if args.output_dir:
        # Resolve relative to the script's location so it's independent of cwd.
        output_dir = Path(args.output_dir)
        if not output_dir.is_absolute():
            output_dir = (Path(__file__).resolve().parent.parent / output_dir).resolve()
        build_py_args.extend(["--output-dir", str(output_dir)])

    if build_py_cmd_prefix:
        # nix-shell --run takes a single command string. Export the env vars
        # explicitly in the inner shell command so they survive the nix-shell
        # boundary.
        env_exports = []
        for key in ["PYXIS_CHECK_CMAKE_TOOLCHAIN_X86", "PYXIS_CHECK_CMAKE_TOOLCHAIN_X64", "XWIN_ROOT"]:
            if env.get(key):
                env_exports.append(f"export {key}={env[key]!r}")
        env_export_str = "; ".join(env_exports)
        if env_export_str:
            env_export_str += "; "
        inner_cmd = env_export_str + " ".join([sys.executable] + build_py_args)
        cmd = build_py_cmd_prefix + [inner_cmd]
    else:
        cmd = [sys.executable] + build_py_args

    result = subprocess.run(cmd, env=env, cwd=str(pyxis_defs))

    if result.returncode == 0:
        print(f"\n{'=' * 60}")
        print("All pyxis-defs build checks passed!")
        print(f"{'=' * 60}")
    else:
        print(f"\n{'=' * 60}")
        print(f"[FAIL] pyxis-defs build checks failed (exit {result.returncode})")
        print(f"{'=' * 60}")
        sys.exit(result.returncode)


if __name__ == "__main__":
    main()
