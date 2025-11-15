#!/usr/bin/env python3
"""
Cross-platform test script for Pyxis.
Runs tests with different pointer sizes, codegen tests, clippy, and fmt.
"""

import os
import sys
import subprocess


def run_command(cmd, env=None):
    """Run a command and exit if it fails."""
    print(f"\n{'=' * 60}")
    print(f"Running: {' '.join(cmd)}")
    print(f"{'=' * 60}\n")

    result = subprocess.run(cmd, env=env)
    if result.returncode != 0:
        print(f"\n[FAIL] Command failed with exit code {result.returncode}")
        sys.exit(result.returncode)
    print("\n[PASS] Command succeeded")
    return result


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

    # Run clippy (with RUSTFLAGS to treat warnings as errors, matching CI)
    clippy_env = os.environ.copy()
    clippy_env["RUSTFLAGS"] = "-Dwarnings"
    run_command(["cargo", "clippy", "--all-targets", "--all-features"], env=clippy_env)

    # Run fmt check
    run_command(["cargo", "fmt", "--", "--check"])

    print(f"\n{'=' * 60}")
    print("All checks passed!")
    print(f"{'=' * 60}\n")


if __name__ == "__main__":
    main()
