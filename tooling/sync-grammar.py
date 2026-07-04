#!/usr/bin/env python3
"""Push tree-sitter-pyxis grammar changes and re-sync the Zed extension pin.

The grammar lives in a separate repo (ferrobrew/tree-sitter-pyxis), vendored
here as the `tooling/tree-sitter-pyxis` submodule, because Zed sources extension
grammars from a git `repository` + `commit`. Updating it therefore spans two
repos: regenerate the parser, test it, push it to the grammar repo, and re-pin
both the submodule and `tooling/zed-pyxis/extension.toml` at the new SHA. This
script does all of that in one go and keeps the two pins in lockstep:

    python tooling/sync-grammar.py -m "Add widget syntax"   # grammar changed
    python tooling/sync-grammar.py                          # just re-sync pins

Pass -m/--message when the grammar tree has changes. With a clean tree it
re-points the submodule and extension.toml at the submodule's current HEAD.
"""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
GRAMMAR = REPO / "tooling" / "tree-sitter-pyxis"
EXTENSION_TOML = REPO / "tooling" / "zed-pyxis" / "extension.toml"
# Zed's compiled dev-extension grammar (gitignored build output). Clearing it
# makes the next reinstall fetch and rebuild against the freshly pinned SHA.
ZED_GRAMMAR_CACHE = [
    REPO / "tooling" / "zed-pyxis" / "grammars" / "pyxis",
    REPO / "tooling" / "zed-pyxis" / "grammars" / "pyxis.wasm",
    REPO / "tooling" / "zed-pyxis" / "extension.wasm",
]


def run(cmd: list[str], cwd: Path, capture: bool = False) -> str:
    """Run a command, streaming (or capturing) output; abort on failure."""
    printed = " ".join(cmd)
    print(f"  $ {printed}  (in {cwd.relative_to(REPO)})")
    result = subprocess.run(
        cmd, cwd=cwd, text=True,
        stdout=subprocess.PIPE if capture else None,
        stderr=subprocess.STDOUT if capture else None,
    )
    if result.returncode != 0:
        if capture and result.stdout:
            print(result.stdout, file=sys.stderr)
        sys.exit(f"error: `{printed}` failed with exit {result.returncode}")
    return (result.stdout or "").strip()


def git(args: list[str], cwd: Path, capture: bool = True) -> str:
    return run(["git", *args], cwd, capture=capture)


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("-m", "--message",
                    help="commit message for grammar changes (required if the "
                         "grammar tree has uncommitted changes)")
    ap.add_argument("--no-push", action="store_true",
                    help="regenerate, test, and commit the grammar but don't "
                         "push or update the pins (dry run of the grammar side)")
    ap.add_argument("--commit-parent", action="store_true",
                    help="also commit the submodule-pin + extension.toml bump in "
                         "this repo (default: stage them and leave the commit to "
                         "you). Does not push this repo.")
    args = ap.parse_args()

    if not (GRAMMAR / ".git").exists():
        sys.exit("error: submodule not checked out. Run "
                 "`git submodule update --init tooling/tree-sitter-pyxis`.")

    branch = git(["branch", "--show-current"], GRAMMAR)
    if branch != "main":
        sys.exit(f"error: grammar submodule is on '{branch or 'a detached HEAD'}', "
                 "not 'main'. Run `git -C tooling/tree-sitter-pyxis checkout main` "
                 "first (Zed tracks main).")

    # Fast-forward onto origin/main so the later push stays a fast-forward.
    print("→ syncing grammar with origin/main")
    git(["fetch", "origin", "main"], GRAMMAR)
    behind = git(["rev-list", "--count", "HEAD..origin/main"], GRAMMAR)
    ahead = git(["rev-list", "--count", "origin/main..HEAD"], GRAMMAR)
    if behind != "0" and ahead != "0":
        sys.exit("error: grammar main has diverged from origin/main. Reconcile "
                 "manually before syncing.")
    if behind != "0":
        git(["merge", "--ff-only", "origin/main"], GRAMMAR)

    print("→ regenerating and testing the parser")
    if not (GRAMMAR / "node_modules" / ".bin").exists():
        run(["npm", "install"], GRAMMAR)
    run(["npm", "run", "generate"], GRAMMAR)  # pinned to --abi 14 in package.json
    run(["npm", "test"], GRAMMAR)

    # Commit any regenerated/edited grammar files.
    dirty = git(["status", "--porcelain"], GRAMMAR)
    if dirty:
        if not args.message:
            sys.exit("error: grammar tree has changes but no -m/--message given.\n"
                     f"{dirty}")
        print("→ committing grammar changes")
        git(["add", "-A"], GRAMMAR, capture=False)
        git(["commit", "-m", args.message], GRAMMAR, capture=False)
    else:
        print("→ grammar tree clean; nothing to commit")

    unpushed = git(["rev-list", "--count", "origin/main..HEAD"], GRAMMAR)
    if args.no_push:
        print(f"→ --no-push: leaving {unpushed} local grammar commit(s) unpushed; "
              "pins not updated")
        return
    if unpushed != "0":
        print(f"→ pushing {unpushed} grammar commit(s) to origin/main")
        git(["push", "origin", "main"], GRAMMAR, capture=False)
    else:
        print("→ nothing to push")

    new_sha = git(["rev-parse", "HEAD"], GRAMMAR)
    print(f"→ grammar is at {new_sha}")

    # Re-point extension.toml at the concrete SHA.
    toml = EXTENSION_TOML.read_text()
    updated, n = re.subn(r'(?m)^commit = ".*"$', f'commit = "{new_sha}"', toml)
    if n != 1:
        sys.exit(f"error: expected exactly one `commit = \"...\"` line in "
                 f"{EXTENSION_TOML.relative_to(REPO)}, found {n}.")
    if updated != toml:
        EXTENSION_TOML.write_text(updated)
        print(f"→ pinned extension.toml to {new_sha}")
    else:
        print("→ extension.toml already pinned to this SHA")

    # Clear Zed's cached compiled grammar so a reinstall fetches the new SHA.
    cleared = [p for p in ZED_GRAMMAR_CACHE if p.exists()]
    for p in cleared:
        run(["rm", "-rf", str(p)], REPO)
    if cleared:
        print("→ cleared Zed dev-extension grammar cache")

    # Stage the two pins in this repo.
    git(["add", str(GRAMMAR), str(EXTENSION_TOML)], REPO, capture=False)

    if args.commit_parent:
        msg = args.message or f"Bump tree-sitter-pyxis grammar to {new_sha[:10]}"
        print("→ committing pin bump in this repo")
        git(["commit", "-m",
             f"Bump tree-sitter-pyxis grammar to {new_sha[:10]}\n\n{msg}"],
            REPO, capture=False)
        print("\nDone. Not pushed — run `git push` here when ready.")
    else:
        print("\nDone. Staged the submodule pin + extension.toml bump in this "
              "repo; commit them when ready (or re-run with --commit-parent).")
    print("Then reinstall the Zed dev extension to pick up the new grammar.")


if __name__ == "__main__":
    main()
