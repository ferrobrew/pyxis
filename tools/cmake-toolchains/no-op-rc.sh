#!/usr/bin/env bash
# No-op stand-in for llvm-rc. Used by xwin-x86.cmake to skip manifest
# preprocessing during cross-compilation on Linux (where CMake's vs_link_exe
# wrapper would otherwise hand the manifest to nix-wrapped cc and trip
# -fPIC injection). Writes a minimal-valid empty Win32 .res header so
# lld-link will accept the file without preprocessing anything.
set -e
out=""
while [ $# -gt 0 ]; do
    case "$1" in
        /fo)   out="$2"; shift 2 ;;
        /fo*)  out="${1#/fo}"; shift ;;
        *)     shift ;;
    esac
done
if [ -n "$out" ]; then
    mkdir -p "$(dirname "$out")"
    # 32-byte empty-marker .res header: DataSize=0, HeaderSize=0x20,
    # Type=0 (ordinal 0xFFFF/0x0000), Name=0 (ordinal 0xFFFF/0x0000), zero
    # everything else. Recognised by lld-link as a valid (but empty) RES.
    printf '\x00\x00\x00\x00\x20\x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00' > "$out"
fi
exit 0
