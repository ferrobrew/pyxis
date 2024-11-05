#!/bin/sh

PYXIS_TEST_POINTER_SIZE=4 cargo test "$@" && PYXIS_TEST_POINTER_SIZE=8 cargo test "$@" && cargo run --example codegen_tests
