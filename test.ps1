#!/usr/bin/env pwsh

$env:PYXIS_TEST_POINTER_SIZE = "4"
cargo test @args
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

$env:PYXIS_TEST_POINTER_SIZE = "8"
cargo test @args
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

cargo run --example codegen_tests
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

