#!/bin/sh
exec node --experimental-wasm-stringref --experimental-wasm-gc --experimental-wasm-stack-switching "$@"
