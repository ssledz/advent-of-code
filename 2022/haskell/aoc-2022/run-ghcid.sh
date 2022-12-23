#!/bin/bash
set -eux
ghcid -c "cabal repl $1"
