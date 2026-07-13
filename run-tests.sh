#!/usr/bin/env bash
# Run the ghostel ERT suites headless.
# Usage: ./run-tests.sh [ert-selector]   e.g. ./run-tests.sh "gtx-.*"
# Section prefixes: gtl- (library), gta- (agents), gtt- (terminals),
# gtx- (cross-kind).
set -euo pipefail
cd "$(dirname "$0")"
exec emacs -batch -l ert \
  -l config/ghostel-toggle.el \
  -l config/ghostel-terminals.el \
  -l config/ghostel-agents.el \
  -l config/ghostel-toggle-tests.el \
  --eval "(ert-run-tests-batch-and-exit \"${1:-t}\")"
