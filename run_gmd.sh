#!/usr/bin/env bash
# Portable launcher for the preserved root-level GMD workflow.
# Scientific scripts and inputs are unchanged from the recorded GMD baseline.
set -euo pipefail
root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
: "${GAMS_DIR:?Set GAMS_DIR to the installed GAMS directory}"
export GAMS_DIR
# Plotting and decomposition depend on objects retained in the same R session.
exec Rscript "$root/tools/run_gmd.R"
