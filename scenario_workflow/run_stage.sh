#!/usr/bin/env bash
# Run inside a scheduler allocation on an HPC system, never on its login node.
set -euo pipefail
root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
stage="${1:?Usage: bash run_stage.sh preflight|calculate|plot|natural|route|compare [config]}"
config="${2:-$root/NflowModel/scenario/eight_cases.env}"
case "$stage" in
  preflight)
    Rscript "$root/NflowModel/scenario/run_nflow_scenarios.R" --config "$config" --list
    Rscript "$root/NflowModel/scenario/run_nflow_scenarios.R" --config "$config" --dry-run
    ;;
  calculate)
    python3 "$root/prepare_workspace.py" --target "$root" --verify
    Rscript "$root/NflowModel/scenario/run_nflow_scenarios.R" --config "$config"
    python3 "$root/prepare_workspace.py" --target "$root" --verify
    ;;
  plot)
    Rscript "$root/NflowModel/scenario/run_nflow_plots.R" --config "$config"
    ;;
  natural)
    python3 "$root/prepare_workspace.py" --target "$root" --verify
    Rscript "$root/VISIToRN/scenario/prepare_natural_inputs.R" \
      --years 2010,2030,2050,2100 --scenarios all
    ;;
  route)
    python3 "$root/prepare_workspace.py" --target "$root" --verify
    # Baseline's manifest includes 2010; all other cases use this shared reference.
    Rscript "$root/VISIToRN/scenario/run_visitorn_scenarios.R" --scenarios all --years all
    ;;
  compare)
    Rscript "$root/VISIToRN/scenario/run_visitorn_scenarios.R" \
      --scenarios all --years all --comparison-only on
    ;;
  *) printf 'Unknown stage: %s\n' "$stage" >&2; exit 2 ;;
esac
