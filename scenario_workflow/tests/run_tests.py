"""Run synthetic regression tests; no scientific input files or cluster jobs."""
import os
from pathlib import Path
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
R_TESTS = (
    "NflowModel/R/test_manure_future_helpers.R",
    "NflowModel/scenario/tests/test_scenario_extensions.R",
    "NflowModel/scenario/tests/test_case_selection.R",
    "NflowModel/scenario/tests/test_output_protection.R",
    "NflowModel/scenario/tests/test_gas_context.R",
    "NflowModel/scenario/tests/test_grid_emissions_csv.R",
    "NflowModel/scenario/tests/test_plot_nflow_scenarios.R",
    "VISIToRN/scenario/tests/test_visitorn_scenario_pipeline.R",
)
if __name__ == "__main__":
    rscript = os.environ.get("RSCRIPT") or shutil.which("Rscript")
    if not rscript:
        sys.exit("Rscript not found; set RSCRIPT to its executable path.")
    subprocess.run([sys.executable, str(ROOT / "tests/test_workspace.py")], check=True)
    for relative in R_TESTS:
        path = ROOT / relative
        print("Testing " + relative, flush=True)
        subprocess.run([rscript, str(path)], cwd=path.parent, check=True)
    print("All synthetic regression tests passed.")
