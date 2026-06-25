#!/bin/bash
export GAMS_DIR="$HOME/gams39.3_linux_x64_64_sfx"

set -e

cd ../R

echo "Working directory: $(pwd)"
echo "Start: $(date)"

echo "Running Make_Result_csv.r ..."
Rscript Make_Result_csv.r

echo "Running Visualization_Nmodel.r ..."
Rscript Visualization_Nmodel.r

echo "Running VISIToRNinput.r ..."
Rscript VISIToRNinput.r

echo "Running Deco.R ..."
Rscript Deco.R

echo "All scripts finished successfully."
echo "End: $(date)"