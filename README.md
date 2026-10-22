# NflowModel and VISIToRN user guide

Yuto Osugi | Updated 22 October 2026

This repository contains the agricultural nitrogen-flow model and its connection
to the existing VISIT vegetation model and VISIToRN river-routing module.
It now provides two **separate workflows**. Use the preserved root workflow for
the GMD paper; use the additional workflow for multi-scenario experiments.
The Word manual, [README.docx](README.docx), covers the same operating procedures.

## 1 Choose the workflow

| Purpose | Code and configuration | Input family |
| --- | --- | --- |
| Reproduce the GMD study | Root `NflowModel/` and `VISIToRN/`; `run_gmd.sh` | Historical Baseline and 500C, without the trailing `_No` |
| Additional eight-case factorial analysis | `scenario_workflow/`; `eight_cases.env` | Four agricultural `_No` cases, each with NUE off/on |
| Additional ten-case analysis | Same code; `scenario_config.env` | Eight cases plus AP and NUE + AP |

The root scientific programs and tracked input files are retained unchanged from
commit `bc4bc551de43fd016c2393e843160819c2885e8c`. New analysis code is an
isolated snapshot of the working NflowModel_Int_0 implementation; it does not
replace the GMD model. Provenance and packaging changes are described in
[RELEASE_NOTES.md](RELEASE_NOTES.md) and
[`scenario_workflow/SOURCE_SNAPSHOT.json`](scenario_workflow/SOURCE_SNAPSHOT.json).

The new runner's `SCENARIO_GMD_INPUTS=on` selects historical **input names**, not
the historical scientific implementation. It is not a substitute for the root
workflow when reproducing the paper. Never mix results from the two workflows.

## 2 System preparation

Use R with GAMS and its `gdxrrw` interface. The multi-scenario workflow also
requires Python 3 for input staging/checksums and a C++ toolchain for VISIToRN
(`inline` and `RcppArmadillo`). Python does not perform the scientific model
calculations. Linux/Bash is recommended for the launchers and PBS example.
On Windows, use the R/Python entry points directly; use an ASCII-only workspace
path if R or GAMS cannot handle non-ASCII command-line paths.

R dependencies used by the additional workflow include `dplyr`, `tidyr`,
`ggplot2`, `gdxrrw`, `ncdf4`, `readr`, `Amelia`, `stringr`, `purrr`,
`terra`, `scales`, `maps`, `inline`, and `RcppArmadillo`. GMD plotting
additionally uses packages declared in the retained scripts, including `sf`,
`rnaturalearth`, `rnaturalearthdata`, `ggrepel`, and `forcats`.
Headless plotting needs an R build with Cairo support.

Set `GAMS_DIR` to your installed GAMS directory. Install and test the matching
`gdxrrw` interface separately. This release does not install software, provide a
GAMS license, or change calibration parameters.

```bash
export GAMS_DIR=/path/to/gams
Rscript -e 'gdxrrw::igdx(Sys.getenv("GAMS_DIR"))'
```

Run model calculations and large input checks in a compute-node allocation,
not on an HPC login node. `job_example.pbs` is a resource template, not a runtime
or memory guarantee. Space and time scale with scenarios, years and saved grids.
This update has not benchmarked a full global run.

## 3 Data preparation and the GMD workflow

A clone is not a complete scientific input archive. Obtain the matching external
inputs from their data providers; do not substitute newer inputs into a paper
reproduction run. The original manual required these additional GMD files:

| Directory | External files |
| --- | --- |
| `NflowModel/data/` | `livdis_SSP2_BaU_NoCC.nc`, `livdis_SSP2_500C_CACN_NoCC.nc`, `results_SSP2_BaU_NoCC.gdx`, `results_SSP2_500C_CACN_NoCC.gdx` |
| `VISIToRN/` | The same two PLUM `results_*.gdx` files; `visit_20260311_atmosclus_NMIP3_TEST_fNleach_natonly.nc` and `visit_20260311_atmosclus_NMIP3_TEST_fNleach_agronly.nc` |

Keep the repository's existing auxiliary data and the archived AIM-Hub,
deposition and livestock inputs together. The expected agricultural scenario
names are `SSP2_BaU_NoCC` and `SSP2_500C_CACN_NoCC`.
Preserve the valid historical 2010 reference.

From a dedicated copy of the repository, with the GMD inputs installed:

```bash
export GAMS_DIR=/path/to/gams
bash run_gmd.sh
```

This runs `Make_Result_csv.r`, `Visualization_Nmodel.r`,
`VISIToRN_input.r` and `Deco.R` from `NflowModel/R/`.
The new launcher corrects the historical shell wrapper's spelling of
`VISIToRN_input.r`; it does not modify model equations or the scientific scripts.
The GMD workflow retains its existing output conventions and can overwrite
generated files, so use a separate working copy.

Main outputs are agricultural nitrogen-flow tables and figures in
`NflowModel/output/`, and river nitrogen-load/concentration results and figures
in `VISIToRN/output/`. Intermediate files also appear under `data/` and
`Data_pd/`, as in the original implementation.

## 4 Additional scenarios and their inputs

The eight-case analysis combines three switches: climate mitigation (CM),
dietary change and food-waste reduction (DFW), and nitrogen-use efficiency (NUE).
The additional AP cases apply air-pollution-control deposition inputs.

| Scenario ID | Figure label | Underlying agricultural input |
| --- | --- | --- |
| `baseline` | REF | `SSP2_BaU_NoCC_No` |
| `500c` | CM | `SSP2_500C_CACN_NoCC_No` |
| `demfwr` | DFW | `SSP2_BaU_DEMFWR_No` |
| `nue` | NUE | REF inputs with NUE adjustment |
| `500c_demfwr` | CM + DFW | `SSP2_500C_CACN_DEMFWR_No` |
| `500c_nue` | CM + NUE | CM inputs with NUE adjustment |
| `demfwr_nue` | DFW + NUE | DFW inputs with NUE adjustment |
| `500c_demfwr_nue` | CM + DFW + NUE | CM + DFW inputs with NUE adjustment |
| `air_pollution` | AP | REF agricultural inputs; APMTFR deposition |
| `nue_air_pollution` | NUE + AP | Same AP inputs with NUE adjustment |

The current registry uses **CACN**, not the newer **2020NDC** pathway. Do not
relabel an input file to change policy meaning. A 2020NDC experiment requires
consistent AIM-Hub, PLUM and GEOS-Chem products and explicit registry updates;
it is not enabled by this publication update.

NUE targets in `NflowModel/scenario/data/nue_targets.csv` are 0.60 (2020),
0.65 (2030), 0.70 (2050) and 0.75 (2100), with linear interpolation.
The adjustment determines the required nitrogen input from crop uptake and the
target NUE, then accounts for manure, deposition and fixation to obtain
synthetic fertilizer input. Check `INCLUDE_RESIDUE_IN_NUE` when interpreting
uptake. The imported code retains signed leaching and the pasture gas-context
correction; this packaging update does not add a new clipping rule.

For each of the four agricultural cases provide matching AIM-Hub IAMC output,
PLUM land use, livestock density and body weight, and wet/dry deposition.
`input_data_config.R` is the authoritative filename resolver:

- AIM-Hub: `global_17_IAMC.gdx`, containing all four scenario records.
- PLUM: `result_<scenario>.gdx` or `results_<scenario>.gdx`;
  livestock: `livdis_<scenario>.nc`.
- Preferred body-weight files: `BW_map_BaU_No.csv`,
  `BW_map_500C_No.csv`, `BW_map_BaU_EAT_No.csv`,
  `BW_map_500C_EAT_No.csv`. The resolver has historical fallbacks;
  inspect the resolved inventory so a fallback is not mistaken for matched data.
- Deposition: `0505NDRYMON_<prefix><year>.nc` and
  `0505NWETMON_<prefix><year>.nc`. Future prefixes normally equal the
  agricultural scenario name; 2010 uses `Base`.
- AP uses prefix `SSP2_BaU_NoCC_No_APMTFR`. An AP comparison is scientifically
  interpretable only after verifying matched meteorology, model settings and
  reference-emission assumptions. Registry support alone does not establish this.

Use PLUM inputs retaining the corrected new-pasture livestock allocation and
boundary-grid multiplier averaging. Those upstream PLUM changes are input
requirements, not code supplied by this repository.

VISIToRN also needs the seven static `Data_pd` arrays listed in
`prepare_workspace.py`, basin IDs/names, TRIP drainage data and the VISIT
NMIP3 natural-leaching file. The staging tool checks common required filenames;
the R preflight checks selected scenario files. Neither check proves scientific
compatibility. All large inputs remain outside this new code package.

## 5 Prepare and calculate an isolated experiment

Create a new directory outside the repository and the source input directories.
The following are templates: replace the absolute paths with your own.
Staging links only selected input files, never an entire writable `Data_pd`
directory or previous output cache. It records SHA-256 hashes.

```bash
python3 scenario_workflow/prepare_workspace.py \
  --target /path/to/new_run \
  --nflow-data /path/to/inputs/NflowModel/data \
  --visitorn-source /path/to/inputs/VISIToRN \
  --mode symlink --plan

# Remove --plan after inspecting the selected files.
python3 scenario_workflow/prepare_workspace.py \
  --target /path/to/new_run \
  --nflow-data /path/to/inputs/NflowModel/data \
  --visitorn-source /path/to/inputs/VISIToRN \
  --mode symlink
```

Source files must remain immutable during the experiment. Use `--mode copy`
for an independent input snapshot or when symlinks are unavailable.
An existing target is refused. Partial preparations are not resumed automatically.

Edit the configuration **inside the new run directory**. Paths in an env file
are relative to that file, not to the shell's current directory.

| Setting | Meaning |
| --- | --- |
| `RUN_ALL_COMBINATIONS=on` | Generate all registered combinations; the current registry has ten, not sixteen |
| `RUN_SCENARIOS` | `all` or a list of generated IDs; `eight_cases.env` selects the eight-case factorial |
| Four `SCENARIO_*` switches | Select one combination when run-all is off; set `RUN_SCENARIOS=all` |
| `YEARS` | Future calculation years; default 2030, 2050, 2100 |
| `SUMMARY_REFERENCE_YEAR` | Shared 2010 Baseline reference |
| `OUTPUT_DIR` | Saved calculation directory; use a new directory for changed inputs |
| `PLOT_SCENARIOS`, `PLOT_YEARS` | Independently select saved cases/years for plotting |

```bash
cd /path/to/new_run
export GAMS_DIR=/path/to/gams
bash run_stage.sh preflight
bash run_stage.sh calculate
```

The launcher defaults to `eight_cases.env`. To use all ten registered cases:

```bash
bash run_stage.sh preflight NflowModel/scenario/scenario_config.env
bash run_stage.sh calculate NflowModel/scenario/scenario_config.env
```

On PBS, adapt `scenario_workflow/job_example.pbs` and submit a stage, for example
`qsub -v WORKSPACE=/path/to/new_run,STAGE=calculate,GAMS_DIR=/path/to/gams job_example.pbs`.
Node and queue names are site-specific. The package never submits jobs itself.

Unsupported AP combinations with CM or DFW fail explicitly. They need their own
upstream inputs before registry expansion. A sixteen-case analysis is not yet
available. No Nflow-to-GEOS-Chem feedback experiment is launched by these scripts.

## 6 Plot saved results and calculate river loads

After successful Nflow calculation, plotting is a separate stage:

```bash
bash run_stage.sh plot
# For ten cases, pass the same ten-case configuration as the second argument.
bash run_stage.sh natural
bash run_stage.sh route
bash run_stage.sh compare
```

`plot` reads completed Nflow outputs. Change only plotting switches to redraw
without recalculating. Global input/output comparisons default to future years;
river comparisons retain the common 2010 Baseline and show 2010, 2030, 2050 and
2100. River-load figures use Tg N per year; some intermediate tables retain
explicitly named Gg/kg columns.

`natural` prepares scenario-specific natural-leaching arrays from VISIT fluxes
and PLUM land use for 2010 and the three future years. It does not run VISIT.
`route` runs VISIToRN for all completed cases in the experiment; it uses fixed
calibrated parameters, not a new MCMC fit. `compare` redraws comparisons from
existing river results. The stage wrapper's routing/natural stages do not read
the env file's plotting selection. For subsets or custom years, invoke the R
entry points directly:

```bash
Rscript VISIToRN/scenario/run_visitorn_scenarios.R \
  --scenarios baseline,500c --years all --comparison-only on
```

Keep the Baseline result available for 2010 reference and basin ranking.
The wrapper is configured for the default `_No`, NMIP3 experiment and years;
advanced input-family experiments require corresponding natural-input settings.

Saved products include:

- `NflowModel/output/scenarios/<id>/`: summaries, per-year R data,
  grid emission/audit tables, resolved inputs, manifest and `completion.csv`.
- `VISIToRN/Data_pd/natural_by_landuse/`: new natural-input caches and metadata.
- `VISIToRN/output/scenarios/<id>/`: routing data, tables and per-case figures.
- `VISIToRN/output/comparisons/<selection>/`: global and basin comparisons.

Nflow refuses a non-empty case output directory. Use a fresh output directory
for recalculation; do not delete completion records merely to reuse old outputs.
The staging hash check detects changed inputs. VISIToRN's resume checks also
compare source signatures. A scheduler exit alone is not scientific validation:
check completion records, every requested case/year, units, input provenance
and the resulting mass balances before using results.

## 7 Verification and publication scope

The update retains the root GMD code and data; it does not rerun the paper.
Synthetic tests cover the scenario registry, NUE transformations, gas context,
manure helpers, grid exports, saved-result plotting, routing-input preparation
and river comparisons. They do not establish global numerical equivalence or
upstream AIM-Hub/PLUM/GEOS-Chem convergence.

```bash
python3 scenario_workflow/tests/run_tests.py
python3 scenario_workflow/prepare_workspace.py --target /path/to/new_run --verify
```

The first command needs Rscript on PATH and the relevant R dependencies.
The second checks a prepared input snapshot; it does not calculate the model.
See [RELEASE_NOTES.md](RELEASE_NOTES.md) for the checks actually performed.

Before the GMD publication release, archive the precise GMD external inputs,
software versions, calibration parameters and release commit with a persistent
identifier. This update does not create a DOI or redistribute upstream datasets
whose availability/licensing has not been established. Preserve existing
VISIT/VISIToRN attribution in [VISIToRN/README.md](VISIToRN/README.md) and the
source files when redistributing.
