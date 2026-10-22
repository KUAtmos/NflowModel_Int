# How to use the nitrogen flow model

Yuto Osugi | Updated 22 October 2026

This guide describes how to run the agricultural nitrogen-flow model and the
linked riverine nitrogen-load calculations used in the GMD study. The model
uses processed inputs from AIM-Hub, AIM-PLUM, GEOS-Chem and VISIT, and calculates
riverine nitrogen loads with VISIToRN.

## 1 Study settings

The GMD study uses the following two scenarios.

| Study scenario | Input scenario identifier | Main analysis years |
| --- | --- | --- |
| Baseline or BaU | `SSP2_BaU_NoCC` | 2010, 2030, 2050, 2100 |
| 500C | `SSP2_500C_CACN_NoCC` | 2030, 2050, 2100 |

Both scenarios share Baseline 2010 as the reference. Baseline 2020 is also
calculated for the ammonia-emission comparison.

## 2 Required software

Install R, GAMS with a compatible `gdxrrw` R interface, and a C++ compiler
compatible with R. On Windows, use the corresponding Rtools version.
Install the following R packages:

```r
install.packages(c(
  "dplyr", "tidyr", "tibble", "purrr", "readr", "stringr",
  "ggplot2", "scales", "patchwork", "cowplot", "ggrepel",
  "forcats", "ncdf4", "terra", "sf", "rnaturalearth",
  "rnaturalearthdata", "Amelia", "maps", "inline",
  "Rcpp", "RcppArmadillo"
))
```

Install `gdxrrw` separately for the GAMS installation in use.
Set `GAMS_DIR` in the shell commands below. Also set `gams_dir` near the
beginning of `NflowModel/R/VISIToRN_input.r` to the same installation path;
that script contains a separate path setting.

## 3 Input data

The repository includes auxiliary inputs, but the following processed files
must be obtained separately from the study authors or the relevant data providers.

Place these four files in `NflowModel/data/`:

- `livdis_SSP2_BaU_NoCC.nc`
- `livdis_SSP2_500C_CACN_NoCC.nc`
- `results_SSP2_BaU_NoCC.gdx`
- `results_SSP2_500C_CACN_NoCC.gdx`

Place the same two `results_*.gdx` files in `VISIToRN/`, together with:

- `visit_20260311_atmosclus_NMIP3_TEST_fNleach_natonly.nc`
- `visit_20260311_atmosclus_NMIP3_TEST_fNleach_agronly.nc`

The other input files are stored as follows:

| Input group | Location and files |
| --- | --- |
| AIM-Hub agricultural activity | `NflowModel/data/global_17_IAMC.gdx` |
| Grid and regional mappings | `NflowModel/data/data_prep.gdx`, `VISIToRN/data_prep.gdx`, and the supplied mapping files |
| Livestock body weights | `NflowModel/data/BW_map_BaU.csv` and `BW_map_500C.csv` |
| Atmospheric deposition | `0505NDRYMON_*.nc` and `0505NWETMON_*.nc` in `NflowModel/data/` |
| Agricultural statistics and soil data | The supplied FAOSTAT, IFA, FUBC and coefficient tables, `resampled_df.rdat`, and `0505PRECTOT_test.nc4` in `NflowModel/data/` |
| Routing and population | Static arrays and population data in `VISIToRN/Data_pd/`; TRIP and basin files in `VISIToRN/` |
| Evaluation data | CEDS ammonia data in `NflowModel/data/`, EDGAR data in `VISIToRN/Data_pd/`, and river observations in `VISIToRN/` |

For deposition, the filename suffixes are `Base2010`, `Base2020`,
`Base2030`, `Base2050`, `Base2100`, `500C2030`, `500C2050` and
`500C2100`. Each requires both a dry and a wet deposition file.

## 4 Run the model

The shell script `run_gmd.sh` runs the agricultural calculations, river routing,
plotting and factor decomposition. Replace the repository and GAMS paths below,
then execute these commands in Bash:

```bash
#!/usr/bin/env bash
set -euo pipefail
cd /path/to/NflowModel_Int
export GAMS_DIR=/path/to/gams
unset VISITORN_CASES VISITORN_ARRAY_ONLY VISITORN_ARRAY_OUTPUT_DIR
bash run_gmd.sh
```

## 5 Main outputs

The following tables and figures are saved in `NflowModel/output/`:

- `Result.csv`: global nitrogen inputs and outputs.
- `OUTgas_species*.csv`: gaseous nitrogen emissions by species and region.
- `plot1_finalrep.png`: global nitrogen flows.
- `Input_17_stacked_components*.png` and `Output_17_finalrep*.png`:
  regional nitrogen inputs and outputs.
- `Input_combined_rev.png` and `Outgas_combined.png`:
  maps of nitrogen inputs and gaseous losses.
- `NH3_emiRegion_2010.png` and `NH3_emiRegion_2020.png`:
  regional ammonia-emission comparisons.

The following results are saved in `VISIToRN/output/`:

- `Obs_compare_NMIP3.png`: comparison of modelled and observed river nitrogen loads.
- `River_Nload_top14_NMIP3.png`: nitrogen-load trends for major river basins and the global total.
- `C_fw_analysis_*_NMIP3.rdat` and `C_fw_summary_*_NMIP3.csv`:
  river nitrogen concentrations and summary statistics.
- `LMDI_Nload_decomposition_BaU_top14_absNload2010.png` and
  `LMDI_Nload_decomposition_500C_top14_absNload2010.png`:
  factor decomposition of river nitrogen-load changes.
