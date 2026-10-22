# Multi-scenario workflow addition

Date: 22 October 2026

## Preserved GMD implementation

The 199 tracked files under the root NflowModel and VISIToRN directories are
unchanged from commit bc4bc551de43fd016c2393e843160819c2885e8c. This includes
the original model, plotting/decomposition programs, MCMC programs and tracked
inputs. No upstream scientific output is replaced by this update.

The new root launcher run_gmd.sh sets the correct working directory, requires an
explicit GAMS_DIR and uses the actual filename VISIToRN_input.r. The R driver
tools/run_gmd.R retains one R session for all stages and handles the working
directory change before decomposition. Separate Rscript processes would lose
the plotting/decomposition objects produced by the calculation stage. This
launcher correction does not modify the preserved scientific programs.
VISIToRN_input.r still has its original installation-specific GAMS path;
the GMD guide explicitly describes adapting that path in a working copy.
GMD numerical results were not recomputed during this packaging task.

## Additional workflow

scenario_workflow contains an allowlisted snapshot of NflowModel_Int_0 sources:
eight CM/DFW/NUE combinations, the two additional AP cases, independently
selected plotting, grid exports, scenario-specific VISIT natural-input assembly,
and VISIToRN routing/comparisons. The upstream source SHA-256 values are recorded
in SOURCE_SNAPSHOT.json, before the packaging changes listed below.

Operating instructions for this additional workflow are in
[scenario_workflow/USER_GUIDE.md](scenario_workflow/USER_GUIDE.md).
README.md and README.docx describe only the GMD study reproduction workflow.

Imported scientific equations, NUE signed-leaching treatment, the pasture gas
context fix and fixed VISIToRN calibrated parameters are retained. The upstream
PLUM livestock-allocation and boundary-grid fixes must be present in the
external input archive; PLUM and AIM-Hub executables are not included.

The registry is the existing CACN/_No family. The new 2020NDC calculations are
not silently relabelled or mixed into it. AP infrastructure is included, but
users must verify that their reference and AP deposition experiments are matched.

## Packaging-only changes to imported sources

- config.R and scenario_registry.R: add RUN_SCENARIOS to select an explicit
  subset of registered cases; retain existing switches and ten-case run-all.
  Clarify that the GMD-input switch selects inputs, not the historical engine.
- run_nflow_scenarios.R: refuse non-empty case outputs; record input MD5 checksums.
- test_grid_emissions_csv.R: explicitly skip the newer single-run export hook
  if Make_Result_csv.r is absent. That newer GMD program is deliberately not
  imported over the preserved root GMD implementation.
- New configs, portable stage/PBS launchers, input staging/checksum tooling and
  packaging tests. Staging copies code and links/copies selected data into a
  new directory; it never copies old result caches or whole writable data folders.

Original whitespace is retained in the imported scientific kernels. Git may
normalize their line endings; the provenance hashes describe the source bytes.

Feedback experiment controllers, credentials, cluster-specific job files,
simulation outputs and new large input datasets are not included.

## Verification on 22 October 2026

Tests use small synthetic fixtures, not a full global rerun:

- Scenario registry and NUE transformations, including historical input names.
- Explicit eight-/ten-case selection and invalid-selection rejection.
- Existing Nflow result protection.
- Future-manure helpers and pasture/cropland gas-context coverage.
- Grid-emission schema, species accounting, signed values and export CLI.
- Saved-result Nflow plotting and the configuration-driven plot-only wrapper.
- VISIToRN scenario input preparation, stable scenario styles, common 2010
  reference, Tg figure conversion and comparison-only plotting.
- Workspace isolation, ignored stale outputs, existing-target rejection and
  changed-input hash detection.
- GMD launcher orchestration with synthetic stage scripts: shared R objects,
  stage order, working directories, required GAMS_DIR and failure propagation.

Environment: Windows, R 4.5.3, Python 3. Synthetic R tests were run from an
ASCII-only temporary copy because Rscript did not preserve the original
Japanese command-line path. The test copy is not a new scientific input archive.
Plot tests include intentionally incomplete fixture records and may emit
missing-value warnings. Full global routing, GAMS input-schema validation,
upstream model convergence and GMD numerical equivalence were not tested here.

README.docx is rebuilt from README.md with tools/build_readme.py. Its pages are
visually checked using Word PDF export and bundled Poppler; the packaged
LibreOffice renderer is unavailable on this Windows host. QA intermediates are
not published. The manual focuses on GMD setup, required input data, shell
execution and the main output files. Detailed data checks, calibration and
reproducibility-limit discussions, and footer text are omitted from the manual.
