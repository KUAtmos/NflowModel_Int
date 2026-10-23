"""Create an isolated run workspace without modifying any source inputs.

Python is used only for file staging/provenance, not scientific calculations.
The default symlink mode requires immutable input files. Use copy for a snapshot.
"""
import argparse
import hashlib
import json
from pathlib import Path
import shutil

PACKAGE = Path(__file__).resolve().parent
COMMON = (
    "FUBC_1_to_9_data.csv", "region_iso_17.map", "data_prep.gdx",
    "FAOSTAT_data_en_10-10-2024.csv", "UNSD_Methodology.csv",
    "IFADATA_Plant_Nutrition_query.csv", "resampled_df.rdat",
    "0505PRECTOT_test.nc4", "FAOSTAT_feed.csv",
    "FAOSTAT_livstock2010.csv", "WAFCT_2019.csv",
)
PATTERNS = (
    "global_17_IAMC*.gdx", "result_SSP2_*.gdx", "results_SSP2_*.gdx",
    "livdis_SSP2_*.nc", "BW_map_*.csv",
    "0505NDRYMON_*.nc", "0505NWETMON_*.nc",
)
STATIC = (
    "LandMask_Loop.rdat", "DistMat.rdat", "VISITro_nat_ag.rdat",
    "RivArea.rdat", "HumanN_future.rdat", "dat_tas.rdat", "dat_dir.rdat",
)
RIVER = ("rivnum05.asc", "rivnum05.txt", "trip_0.5x0.5.nc",
         "visit_20260311_atmosclus_NMIP3_TEST_fNleach_natonly.nc")


def sha256(path):
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(4 * 1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def input_plan(nflow, visitorn):
    required = [(nflow / name, Path("NflowModel/data") / name) for name in COMMON]
    required += [(visitorn / "Data_pd" / name, Path("VISIToRN/Data_pd") / name)
                 for name in STATIC]
    required += [(visitorn / name, Path("VISIToRN") / name) for name in RIVER]
    missing = [str(src) for src, _ in required if not src.is_file()]
    if missing:
        raise ValueError("Required inputs are missing:\n" + "\n".join(missing))
    optional = {(p, Path("NflowModel/data") / p.name)
                for pattern in PATTERNS for p in nflow.glob(pattern) if p.is_file()}
    return sorted(set(required) | optional, key=lambda item: str(item[1]))


def prepare(target, nflow, visitorn, mode, plan_only=False):
    target, nflow, visitorn = [p.resolve() for p in (target, nflow, visitorn)]
    # Neither nested output nor a broad ancestor can be a safe new workspace.
    for source in (PACKAGE, nflow, visitorn):
        if source == target or source in target.parents or target in source.parents:
            raise ValueError("Workspace must be separate from the package and input sources.")
    if target.exists():
        raise FileExistsError("Workspace already exists; use a new path: " + str(target))
    plan = input_plan(nflow, visitorn)
    if plan_only:
        print(json.dumps([{"source": str(s), "destination": str(d)} for s, d in plan],
                         indent=2))
        return
    target.mkdir(parents=True, exist_ok=False)
    # Copy code/config only. Never inherit outputs or a writable Data_pd directory.
    for relative in ("NflowModel/R", "NflowModel/scenario", "VISIToRN/scenario"):
        shutil.copytree(PACKAGE / relative, target / relative)
    for relative in ("VISIToRN/VISIToRN_func.R", "VISIToRN/safe_save_rdata.R",
                     "SOURCE_SNAPSHOT.json", "prepare_workspace.py", "run_stage.sh"):
        shutil.copy2(PACKAGE / relative, target / relative)
    records = []
    for src, relative in plan:
        dst = target / relative
        dst.parent.mkdir(parents=True, exist_ok=True)
        before = sha256(src)
        if mode == "symlink":
            dst.symlink_to(src)
        else:
            shutil.copy2(src, dst)
        if sha256(dst) != before:
            raise RuntimeError("Input changed while staging: " + str(src))
        records.append({"path": relative.as_posix(), "source": str(src),
                        "size_bytes": dst.stat().st_size, "sha256": before})
    manifest = {"mode": mode, "inputs": records,
                "note": "Files selected by name, not a scientific compatibility approval."}
    (target / "INPUT_SNAPSHOT.json").write_text(json.dumps(manifest, indent=2) + "\n",
                                                encoding="utf-8")
    print("Prepared " + str(target))
    print("Next: run the R --list and --dry-run checks; verify input provenance before computing.")


def verify(target):
    manifest = json.loads((target / "INPUT_SNAPSHOT.json").read_text(encoding="utf-8"))
    for record in manifest["inputs"]:
        path = target / record["path"]
        if not path.is_file() or sha256(path) != record["sha256"]:
            raise RuntimeError("Missing or changed input: " + str(path))
    print("Verified " + str(len(manifest["inputs"])) + " input hashes.")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--target", type=Path, required=True)
    parser.add_argument("--nflow-data", type=Path)
    parser.add_argument("--visitorn-source", type=Path)
    parser.add_argument("--mode", choices=("symlink", "copy"), default="symlink")
    parser.add_argument("--plan", action="store_true")
    parser.add_argument("--verify", action="store_true")
    args = parser.parse_args()
    if args.verify:
        verify(args.target.resolve())
    else:
        if args.nflow_data is None or args.visitorn_source is None:
            parser.error("--nflow-data and --visitorn-source are required for staging")
        prepare(args.target, args.nflow_data, args.visitorn_source, args.mode, args.plan)


if __name__ == "__main__":
    main()
