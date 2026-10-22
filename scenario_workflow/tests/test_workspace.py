import importlib.util
from pathlib import Path
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("prepare", ROOT / "prepare_workspace.py")
prepare = importlib.util.module_from_spec(spec)
spec.loader.exec_module(prepare)


class WorkspaceTests(unittest.TestCase):
    def test_isolation_and_hashes(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            nflow, river = base / "inputs/nflow", base / "inputs/river"
            for name in prepare.COMMON:
                p = nflow / name
                p.parent.mkdir(parents=True, exist_ok=True)
                p.write_bytes(b"fixture")
            for name in prepare.STATIC:
                p = river / "Data_pd" / name
                p.parent.mkdir(parents=True, exist_ok=True)
                p.write_bytes(b"fixture")
            for name in prepare.RIVER:
                (river / name).write_bytes(b"fixture")
            (nflow / "Result2030.csv").write_bytes(b"stale")
            (river / "Data_pd/OUTleadis2030.rdat").write_bytes(b"stale")
            (nflow / "global_17_IAMC.gdx").write_bytes(b"fixture")
            target = base / "run"
            prepare.prepare(target, nflow, river, "copy", plan_only=True)
            self.assertFalse(target.exists())
            prepare.prepare(target, nflow, river, "copy")
            prepare.verify(target)
            self.assertFalse((target / "NflowModel/data/Result2030.csv").exists())
            self.assertFalse((target / "VISIToRN/Data_pd/OUTleadis2030.rdat").exists())
            self.assertTrue((target / "NflowModel/scenario/eight_cases.env").exists())
            with self.assertRaises(FileExistsError):
                prepare.prepare(target, nflow, river, "copy")
            with self.assertRaises(ValueError):
                prepare.prepare(nflow / "nested", nflow, river, "copy")
            (target / "NflowModel/data/global_17_IAMC.gdx").write_bytes(b"changed")
            with self.assertRaises(RuntimeError):
                prepare.verify(target)


if __name__ == "__main__":
    unittest.main()
