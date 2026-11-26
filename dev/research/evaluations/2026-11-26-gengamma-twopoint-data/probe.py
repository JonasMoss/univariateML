"""Replay the TASK-285 samples through the production binding before and after."""

import importlib.util
import json
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[4]
EVIDENCE = Path(__file__).resolve().parent
BASE = "2d89fd4a"
HEADER = "cppunivariate/distributions/gengamma/mle.hpp"
WORK = ROOT / "build/scratch/gengamma-twopoint-probe"
spec = importlib.util.spec_from_file_location("fit_snapshot", ROOT / "dev/validation/fit-snapshot.py")
snapshot = importlib.util.module_from_spec(spec)
spec.loader.exec_module(snapshot)

samples = json.loads((EVIDENCE / "reference.json").read_text())
holds = {"mu": 0.25, "sigma": 1.0, "Q": 0.4}
original_flags = snapshot.FLAGS
old_root = WORK / "baseline" / "include"
old_header = old_root / HEADER
old_header.parent.mkdir(parents=True, exist_ok=True)
old_header.write_text(subprocess.check_output(
    ["git", "show", f"{BASE}:univariateML-cpp/include/{HEADER}"], cwd=ROOT, text=True
).replace('"../../core/outcome.hpp"', '"cppunivariate/core/outcome.hpp"'))

output = {}
for mode, double in (("native", False), ("double", True)):
    output[mode] = {}
    for revision in ("baseline", "current"):
        build_dir = WORK / revision / mode
        build_dir.mkdir(parents=True, exist_ok=True)
        snapshot.FLAGS = [*original_flags, "-I" + str(old_root)] if revision == "baseline" else original_flags
        probe = snapshot.Probe(snapshot.build("gengamma", build_dir, double, snapshot.boost_include()))
        rows = []
        try:
            for name, sample in samples.items():
                for mask in range(8 if name == "four" else 1):
                    selected = {k: v for i, (k, v) in enumerate(holds.items()) if mask & (1 << i)}
                    case = {"held": selected, "mask": mask, "data": list(map(float, sample["data"]))}
                    result = probe.ask(snapshot.line(case, list(holds)), 300)
                    if not isinstance(result, dict):
                        raise RuntimeError(result)
                    rows.append({"sample": name, "held": selected, "result": result})
            endpoint = {"held": {"mu": 0.0}, "mask": 1, "data": [1.0, 1.0, 2.0, 2.0]}
            result = probe.ask(snapshot.line(endpoint, list(holds)), 300)
            if not isinstance(result, dict):
                raise RuntimeError(result)
            rows.append({"sample": "four", "held": endpoint["held"], "result": result})
        finally:
            probe.close()
        output[mode][revision] = rows

(EVIDENCE / "probe.json").write_text(json.dumps(output, indent=2) + "\n")

for mode, revisions in output.items():
    for before, after in zip(revisions["baseline"], revisions["current"]):
        old, new = before["result"], after["result"]
        route = lambda row: (row["outcome"]["kind"], row["reason"], row["model"])
        print(mode, after["sample"], after["held"], route(old), "→", route(new),
              "identical" if old == new else "changed")
