"""Reuse the prior sweep, timing workloads and symmetric SGED replay."""
import argparse
import json
from pathlib import Path
import runpy

import sweep

sweep.DATA = Path(__file__).resolve().parent
work = Path("build/scratch/ged-speed")
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("phases", nargs="+", choices=("sweep", "benchmark", "replay_symmetric"))

for phase in parser.parse_args().phases:
    sweep.BIN = work / ("symmetric" if phase == "replay_symmetric" else "bin")
    if phase == "sweep":
        result, probes = sweep.summarize(list(sweep.observations()))
        (sweep.DATA / "accuracy.json").write_text(
            json.dumps(sweep.json_safe(result), indent=2, allow_nan=False) + "\n")
        (sweep.BIN / "inputs").write_text(probes)
        for mode in ("native", "double"):
            for metric, stats in result[f"gated-{mode}"].items():
                if "roundtrip" in metric:
                    continue
                assert stats["over"] == stats["subnormal_over"] == stats["nan"] == 0, (
                    mode, metric, stats["examples"], stats["subnormal_examples"])
            assert result[f"gated-{mode}"]["p/natural"]["subnormal"] == 496
    else:
        runpy.run_module(phase, run_name="__main__")
