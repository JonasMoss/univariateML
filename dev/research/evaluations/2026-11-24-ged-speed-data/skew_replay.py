"""Compare both skew-coordinate users on the TASK-220 frozen oracle grid."""
import gzip
import json
import math
import os
from pathlib import Path
import subprocess
import sys

import mpmath as mp

mp.mp.dps = 60
data = Path(__file__).resolve().parent
previous = data.parent / "2026-11-14-sged-tail-data"
work = Path("build/scratch/ged-speed")
include = Path("univariateML-cpp/include")
boost = os.environ.get("BOOST_INCLUDE") or subprocess.check_output(
    ["Rscript", "-e", 'cat(system.file("include", package="BH"))'], text=True)

with gzip.open(previous / "reference.json.gz", "rt") as stream:
    rows = [r for r in json.load(stream)["rows"]
            if r["family"] in ("sged", "sstd") and r["theta"][3] != 1]

probes = "".join(f'p {r["family"]} ' + " ".join(map(repr, r["theta"])) +
                 f' {r["x"]!r}\n' for r in rows)
reference = []
min_log = mp.log(sys.float_info.min)
for row in rows:
    logp = list(map(mp.mpf, row["ref"]))
    reference.append([p if mp.isfinite(p) and p != 0 else None for p in logp] +
                     [mp.exp(p) if min_log <= p < 0 else None for p in logp])


def evaluate(mode, headers):
    binary = work / "skew-probe"
    flags = ["-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS"] if mode == "double" else []
    subprocess.run([os.environ.get("CXX", "g++"), "-std=c++17", "-O2", *flags,
                    "-I", boost, "-I", str(headers), str(previous / "evaluate.cpp"),
                    "-o", str(binary)], check=True)
    output = subprocess.check_output([binary], text=True, input=probes)
    return [list(map(float, line.split())) for line in output.splitlines()]


def error(value, exact, log):
    if not math.isfinite(value):
        return math.inf
    scale = max(1, abs(exact)) if log else exact
    return float(abs(value - exact) / scale)


results = {}
failures = {}
for mode in ("native", "double"):
    before = evaluate(mode, work / "master" / include)
    after = evaluate(mode, include)
    summary = {}
    failures[mode] = []
    for row, refs, old, new in zip(rows, reference, before, after, strict=True):
        for i, (ref, a, b) in enumerate(zip(refs, old, new, strict=True)):
            if ref is None:
                continue
            log = i < 2
            key = f'{row["family"]}/{"log" if log else "natural"}'
            counts = summary.setdefault(key, dict(checks=0, changed=0, before_failures=0,
                                                 after_failures=0, new_failures=0))
            old_error, new_error = error(a, ref, log), error(b, ref, log)
            counts["checks"] += 1
            counts["changed"] += a != b and not (math.isnan(a) and math.isnan(b))
            counts["before_failures"] += old_error > 1e-10
            counts["after_failures"] += new_error > 1e-10
            counts["new_failures"] += old_error <= 1e-10 < new_error
            if old_error > 1e-10 or new_error > 1e-10:
                failures[mode].append(dict(
                    family=row["family"], theta=row["theta"], x=row["x"], lower=i % 2 == 0,
                    log=log, reference=str(ref), master=repr(a), optimized=repr(b)))
    results[mode] = summary
    print(mode, summary, flush=True)
(data / "skew-replay.json").write_text(
    json.dumps(dict(summary=results, failures=failures), indent=2) + "\n")
assert all(s["new_failures"] == 0 for mode in results.values() for s in mode.values())
