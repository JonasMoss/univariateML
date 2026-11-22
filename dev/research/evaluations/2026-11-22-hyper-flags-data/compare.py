"""Compare the three fresh fit snapshots to the stored baseline.

Run from the worktree root after writing build/snapshots/hyper-flags/{native,double}.
The raw comparison records the transport changes. The second comparison
checks every other field using the worktree's diff_baseline.py.
"""
from collections import Counter
import gzip
import json
from pathlib import Path
import subprocess
import sys

families = {"hyper", "nhyper", "triang"}
root = Path("build/snapshots/hyper-flags")
baseline = Path("dev/research/evaluations/2026-11-15-fit-snapshot-data")
diff = Path("build/scratch/diff_baseline.py")
allowed = {"warnings", "candidates"}


def compare(mode):
    old = root / f"{mode}-baseline.jsonl.gz"
    with gzip.open(baseline / f"{mode}.jsonl.gz", "rt") as source, gzip.open(old, "wt") as subset:
        for line in source:
            if json.loads(line)["family"] in families:
                subset.write(line)

    raw = root / mode
    print(f"{mode}, raw binding output", flush=True)
    subprocess.run([sys.executable, diff, raw, old], check=False)
    verify_maximizer_payloads(raw)
    normalized = root / f"{mode}-core"
    normalized.mkdir(exist_ok=True)
    old_core = root / f"{mode}-baseline-core.jsonl.gz"
    with gzip.open(old, "rt") as source, gzip.open(old_core, "wt") as subset:
        for line in source:
            subset.write(normalize(line) + "\n")
    for file in raw.glob("*.jsonl"):
        with file.open() as source, (normalized / file.name).open("w") as target:
            target.write(next(source))
            for line in source:
                target.write(normalize(line) + "\n")
    print(f"{mode}, ignoring estimate warning and maximizer payloads", flush=True)
    return subprocess.run([sys.executable, diff, normalized, old_core], check=False).returncode


def verify_maximizer_payloads(raw):
    counts = Counter()
    singles = []
    for file in sorted(raw.glob("*.jsonl")):
        with file.open() as source:
            next(source)
            for line in source:
                row = json.loads(line)
                fit = row["result"]
                if fit.get("unique") is not False:
                    continue
                assert not fit["warnings"], row
                values = [candidate["estimates"] for candidate in fit["candidates"]]
                assert values and values[0] == fit["estimates"], row
                assert len({json.dumps(value, sort_keys=True) for value in values}) == len(values), row
                counts[row["family"]] += 1
                if len(values) < 2:
                    singles.append((row["family"], row["case"], row["held"]))
    print("Verified non-unique payload counts:", dict(counts), flush=True)
    print("Cases with only one representable listed point:", singles, flush=True)


def normalize(line):
    row = json.loads(line)
    result = row["result"]
    if (result.get("outcome") or {}).get("kind") == "estimate":
        for field in allowed:
            result.pop(field, None)
    return json.dumps(row)


if __name__ == "__main__":
    sys.exit(max(compare(mode) for mode in ("native", "double")))
