"""Compare stored snapshots and replay all-zero hold patterns through the snapshot probe."""
import gzip
import json
from pathlib import Path
import subprocess
import sys

EVALUATIONS = Path(__file__).resolve().parents[1]
BASELINE = EVALUATIONS / "2026-11-15-fit-snapshot-data"
SCRATCH = Path("build/scratch/nhyper-zero")
DIFF = Path("build/scratch/diff_baseline.py")
CAP = 2**53 - 1


def key(row):
    return row["case"], row["mask"], row["input_sha256"]


def coordinates(estimates):
    return tuple(float.fromhex(estimates[name]) for name in ("m", "N", "r"))


def compare(mode):
    directory = SCRATCH / mode
    baseline = SCRATCH / f"{mode}-baseline.jsonl.gz"
    before = {}
    with gzip.open(BASELINE / f"{mode}.jsonl.gz", "rt") as source:
        with gzip.open(baseline, "wt") as subset:
            for line in source:
                row = json.loads(line)
                if row["family"] == "nhyper":
                    before[key(row)] = row
                    subset.write(line)

    print(f"\n{mode} stored-baseline comparison", flush=True)
    subprocess.run([sys.executable, DIFF, directory, baseline], check=False)
    with (directory / "nhyper.jsonl").open() as source:
        next(source)
        after = {key(row): row for row in map(json.loads, source)}
    assert before.keys() == after.keys()
    for k, row in after.items():
        old = before[k]["result"]
        new = row["result"]
        if old == new:
            continue
        fields = {field for field in old.keys() | new.keys() if old.get(field) != new.get(field)}
        assert fields <= {"warnings", "candidates", "unique", "outcome"}, (k, fields)
        assert new["outcome"]["kind"] == "estimate", (k, new)
        m, N, r = coordinates(new["estimates"])
        if {"unique", "outcome"} & fields:
            assert old["unique"] and not new["unique"], (k, old, new)
            assert old["outcome"] == dict(new["outcome"], unique=True), k
            assert r == 0 and float.fromhex(new["logLik"]) == 0, k
        candidates = [coordinates(candidate["estimates"]) for candidate in new["candidates"]]
        print(row["case"], row["held"], (m, N, r), sorted(fields), candidates)
    for label in ("constant", "zeros"):
        row = next(row for row in after.values() if row["source"] == "class" and row["case"] == label)
        print("phenomenon class", label, row["data"], row["held"], row["result"]["outcome"])


def replay(mode):
    cases = [
        (0, (0, 0, 0), [(0, 0, 0), (0, 1, 0)]),
        (4, (0, 0, 0), [(0, 0, 0), (0, 1, 0)]),
        (1, (2, 0, 0), [(2, 2, 0), (2, 3, 0)]),
        (5, (2, 0, 0), [(2, 2, 0), (2, 3, 0)]),
        (2, (0, 5, 0), [(0, 5, 0), (5, 5, 0)]),
        (6, (0, 5, 0), [(0, 5, 0), (5, 5, 0)]),
        (3, (2, 5, 0), [(2, 5, 0)]),
        (7, (2, 5, 0), [(2, 5, 0)]),
        (2, (0, 0, 0), [(0, 0, 0)]),
        (6, (0, 0, 0), [(0, 0, 0)]),
        (1, (CAP, 0, 0), [(CAP, CAP, 0)]),
        (5, (CAP, 0, 0), [(CAP, CAP, 0)]),
    ]
    inputs = "".join(f"{mask} 2 {m} {N} {r} 0 0\n" for mask, (m, N, r), _ in cases)
    results = probe(SCRATCH / mode / "build/nhyper", inputs)
    before = probe(SCRATCH / "base" / mode / "build/nhyper", inputs)
    assert len(results) == len(before) == len(cases)
    print(f"\n{mode} all-zero hold replay (mask, supplied m/N/r, uniqueness before/after, witnesses)")
    for (mask, held, expected), old, result in zip(cases, before, results):
        singleton = len(expected) == 1
        if not singleton and mask != 6:
            assert old["unique"] and old["outcome"]["unique"] and not old["candidates"]
            assert old == dict(result, unique=True, candidates=[],
                               outcome=dict(result["outcome"], unique=True))
        else:
            assert old == result
        assert result["outcome"] == dict(kind="estimate", reason="none", certified=True, unique=singleton)
        assert coordinates(result["estimates"]) == expected[0]
        assert float.fromhex(result["logLik"]) == 0
        candidates = [coordinates(candidate["estimates"]) for candidate in result["candidates"]]
        assert candidates == ([] if singleton else expected)
        print(mask, held, old["unique"], singleton, candidates)


def probe(executable, inputs):
    output = subprocess.run([executable], input=inputs, text=True,
                            capture_output=True, check=True).stdout
    return list(map(json.loads, output.splitlines()))


for precision in ("native", "double"):
    compare(precision)
    replay(precision)
