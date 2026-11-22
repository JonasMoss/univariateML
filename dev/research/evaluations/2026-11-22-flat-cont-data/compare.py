"""Compare fresh direct-fit snapshots with the frozen corpus and the branch base."""
from collections import Counter
import gzip
import json
from pathlib import Path
import subprocess
import sys

DATA = Path(__file__).resolve().parent
STORED = DATA.parent / "2026-11-15-fit-snapshot-data"
WORK = Path("build/scratch/flat-cont")
FAMILIES = {"gev", "gompertz", "gpd", "sged", "tlambda"}


def key(row):
    return row["family"], row["case"], row["mask"], row["input_sha256"]


def records(directory):
    for family in sorted(FAMILIES):
        with (directory / f"{family}.jsonl").open() as file:
            next(file)  # fingerprint
            yield from map(json.loads, file)


def describe(result):
    outcome = result.get("outcome", {})
    return {name: result.get(name) for name in ("estimates", "logLik", "restricted")} | {
        "kind": outcome.get("kind", result.get("kind")), "reason": outcome.get("reason"),
        "certified": outcome.get("certified"), "unique": outcome.get("unique"),
        "witnesses": [c["estimates"] for c in result.get("candidates", [])],
    }


for mode in ("native", "double"):
    print(f"\n{mode}", flush=True)
    current = WORK / mode
    filtered = WORK / f"{mode}-stored.jsonl.gz"
    with gzip.open(STORED / f"{mode}.jsonl.gz", "rt") as source, gzip.open(filtered, "wt") as target:
        frozen = {}
        for line in source:
            row = json.loads(line)
            if row["family"] in FAMILIES:
                frozen[key(row)] = row
                target.write(line)
    subprocess.run([sys.executable, "build/scratch/diff_baseline.py", str(current), str(filtered)], check=False)
    old = {key(row): row for row in records(WORK / "base" / mode)}
    new = {key(row): row for row in records(current)}
    assert old.keys() == new.keys()
    assert frozen.keys() <= new.keys()
    for name, previous in (("stored", frozen), ("branch-base", old)):
        changed = Counter()
        print(f"{name}: {len(previous)} before, {len(new)} after")
        for k, row in new.items():
            family, label, _, _ = k
            if k not in previous:
                assert row["source"] == "class" and "-flat-" in label, k
                continue
            before, after = previous[k]["result"], row["result"]
            if before == after:
                continue
            fields = [f for f in sorted(before.keys() | after.keys()) if before.get(f) != after.get(f)]
            origin = "inherited" if before != old[k]["result"] and after == old[k]["result"] else "job"
            changed[family, origin] += 1
            print(f"  {family} {label} holds={row['held']} origin={origin} changed={','.join(fields)}")
            print(f"    before: {json.dumps(describe(before), sort_keys=True)}")
            print(f"    after:  {json.dumps(describe(after), sort_keys=True)}")
            if name == "branch-base" and "-flat-" in label:
                assert after["outcome"] == dict(kind="estimate", reason="none", certified=True, unique=False)
                assert len(after["candidates"]) == 2 and after["outcome_message"]
        print(f"{name} change counts: {dict(sorted(changed.items()))}")
