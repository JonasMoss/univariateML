"""Diff every archived sample and hold subset against the TASK-180 replay."""
import csv
from collections import Counter
from pathlib import Path

HERE = Path(__file__).resolve().parent
OLD = HERE.with_name("2026-11-12-ew-fit-data")


def load(path):
    with path.open() as stream:
        return {(row["sample"], row["mask"]): row for row in csv.DictReader(stream)}


for mode, baseline in (("native", "archive.csv"), ("double", "archive-double.csv")):
    old = load(OLD / baseline)
    new = load(HERE / f"archive-{mode}.csv")
    assert old.keys() == new.keys() and len(new) == 328
    print(mode, dict(sorted(Counter(
        "/".join(filter(None, (r["kind"], r["reason"], r["model"])))
        for r in new.values()).items())))
    print("sample mask old-status new-kind new-reason new-model")
    maximum = (0., (), "")
    for key, after in new.items():
        before = old[key]
        if before["status"] == "Ok" and after["kind"] == "estimate":
            for parameter in ("scale", "shape", "exponent"):
                x, y = float(before[parameter]), float(after[parameter])
                maximum = max(maximum, (abs(y-x)/x, key, parameter))
        else:
            print(*key, before["status"], after["kind"], after["reason"], after["model"] or "-")
    print("maximum estimate-to-estimate relative move", maximum)
