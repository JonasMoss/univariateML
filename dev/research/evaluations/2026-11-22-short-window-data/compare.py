"""Find the largest relative discrepancies between the compiled binding replays."""
from collections import defaultdict
import csv
import math
from pathlib import Path
import sys

fixture = Path("bindings/r/tests/testthat/fixtures/short-window.tsv")
with fixture.open() as stream:
    rows = [row for row in csv.DictReader(stream, delimiter="\t") if row["op"] != "refuse"]
r, js = (list(map(float, Path(file).read_text().splitlines())) for file in sys.argv[1:3])
maxima = defaultdict(lambda: defaultdict(float))


def relative(a, b):
    if a == b:
        return 0.
    if not math.isfinite(a) or not math.isfinite(b) or a == 0 or b == 0:
        return math.inf
    return abs(a - b) / abs(b)


for row, native, wasm in zip(rows, r, js, strict=True):
    ref = float(row["expected"])
    op = row["op"]
    if op == "q":
        assert native == wasm == ref, (row, native, wasm)
    result = maxima[row["case"]]
    result[op] = max(result[op], relative(wasm, native))
    result["R vs oracle"] = max(result["R vs oracle"], relative(native, ref))
    result["JS vs oracle"] = max(result["JS vs oracle"], relative(wasm, ref))

print("| Window | d | p | q | R vs oracle | JS vs oracle |")
print("| --- | ---: | ---: | ---: | ---: | ---: |")
for name, result in maxima.items():
    metrics = (result[key] for key in ("d", "p", "q", "R vs oracle", "JS vs oracle"))
    print("|", name, "|", " | ".join(f"{v:.3g}" for v in metrics), "|")
    assert max(result.values()) < 2e-12, (name, result)
