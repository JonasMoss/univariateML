"""Compare one fit-snapshot directory against its stored baseline."""
from pathlib import Path
import collections
import math
import sys

root = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(root / "dev" / "validation"))
from strict_common import load, number

current, baseline = (load(path) for path in sys.argv[1:])
families = {key[0] for key in current}
baseline = {key: value for key, value in baseline.items() if key[0] in families}
print("only baseline", len(baseline.keys() - current.keys()),
      "only current", len(current.keys() - baseline.keys()))
counts = collections.defaultdict(collections.Counter)
max_moves = {}
outcome_changes = []

def outcome(row):
    result = row["result"]
    decision = result.get("outcome") or {}
    return result["kind"], decision.get("kind"), decision.get("reason"), decision.get("certified"), decision.get("unique")

for key in sorted(baseline.keys() & current.keys()):
    family = key[0]
    a, b = baseline[key]["result"], current[key]["result"]
    counts[family]["compared"] += 1
    if a != b:
        counts[family]["changed"] += 1
    ka, kb = outcome(baseline[key]), outcome(current[key])
    if ka != kb:
        outcome_changes.append((key, ka, kb, b.get("message")))
        counts[family]["outcome changes"] += 1
    if a["kind"] != "ok" or b["kind"] != "ok":
        continue
    for coef in a["estimates"].keys() & b["estimates"].keys():
        old, new = number(a["estimates"][coef]), number(b["estimates"][coef])
        if old == new:
            continue
        if not (math.isfinite(old) and math.isfinite(new)):
            move = math.inf
        elif old == 0:
            move = math.inf
        else:
            move = abs(new - old) / abs(old)
        if move > max_moves.get((family, coef), (0,))[0]:
            max_moves[(family, coef)] = move, key[1:], old, new

for family, counter in sorted(counts.items()):
    print(family, dict(counter))
    for (f, coef), (move, case, old, new) in sorted(max_moves.items()):
        if f == family:
            print(f"  {coef}: {move:.6g} {case}, {old:.17g} -> {new:.17g}")
print("Outcome changes")
for row in outcome_changes:
    print(row)
