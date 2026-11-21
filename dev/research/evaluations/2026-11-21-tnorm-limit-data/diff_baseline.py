"""Compare selected snapshot records with the stored 2026-11-15 baseline."""
from collections import Counter, defaultdict
from pathlib import Path
import math
import sys

root = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(root / "dev" / "validation"))
from strict_common import load, number

current, baseline = (load(path) for path in sys.argv[1:])
families = {key[0] for key in current}
baseline = {key: value for key, value in baseline.items() if key[0] in families}
assert current.keys() == baseline.keys(), (baseline.keys() - current.keys(),
                                          current.keys() - baseline.keys())
counts = defaultdict(Counter)
max_moves = {}
changes = []
changed_fields = defaultdict(Counter)


def coefficients(result):
    for field in ("estimates", "limit_estimates"):
        for name, value in result[field].items():
            yield f"{field}.{name}", value
    for field in ("logLik", "limit_loglik", "loglik_lower", "loglik_upper"):
        yield field, result[field]


for key in sorted(current):
    old, new = baseline[key]["result"], current[key]["result"]
    group = (*key[:2], old["kind"])
    counts[group]["total"] += 1
    if old != new:
        counts[group]["changed"] += 1
        changed_fields[group].update(field for field in old.keys() | new.keys()
                                     if old.get(field) != new.get(field))
    decision = ("kind", "core_status", "outcome")
    if any(old[field] != new[field] for field in decision):
        changes.append((key, *(tuple(r[field] for field in decision) for r in (old, new))))
    previous = dict(coefficients(old))
    for field, value in coefficients(new):
        if field not in previous or previous[field] == value:
            continue
        a, b = number(previous[field]), number(value)
        if not (math.isfinite(a) and math.isfinite(b)) or a == b:
            continue
        relative = abs(b - a) / abs(a) if a != 0 else math.inf
        labels = [field]
        if field == "limit_estimates.rate" and a != 0 and b != 0:
            labels.append("limit_estimates.rate (nonzero rates)")
        for label in labels:
            if relative > max_moves.get((group, label), (0,))[0]:
                max_moves[group, label] = relative, key[-1], a, b

for group, count in sorted(counts.items()):
    print(*group, f"{count['changed']}/{count['total']} changed")
    if changed_fields[group]:
        print("  fields:", ", ".join(f"{f} ({n})" for f, n in sorted(changed_fields[group].items())))
    for (g, field), (relative, case, a, b) in sorted(max_moves.items()):
        if g == group:
            print(f"  {field}: {relative:.9g} relative ({case}, {a:.17g} -> {b:.17g})")
print("Outcome or status changes:", len(changes))
for change in changes:
    print(change)
