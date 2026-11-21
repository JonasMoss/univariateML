"""Compare canonical fit snapshots and check the named high-precision references."""
import gzip
import json
import sys
from decimal import Decimal, localcontext
from pathlib import Path

HERE = Path(__file__).resolve().parent
BASE = HERE.parent / "2026-11-15-fit-snapshot-data"
REF = json.loads((HERE / "reference.json").read_text())


def key(row):
    return (row["family"], row["source"], row["case"], row["input_sha256"])


def rows(lines):
    return {key(row): row for line in lines if line.startswith("{")
            for row in [json.loads(line)]}


def decimal_hex(value):
    return Decimal(float.fromhex(value))


def compare(mode, path):
    with gzip.open(BASE / f"{mode}.jsonl.gz", "rt") as lines:
        old = rows(lines)
    result = {}
    for snapshot in sorted(path.glob("*.jsonl")):
        new = rows(snapshot.read_text().splitlines())
        families = {k[0] for k in new}
        previous = {k: r for k, r in old.items() if k[0] in families}
        stats = dict(cases=len(new), changed=0, max_relative_coefficient_move=0,
                     max_move_case=None, max_scaled_logLik_move=0, kinds=[], references={},
                     removed=sorted(previous.keys()-new.keys()),
                     added=sorted(new.keys()-previous.keys()))
        for k in new.keys() & previous.keys():
            before, after = old[k]["result"], new[k]["result"]
            stats["changed"] += before != after
            if before["kind"] != after["kind"]:
                stats["kinds"].append([*k[1:3], before["kind"], after["kind"]])
            if before["kind"] == after["kind"] == "ok":
                ll_before, ll_after = (decimal_hex(r["logLik"]) for r in (before,after))
                if ll_before.is_finite() and ll_after.is_finite():
                    stats["max_scaled_logLik_move"] = max(stats["max_scaled_logLik_move"],
                        float(abs(ll_after-ll_before)/max(1,abs(ll_before))))
                for name, raw in before["estimates"].items():
                    a, b = map(decimal_hex, (raw, after["estimates"][name]))
                    if not a.is_finite() or a == 0:
                        continue
                    error = float(abs(b/a-1))
                    if error > stats["max_relative_coefficient_move"]:
                        stats["max_relative_coefficient_move"] = error
                        stats["max_move_case"] = [*k[1:3], name]
            ref = REF.get(k[2]) if k[1] == "archive" else None
            if ref and after["kind"] == "ok" and "profile" not in ref:
                errors = {name: float(abs(decimal_hex(value)/Decimal(ref[name])-1))
                          for name, value in after["estimates"].items()
                          if name in ref and name not in new[k]["held"]}
                stats["references"][k[2]] = dict(coefficient_relative_errors=errors,
                    logLik_relative_error=float(abs(decimal_hex(after["logLik"])/Decimal(ref["logLik"])-1)))
            if k[0] == "beta" and k[1:3] == ("golden","golden"):
                truth = Decimal(REF["beta"]["logLik"])
                stats["golden_logLik_relative_errors"] = {
                    label: float(abs(decimal_hex(value["logLik"])/truth-1))
                    for label, value in [("baseline", before), ("revised",after)]}
        stats["kinds"].sort()
        result[snapshot.stem] = stats
    return result


if __name__ == "__main__":
    with localcontext() as ctx:
        ctx.prec = 70
        print(json.dumps({mode:compare(mode,Path(path)) for mode,path in
                          zip(("native","double","routes-native","routes-double"),sys.argv[1:])}, indent=2, sort_keys=True))
