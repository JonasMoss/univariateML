"""Run the strict nesting judge on the two large-count tbinom route cases."""
import importlib.util
from pathlib import Path
import sys

root = Path(__file__).resolve().parents[4]
validation = root / "dev" / "validation"
sys.path.insert(0, str(validation))
from strict_common import archive_data, load, number

spec = importlib.util.spec_from_file_location("fit_judge", validation / "fit-judge.py")
judge = importlib.util.module_from_spec(spec)
spec.loader.exec_module(judge)
data = archive_data()
route = "binom +truncate=tbinom"

for path in sys.argv[1:]:
    rows = load(path)
    selected = {(route, "archive", case): rows[(route, "archive", case)]
                for case in ("007325", "007353")}
    siblings = {key: row for key, row in rows.items()
                if key[:2] == (route, "archive") and data[int(key[2])] == data[7325]}
    failures = judge.judge_one(path, siblings, data)
    nesting = {key: detail for key, detail in failures.items()
               if key.startswith("nesting|") and key.endswith(("|007325", "|007353"))}
    free, held = (r["result"] for r in selected.values())
    print(path, free["kind"], free["limit_loglik"], held["kind"], held["logLik"], nesting)
    assert not nesting
    assert free["kind"] == "limit" and free["model"] == "tpois"
    assert free["outcome"]["certified"]
    assert held["kind"] == "ok" and number(held["logLik"]) <= number(free["limit_loglik"])
