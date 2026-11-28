"""Refit the specified archive rows and reuse the strict density evaluator."""
import gzip
import hashlib
import importlib
import json
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT / "dev/validation"))
density = importlib.import_module("fit-density")
fs = density.fs
CASES = {
    "fpareto": {"001624", "001625", "001627", "001628", "010305", "010306"},
    "gb2": {"002096", "002097", "002099", "002100", "010389", "010390"},
}


def main(directory=ROOT / "build/scratch/trgamma-replay"):
    archive = {family: [] for family in CASES}
    for case in json.loads(gzip.decompress(fs.ARCHIVE.read_bytes())):
        if case["family"] in archive:
            archive[case["family"]].append(case)
    directory.mkdir(parents=True, exist_ok=True)
    density.WORK = directory / "density-probe"
    table = {name: record for name, record in density.laws().items() if name == "invtrgamma"}
    failed = False
    for mode in ("native", "double"):
        work = directory / mode
        work.mkdir(exist_ok=True)
        rows = {}
        for family, ids in CASES.items():
            meta, _ = fs.read(family)
            names = [parameter["name"] for parameter in meta["params"]]
            probe = fs.Probe(fs.build(family, work, mode == "double", fs.boost_include()))
            try:
                for case in fs.cases(family, archive):
                    if case["source"] != "archive" or case["case"] not in ids:
                        continue
                    line = fs.line(case, names)
                    result = probe.ask(line, 300)
                    if (not isinstance(result, dict) or result.get("kind") != "limit"
                            or not result.get("certified")):
                        raise RuntimeError((mode, family, case["case"], result))
                    rows[family, "archive", case["case"]] = dict(
                        case, family=family, result=result,
                        input_sha256=hashlib.sha256(line.encode()).hexdigest()[:16])
            finally:
                probe.close()
        assert len(rows) == sum(map(len, CASES.values()))
        (work / "payloads.jsonl").write_text(
            "".join(json.dumps(row, sort_keys=True) + "\n" for row in rows.values()))
        output = work / "density.jsonl"
        with output.open("w") as records:
            failures, _, _, unchecked = density.evaluate(rows, mode, table, archive, records)
        observations = [json.loads(line) for line in output.read_text().splitlines()]
        limits = [row for row in observations if row["item"] == "limit"]
        assert len(limits) == len(rows), (limits, unchecked)
        print(mode, len(limits), "limits, worst relative error",
              max(row["relative"] for row in limits))
        for row in limits:
            print(row["family"], row["case"], row["reported"], row["summed"], row["relative"])
        if failures:
            print(failures)
            failed = True
    return int(failed)


if __name__ == "__main__":
    sys.exit(main())
