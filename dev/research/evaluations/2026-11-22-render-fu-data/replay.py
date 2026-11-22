"""Replay the brief's concrete cases through fit-snapshot.py's compiled drivers."""
import json
from pathlib import Path
import subprocess
import sys

CASES = {
    "cauchy": {
        "reflected-pair": "2 4 0 1 1 1 5 5",
        "overlap-without-proof": "2 4 0 1 -3 -2 2 3",
        "coalesced-quartic": "2 2 0 1 -1 1",
    },
    "st": {"flat-alpha": "11 3 2 1 0 5 2 2 2"},
    "bbinom": {
        "zero-size": "1 2 0 0 0 0 0",
        "bernoulli-ratio": "1 3 1 0 0 0 1 1",
        "free-size-all-zero": "2 2 0 2 0 0 0",
        "positive-size-all-zero": "1 2 3 0 0 0 0",
        "exact-binomial-frequency-table": "1 8 3 0 0 0 1 1 1 2 2 2 3",
        "overdispersed-frequency-table": "1 6 3 0 0 0 1 1 2 2 3",
    },
    "bhatt": {"normal-transition": "3 2 0 1 0 -1.0000001 1.0000001"},
    "emg": {
        "unseparated-normal": "1 5 0 0 0 -2 -1 0 1 2",
        "near-closure-tie": "3 2 0 1 0 -10 2.746841848406631",
    },
    "cmb": {"zero-size": "1 2 0 0 0 0 0"},
    "gompertz": {"empty": "0 0 0 0", "nonfinite": "0 2 0 0 1 nan"},
    "tpois": {"singleton": "6 2 0 1 2 2 2"},
    "tbinom": {"singleton": "14 2 0 1 2 5 2 2"},
}


def main(directories):
    for directory in directories:
        for family, cases in CASES.items():
            driver = Path(directory) / "build" / family
            if not driver.is_file():
                continue
            data = "\n".join(cases.values()) + "\n"
            process = subprocess.run([str(driver)], input=data, text=True,
                                     capture_output=True, check=True, timeout=120)
            rows = process.stdout.splitlines()
            assert len(rows) == len(cases), (driver, rows)
            for name, row in zip(cases, rows):
                print(json.dumps(dict(run=Path(directory).name, family=family,
                                      case=name, input=cases[name], result=json.loads(row)), sort_keys=True))


if __name__ == "__main__":
    main(sys.argv[1:])
