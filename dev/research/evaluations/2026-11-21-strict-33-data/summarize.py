#!/usr/bin/env python3
"""Summarize canonical snapshot changes against the stored November baseline."""
import json
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[4] / "dev/validation"))
from strict_common import load_one, number


def describe(result):
    return {k: result.get(k) for k in ["kind", "reason", "certified", "model"]}


new, old = map(load_one, sys.argv[1:])
for family in sorted({key[0] for key in new}):
    keys = sorted(key for key in new if key[0] == family)
    assert set(keys) == {key for key in old if key[0] == family}
    changed, outcomes = [], []
    paired = 0
    maximum = 0.
    for key in keys:
        a, b = old[key], new[key]
        assert a["input_sha256"] == b["input_sha256"], key
        a, b = a["result"], b["result"]
        if a != b:
            changed.append(key[1:])
        if describe(a) != describe(b):
            outcomes.append([*key[1:], describe(a), describe(b)])
        if a["kind"] == b["kind"] == "ok":
            paired += 1
            for name, old_hex in a["estimates"].items():
                x, y = number(old_hex), number(b["estimates"][name])
                if x != y:
                    maximum = max(maximum, abs(y-x)/(abs(x) if x else 1.))
    print(json.dumps(dict(family=family, compared=len(keys), changed=changed,
                          estimate_pairs=paired, max_relative=maximum, outcomes=outcomes)))
