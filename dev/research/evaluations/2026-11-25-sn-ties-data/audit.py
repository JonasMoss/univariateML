"""Attribute snapshot changes and count TASK-255 records using exact input rationals."""
from collections import Counter
from fractions import Fraction
import gzip
import json
from pathlib import Path
import runpy
import sys

root = Path(__file__).resolve().parents[4]
driver = runpy.run_path(str(root / 'dev/validation/fit-snapshot.py'))
families = ('sn', 'snorm')
archive = {family: [] for family in families}
for case in json.loads(gzip.decompress(driver['ARCHIVE'].read_bytes())):
    if case['family'] in archive:
        archive[case['family']].append(case)
inputs = {(family, case['source'], case['case']): case['data']
          for family in families for case in driver['cases'](family, archive)}


def key(record):
    return record['family'], record['source'], record['case']


def symmetric(record):
    values = sorted(Fraction(float(value)) for value in inputs[key(record)])
    center = (values[0] + values[-1]) / 2
    return all(a + b == 2 * center for a, b in zip(values, reversed(values)))


def eligible(record):
    return record['result'].get('outcome') == {
        'kind': 'estimate', 'reason': 'none', 'certified': True, 'unique': True
    }


def unresolved_cardinality(record):
    family, held = record['family'], record['held']
    # The conditional fixed-shape likelihood has a unique finite maximizer.
    # For sn, the held-location shape/scale likelihood is also strictly
    # concave whenever the accepted solution is finite and nonflat.
    return ('alpha' not in held and 'xi' not in held if family == 'sn'
            else 'xi' not in held)


def check_move(before, after):
    assert before['family'] in families and symmetric(before), key(before)
    old, new = before['result'], after['result']
    assert new['outcome'] == {
        'kind': 'estimate', 'reason': 'none', 'certified': True, 'unique': False
    }, key(before)
    assert new['maximizer_set'] and len(new['maximizers']) == 2, key(before)
    changed = sorted(field for field in old.keys() | new.keys()
                     if old.get(field) != new.get(field))
    if eligible(before):
        assert set(changed) <= {'outcome', 'maximizer_set', 'maximizers'}, (key(before), changed)
    location = 'xi' if before['family'] == 'sn' else 'mean'
    shape = 'alpha' if before['family'] == 'sn' else 'xi'
    values = sorted(Fraction(float(value)) for value in inputs[key(before)])
    center = (values[0] + values[-1]) / 2
    held = before['held']
    assert shape not in held and (location not in held or held[location] == center)
    return changed


snapshots = Path(sys.argv[1])
baseline = root / 'dev/research/evaluations/2026-11-15-fit-snapshot-data'
for mode in ('native', 'double', 'routes-native', 'routes-double'):
    with gzip.open(baseline / f'{mode}.jsonl.gz', 'rt') as source:
        old = {key(row): row for row in map(json.loads, source)}
    new = old if str(snapshots) == "baseline" else driver["load"](snapshots / mode)
    assert old.keys() == new.keys(), mode
    counts = Counter()
    moves = []
    for case_key, after in new.items():
        before = old[case_key]
        assert before['input_sha256'] == after['input_sha256'], case_key
        if before['result'] != after['result']:
            moves.append({'case': case_key, 'fields': check_move(before, after)})
        if after['family'] not in families or not eligible(after):
            continue
        family = after['family']
        counts[family, 'certified_unique'] += 1
        if not symmetric(after):
            counts[family, 'asymmetric_certified_unique'] += 1
            if unresolved_cardinality(after):
                counts[family, 'asymmetric_unproved_cardinality'] += 1

    print(json.dumps({
        'mode': mode,
        'records': len(new),
        'moves': moves,
        'task_255': {family: {label: counts[family, label] for label in (
            'certified_unique', 'asymmetric_certified_unique', 'asymmetric_unproved_cardinality'
        )} for family in families},
    }, sort_keys=True))
