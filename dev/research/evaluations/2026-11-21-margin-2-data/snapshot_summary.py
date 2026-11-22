"""Compare selected current snapshots with the stored November 15 baselines."""
from collections import Counter, defaultdict
import gzip
import json
from pathlib import Path
import sys


def read(lines):
    output = {}
    for line in lines:
        row = json.loads(line)
        key = (row['family'], row['source'], row['case'], row['mask'],
               json.dumps(row['held'], sort_keys=True), row['input_sha256'])
        assert key not in output, key
        output[key] = row['result']
    return output


def label(result):
    outcome = result.get('outcome') or {}
    return (result['kind'], outcome.get('kind'), outcome.get('reason'),
            outcome.get('certified'))


def coefficient_moves(old, new):
    for key, value in old.get('estimates', {}).items():
        a = float.fromhex(value)
        b = float.fromhex(new['estimates'][key])
        if a == b:
            yield 0.
        else:
            yield abs(a - b) / abs(a) if a else float('inf')


def compare(current, baseline):
    with gzip.open(baseline, 'rt') as stream:
        old = read(stream)
    new = read(line for file in sorted(current.glob('*.jsonl'))
               for line in file.read_text().splitlines()[1:])
    family_records = defaultdict(list)
    for key, value in new.items():
        family_records[key[0]].append((key, old[key], value))
    assert new and set(new) == {key for key in old if key[0] in family_records}
    print(f'=== {current.name} ===')
    for family, rows in sorted(family_records.items()):
        pairs = 0
        moves = []
        fields = Counter()
        changes = []
        for key, before, after in rows:
            if label(before)[1] == label(after)[1] == 'estimate':
                pairs += 1
                moves.extend(coefficient_moves(before, after))
            if before == after:
                continue
            different = sorted(f for f in before.keys() | after.keys()
                               if before.get(f) != after.get(f))
            fields.update(different)
            changes.append((key[1:3], label(before), label(after), different))
        print(f'{family}: {len(rows)} compared, {len(changes)} changed, '
              f'{pairs} estimate pairs, max relative coefficient move {max(moves, default=0):.6g}')
        for case, before, after, different in changes:
            print(f'  {case}: {before} -> {after} | {", ".join(different)}')
        if changes:
            print('  Fields:', dict(sorted(fields.items())))


if __name__ == '__main__':
    current, baseline = map(Path, sys.argv[1:])
    for mode in ('native', 'double', 'routes-native', 'routes-double'):
        compare(current / mode, baseline / f'{mode}.jsonl.gz')
