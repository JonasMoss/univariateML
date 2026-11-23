"""Show changes beyond the added nullable snapshot field with full case keys."""
import argparse
from collections import Counter
import gzip
import json
from pathlib import Path


def key(record):
    return (record['family'], record['source'], record['case'],
            json.dumps(record['held'], sort_keys=True),
            record.get('mask'), record.get('input_sha256'))


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('new', type=Path)
parser.add_argument('old', type=Path)
args = parser.parse_args()

with gzip.open(args.old, 'rt') as stream:
    old = {key(row): row['result'] for line in stream if (row := json.loads(line))}
new = {}
for path in sorted(args.new.glob('*.jsonl')):
    for line in path.read_text().splitlines()[1:]:
        row = json.loads(line)
        new[key(row)] = row['result']

changes = Counter()
for case in sorted(old.keys() & new.keys(), key=repr):
    a, b = old[case], new[case]
    fields = tuple(sorted(field for field in a.keys() | b.keys()
                          if a.get(field) != b.get(field)))
    if not fields:
        changes['schema only' if a != b else 'identical'] += 1
        continue
    changes.update(fields)
    print(json.dumps(dict(case=case, fields=fields,
                          before={field: a.get(field) for field in fields},
                          after={field: b.get(field) for field in fields}),
                     sort_keys=True))
for label, keys in [('only_fresh', new.keys() - old.keys()), ('only_baseline', old.keys() - new.keys())]:
    for case in sorted(keys, key=repr):
        print(json.dumps({label: case}))
print(json.dumps(dict(fresh=len(new), baseline=len(old), common=len(old.keys() & new.keys()),
                      differing_fields=changes), sort_keys=True))
