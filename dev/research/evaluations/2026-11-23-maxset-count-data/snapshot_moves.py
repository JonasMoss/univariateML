"""Print the changed fields for every shared key, and any input-key moves."""
from collections import Counter
import gzip
import json
from pathlib import Path
import sys


def key(row):
    return (row['family'], row['case'], json.dumps(row['held'], sort_keys=True),
            row.get('mask'), row.get('input_sha256'))


def read(snapshot, families=None):
    if snapshot.is_dir():
        for path in sorted(snapshot.glob('*.jsonl')):
            for line in path.read_text().splitlines()[1:]:
                yield json.loads(line)
    else:
        with gzip.open(snapshot, 'rt') as source:
            for line in source:
                row = json.loads(line)
                if families is None or row['family'] in families:
                    yield row


def compare(snapshot, baseline):
    fresh = {key(row): row['result'] for row in read(snapshot)}
    families = {k[0] for k in fresh}
    old = {key(row): row['result'] for row in read(baseline, families)}
    groups = Counter()
    print(snapshot.name, len(fresh), 'records,', len(families), 'families')
    for k in sorted(fresh.keys() | old.keys()):
        if k not in old:
            change, fields = 'added fixture', []
        elif k not in fresh:
            change, fields = 'missing input key', []
        elif fresh[k] == old[k]:
            continue
        else:
            a, b = old[k], fresh[k]
            fields = sorted(f for f in a.keys() | b.keys() if a.get(f) != b.get(f))
            if any(f in fields for f in ('estimates', 'logLik', 'kind', 'limit_loglik', 'limit_estimates')):
                change = 'coefficient, likelihood or result kind'
            elif b.get('maximizer_set'):
                change = 'typed maximum'
            elif b.get('unique') is None and a.get('unique') is not None:
                change = 'undetermined uniqueness'
            else:
                change = 'other metadata'
        groups[change] += 1
        print('\t'.join((k[0], k[1], k[2], change, ','.join(fields))))
        if change == 'coefficient, likelihood or result kind':
            for f in fields:
                if f in ('estimates', 'logLik', 'kind', 'limit_loglik', 'limit_estimates'):
                    print(f'  {f} {a.get(f)} -> {b.get(f)}')
    print('Totals', dict(groups))


if __name__ == '__main__':
    compare(Path(sys.argv[1]), Path(sys.argv[2]))
