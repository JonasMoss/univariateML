"""Compare doctest inventories, worker expectation counts and snapshot payloads."""
import argparse
from collections import Counter
import csv
import gzip
import hashlib
from pathlib import Path
import re


def inventories(path, tier):
    with path.open(newline='') as source:
        return Counter((re.sub(r'_part[0-9]+$', '', row['target']), row['case'])
                       for row in csv.DictReader(source) if tier in row['labels'].split(';'))


def ctest_assertions(path):
    _, *sections = re.split(r'^\d+/\d+ Testing: (\S+)\n', path.read_text(), flags=re.M)
    counts = Counter()
    for target, output in zip(sections[::2], sections[1::2], strict=True):
        assert 'Test Passed.' in output, target
        assertions = re.search(r'^\[doctest\] assertions:\s*(\d+)', output, re.M)
        if assertions:
            counts[re.sub(r'_part[0-9]+$', '', target)] += int(assertions[1])
    assert counts, path
    return counts


def counts(path):
    with path.open(newline='') as source:
        return {row['file']: {key: int(value) for key, value in row.items()
                              if key.startswith('expectation_')}
                for row in csv.DictReader(source)}


def collect_r_files(source, destination):
    rows = []
    for path in sorted(source.glob('*.csv')):
        with path.open(newline='') as stream:
            row, = csv.DictReader(stream)
        rows.append({'file': path.stem, **row})
    assert rows, source
    with destination.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def verify_snapshot(before, after):
    before_parts = {path.name for path in before.glob('*.jsonl')}
    after_parts = {path.name for path in after.glob('*.jsonl')}
    assert before_parts and before_parts == after_parts, before_parts ^ after_parts
    for name in sorted(before_parts):
        # The first line is the provenance tag used to invalidate resumed jobs.
        assert (before/name).read_bytes().split(b'\n', 1)[1] == (after/name).read_bytes().split(b'\n', 1)[1], name
    original = (before/'snapshot.jsonl.gz').read_bytes()
    scheduled = (after/'snapshot.jsonl.gz').read_bytes()
    baseline = gzip.decompress(original)
    assert baseline == gzip.decompress(scheduled), after
    # gzip.open stamps the current time in bytes 4 to 7.
    assert original[:4] + original[8:] == scheduled[:4] + scheduled[8:], after
    return len(baseline.splitlines()), hashlib.sha256(baseline).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('data', type=Path)
    parser.add_argument('scratch', type=Path)
    args = parser.parse_args()
    for mode in ('native', 'double'):
        for tier in ('cran', 'dev', 'full'):
            before, after = [inventories(args.data/f'cpp-{phase}-{mode}.inventory.csv', tier)
                             for phase in ('before', 'after')]
            assert before == after, (mode, tier, before - after, after - before)
            named = sum(count for (_, case), count in before.items() if case)
            print(f'C++ {mode} {tier}: {named} matching doctest cases, '
                  f'{before.total() - named} matching non-doctest registrations')
        before, after = [ctest_assertions(args.scratch/f'cpp-{phase}-{mode}-ctest.log')
                         for phase in ('before', 'after')]
        assert before == after, (mode, before - after, after - before)
        print(f'C++ {mode}: {before.total()} matching assertions')
    for tier in ('dev', 'full'):
        for phase in ('before', 'after'):
            collect_r_files(args.scratch/f'{phase}-r-{tier}', args.data/f'r-{phase}-{tier}.csv')
        before, after = [counts(args.data/f'r-{phase}-{tier}.csv') for phase in ('before', 'after')]
        assert before == after, (tier, {key for key in before.keys() | after.keys()
                                      if before.get(key) != after.get(key)})
        print(f'R {tier}: {len(before)} matching files, {sum(sum(row.values()) for row in before.values())} events')
    for mode in ('native', 'double', 'routes-native', 'routes-double'):
        count, digest = verify_snapshot(args.scratch/'snapshots-before'/mode,
                                        args.scratch/'snapshots-after'/mode)
        print(f'Snapshot {mode}: {count} matching rows, SHA-256 {digest}')


if __name__ == '__main__':
    main()
