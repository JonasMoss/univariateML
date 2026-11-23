"""List all snapshot changes and compare strict findings against the starting tree."""
import argparse
import gzip
import json
from pathlib import Path
import re


def rows(path):
    if path.is_dir():
        lines = (line for part in sorted(path.glob('*.jsonl'))
                 for line in part.read_text().splitlines()[1:])
    else:
        lines = gzip.open(path, 'rt')
    try:
        return {(row['family'], row['source'], row['case']): row
                for row in map(json.loads, lines)}
    finally:
        if not path.is_dir():
            lines.close()


def findings(path, phase, check):
    lines = (path / f'{phase}-{check}.txt').read_text().splitlines()
    return {match[1]: match[2] for line in lines
            if (match := re.fullmatch(r'  NEW\s+(\S+): (.*)', line))}


def compare(folder, before, stored):
    current = rows(folder)
    baseline = rows(stored)
    starting = rows(before) if before is not None else {}
    allowable = {'model', 'limit_estimates', 'limit_loglik', 'candidates'}
    unexpected = 0
    for key in sorted(current.keys() | baseline.keys()):
        old = baseline.get(key)
        new = current.get(key)
        prior = starting.get(key, old)
        if old == new:
            continue
        prefix = '\t'.join((folder.name, *key))
        if old is None or new is None:
            label = 'added' if new is not None else 'removed'
            print(f'{prefix}\t{label}')
            if key[1] != 'class' or not key[2].startswith(('threshold-', 'zero-tied-')):
                unexpected += 1
            continue
        fields = {field for field in old['result'].keys() | new['result'].keys()
                  if old['result'].get(field) != new['result'].get(field)}
        changed = {field for field in prior['result'].keys() | new['result'].keys()
                   if prior['result'].get(field) != new['result'].get(field)}
        print(f'{prefix}\tstored:{",".join(sorted(fields))}\tworktree:{",".join(sorted(changed))}')
        unexpected += bool(changed - allowable)
        previous, result = prior['result'], new['result']
        if 'limit_loglik' in changed and previous['limit_loglik'] != 'nan':
            unexpected += 1
        if 'limit_estimates' in changed:
            unexpected += not (previous['limit_estimates'].items() <= result['limit_estimates'].items())
        if 'candidates' in changed:
            for old_candidate in previous['candidates']:
                unexpected += not any(
                    old_candidate['estimates'].items() <= candidate['estimates'].items()
                    and all(old_candidate[bound] == candidate[bound]
                            for bound in ('loglik_lower', 'loglik_upper'))
                    for candidate in result['candidates'])
        unexpected += prior['input_sha256'] != new['input_sha256']
    return unexpected


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('snapshots', type=Path)
    parser.add_argument('baseline', type=Path)
    parser.add_argument('evidence', type=Path)
    args = parser.parse_args()
    unexpected = 0
    for mode in ('native', 'double', 'routes-native', 'routes-double'):
        prior = None if mode.startswith('routes-') else args.snapshots / f'start-{mode}'
        unexpected += compare(args.snapshots / mode, prior, args.baseline / f'{mode}.jsonl.gz')
    print('Snapshot changes outside the payload allowance:', unexpected)
    for check in ('density', 'consistency'):
        old = findings(args.evidence, 'start', check)
        new = findings(args.evidence, 'fresh', check)
        fresh = {key: reason for key, reason in new.items() if key not in old}
        resolved = old.keys() - new.keys()
        print(f'{check}: {len(old)} before, {len(new)} after, {len(fresh)} newly failing, {len(resolved)} resolved')
        for key, reason in sorted(fresh.items()):
            print(f'new\t{key}\t{reason}')
        for key in sorted(resolved):
            print(f'resolved\t{key}')
        unexpected += len(fresh)
    raise SystemExit(bool(unexpected))


if __name__ == '__main__':
    main()
