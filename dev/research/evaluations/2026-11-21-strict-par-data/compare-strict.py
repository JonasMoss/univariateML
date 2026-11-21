#!/usr/bin/env python3
"""Verify strict-par measurements and write the reproducible comparison evidence."""
import argparse
import csv
import difflib
import hashlib
import json
import re
from pathlib import Path

RUNS = ('old', 'serial', 'cold', 'warm', 'warm-serial', 'touched')
SCRIPTS = ('fit-judge', 'fit-density', 'fit-consistency', 'fit-refine', 'cert-replay', 'strict-seam')
TIMING = re.compile(r'^cert-replay: \d+(?:\.\d+)? (?:CPU|wall) s, '
                    r'\d+ digits(?:, \d+/\d+ records cached)?$|'
                    r'^seam-check (?:native|double): \d+/\d+ probes cached$|'
                    r'^seam-check (?:native|double): \d+ guarded fits replayed with a \d+ s guard$')


def lines(path):
    return path.read_text().splitlines(keepends=True)


def json_rows(path):
    return [json.loads(text) for text in lines(path)]


def canonical(records):
    return [json.dumps(row, sort_keys=True, separators=(',', ':'))+'\n' for row in records]


def replay(path):
    rows = json_rows(path/'replay.jsonl')
    for row in rows:
        del row['seconds']
    return canonical(sorted(rows, key=lambda row: (row['mode'], row['family'], row['source'], row['case'])))


def seam(path):
    records = []
    for mode in ('native', 'double'):
        retry_path = path/(mode+'-retry.jsonl')
        retry = ({r['index']: r for r in json_rows(retry_path) if r['source'] == 'archive'}
                 if retry_path.exists() else {})
        for row in json_rows(path/(mode+'.jsonl')):
            if row['source'] == 'archive' and row['outcome'] == 'open fit':
                row = retry.get(row['index'], row)
            records.append(dict(mode=mode, **row))
    return canonical(sorted(records, key=lambda row: (row['mode'], row['family'], row['source'], row['index'])))


def ratchet(path):
    return canonical([dict(script=script, **json.loads((path/(script+'.ratchet.json')).read_text()))
                      for script in SCRIPTS])


def output(path):
    return [line for line in lines(path/'strict.log') if not TIMING.fullmatch(line.rstrip('\n'))]


def measure(path):
    for name in (*SCRIPTS, 'total'):
        file = 'total.time' if name == 'total' else name+'.py.time'
        wall, user, system, status = (path/file).read_text().splitlines()[-1].split()
        yield name, float(wall), float(user), float(system), int(status)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('scratch', type=Path, help='directory containing the strict-* measurement folders')
    parser.add_argument('evidence', type=Path, help='save unified diffs, hashes and timings here')
    args = parser.parse_args()
    args.evidence.mkdir(parents=True, exist_ok=True)
    paths = {run: args.scratch/('strict-'+run) for run in RUNS}
    equal = True
    with (args.evidence/'timings.csv').open('w', newline='') as out:
        writer = csv.writer(out)
        writer.writerow(('run', 'script', 'wall_s', 'user_cpu_s', 'system_cpu_s', 'exit'))
        for run, path in paths.items():
            for name, wall, user, system, status in measure(path):
                writer.writerow((run, name, wall, user, system, status))
                equal &= status == 0
    with (args.evidence/'comparison-hashes.csv').open('w', newline='') as out:
        writer = csv.writer(out)
        writer.writerow(('run', 'artifact', 'lines', 'sha256'))
        for name, reader in [('replay', replay), ('seam', seam), ('ratchet', ratchet),
                             ('stdout', output), ('stderr', lambda path: lines(path/'strict.stderr'))]:
            baseline = reader(paths['old'])
            for run, path in paths.items():
                check = baseline if run == 'old' else reader(path)
                writer.writerow((run, name, len(check), hashlib.sha256(''.join(check).encode()).hexdigest()))
                if run == 'old':
                    continue
                diff = difflib.unified_diff(baseline, check, 'old/'+name, run+'/'+name)
                changes = ''.join(diff)
                (args.evidence/(run+'-'+name+'.diff')).write_text(changes)
                equal &= not changes
    for run, path in paths.items():
        for line in lines(path/'strict.log'):
            if TIMING.fullmatch(line.rstrip('\n')):
                print(run, line.rstrip())
    if not equal:
        raise SystemExit('strict equivalence failed: inspect exit statuses and unified diffs')


if __name__ == '__main__':
    main()
