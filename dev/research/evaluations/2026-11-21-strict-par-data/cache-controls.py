#!/usr/bin/env python3
"""Exercise the cache inputs on a small native subset under a CPU reservation."""
import argparse
import gzip
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys

SUMMARY = re.compile(r'\b(\d+)/(\d+) (?:records|probes) cached\b')


def verify(logs, name, command, expected):
    cwd = Path.cwd()
    env = dict(os.environ, PYTHONUNBUFFERED='1', GIT_OPTIONAL_LOCKS='0')
    result = subprocess.run(command, text=True, capture_output=True, env=env, cwd=cwd)
    (logs/(name+'.stdout')).write_text(result.stdout)
    (logs/(name+'.stderr')).write_text(result.stderr)
    if expected is None:
        if result.returncode == 0 or 'snapshot input differs' not in result.stderr:
            raise RuntimeError(f'{name}: expected rejection of a bad digest')
        print(name, 'rejected')
        return
    if result.returncode:
        raise RuntimeError(f'{name}: {result.stderr}')
    hits, total = map(int, SUMMARY.search(result.stdout).groups())
    should_hit = total if expected == 'all' else 0
    if hits != should_hit or not total:
        raise RuntimeError(f'{name}: {hits}/{total} hits, wanted {should_hit}')
    print(name, f'{hits}/{total} cached')


def shadow(root, logs):
    layer = logs/'layer'
    validation = layer/'dev/validation'
    validation.mkdir(parents=True, exist_ok=True)
    for pattern in ('cert-replay.py', 'cert_replay_*.py', 'fit-snapshot.py', 'strict_common.py', 'strict-known.json'):
        for file in (root/'dev/validation').glob(pattern):
            shutil.copyfile(file, validation/file.name)
    for folder in ('univariateML-cpp', 'bindings'):
        (layer/folder).symlink_to(root/folder, target_is_directory=True)
    audits = layer/'dev/research/evaluations'
    for folder in ('2026-11-07-fit-audit-data', '2026-11-13-retire-16-data'):
        (audits/folder).mkdir(parents=True, exist_ok=True)
        file = 'cases.json.gz' if 'fit-audit' in folder else 'route-inputs.json'
        shutil.copyfile(root/'dev/research/evaluations'/folder/file, audits/folder/file)
    return layer, validation, audits/'2026-11-07-fit-audit-data/cases.json.gz'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--jobs', type=int, default=1)
    args = parser.parse_args()
    root = Path.cwd()
    logs = root/'build/scratch/cache-controls'
    logs.mkdir(parents=True)
    snapshots = root/'build/snapshots/native'
    _, validation, archive = shadow(root, logs)

    def replay(name, expected, *options, data=snapshots):
        verify(logs, name,
               [sys.executable, str(validation/'cert-replay.py'), str(data),
                '--case', '^class/(base6|ties|two)$', '--family', 'nbinom',
                '--jobs', str(args.jobs), '--cache', str(logs/'replay.sqlite3'), *options],
               expected)

    replay('replay-empty', 'miss')
    replay('replay-warm', 'all')
    cached_bytes = (logs/'replay.sqlite3').read_bytes()
    replay('replay-disabled', 'miss', '--no-cache')
    assert (logs/'replay.sqlite3').read_bytes() == cached_bytes
    replay('replay-precision', 'miss', '--dps', '60')
    replay('replay-budget', 'miss', '--budget', '3999')
    replay('replay-original-options', 'all')
    baseline_source = (validation/'cert_replay_faces.py').read_bytes()
    (validation/'cert_replay_faces.py').write_bytes(baseline_source+b'\n')
    replay('replay-source-changed', 'miss')
    (validation/'cert_replay_faces.py').write_bytes(baseline_source)
    replay('replay-source-restored', 'all')
    archive.write_bytes(gzip.compress(gzip.decompress(archive.read_bytes()), mtime=1))
    replay('replay-archive-changed', 'miss')

    bad = logs/'bad-digest'
    bad.mkdir()
    shutil.copyfile(snapshots/'provenance.json', bad/'provenance.json')
    header, *rows = (snapshots/'nbinom.jsonl').read_text().splitlines()
    rows = [json.loads(row) for row in rows]
    for row in rows:
        if row['source'] == 'class' and row['case'] == 'base6':
            row['input_sha256'] = 'bad-digest'
    (bad/'nbinom.jsonl').write_text(header+'\n'+''.join(json.dumps(row)+'\n' for row in rows))
    replay('replay-invalid-digest', None, data=bad)

    def seam(name, expected, *options):
        verify(logs, name,
               [sys.executable, str(root/'dev/validation/seam-check.py'), '--family', 'nbinom',
                '--jobs', str(args.jobs), '--output', str(logs/(name+'.jsonl')),
                '--cache', str(logs/'seam.sqlite3'), *options],
               expected)
    seam('seam-empty', 'miss')
    seam('seam-warm', 'all')
    cached_bytes = (logs/'seam.sqlite3').read_bytes()
    seam('seam-disabled', 'miss', '--no-cache')
    assert (logs/'seam.sqlite3').read_bytes() == cached_bytes
    seam('seam-double-flags', 'miss', '--no-long-double')
    seam('seam-double-warm', 'all', '--no-long-double')
    seam('seam-guard-changed', 'miss', '--timeout', '31')
    seam('seam-original-guard', 'all')
    base = json.loads((logs/'seam-empty.provenance.json').read_text())
    retry = json.loads((logs/'seam-guard-changed.provenance.json').read_text())
    assert base['probe_sha256'] == retry['probe_sha256']
    assert base['cache_sha256'] != retry['cache_sha256']
    print('seam-guard-changed reused the executable fingerprint')
    assert (logs/'seam-empty.jsonl').read_bytes() == (logs/'seam-warm.jsonl').read_bytes()


if __name__ == '__main__':
    main()
