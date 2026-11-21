#!/usr/bin/env python3
"""Strict layer: seam-check.py with the known-failure ratchet.

seam-check.py refits the frozen fit-audit archive and the golden samples
through the binding adapters and compares each returned logLik with the
summed public log density, attributing a disagreement with an independent
mpmath value (fitter, dpqr, both). This runs it for each requested precision
mode into build/strict/seam/<mode>.jsonl and judges its records:

  seam       a disagreement beyond 1e-10 * max(1, |logLik|), or returned
             coefficients that put an observation outside the support
  seam-open  a fit with no terminal result within seam-check's audit guard,
             also when replayed once with a 300 s guard (the fit snapshot's)

Failures are keyed 'seam|mode|family|source|case' (archive cases by their
six-digit index, goldens as 'golden') and compared with strict-known.json.

Usage:
  strict-seam.py MODE [MODE] [--jobs N] [--no-cache]
                 [--prune | --add REF [--match RE]]   (MODE: native or double)
Reserve one CPU slot per job. The first run of a mode refits every case.
"""
import argparse, json, subprocess, sys
from strict_common import ROOT, HERE, add_arguments, positive_jobs, ratchet

WORK = ROOT/'build/strict/seam'
CHECKS = ('seam', 'seam-open')

RETRY = 300   # seconds: the fit snapshot's guard; seam-check's own 30 s wall-clock guard depends on load

def run(mode, output=None, extra=()):
    output = output or WORK/(mode+'.jsonl')
    WORK.mkdir(parents=True, exist_ok=True)
    command = [sys.executable, str(HERE/'seam-check.py'), '--output', str(output), *extra]+(['--no-long-double'] if mode == 'double' else [])
    result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True)
    if result.returncode:
        sys.stderr.write(result.stdout[-2000:]+result.stderr[-2000:])
        raise SystemExit('seam-check.py failed in %s mode' % mode)
    print('seam-check %s: %s' % (mode, result.stdout.splitlines()[-1].removeprefix('seam-check: ')))
    return output

def records(mode, probe_options=()):
    """The mode's records; fits that hit seam-check's guard are replayed once with the longer guard."""
    rows = [json.loads(text) for text in run(mode, extra=probe_options).read_text().splitlines()]
    guarded = sorted(r['index'] for r in rows if r['outcome'] == 'open fit' and r['source'] == 'archive')
    if guarded:
        extra = [*probe_options, '--timeout', str(RETRY)]+[a for i in guarded for a in ('--case', str(i))]
        retry = {r['index']: r for r in map(json.loads, run(mode, WORK/(mode+'-retry.jsonl'), extra).read_text().splitlines())
                 if r['source'] == 'archive'}
        rows = [retry.get(r['index'], r) if r['source'] == 'archive' and r['outcome'] == 'open fit' else r for r in rows]
        print('seam-check %s: %d guarded fits replayed with a %d s guard' % (mode, len(guarded), RETRY))
    return rows

def judge(mode, rows):
    failures = {}; judged = set(); counts = {}
    for r in rows:
        key = (r['family'], r['source'], 'golden' if r['source'] == 'golden' else '%06d' % r['index'])
        judged.add((mode,)+key)
        counts[r['outcome']] = counts.get(r['outcome'], 0)+1
        if r['outcome'] == 'mismatch':
            detail = 'reported %r, summed density %r' % (r.get('reported'), r.get('dpqr'))
            if r.get('outside_support'): detail += ', observations outside the support: %s' % r['outside_support']
            if 'attribution' in r: detail += '; attribution: %s' % r['attribution']
            failures['|'.join(('seam', mode)+key)] = detail
        elif r['outcome'] == 'open fit':
            failures['|'.join(('seam-open', mode)+key)] = r.get('reason', 'no terminal result')
    print('seam-check %s: %s' % (mode, counts))
    return failures, judged

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('modes', nargs='+', choices=('native', 'double'))
    ap.add_argument('--jobs', type=positive_jobs, default=1, help='parallel probe workers (default: 1)')
    ap.add_argument('--no-cache', action='store_true', help='force all seam probes to run')
    add_arguments(ap)
    args = ap.parse_args()
    probe_options = ['--jobs', str(args.jobs), *(['--no-cache'] if args.no_cache else [])]
    failures = {}; judged = set()
    for mode in args.modes:
        f, j = judge(mode, records(mode, probe_options)); failures.update(f); judged |= j
    sys.exit(ratchet(failures, judged, lambda check: check in CHECKS, args, 'strict-seam'))

if __name__ == '__main__': main()
