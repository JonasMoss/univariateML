#!/usr/bin/env python3
"""Strict layer: seam-check.py with the known-failure ratchet.

seam-check.py refits the frozen fit-audit archive and the golden samples
through the binding adapters and compares each returned logLik with the
summed public log density, attributing a disagreement with an independent
mpmath value (fitter, dpqr, both). This runs it for each requested precision
mode into build/strict/seam/<mode>.jsonl (resumable; a stale output from
changed sources is replaced, never reused) and judges its records:

  seam       a disagreement beyond 1e-10 * max(1, |logLik|), or returned
             coefficients that put an observation outside the support
  seam-open  a fit with no terminal result within seam-check's audit guard,
             also when replayed once with a 300 s guard (the fit snapshot's)

Failures are keyed 'seam|mode|family|source|case' (archive cases by their
six-digit index, goldens as 'golden') and compared with strict-known.json.

Usage:
  strict-seam.py MODE [MODE] [--prune | --add REF [--match RE]]   (MODE: native or double)
Run through /tmp/umvl-cpu/run; the first run of a mode refits every case.
"""
import argparse, json, subprocess, sys
from strict_common import ROOT, HERE, add_arguments, ratchet

WORK = ROOT/'build/strict/seam'
CHECKS = ('seam', 'seam-open')

RETRY = 300   # seconds: the fit snapshot's guard; seam-check's own 30 s wall-clock guard depends on load

def run(mode, output=None, extra=()):
    output = output or WORK/(mode+'.jsonl')
    WORK.mkdir(parents=True, exist_ok=True)
    command = [sys.executable, str(HERE/'seam-check.py'), '--output', str(output), *extra]+(['--no-long-double'] if mode == 'double' else [])
    for attempt in (1, 2):
        result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True)
        if result.returncode == 0: return output
        if attempt == 1 and 'source/selection changed' in result.stderr+result.stdout:
            for suffix in ('.jsonl', '.provenance.json', '.summary.json'):   # stale: refit from scratch
                output.with_suffix(suffix).unlink(missing_ok=True)
            continue
        sys.stderr.write(result.stdout[-2000:]+result.stderr[-2000:])
        raise SystemExit('seam-check.py failed in %s mode' % mode)

def records(mode):
    """The mode's records; fits that hit seam-check's guard are replayed once with the longer guard."""
    rows = [json.loads(text) for text in run(mode).read_text().splitlines()]
    guarded = sorted(r['index'] for r in rows if r['outcome'] == 'open fit' and r['source'] == 'archive')
    if guarded:
        extra = ['--timeout', str(RETRY)]+[a for i in guarded for a in ('--case', str(i))]
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
    add_arguments(ap)
    args = ap.parse_args()
    failures = {}; judged = set()
    for mode in args.modes:
        f, j = judge(mode, records(mode)); failures.update(f); judged |= j
    sys.exit(ratchet(failures, judged, lambda check: check in CHECKS, args, 'strict-seam'))

if __name__ == '__main__': main()
