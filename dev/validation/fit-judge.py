#!/usr/bin/env python3
"""Strict layer, first part: judge stored fit snapshots without refitting.

fit-snapshot.py records what every fitter returns and judges nothing. This
script applies checks that need only those records, and compares the failures
with a tracked list of known ones (the ratchet): a failure that is not on the
list makes the run fail, and a listed failure that no longer occurs is
reported so the list can shrink. See dev/design/consolidation.md, step 6.

Checks:
  terminates     no case ends in the snapshot's CPU guard or a crash
  finite         an estimate returns finite coefficients and a finite log
                 likelihood; an infinite coefficient is allowed only where the
                 caller held it at that value
  held           an estimate returns every held coefficient unchanged
  invalid-input  empty, NaN and infinite samples end in a plain error
  nesting        on the same data, a fit with fewer held coefficients has a
                 log likelihood at least that of a fit that holds more (same
                 values). The larger fit may be an estimate or a limit stop
                 with a stated supremum; fits on a restriction boundary are
                 skipped, since their parameter space is smaller
  mode           native and double-only builds end in the same kind

Usage:
  fit-judge.py NATIVE [DOUBLE]            judge; exit 1 on a failure not listed
  fit-judge.py NATIVE [DOUBLE] --prune    also drop listed failures that pass
  fit-judge.py NATIVE [DOUBLE] --add REF  list the new failures under REF
                                          (a card id; deliberate, reviewed)
NATIVE and DOUBLE are snapshot directories or stored snapshot.jsonl.gz files;
several can be given separated by commas (families and modifier routes).
"""
import argparse, gzip, json, math, sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ARCHIVE = ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz'
KNOWN = ROOT/'dev/validation/strict-known.json'
INVALID = ('empty', 'nan', 'inf', 'neginf')
NEST_TOL = 1e-8   # relative to max(1, |logLik|); far above rounding, far below any real gap

def load(paths):
    rows = {}
    for path in str(paths).split(','): rows.update(load_one(path))
    return rows

def load_one(path):
    rows = {}
    path = Path(path)
    if path.is_file():
        with gzip.open(path, 'rt') as source:
            for text in source:
                r = json.loads(text); rows[(r['family'], r['source'], r['case'])] = r
        return rows
    for part in sorted(path.glob('*.jsonl')):
        for text in part.read_text().splitlines()[1:]:
            r = json.loads(text); rows[(r['family'], r['source'], r['case'])] = r
    return rows

def number(text):
    return float(text) if text in ('nan', 'inf', '-inf') else float.fromhex(text)

def judge_one(mode, rows, data):
    failures = {}
    def fail(check, key, detail): failures['|'.join((check, mode)+key)] = detail
    groups = {}
    for key, r in rows.items():
        res = r['result']; kind = res['kind']
        if kind in ('timeout', 'crash'): fail('terminates', key, kind)
        if kind == 'ok':
            bad = [n for n, v in res['estimates'].items()
                   if not math.isfinite(number(v)) and not (n in r['held'] and r['held'][n] == number(v))]
            if bad or not math.isfinite(number(res['logLik'])):
                fail('finite', key, 'non-finite: '+', '.join(bad or ['logLik']))
            # A modifier route ('base +modifier=kernel') takes its inputs in the retired
            # family's coordinates; a held name that is not a coefficient of the base
            # family (a window bound) is not an estimate there.
            route = '+' in key[0]
            moved = [n for n, v in r['held'].items()
                     if (n not in res['estimates'] and not route) or (n in res['estimates'] and number(res['estimates'][n]) != v)]
            if moved: fail('held', key, 'held changed: '+', '.join(moved))
        if r['source'] == 'class' and key[2] in INVALID and kind != 'error':
            fail('invalid-input', key, 'ends in '+kind)
        if r['source'] == 'archive' and int(key[2]) in data:
            groups.setdefault((key[0], data[int(key[2])]), []).append((key, r))
    for members in groups.values():
        for key, larger in members:
            res = larger['result']
            if res['kind'] in ('timeout', 'crash') or res['restricted']: continue
            if res['kind'] == 'ok': top = number(res['logLik'])
            elif res['kind'] == 'limit' and res['limit_loglik'] != 'nan': top = number(res['limit_loglik'])
            else: continue
            worst = None
            for other, nested in members:
                inner = nested['result']
                if inner['kind'] != 'ok' or not set(larger['held']) < set(nested['held']): continue
                if any(nested['held'][n] != v for n, v in larger['held'].items()): continue
                value = number(inner['logLik'])
                if math.isfinite(value) and value-top > NEST_TOL*max(1, abs(value)) and (worst is None or value-top > worst[0]):
                    worst = (value-top, other[2])
            if worst:
                flag = res['extra'].get('certified')
                fail('nesting', key, 'case %s holds more and is higher by %.4g; this fit is %s' % (
                    worst[1], worst[0], 'unflagged' if flag is None else 'certified' if number(flag) else 'uncertified'))
    return failures

def judge(native, double, data):
    failures = judge_one('native', native, data)
    if double is not None:
        failures.update(judge_one('double', double, data))
        for key in sorted(set(native) & set(double)):
            a, b = native[key]['result']['kind'], double[key]['result']['kind']
            if a != b and 'timeout' not in (a, b):
                failures['|'.join(('mode', 'both')+key)] = 'native %s, double-only %s' % (a, b)
    return failures

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('native'); ap.add_argument('double', nargs='?')
    ap.add_argument('--known', type=Path, default=KNOWN)
    ap.add_argument('--prune', action='store_true', help='drop listed failures that no longer occur')
    ap.add_argument('--add', metavar='REF', help='list the new failures under this reference')
    args = ap.parse_args()
    with gzip.open(ARCHIVE, 'rt') as source:
        data = {c['index']: tuple(c['data']) for c in json.load(source)}
    native = load(args.native); double = load(args.double) if args.double else None
    failures = judge(native, double, data)
    known = json.loads(args.known.read_text())['known'] if args.known.exists() else {}
    # A listed failure counts as fixed only if its case was judged in this run.
    judged = {('native',)+k for k in native} | {('both',)+k for k in native}
    if double is not None: judged |= {('double',)+k for k in double}
    seen = lambda entry: tuple(entry.split('|')[1:5]) in judged and (double is not None or entry.split('|')[1] == 'native')
    new = sorted(set(failures)-set(known)); fixed = sorted(k for k in known if k not in failures and seen(k))
    counts = {}
    for key in failures: counts[key.split('|')[0]] = counts.get(key.split('|')[0], 0)+1
    print('%d cases judged%s; failures by check: %s' % (len(native), '' if double is None else ' in each of two modes', counts or 'none'))
    print('%d listed as known, %d new, %d listed but passing' % (len(set(failures) & set(known)), len(new), len(fixed)))
    for key in new: print('  NEW   %s: %s' % (key, failures[key]))
    for key in fixed: print('  FIXED %s (%s)' % (key, known[key]['ref']))
    if args.add:
        for key in new: known[key] = dict(ref=args.add, detail=failures[key])
    if args.prune:
        for key in fixed: del known[key]
    if args.add or args.prune:
        args.known.write_text(json.dumps(dict(
            note='Known failures of the strict layer (fit-judge.py). Entries leave when fixed (--prune); '
                 'adding one (--add) is a reviewed decision with a card.',
            known=dict(sorted(known.items()))), indent=1)+'\n')
    sys.exit(1 if new and not args.add else 0)

if __name__ == '__main__': main()
