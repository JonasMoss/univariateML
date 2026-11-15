#!/usr/bin/env python3
"""Add the reviewed new fit-consistency failures to strict-known.json, group by group.

Equivalent to one `fit-consistency.py NATIVE DOUBLE --add REF --match RE --finding SLUG`
run per group below, but judges the snapshots once. Each group is a reviewed
phenomenon (report section 4). Run from the repository root:
  python3 dev/research/evaluations/2026-11-14-strict-rules-data/add_entries.py NATIVE DOUBLE [--dry-run]
"""
import argparse, importlib.util, sys
from pathlib import Path
ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev/validation'))
from strict_common import KNOWN, load, mode_of, ratchet
spec = importlib.util.spec_from_file_location('fit_consistency', ROOT/'dev/validation/fit-consistency.py')
fc = importlib.util.module_from_spec(spec); spec.loader.exec_module(fc)

M = r'\|(native|double)\|'
GROUPS = [
    ('TASK-214.24', 'near-constant-equivariant-numerical-failure',
     r'^phenomenon:near-constant'+M+r'(burr|emg|expweibull|fpareto|gengamma|huber|invburr|lst|pareto4|sn|std|trgamma|triang)\|'),
    ('TASK-214.24', 'near-constant-numerical-failure-unsettled',
     r'^phenomenon:near-constant'+M+r'(betapr|fatigue|genbeta|invparalogis|paralogis)\|'),
    ('TASK-214.24', 'near-constant-unrepresentable-maximizer', r'^phenomenon:near-constant'+M+r'(gompertz|kumar|sgomp)\|'),
    ('TASK-214.24', 'near-constant-differs-from-preimage', r'^phenomenon:near-constant-tie3\|'),
    ('TASK-214.24', 'collapse-not-reported', r'^phenomenon:tie-collapse'+M+r'lst\|'),
    ('TASK-214.17', 'tlambda-collapse-and-two-point-evaluation-failure',
     r'^phenomenon:(tie-collapse|two-point)'+M+r'tlambda\|'),
    ('TASK-214.24', 'two-point-undecided-below-min-n', r'^phenomenon:two-point'+M+r'(expweibull|gengamma|sstd|st)\|'),
]

def main():
    ap = argparse.ArgumentParser(); ap.add_argument('native'); ap.add_argument('double')
    ap.add_argument('--known', type=Path, default=KNOWN); ap.add_argument('--dry-run', action='store_true')
    args = ap.parse_args()
    table = fc.fd.laws(); failures = {}; judged = set()
    for paths in (args.native, args.double):
        mode = mode_of(paths)
        exe, index = fc.fd.build(table, mode == 'double')
        probe = fc.fs.Probe(exe)
        try: f, j, _ = fc.judge(load(paths), mode, table, probe, index)
        finally: probe.close()
        failures.update(f); judged |= j
    owns = lambda check: check.split(':')[0] == fc.CHECK
    for ref, finding, match in GROUPS:
        ns = argparse.Namespace(known=args.known, prune=False, add=None if args.dry_run else ref, match=match, finding=finding)
        print('== %s %s' % (ref, finding))
        ratchet(failures, judged, owns, ns, 'fit-consistency')
    print('== remaining')
    sys.exit(ratchet(failures, judged, owns, argparse.Namespace(known=args.known, prune=False, add=None, match=None, finding=None), 'fit-consistency'))

if __name__ == '__main__': main()
