#!/usr/bin/env python3
"""Summarize sweep.jsonl: per path counts, bit-match, claim failures, derived widths."""
import json, sys, collections
rows = [json.loads(l) for l in open(sys.argv[1])]
print('rows', len(rows), 'errors', sum('error' in r for r in rows))
for r in rows:
    if 'error' in r: print('ERROR', r)
by = collections.defaultdict(list)
for r in rows:
    if 'error' not in r: by[(r['path'], r['mode'], r['family'])].append(r)
print('%-22s %-7s %-10s %4s %5s %5s %5s | %9s %9s %9s %9s | %7s %7s' % ('path', 'mode', 'family', 'n', 'match', 'fail', 'inA', 'max|off|', 'max W_act', 'max W_bnd', 'max.Wb/1e-10', 'min A/off', 'max off/W'))
for key, rs in sorted(by.items()):
    ok = [r for r in rs if r['match']]
    inA = sum(abs(r['offset']) <= r['W_actual'] for r in ok); 
    print('%-22s %-7s %-10s %4d %5d %5d %5d | %9.2e %9.2e %9.2e %9.1e |' % (key + (len(rs), len(ok), sum(r['fails_claim'] for r in rs), inA,
          max(abs(r['offset']) for r in rs), max(r['W_actual'] for r in rs), max(r['W_bound'] for r in rs), max(r['W_bound'] for r in rs)/1e-10)))
print('\nlargest offset / W_actual and offset / W_bound per path (native):')
for key, rs in sorted(by.items()):
    if key[1] != 'native': continue
    ok = [r for r in rs if r['match']]
    print('  %-22s %-10s %.3f  %.3f   cases with 4eps-type claim failing: %d of %d' % (key[0], key[2], max(abs(r['offset'])/r['W_actual'] for r in ok), max(abs(r['offset'])/r['W_bound'] for r in ok), sum(r['fails_claim'] for r in rs), len(rs)))
print('\nunmatched emulations (shape differs from the snapshot):')
for r in rows:
    if 'error' not in r and not r['match']: print(' ', r['mode'], r['family'], r['source'], r['case'], r['shape_returned'], r['shape_emulated'], 'off %.2e' % r['offset'])
print('\nfailures of the replay claim (native):')
for r in rows:
    if 'error' not in r and r['fails_claim'] and r['mode'] == 'native':
        print(' %-10s %-8s %-7s m%d offset %9.2e claim %8.2e | half %8.2e cond_a %8.2e (data_b %8.2e data_act %8.2e arith %8.2e) cond_b %8.2e conv %8.2e | W_act %8.2e W_bnd %8.2e in %s' % (
            r['family'], r['source'], r['case'], r['mask'], r['offset'], r['claim'], r['half'], r['cond_actual'], r.get('data_bound', 0) or 0, r.get('data_actual', 0) or 0, r.get('arith_actual', 0) or 0, r['cond_bound'], r['conv_actual'], r['W_actual'], r['W_bound'],
            'A' if abs(r['offset']) <= r['W_actual'] else '-'))
print('\ncases NOT inside W_actual or W_bound:')
for r in rows:
    if 'error' not in r and r['match'] and (abs(r['offset']) > r['W_actual'] or abs(r['offset']) > r['W_bound']):
        print(' ', r['mode'], r['family'], r['source'], r['case'], 'off %.2e W_act %.2e W_bnd %.2e' % (r['offset'], r['W_actual'], r['W_bound']))
