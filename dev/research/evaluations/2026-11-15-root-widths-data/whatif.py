#!/usr/bin/env python3
"""What-if variants (not in the C++): location roots bisected in log kappa (coord='kappa'), weibull log ratios by log(x/anchor).
Usage: whatif.py OUT.jsonl"""
import sys, json
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
import sweep as S
from paths import *

def main(out):
    with open(out, 'w') as fh:
        for r in S.rows('native'):
            fam, mask = r['family'], r['mask']
            if r['result']['kind'] != 'ok' or (r['result'].get('outcome') or {}).get('kind') != 'estimate': continue
            rec = None
            if (fam in ('invweibull', 'llogis') and mask == 2) or (fam == 'frechet' and mask == 6):
                x, held = S.data_of(r)
                if fam == 'frechet':
                    mu = held['mu']; xs = [v-mu for v in x]; xe = [D(v)-D(mu) for v in x]; scale = held['sigma']
                    if any(v <= 0 for v in xs): continue
                else: xs = x; xe = [D(v) for v in x]; scale = held['scale']
                o = location_scale_held(xs, xe, scale, fam != 'llogis', coord='kappa')
                sl = abs(o['slope']); cb = (o['E_data']+o['E_arith_bound'])/sl; ca = (o['E_data']+o['E_arith'])/sl
                rec = dict(variant='kappa', family=fam, source=r['source'], case=r['case'], offset=float(o['offset']), t=o['z'], half=o['half'],
                           cond_actual=float(ca), cond_bound=float(cb), W_actual=float(o['half']+ca+o['conv_actual']), W_bound=float(o['half']+cb+o['conv_bound']),
                           ulp=ulp(o['z']))
            elif fam == 'weibull' and mask == 0:
                x, _ = S.data_of(r)
                o = weibull_free(x, 'x', ratio_fix=True)
                sl = abs(o['slope_rel']); cb = (o['E_data']+o['E_arith_bound'])/sl; ca = (o['E_data']+o['E_arith'])/sl
                rec = dict(variant='ratio', family=fam, source=r['source'], case=r['case'], offset=float(o['offset']), half=o['half'],
                           cond_actual=float(ca), cond_bound=float(cb), data_actual=float(o['data_actual']), W_actual=float(o['half']+ca+o['conv_actual']), W_bound=float(o['half']+cb+o['conv_bound']))
            if rec: fh.write(json.dumps(rec)+'\n'); fh.flush()
main(sys.argv[1])
