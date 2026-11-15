#!/usr/bin/env python3
"""Run the five root-finder emulations on every baseline case of their path (native and double snapshots).
Usage: sweep.py OUTDIR   (writes sweep.jsonl)"""
import sys, gzip, json, math, time
from pathlib import Path
import mpmath as mp
sys.path.insert(0, str(Path(__file__).resolve().parent))
from paths import *

ROOT = Path(__file__).resolve().parents[4]
EV = ROOT/'dev/research/evaluations'
HALF = 2.0**-53
arch = {('%06d' % c['index']): c for c in json.load(gzip.open(EV/'2026-11-07-fit-audit-data/cases.json.gz', 'rt'))}
DISTS = ROOT/'univariateML-cpp/include/cppunivariate/distributions'

def rows(mode):
    for l in gzip.open(EV/('2026-11-15-fit-snapshot-data/%s.jsonl.gz' % mode), 'rt'):
        yield json.loads(l)

def data_of(r):
    if r['source'] == 'archive': return [float(v) for v in arch[r['case']]['data']], arch[r['case']]['held']
    if r['source'] == 'golden':
        return [float(v) for v in json.load(open(DISTS/r['family']/'golden.json'))['data']], r['held']
    return [float(v) for v in r['data']], r['held']

def est(r, name): return float.fromhex(r['result']['estimates'][name])
def f(v): return float(v) if v is not None else None

def record(r, mode, path, o, shape_name, claim, W_parts):
    row = dict(mode=mode, family=r['family'], source=r['source'], case=r['case'], mask=r['mask'], n=r['n'], path=path,
               shape_returned=est(r, shape_name), shape_emulated=o['shape'], match=o['shape'] == est(r, shape_name),
               offset=f(o['offset']), claim=claim)
    row.update({k: f(v) for k, v in W_parts.items()})
    row['fails_claim'] = abs(row['offset']) > claim
    return row

def main(out):
    out = Path(out); out.mkdir(exist_ok=True, parents=True)
    t0 = time.time(); n = 0
    with open(out/'sweep.jsonl', 'w') as fh:
        for mode in ('native', 'double'):
            for r in rows(mode):
                fam, mask = r['family'], r['mask']
                if r['result']['kind'] != 'ok' or (r['result'].get('outcome') or {}).get('kind') != 'estimate': continue
                rec = None
                try:
                    if (fam in ('invweibull', 'llogis') and mask == 2) or (fam == 'frechet' and mask == 6):
                        x, held = data_of(r)
                        if fam == 'frechet':
                            mu = held['mu']; xs = [v-mu for v in x]; xe = [D(v)-D(mu) for v in x]; scale = held['sigma']; shp = 'lambda'
                            if any(v <= 0 for v in xs): continue
                        else:
                            xs = x; xe = [D(v) for v in x]; scale = held['scale']; shp = 'shape'
                        o = location_scale_held(xs, xe, scale, fam != 'llogis')
                        slope = abs(o['slope'])
                        cond_a = (o['E_data']+o['E_arith'])/slope; cond_b = (o['E_data']+o['E_arith_bound'])/slope
                        half = o['half']
                        Wa = half+cond_a+o['conv_actual']; Wb = half+cond_b+o['conv_bound']
                        rec = record(r, mode, 'location_scale_held', o, shp, 4*EPS+HALF*0,
                                     dict(half=half, cond_actual=cond_a, cond_bound=cond_b, data_bound=o['E_data']/slope, data_actual=o['data_actual'],
                                          arith_actual=o['E_arith']/slope, arith_bound=o['E_arith_bound']/slope, conv_actual=o['conv_actual'],
                                          conv_bound=o['conv_bound'], W_actual=Wa, W_bound=Wb, z=o['z'], ulp_z=ulp(o['z']), unit=o['unit'], slope=slope))
                        # offset of the emulation is in log shape of the true problem; the public shape is the C++ output
                    elif fam == 'weibull' and mask == 0:
                        x, _ = data_of(r)
                        o = weibull_free(x, 'x' if mode == 'native' else 'd')
                        sl = abs(o['slope_rel'])
                        cond_a = (o['E_data']+o['E_arith'])/sl; cond_b = (o['E_data']+o['E_arith_bound'])/sl
                        Wa = o['half']+cond_a+o['conv_actual']; Wb = o['half']+cond_b+o['conv_bound']
                        rec = record(r, mode, 'weibull_free', o, 'shape', 0.5e-14+HALF,
                                     dict(half=o['half'], cond_actual=cond_a, cond_bound=cond_b, data_bound=o['E_data']/sl, data_actual=o['data_actual'],
                                          arith_actual=o['E_arith']/sl, arith_bound=o['E_arith_bound']/sl, conv_actual=o['conv_actual'], conv_bound=o['conv_bound'],
                                          W_actual=Wa, W_bound=Wb, slope=sl, d_err_actual=o['d_err_actual'], y_err_bound=o['y_err_max']))
                    elif fam == 'gamma' and mask == 0:
                        x, _ = data_of(r)
                        o = gamma_bracket(x)
                        if o is None: continue
                        sl = o['slope']
                        cond_a = (o['E_G_actual']+o['E_s_actual']+o['E_trunc'])/sl; cond_b = (o['E_G_bound']+o['E_s_bound']+o['E_trunc'])/sl
                        Wa = o['half']+cond_a+o['conv_actual']; Wb = o['half']+cond_b+o['conv_bound']
                        rec = record(r, mode, 'gamma_bracket', o, 'shape', 2*EPS+HALF,
                                     dict(half=o['half'], cond_actual=cond_a, cond_bound=cond_b, conv_actual=o['conv_actual'], conv_bound=o['conv_bound'],
                                          W_actual=Wa, W_bound=Wb, slope=sl, k=o['shape'], s=o['s']))
                    elif fam == 'naka' and mask == 2:
                        x, held = data_of(r)
                        o = naka_square_held(x, held['scale'])
                        if o is None: continue
                        sl = o['slope']
                        cond_a = (o['E_G_actual']+o['E_s_actual']+o['E_trunc'])/sl; cond_b = (o['E_G_bound']+o['E_s_bound']+o['E_trunc'])/sl
                        Wa = o['half']+cond_a+o['conv_actual']; Wb = o['half']+cond_b+o['conv_bound']
                        rec = record(r, mode, 'naka_square_held', o, 'shape', 2*EPS+HALF,
                                     dict(half=o['half'], cond_actual=cond_a, cond_bound=cond_b, conv_actual=o['conv_actual'], conv_bound=o['conv_bound'],
                                          W_actual=Wa, W_bound=Wb, slope=sl, k=o['shape'], s=o['s']))
                except Exception as ex:
                    rec = dict(mode=mode, family=fam, source=r['source'], case=r['case'], mask=mask, error=repr(ex))
                if rec is not None:
                    fh.write(json.dumps(rec)+'\n'); fh.flush(); n += 1
    print('rows', n, 'seconds', round(time.time()-t0))

if __name__ == '__main__': main(sys.argv[1])
