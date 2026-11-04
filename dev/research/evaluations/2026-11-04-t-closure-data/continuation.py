#!/usr/bin/env python3
"""Targeted follow-up: move the first mode into the std shape domain."""
import json
from pathlib import Path
import numpy as np
from scipy.optimize import brentq
from research import conditional,profile,mp_refine,moments
OUT=Path(__file__).resolve().parent
base=json.loads((OUT/'fixtures.json').read_text())['two-interior-maxima']
x=np.array(base['x']);basew=np.array(base['weights']);kept=[]
for trial,factor in enumerate(np.linspace(.7,2.5,37),1):
    w=basew.copy();w[-2:]*=factor
    ps=profile(x,w,np.geomspace(1,1000,130));roots=[]
    for l,r in zip(ps,ps[1:]):
        if l['dnu']*r['dnu']<0:
            nu=brentq(lambda v:conditional(x,w,v)['dnu'],l['nu'],r['nu'],xtol=1e-9)
            p=conditional(x,w,nu);p['kind']='max' if l['dnu']>0 else 'min';roots.append(p)
    if len([p for p in roots if p['kind']=='max' and p['nu']>2])>=2:
        # Rational empirical frequencies: replace the decimal factor by /20.
        wi=np.rint(w*20).astype(int);wf=wi.astype(float)
        ps=profile(x,wf,np.geomspace(2,1000,160));rr=[]
        for l,r in zip(ps,ps[1:]):
            if l['dnu']*r['dnu']<0:
                nu=brentq(lambda v:conditional(x,wf,v)['dnu'],l['nu'],r['nu'],xtol=1e-9)
                p=conditional(x,wf,nu);q=mp_refine(x,wf,p);q['kind']='max' if l['dnu']>0 else 'min';rr.append(q)
        kept.append({'factor':factor,'x':x.tolist(),'weights':wi.tolist(),'moments':moments(x,wf),'stationary_candidates':rr})
        break
(OUT/'continuation.json').write_text(json.dumps({'trials':trial,'budget':37,'found':kept},indent=2)+'\n')
print('Continuation completed:',len(kept),'two-mode std examples')
