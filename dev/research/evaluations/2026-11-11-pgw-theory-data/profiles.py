#!/usr/bin/env python3
"""Illustrative finite-box profiles, not a global optimization certificate."""
import json
from pathlib import Path
import numpy as np
from scipy.optimize import minimize
from scipy.special import expit

samples = {'spread': [0.4,0.7,1.0,1.3,2.0,4.0], 'two_point': [0.8,0.8,1.6,1.6,1.6], 'skew': [0.2,0.3,0.5,0.7,1.0,5.0,9.0]}

def objective(v, y, adapted):
    mu, logb, logc = v
    b, c = np.exp(logb), np.exp(logc)
    z = b*(y-mu)
    if adapted:
        L = np.logaddexp(0, z-np.log1p(c))
        h = np.expm1(c*L)*(1+c)/c
        logh = logb-y+z+(c-1)*L
    else:
        L = np.logaddexp(0,z)
        h = np.expm1(c*L)
        logh = logb+logc-y+z+(c-1)*L
    val = np.sum(logh-h)
    return -val if np.isfinite(val) else 1e200

out = {'warning':'Feasible log-likelihood lower bounds from finite-box searches; no estimates or global claims.', 'samples':{}}
with np.errstate(over='ignore',invalid='ignore'):
 for name, sample in samples.items():
    y=np.log(sample); details={}
    for model in ('ordinary','adapted'):
        fits=[minimize(objective,[np.median(y),lb,lc],args=(y,model=='adapted'),method='L-BFGS-B',bounds=[(-8,8),(-4,4),(-4,4)],options={'maxiter':2000,'ftol':1e-13}) for lb,lc in [(0,0),(-1,-2),(1,1),(0,3)]]
        fit=min(fits,key=lambda t:t.fun)
        details[model]={'feasible_loglik_lower_bound':-float(fit.fun),'at_box_edge':bool(any(abs(v-lo)<1e-4 or abs(v-hi)<1e-4 for v,(lo,hi) in zip(fit.x,[(-8,8),(-4,4),(-4,4)])))}
    out['samples'][name]=details
Path(__file__).with_name('profiles.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
