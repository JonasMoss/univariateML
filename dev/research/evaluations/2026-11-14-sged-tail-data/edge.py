"""Spot check of psged at extreme radii, both builds (TASK-220).

    python3 dev/research/evaluations/2026-11-14-sged-tail-data/edge.py > edge.txt   (from the repository root)

Prints, per input, the normalized log-tail errors [lower, upper] against the
truth.py reference; a pair (value, reference) marks a non-finite case.
"""
import sys, subprocess, mpmath as mp
sys.path.insert(0,'dev/validation')
from quality.truth import module_at
from pathlib import Path
route=module_at(Path('univariateML-cpp/include/cppunivariate/distributions/sged/truth.py'))
mp.mp.dps=60
cases=[(0.06,3.0,x) for x in (1e300,-1e300,1e200,1.7e308,5e-324,1e-310,-1e-310)]+[(0.5,0.4,x) for x in (1e300,-1e300,1e-320)]+[(1.5,3.0,1e300)]
for b in ('baseline','current'):
    out=subprocess.run(['build/sged-tail/eval-'+b],input=''.join(f'p sged 0 1 {nu!r} {xi!r} {x!r}\n' for nu,xi,x in cases),capture_output=True,text=True).stdout.split('\n')
    for (nu,xi,x),line in zip(cases,out):
        ref=route.build({'mean':mp.mpf(0),'sd':mp.mpf(1),'nu':mp.mpf(nu),'xi':mp.mpf(xi)}).tails(mp.mpf(x))
        g=[float(t) for t in line.split()]
        err=[float(abs(mp.mpf(g[k])-ref[k])/max(1,abs(ref[k]))) if mp.isfinite(ref[k]) and g[k]==g[k] else (g[k],mp.nstr(ref[k],5)) for k in (0,1)]
        print(b,nu,xi,x,'log errors (normalized)',err)
