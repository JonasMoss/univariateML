"""Independent numerical cross-check, not the global certificate proof.
Run with python3 oracle.py timings-after.txt; scipy brackets every sampled
score sign change and includes every cusp. High-precision analytic tie uses
mpmath. All likelihoods use independent gamma moment formulas.
"""
import json
import math
import sys
from pathlib import Path
import mpmath as mp
import numpy as np
from scipy.optimize import brentq
from scipy.special import gammaln

x = np.array([-3., -1., -.2, 0., .3, .4, 2.])
records = []
for row in Path(sys.argv[1]).read_text().splitlines():
    nu, xi, status, sd, ll, certified, us = map(float, row.split())
    lam = math.exp(-math.log(2)/nu + (gammaln(1/nu)-gammaln(3/nu))/2)
    m1 = math.exp(gammaln(2/nu)-(gammaln(1/nu)+gammaln(3/nu))/2)
    shift = m1*(xi-1/xi)
    su = math.sqrt((1-m1*m1)*(xi*xi+xi**-2)+2*m1*m1-1)
    normal = math.log(su)+math.log(2/(xi+1/xi)) + math.log(nu) - math.log(lam) - (1+1/nu)*math.log(2)-gammaln(1/nu)
    a = (x-.1)*su
    def objective(logsd):
        z = a*math.exp(-logsd)+shift
        return len(x)*(normal-logsd) - sum(.5*(abs(z)/(lam*np.where(z>0,xi,1/xi)))**nu)
    def score(logsd):
        q = a*math.exp(-logsd)
        z = q+shift
        return -len(x)+sum(.5*nu*q*np.sign(z)*abs(z)**(nu-1)/(lam*np.where(z>0,xi,1/xi))**nu)
    knots = sorted(math.log(-v/shift) for v in a if -v/shift>0)
    candidates = [(-40.,objective(-40.)),(40.,objective(40.))]
    # At knots zero the corresponding residual analytically, avoiding the
    # roundoff cusp penalty in the comparison value.
    for knot in knots:
        z = a*math.exp(-knot)+shift
        z[abs(z)<1e-12*max(1,abs(shift))] = 0
        value = len(x)*(normal-knot)-sum(.5*(abs(z)/(lam*np.where(z>0,xi,1/xi)))**nu)
        candidates.append((knot,value))
    cuts = [-40.]+knots+[40.]
    for left,right in zip(cuts,cuts[1:]):
        grid = np.linspace(left+1e-10,right-1e-10,1001)
        for lo,hi in zip(grid,grid[1:]):
            if score(lo)*score(hi)<0:
                root=brentq(score,lo,hi,xtol=1e-13)
                candidates.append((root,objective(root)))
    best=max(candidates,key=lambda p:p[1])
    record=dict(nu=nu,xi=xi,status=status,certified=bool(certified),sd=sd,oracle_sd=math.exp(best[0]),logLik=ll,oracle_logLik=best[1],relative_scale_error=abs(sd/math.exp(best[0])-1),loglik_error=abs(ll-best[1]))
    assert status==0 and certified and record['relative_scale_error']<2e-7 and record['loglik_error']<2e-6, record
    records.append(record)
mp.mp.dps=70
R=mp.findroot(lambda r:mp.log(r)-2*(1-1/r),(4,6))
K=2*mp.sqrt(R-1)/R
xi=mp.sqrt(1+K*K/6)
print(json.dumps(dict(comparisons=records,analytic_tie=dict(R=str(R),K=str(K),xi=str(xi),cost_difference=str(-mp.log(R)+K*mp.sqrt(R-1)))),indent=2))
