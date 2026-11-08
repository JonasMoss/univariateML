#!/usr/bin/env python3
"""Compare probe output with independent mpmath density and incomplete gamma.
Run from the repository root: python3 <this file> <compiled probe> <output.json>.
The inputs are interpreted as binary64, then lifted exactly to 100 digits.
"""
import json
from pathlib import Path
import subprocess
import sys
import mpmath as mp
mp.mp.dps = 100

def reference(x, mean, sd, nu, xi):
    ls = (mp.loggamma(1/nu)-mp.loggamma(3/nu))/2
    moment = mp.exp(mp.loggamma(2/nu)-(mp.loggamma(1/nu)+mp.loggamma(3/nu))/2)
    mu = moment*(xi-1/xi)
    sigma = mp.sqrt(1+(1-moment**2)*(xi-1/xi)**2)
    z = (x-mean)/sd*sigma+mu
    scale = 1/xi if z < 0 else xi
    a = mp.exp(nu*(mp.log(abs(z))-mp.log(scale)-ls))
    ld = mp.log(2/(xi+1/xi))-mp.log(2)-ls-mp.loggamma(1+1/nu)-a-mp.log(sd)+mp.log(sigma)
    mass = scale/(xi+1/xi)*mp.gammainc(1/nu,a,mp.inf,regularized=True)
    lp, lq = (mp.log(mass),mp.log1p(-mass)) if z<0 else (mp.log1p(-mass),mp.log(mass))
    return ld,lp,lq

records=[]
for row in subprocess.check_output([sys.argv[1]],text=True).splitlines():
    fields=row.split()
    if fields[0]=='quantile':
        xi,nu,value=map(lambda s:mp.mpf(float(s)),fields[1:])
        sd=mp.mpf(float(1e300))
        # At this shape the unit quantile is negligible relative to the
        # shift. Bound it independently by bracketing the gamma quantile.
        left=mp.mpf('.1')<1/(1+xi**2)
        scale=1/xi if left else xi
        tail=(mp.mpf(float(.1)) if left else 1-mp.mpf(float(.1)))*(xi+1/xi)/scale
        lo,hi=mp.mpf(0),mp.mpf(1e5)
        for _ in range(400):
            mid=(lo+hi)/2
            if mp.gammainc(1/nu,mid,mp.inf,regularized=True)>tail: lo=mid
            else: hi=mid
        ls=(mp.loggamma(1/nu)-mp.loggamma(3/nu))/2
        moment=mp.exp(mp.loggamma(2/nu)-(mp.loggamma(1/nu)+mp.loggamma(3/nu))/2)
        mu=moment*(xi-1/xi)
        sigma=mp.sqrt(1+(1-moment**2)*(xi-1/xi)**2)
        bounds=[sd*((-1 if left else 1)*scale*mp.exp(ls+mp.log(g)/nu)-mu)/sigma for g in (lo,hi)]
        expected=sum(bounds)/2
        error=abs(value/expected-1)
        assert error<mp.mpf('2e-10'),(row,expected,error)
        records.append(dict(row=row,reference=str(expected),relative_error=str(error),gamma_bracket_width=str(hi-lo)))
        continue
    name=fields[0]
    x,mean,sd,nu,xi=map(lambda s:mp.mpf(float(s)),fields[1:6])
    actual=list(map(mp.mpf,fields[6:]))
    expected=reference(x,mean,sd,nu,xi)
    errors=[abs(a-b) for a,b in zip(actual,expected)]
    # Absolute error for log densities and log probabilities; tails with
    # magnitude above one get a relative comparison as well.
    assert all(e<(mp.mpf('3e-11')*max(1,abs(b)) if abs(b)>mp.mpf('.1') else mp.mpf('3e-10')*abs(b)) for e,b in zip(errors,expected)),(row,expected,errors)
    records.append(dict(row=row,reference=list(map(str,expected)),absolute_errors=list(map(str,errors))))
Path(sys.argv[2]).write_text(json.dumps(dict(dps=mp.mp.dps,records=records),indent=2)+'\n')
print(f'{len(records)} independent high-precision checks passed')
