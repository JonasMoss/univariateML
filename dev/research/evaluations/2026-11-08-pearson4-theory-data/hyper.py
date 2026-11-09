#!/usr/bin/env python3
"""Own complex 2F1 partial sums, with a uniform future-ratio remainder bound."""
import mpmath as mp
import json,pathlib,hashlib
mp.mp.dps=90
HERE=pathlib.Path(__file__).resolve().parent
def series(a,b,c,w):
    t=mp.mpc(1);value=t;absolute=mp.mpf(1)
    assert abs(w)<1
    for j in range(10000):
        nxt=t*(a+j)*(b+j)*w/((c+j)*(j+1))
        # All ratios after this next term have index >= J.
        J=j+1
        if J>max(0,-mp.re(c)):
            q=abs(w)*(1+abs(a)/J)*(1+abs(b)/J)/(1+min(0,mp.re(c))/J)
            if q<1:
                bound=abs(nxt)/(1-q)
                if bound<mp.mpf('1e-60')*max(abs(value),mp.mpf('1e-30')):
                    return value,bound,j+1,absolute/max(abs(value),mp.mpf('1e-100'))
        t=nxt;value+=t;absolute+=abs(t)
    raise ArithmeticError('research hypergeometric cap')
rows=[]
for ms,vs,zs in [('2.3','4','.4'),('.7','-1.2','-.3')]:
    m,v,z=map(mp.mpf,(ms,vs,zs));P=m-1j*v/2;Q=m+1j*v/2
    for w in [mp.mpf('.5'),(1-1j*z)/2]:
        ref=mp.hyp2f1(1-Q,P,2-Q,w)
        with mp.workdps(65):
            val,bound,terms,condition=series(1-Q,P,2-Q,w)
        arithmetic=mp.mpf('1e-61')*(1+abs(ref))*condition
        assert abs(val-ref)<=bound+arithmetic
        rows.append(dict(m=ms,nu=vs,z=zs,w=str(w),error=mp.nstr(abs(val-ref),14),
                         mathematical_bound=mp.nstr(bound,14),working_arithmetic_allowance=mp.nstr(arithmetic,14),
                         terms=terms,cancellation_condition=mp.nstr(condition,14)))
out=dict(reference_digits=90,working_digits=65,checks=rows,passed=len(rows),
         script_sha256=hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest())
(HERE/'hyper.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps({'own_hypergeometric_series_checks':len(rows)}))
