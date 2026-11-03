"""Exact rational instantiation of the seam theorem on the closure witness.

Reuses the previous exact exponential/log enclosures read-only. All assertions
compare rational endpoints; displayed floating-point values are summaries.
"""
from fractions import Fraction as F
import json
from pathlib import Path
import sys

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent/'2026-11-03-burr-closure-data'))
from cert import I, exp, log, root_bracket, second_order


def magnitude(t):
    return max(abs(t.lo),abs(t.hi))


def main():
    y = [F(t,15) for t in [0,5,7,8,8,15,15]]
    n = len(y)
    lo,hi = root_bracket(y)
    c = I(lo,hi)
    eta = log(n/sum((exp(c*t) for t in y),I(0)))
    v = [exp(c*t+eta) for t in y]
    A = sum((a*a/2 for a in v),I(0))-n
    C = second_order(y,c)
    assert A.hi < -F(55,1000)
    assert 2*C.lo > F(59,100)
    R = F(1,50)
    assert R < lo
    # ||(y_i,1)|| <= |y_i|+1 avoids square-root enclosures.
    norms = [abs(t)+1 for t in y]
    vlow = [exp(c*t+eta-R*r).lo for t,r in zip(y,norms)]
    vhigh = [exp(c*t+eta+R*r).hi for t,r in zip(y,norms)]
    d = [sum((t*a*(a-1) for t,a in zip(y,v)),I(0)),sum((a*(a-1) for a in v),I(0))]
    E = sum(a.hi*a.hi*magnitude(a-1)*r for a,r in zip(v,norms))
    h = F(1,64)
    for halvings in range(100):
        weights = [a/(1+h*b)**2 for a,b in zip(vlow,vhigh)]
        mcc = n/(hi+R)**2+sum(w*t*t for w,t in zip(weights,y))
        mce = sum(w*t for w,t in zip(weights,y))
        mee = sum(weights)
        determinant = mcc*mee-mce*mce
        assert determinant > 0
        mu = determinant/(mcc+mee)
        D = sum(magnitude(t) for t in d)+h*E
        B = F(n,6)+D*D/(2*mu)
        if h*D < mu*R and h*B <= -A.hi/2:
            break
        h /= 2
    else:
        raise RuntimeError('exact signed-tail budget')
    result = dict(log_data=[str(t) for t in y],weibull_c_bracket=[str(lo),str(hi)],
                  A=A.pair(),C=C.pair(),radius=str(R),h=str(h),K=str(1/h),halvings=halvings,
                  mu_lower=float(mu),D_upper=float(D),B_upper=float(B),
                  containment_ratio_upper=float(h*D/(mu*R)),
                  slope_ratio_upper=float(h*B/(-A.hi)),
                  conclusion='H(xi) <= W + A_upper*xi/2 < W for 0 < xi <= h; H_second(0) > .59',
                  arithmetic='All assertions are exact rational comparisons with enclosed exp/log.')
    (HERE/'exact.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps(result,indent=2))


if __name__ == '__main__':
    main()
