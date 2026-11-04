"""Exact rational root and local maximum certificate for the golden sample.

Uses prior rational exp/log enclosures read-only. No float comparisons decide
certificate inequalities. Chosen centers and preconditioners are binary64
values interpreted as exact rationals, not presumed exact roots/inverses.
"""
import json
from fractions import Fraction as F
from pathlib import Path
import sys
import time

HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(HERE.parent/'2026-11-03-burr-closure-data'))
from cert import I, exp, log


def mag(x):return max(abs(x.lo),abs(x.hi))


def evaluate(y,t):
    k,c,b=map(I,t);n=len(y)
    z=[c*v+b for v in y]
    p=[1/(1+exp(-v)) for v in z]
    w=[v*(1-v) for v in p]
    sy=sum(y,I(0));sp=sum(p,I(0));syp=sum((v*a for v,a in zip(y,p)),I(0))
    g=[n/k-sum((log(1+exp(v)) for v in z),I(0)),n/c+sy-(k+1)*syp,n-(k+1)*sp]
    h00=-n/(k*k);h01=-syp;h02=-sp
    h11=-n/(c*c)-(k+1)*sum((v*v*a for v,a in zip(y,w)),I(0))
    h12=-(k+1)*sum((v*a for v,a in zip(y,w)),I(0));h22=-(k+1)*sum(w,I(0))
    return g,[[h00,h01,h02],[h01,h11,h12],[h02,h12,h22]]


def matmul(a,b):
    return [[sum((a[i][j]*b[j][k] for j in range(len(b))),I(0))
             for k in range(len(b[0]))] for i in range(len(a))]


def main():
    start=time.perf_counter()
    info=json.loads((HERE/'isolation.json').read_text())
    r=next(a for a in info['certificates'] if a['sample']=='golden')
    samples=json.loads((HERE.parent/'2026-11-03-burr-xi-data/samples.json').read_text())
    y=[log(I(F(v))) for v in samples['golden']]
    t=list(map(F,r['center']));rad=list(map(F,r['radii']))
    R=[[I(F(v)) for v in row] for row in r['local']['R']]
    g,_=evaluate(y,t)
    box=[I(a-d,a+d) for a,d in zip(t,rad)]
    _,h=evaluate(y,box)
    E=matmul(R,h)
    E=[[v+int(i==j) for j,v in enumerate(row)] for i,row in enumerate(E)]
    eg=matmul(R,[[v] for v in g])
    ratios=[(mag(eg[i][0])+sum(mag(E[i][j])*rad[j] for j in range(3)))/rad[i] for i in range(3)]
    assert all(v<F(9,10) for v in ratios)
    scale=exp(-box[2]/box[1])
    # Golden values live at the canonical family source.
    root=HERE.parents[3]
    golden=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/burr/golden.json').read_text())
    bounds=[box[0],box[1],scale]
    errors=[]
    for key,v in zip(['shape1','shape2','scale'],bounds):
        err=mag(v/F(golden['estimates'][key])-1)
        assert err<F(1,10**6)
        errors.append(str(err))
    # Exact positive definiteness of the information on the local root box.
    j=[[-v for v in row] for row in h]
    det2=j[0][0]*j[1][1]-j[0][1]*j[1][0]
    det3=(j[0][0]*(j[1][1]*j[2][2]-j[1][2]*j[2][1])
          -j[0][1]*(j[1][0]*j[2][2]-j[1][2]*j[2][0])
          +j[0][2]*(j[1][0]*j[2][1]-j[1][1]*j[2][0]))
    assert j[0][0].lo>0 and det2.lo>0 and det3.lo>0
    result=dict(arithmetic='exact rational, exp/log Taylor enclosures; comparisons use Fraction',
                root_box=[[str(v.lo),str(v.hi)] for v in bounds],
                ratio_upper=[str(v) for v in ratios],
                coefficient_relative_errors_upper=errors,
                positive_information_minors=[j[0][0].pair(),det2.pair(),det3.pair()],
                seconds=time.perf_counter()-start)
    (HERE/'exact-root.json').write_text(json.dumps(result,indent=2)+'\n')
    print('Exact rational golden root certificate passed:',result['seconds'],'seconds',flush=True)

if __name__=='__main__':main()
