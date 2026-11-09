#!/usr/bin/env python3
"""Additional formula, envelope, positive quadrature and normalizer checks."""
import mpmath as mp
import pathlib, json, hashlib
from check import C, lg_stirling, tail_reference, logpdf
mp.mp.dps=90
HERE=pathlib.Path(__file__).resolve().parent
rows=[]
def add(name,error,bound,**extra):
    assert abs(error)<=bound,(name,error,bound)
    rows.append(dict(name=name,error=mp.nstr(abs(error),14),bound=mp.nstr(bound,14),**extra))
# Check complete log normalizers, including a near-pole and the large-skew seam.
for ms,vs in [('.500000000001','0'),('.51','1000'),('3.2','7'),('100','1e6'),('1e5','2')]:
    m,v=map(mp.mpf,(ms,vs));ref=C(m,v)
    with mp.workdps(65):
        g,err,_,_=lg_stirling(m+1j*v/2)
        val=(2*m-2)*mp.log(2)-mp.log(mp.pi)+2*g-mp.loggamma(2*m-1)
    add('complete-log-normalizer',val-ref,2*err+mp.mpf('1e-54')*(1+abs(ref)),m=ms,nu=vs)
# Formula (2) independently checked by an infinite product summed numerically.
for ms,vs in [('2','0.2'),('10','3')]:
    m,v=map(mp.mpf,(ms,vs))
    value=-mp.nsum(lambda k:mp.log1p((v/(m+k))**2),[0,mp.inf])
    ref=2*mp.re(mp.loggamma(m+1j*v)-mp.loggamma(m))
    add('gamma-product',value-ref,mp.mpf('1e-70'))
# Explicit positive panel bounds; u grid is uniform in t^a.
for ms,vs,rs,s in [('.5001','0','10',1),('.51','80','100',1),('12','-100','1000',-1)]:
    m,v,R=map(mp.mpf,(ms,vs,rs));a=2*m-1;b=1/R;N=2048
    lower=mp.mpf(0);upper=mp.mpf(0)
    for j in range(N):
        l=b*(mp.mpf(j)/N)**(1/a);r=b*(mp.mpf(j+1)/N)**(1/a)
        points=[l,r]
        mode=s*v/(2*m)
        if l<mode<r:points.append(mode)
        hs=[mp.exp(-m*mp.log1p(t*t)+s*v*mp.atan(t)) for t in points]
        lower+=min(hs)/N;upper+=max(hs)/N
    lp=tail_reference(R,m,v,s)
    integral=mp.exp(lp-C(m,v)+s*v*mp.pi/2-a*mp.log(b)+mp.log(a))
    assert lower<=integral<=upper
    gap=(upper-lower)/lower
    add('positive-panel-relative-width',gap,mp.mpf('.003'),m=ms,nu=vs,panels=N,
        lower=mp.nstr(lower,20),reference=mp.nstr(integral,20),upper=mp.nstr(upper,20))
# Explicit rational score sign counterexample: three scale stationary roots.
def score(al):
    return -2+(2-10*al)/(al*al+1)+(200+100*al)/(al*al+100)
assert score(mp.mpf(0))>0 and score(mp.mpf(1))<0 and score(mp.mpf(10))>0 and score(mp.mpf(100))<0
rows.append(dict(name='three-scale-roots',m=1,nu=10,residuals=[-1,10],
                 score_signs=['+','-','+','-'],scales=[0,1,10,100]))
# Exact balanced Cauchy continuum vs collapse face.
for loc in [mp.mpf('.1'),mp.mpf('.5'),mp.mpf('.9')]:
    al=mp.sqrt(loc*(1-loc))
    ll=logpdf(mp.mpf(0),1,0,loc,al)+logpdf(mp.mpf(1),1,0,loc,al)
    add('Cauchy-continuum',ll+2*mp.log(mp.pi),mp.mpf('1e-75'))
# Uniform envelope in the mode chart and exact score derivative identity.
max_ratio=mp.mpf(0)
for ms in ['.5001','.7','1','5','10000']:
    m=mp.mpf(ms);D=mp.sqrt(2*m);M=min(m,mp.mpf(1));D0=mp.sqrt(2*M)
    for vs in ['0','-8','1000','1e8']:
        v=mp.mpf(vs);r=v/(2*m);k=r/mp.sqrt(1+r*r)
        al=mp.mpf(1);w=mp.sqrt(1+r*r)/D;c=-r
        for us in ['-100','-2','-.1','.1','2','100']:
            u=mp.mpf(us);x=c+w*u
            kernel=logpdf(x,m,v)-logpdf(c,m,v)
            bound=2*M-2*M*mp.log1p(abs(u)/D0)
            assert kernel<=bound+mp.mpf('1e-60')
            z=x
            du=w*(-(2*m*z+v)/(1+z*z))
            exact=-u/(1-2*k*u/D+(u/D)**2)
            add('mode-chart-score',du-exact,mp.mpf('1e-60'))
            max_ratio=max(max_ratio,mp.exp(kernel-bound))
rows.append(dict(name='uniform-envelope-grid',points=120,max_ratio=mp.nstr(max_ratio,20)))
# Rank evidence is independent of the proof: evaluation matrix at four z values.
for m,v in [(mp.mpf('.7'),mp.mpf(0)),(mp.mpf('1.3'),mp.mpf('2.1'))]:
    am=2*mp.re(mp.digamma(m+1j*v/2))-2*mp.digamma(2*m-1)+2*mp.log(2)
    av=-mp.im(mp.digamma(m+1j*v/2))
    A=mp.matrix([[(2*m*z+v)/(1+z*z),-1+(2*m*z*z+v*z)/(1+z*z),am-mp.log1p(z*z),av-mp.atan(z)] for z in map(mp.mpf,['-3','-.4','.8','4'])])
    det=mp.det(A);assert abs(det)>mp.mpf('1e-10')
    rows.append(dict(name='score-evaluation-rank',m=str(m),nu=str(v),determinant=mp.nstr(det,20)))
out=dict(decimal_digits=90,checks=rows,passed=len(rows),script_sha256=hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest())
(HERE/'structure.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps({'structural_checks':len(rows)}))
