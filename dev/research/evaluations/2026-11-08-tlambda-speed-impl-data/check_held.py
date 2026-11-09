from pathlib import Path
import importlib.util,json,mpmath as mp
p=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('mpcheck',p/'check_golden.py');m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
mp.mp.dps=90
row=next(line.split() for line in (p/'extended-data.txt').read_text().splitlines() if line.startswith('-20 '))
m.x=[mp.mpf(float(v)) for v in row[2:]]
fit=next(line.split() for line in (p/'extended-after.log').read_text().splitlines() if line.startswith('-20 1 '));assert fit[2]=='0'
start=[mp.mpf(float(fit[4])),mp.mpf(float(fit[5]))];theta=start.copy()
def f(t):
    ll,g=m.evaluate(mp.mpf(0),t[0],t[1]);return ll,mp.matrix([g[1],g[2]])
h=mp.mpf('1e-30')
for iteration in range(10):
    ll,g=f(theta);H=mp.matrix(2)
    for j in range(2):
        a=theta.copy();b=theta.copy();a[j]-=h;b[j]+=h
        ga=f(a)[1];gb=f(b)[1]
        for i in range(2):H[i,j]=(gb[i]-ga[i])/(2*h)
    assert H[0,0]<0 and mp.det(H)>0
    step=mp.lu_solve(H,-g);theta=[theta[i]+step[i] for i in range(2)]
    if max(abs(v) for v in step)<mp.mpf('1e-55'):break
else:raise AssertionError('high-precision Newton did not converge')
ll,g=f(theta);assert max(abs(v) for v in g)<mp.mpf('1e-55')
errors=[abs(start[i]/theta[i]-1) for i in range(2)];assert max(errors)<mp.mpf('1e-10')
legacy=next(line.split() for line in (p/'extended-before.log').read_text().splitlines() if line.startswith('-20 1 '))
legacy_ll=m.evaluate(mp.mpf(0),mp.mpf(float(legacy[4])),mp.mpf(float(legacy[5])))[0]
out=dict(precision=mp.mp.dps,reference_sigma=mp.nstr(theta[0],75),reference_shape=mp.nstr(theta[1],75),reference_logLik=mp.nstr(ll,75),free_score_max=mp.nstr(max(abs(v) for v in g),20),relative_errors=[mp.nstr(v,20) for v in errors],legacy_logLik_checked=mp.nstr(legacy_ll,75),improvement=mp.nstr(ll-legacy_ll,50),positive_information=True)
(p/'held-checks.json').write_text(json.dumps(out,indent=2)+'\n');print(json.dumps(out,indent=2))
