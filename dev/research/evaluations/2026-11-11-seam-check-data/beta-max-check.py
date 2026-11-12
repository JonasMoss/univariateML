"""Independent exact-input maximum and representation check (no C++ calls)."""
import gzip,json
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent

def rows(name):
    p=HERE/name
    text=p.read_text() if p.exists() else gzip.decompress(p.with_suffix(p.suffix+'.gz').read_bytes()).decode()
    return [json.loads(l) for l in text.splitlines()]
before=next(r for r in rows('before.jsonl') if r['family']=='beta' and r['index']==100)
after=next(r for r in rows('after-current-beta.jsonl') if r['family']=='beta' and r['index']==100)
checks=[]
for digits in [180,240]:
    mp.mp.dps=digits
    x=list(map(mp.mpf,before['data']));n=len(x)
    s=mp.fsum(mp.log(v) for v in x);q=mp.fsum(mp.log1p(-v) for v in x)
    def score(a,b):return mp.matrix([mp.digamma(a)-mp.digamma(a+b)-s/n,mp.digamma(b)-mp.digamma(a+b)-q/n])
    def jac(a,b):
        t=mp.polygamma(1,a+b)
        return mp.matrix([[mp.polygamma(1,a)-t,-t],[-t,mp.polygamma(1,b)-t]])
    a,b=mp.findroot(score,tuple(map(mp.mpf,before['parameters'])),J=jac,tol=mp.mpf(10)**(-digits+40),maxsteps=20)
    def ll(a,b):return (a-1)*s+(b-1)*q-n*(mp.loggamma(a)+mp.loggamma(b)-mp.loggamma(a+b))
    maximum=ll(a,b);old=ll(*map(mp.mpf,before['parameters']));new=ll(*map(mp.mpf,after['parameters']))
    step=mp.lu_solve(jac(a,b),score(a,b))
    checks.append(dict(digits=digits,alpha=mp.nstr(a,110),beta=mp.nstr(b,110),maximum=mp.nstr(maximum,110),before_point_value=mp.nstr(old,80),after_point_value=mp.nstr(new,80),before_point_relative_loss=float((maximum-old)/abs(maximum)),after_point_relative_loss=float((maximum-new)/abs(maximum)),after_reported_relative_error=float(abs(mp.mpf(after['reported'])-maximum)/abs(maximum)),score_norm=mp.nstr(mp.norm(score(a,b)),20),relative_newton_step=mp.nstr(max(abs(step[0]/a),abs(step[1]/b)),20),information_determinant=mp.nstr(mp.det(jac(a,b)),20)))
mp.mp.dps=240
assert abs(mp.mpf(checks[0]['maximum'])-mp.mpf(checks[1]['maximum']))<mp.mpf('1e-100')
assert checks[1]['after_point_relative_loss']<1e-10
assert checks[1]['after_reported_relative_error']<1e-10
(HERE/'beta-maximized-value-final.json').write_text(json.dumps(dict(data=before['data'],before_parameters=before['parameters'],after_parameters=after['parameters'],checks=checks),indent=2)+'\n')
print('180/240-digit beta maximum agrees; final point and reported value meet 1e-10:',checks[-1]['after_point_relative_loss'])
