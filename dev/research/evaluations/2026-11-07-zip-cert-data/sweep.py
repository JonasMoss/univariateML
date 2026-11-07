"""Deterministic held-inflation sweep against an independent mpmath score scan."""
import json,math,subprocess,sys
import mpmath as mp
mp.mp.dps=70
cases=[(z,k,c,p) for z,k in [(1,1),(9,1),(99,1),(3,7)]
       for c in [1,2,5,10,100,2147483647]
       for p in [1e-8,.001,.1,.5,.7,.75,.9,1-1e-8]]
rows=subprocess.check_output([sys.argv[1]],input=''.join('%d %d %d %.17g\n'%q for q in cases),text=True).splitlines()
passed=stops=0;worst_rate=worst_ll=0.;failures=[]
for case,row in zip(cases,rows):
    z,k,c,p=case;n=z+k;S=k*c
    status,rr,ll=row.split();status=int(status)
    if status!=0:
        assert status in [2,4,5]
        stops+=1
        continue
    p=mp.mpf(p);u=mp.mpf(S)/n;v=mp.mpf(S)/k
    def score(t):return S/t-k-z/(1+p/(1-p)*mp.exp(t))
    # Enumerate the analytically bounded branches; evaluate the original score
    # with independent 70-digit arithmetic rather than the C++ h evaluator.
    d=v-u;cuts=[u,v]
    if d>4:cuts[1:1]=[(u+v-mp.sqrt(d*(d-4)))/2,(u+v+mp.sqrt(d*(d-4)))/2]
    roots=[]
    for a,b in zip(cuts,cuts[1:]):
        if score(a)<=0 or score(b)>=0:continue
        for _ in range(250):
            mid=(a+b)/2
            if score(mid)>0:a=mid
            else:b=mid
        roots.append((a+b)/2)
    def objective(t):return z*mp.log(p+(1-p)*mp.exp(-t))+k*(mp.log(1-p)-t+c*mp.log(t)-mp.loggamma(c+1))
    best=max(roots,key=objective);want=objective(best)
    rate_error=float(abs(mp.mpf(rr)/best-1));ll_error=float(abs(mp.mpf(ll)-want))
    worst_rate=max(worst_rate,rate_error);worst_ll=max(worst_ll,ll_error)
    if rate_error>1e-9 or ll_error>1e-7*(1+abs(float(want))):failures.append(dict(case=case,rate_error=rate_error,ll_error=ll_error))
    passed+=1
out=dict(cases=len(cases),passed=passed,allowed_stops=stops,max_relative_rate_error=worst_rate,max_absolute_loglik_error=worst_ll,failures=failures)
print(json.dumps(out,indent=2));assert not failures
