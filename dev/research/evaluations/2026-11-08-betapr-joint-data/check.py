"""Independent 80-digit inequality checks; numerical evidence, not the proof.
Run from the worktree root after producing probe.txt with probe.cpp.
"""
import json
import pathlib
import random
import mpmath as mp
mp.mp.dps = 80
root = pathlib.Path(__file__).resolve().parents[4]
data = pathlib.Path(__file__).resolve().parent
small = [.03,.1,.4,.7,1,2,4,12]
golden = json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/betapr/golden.json').read_text())['data']
rows = [line.split() for line in (data/'probe.txt').read_text().splitlines()]
results = []
rng = random.Random(11646)
for values, row in zip([small,golden,[1,2,3,4]], rows):
    x = list(map(mp.mpf,values)); logs = list(map(mp.log,x)); n = len(x)
    L,lo,H,G,IG,Q,P = map(mp.mpf,row[3:10])
    sorted_logs=sorted(logs)
    J=sum((sorted_logs[-1-i]-sorted_logs[i])**2/(4+2*(sorted_logs[-1-i]-sorted_logs[i])) for i in range(n//2))
    lm=sum(logs)/n
    def ll(p,q,ls):
        lb=mp.loggamma(p)+mp.loggamma(q)-mp.loggamma(p+q)
        return -n*lb-sum(p*mp.log1p(mp.exp(ls-y))+q*mp.log1p(mp.exp(y-ls))+y for y in logs)
    # Include very unequal shapes and a large joint-shape/scale path.
    max_violation = mp.mpf('-inf')
    for j in range(80):
        p=mp.exp(rng.uniform(-7,12)); q=mp.exp(rng.uniform(-7,12)); ls=mp.mpf(rng.uniform(-30,30))
        h=p*q/(p+q); likelihood=ll(p,q,ls)
        small_bound=n*mp.log(h)-sum(logs)
        large_bound=n/2*mp.log(h/(2*mp.pi))+n/(48*h)-h*J-sum(logs)
        max_violation=max(max_violation,likelihood-small_bound,likelihood-large_bound)
        assert likelihood <= small_bound and likelihood <= large_bound
    high_bound=n/2*mp.log(H/(2*mp.pi))+n/(48*H)-H*J-sum(logs)
    assert high_bound<L
    tail_checks=0
    for side,cutoff,upper in [(0,Q,G),(1,P,IG)]:
        ys=[-y if side else y for y in logs]
        mean=sum(mp.exp(y) for y in ys)/n
        for p in [lo,mp.sqrt(lo*2*H),2*H]:
            # Independently evaluate the profiled gamma at this shape.
            f=n*(p*mp.log(p)-mp.loggamma(p)-p-p*mp.log(mean))+(p-1)*sum(ys)
            if side:f-=2*sum(logs)
            assert f<=upper
            for q in ([cutoff,2*cutoff] if mp.isfinite(cutoff) else []):
                left=mp.log(q/p)+min(ys); right=mp.log(q/p)+max(ys)
                for _ in range(150):
                    mid=(left+right)/2
                    score=(p+q)*sum(1/(1+mp.exp(mid-y)) for y in ys)/n-p
                    if score>0:left=mid
                    else:right=mid
                ls=(left+right)/2
                likelihood=ll(q,p,-ls) if side else ll(p,q,ls)
                assert likelihood<L
                V=sum((mp.exp(y)/mean)**2 for y in ys)/n
                bound=upper+n*(2*H)**2*(1+2*V)/q
                assert likelihood<=bound<L
                tail_checks+=1
    signed_checks=0
    for side,cutoff in enumerate(map(mp.mpf,row[11:13])):
        if not mp.isfinite(cutoff):continue
        ys=[-y if side else y for y in logs]
        lm_side=sum(ys)/n; la=mp.log(sum(mp.exp(y) for y in ys)/n)
        left=lo;right=2*H
        for _ in range(200):
            mid=(left+right)/2
            if mp.log(mid)-mp.digamma(mid)+lm_side-la>0:left=mid
            else:right=mid
        a=(left+right)/2
        gamma_sup=n*(a*mp.log(a)-mp.loggamma(a)-a+a*(lm_side-la)-lm_side)
        if side:gamma_sup-=2*sum(logs)
        for j in range(13):
            p=lo*(2*H/lo)**(mp.mpf(j)/12)
            q=cutoff
            left=mp.log(q/p)+min(ys);right=mp.log(q/p)+max(ys)
            for _ in range(150):
                mid=(left+right)/2
                score=(p+q)*sum(1/(1+mp.exp(mid-y)) for y in ys)/n-p
                if score>0:left=mid
                else:right=mid
            ls=(left+right)/2
            likelihood=ll(q,p,-ls) if side else ll(p,q,ls)
            assert likelihood<gamma_sup
            signed_checks+=1
    results.append({'n':n,'signed_profiled_tail_checks':signed_checks,'random_inequality_checks':80,'profiled_tail_checks':tail_checks,
                    'largest_loglik_minus_bound':str(max_violation),
                    'exact_large_h_upper':str(high_bound),'witness_lower':str(L)})
(data/'independent-check.json').write_text(json.dumps({'mpmath_dps':80,'results':results},indent=2)+'\n')
print(json.dumps(results,indent=2))
