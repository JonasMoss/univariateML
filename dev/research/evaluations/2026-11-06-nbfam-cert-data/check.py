"""Independent 60-digit root/likelihood and interval-bound checks; seed 1165.
Run with the C++ probe path and output JSON path. No fixture regeneration.
"""
import collections,json,math,subprocess,sys
from pathlib import Path
import mpmath as mp
import numpy as np
mp.mp.dps=60
rng=np.random.default_rng(1165)
root=Path(__file__).resolve().parents[4]
cases=[]
for family in ['ztnbinom','zmnbinom','zinb']:
    g=json.loads((root/f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())
    x=[v for v in g['data'] if v>0]
    for r,p in [(-1,-1),(2.5,-1),(-1,g['estimates']['prob'])]:
        cases.append((family,x,r,p))
for r in [.2,1,4,50]:
    for i in range(12):
        x=[]
        while len(x)<30:
            x.extend(int(v) for v in rng.negative_binomial(r,.4,100) if v>0)
        cases.append((f'random-{r}-{i}',x[:30],-1,-1))
for i in range(12):
    x=[]
    while len(x)<30:x.extend(int(v) for v in rng.poisson(2,100) if v>0)
    cases.append((f'poisson-{i}',x[:30],-1,-1))
for x in [[1,2,2,3,3,3],[1,1,1,1,1,2,3,10,30],[1,2,5],[1,1],
          [1]*3958+[2]*4063+[4]*1979]:
    for r,p in [(-1,-1),(2.5,-1),(-1,.4),(-1,.001)]:cases.append(('edge',x,r,p))
request=''.join(f'{len(x)} {r} {p} 200 '+' '.join(map(str,x))+'\n' for _,x,r,p in cases)
out=subprocess.run([sys.argv[1]],input=request,text=True,capture_output=True,check=True)
lines=iter(out.stdout.splitlines());results=[];bounds_checked=0;max_errors=[0.,0.,0.]
def theta(r,m):
    lo=mp.mpf(0);hi=2*mp.log1p(m) if r==0 else mp.log1p(m/r)
    for _ in range(190):
        t=(lo+hi)/2
        value=mp.expm1(t)/t if r==0 else r*mp.expm1(t)/(-mp.expm1(-r*t))
        if value<m:lo=t
        else:hi=t
    return (lo+hi)/2
for number,(label,x,held_r,held_p) in enumerate(cases):
    fields=next(lines).split();status=int(fields[0]);r,p,ll=map(float,fields[1:4]);work=int(fields[4])
    old_status=int(fields[5]);old_r,old_p,old_ll,old_time,new_time=map(float,fields[6:11])
    counts=collections.Counter(x);n=len(x);m=mp.mpf(sum(x))/n
    item=dict(label=label,n=n,fixed_size=held_r,fixed_prob=held_p,status=status,work=work,
              old_status=old_status,old_size=old_r,old_prob=old_p,old_loglik=old_ll,
              old_seconds=old_time,new_seconds=new_time)
    if status==0:
        rr=mp.mpf(r)
        if m==1 and held_p<0:oracle_r=rr;oracle_p=mp.mpf(1)
        elif held_p>0:
            t=-mp.log(mp.mpf(held_p));s0=sum(mp.harmonic(v-1)*c for v,c in counts.items())/n-t/2
            if s0<=0:oracle_r=mp.mpf(0)
            else:
                def score(q):return sum(c*(mp.digamma(q+v)-mp.digamma(q)) for v,c in counts.items())/n-t/(-mp.expm1(-q*t))
                a=mp.mpf(0);b=mp.mpf(max(r,1))*2
                while score(b)>0:b*=2
                for _ in range(190):
                    mid=(a+b)/2
                    if score(mid)>0:a=mid
                    else:b=mid
                oracle_r=(a+b)/2
            oracle_p=mp.mpf(held_p)
        elif held_r>=0 or r==0:
            oracle_r=mp.mpf(held_r if held_r>=0 else 0);oracle_p=mp.exp(-theta(oracle_r,m))
            if r==0 and held_r<0:
                s0=sum(mp.harmonic(v-1)*c for v,c in counts.items())/n+mp.log(oracle_p)/2
                assert s0<0,(label,'wrong logarithmic endpoint',str(s0))
        else:
            def equations(q,t):
                return (sum(c*(mp.digamma(q+v)-mp.digamma(q)) for v,c in counts.items())/n-t/(-mp.expm1(-q*t)),
                        q*mp.expm1(t)/(-mp.expm1(-q*t))-m)
            oracle_r,t=mp.findroot(equations,(rr,-mp.log(p)),tol=mp.mpf('1e-45'))
            oracle_p=mp.exp(-t)
        if oracle_p==1:oracle_ll=mp.mpf(0)
        elif oracle_r==0:
            oracle_ll=sum(c*(v*mp.log1p(-oracle_p)-mp.log(v)-mp.log(-mp.log(oracle_p))) for v,c in counts.items())
        else:
            oracle_ll=sum(c*(mp.loggamma(oracle_r+v)-mp.loggamma(oracle_r)-mp.loggamma(v+1)+oracle_r*mp.log(oracle_p)+v*mp.log1p(-oracle_p)-mp.log1p(-oracle_p**oracle_r)) for v,c in counts.items())
        errors=[float(abs(rr-oracle_r)/max(abs(oracle_r),mp.mpf('1e-100'))),float(abs(mp.mpf(p)/oracle_p-1)),float(abs(mp.mpf(ll)-oracle_ll)/n)]
        assert errors[0]<2e-6 and errors[1]<2e-6 and errors[2]<1e-7,(label,errors)
        max_errors=[max(a,b) for a,b in zip(max_errors,errors)]
        item.update(size=r,prob=p,loglik=ll,oracle_size=str(oracle_r),oracle_prob=str(oracle_p),errors=errors)
        if label in ['ztnbinom','zmnbinom'] and held_r<0 and held_p<0:
            assert r==old_r and p==old_p and ll==old_ll,(label,'golden proposer changed')
    elif status==3:
        lam=theta(mp.mpf('1e60'),m)*mp.mpf('1e60')
        k=sum(mp.mpf(v*(v-1))*c/2 for v,c in counts.items())/n
        assert m*lam/2-k>0,(label,'wrong Poisson tail sign')
    elif status!=4:raise AssertionError((label,status))
    for _ in range(6):
        b=next(lines).split();assert b[0]=='B';a,z,fl,fh,dl,dh=map(float,b[1:])
        if number>=12 or m==1:continue
        q=(mp.mpf(a)+mp.mpf(z))/2;t=theta(q,m)
        ee=lambda u:1/mp.expm1(u)-1/u
        j=1-t/mp.expm1(t)
        jp=(mp.exp(t)*(t-1)+1)/mp.expm1(t)**2
        h=sum(c*sum(mp.mpf(k)/(q+k) for k in range(1,v)) for v,c in counts.items())/n
        h2=sum(c*sum(mp.mpf(k)/(q+k)**2 for k in range(1,v)) for v,c in counts.items())/n
        f=m*j-h;der=m*jp*t*ee(q*t)/(1+ee(t)-q*ee(q*t))+h2
        assert fl<=f<=fh,(label,a,z,'score bound',fl,str(f),fh)
        assert dl<=der<=dh,(label,a,z,'derivative bound',dl,str(der),dh)
        bounds_checked+=1
    results.append(item)
# An explicit held-inflation inner profile with two probability maxima.
r=mp.mpf(10);pi=mp.mpf('.7');nz=99;nk=1;s=10;n=nz+nk
E=lambda p:pi/(1-pi)*(nk*r-(nk*r+s)*p)+p**r*(n*r-(n*r+s)*p)
roots=[]
a=mp.mpf('.5')
for i in range(1,2001):
    b=mp.mpf('.5')+mp.mpf(i)*mp.mpf('.49')/2000
    if E(a)*E(b)<0:
        lo,hi=a,b
        for _ in range(190):
            mid=(lo+hi)/2
            if E(lo)*E(mid)>0:lo=mid
            else:hi=mid
        roots.append((lo+hi)/2)
    a=b
profile=[]
for p in roots:
    value=nz*mp.log(pi+(1-pi)*p**r)+mp.log1p(-pi)+mp.loggamma(r+10)-mp.loggamma(r)-mp.loggamma(11)+r*mp.log(p)+10*mp.log1p(-p)
    profile.append(dict(prob=str(p),loglik=str(value),kind='maximum' if mp.diff(E,p)<0 else 'minimum'))
assert [v['kind'] for v in profile]==['maximum','minimum','maximum'],profile
# Retry genuine exhausted budgets with ten times the outer evaluation budget.
failed=[(case,result) for case,result in zip(cases,results) if result['status']==4]
retry_request=''.join(f'{len(x)} {r} {p} 2000 '+' '.join(map(str,x))+'\n' for (label,x,r,p),_ in failed)
retry_lines=iter(subprocess.run([sys.argv[1]],input=retry_request,text=True,capture_output=True,check=True).stdout.splitlines())
retries=[]
for (label,x,held_r,held_p),previous in failed:
    fields=next(retry_lines).split();status=int(fields[0]);r,p,ll=map(float,fields[1:4]);work=int(fields[4])
    item=dict(label=label,n=len(x),maxit=2000,status=status,work=work,new_seconds=float(fields[10]))
    for _ in range(6):next(retry_lines)
    if status==0:
        counts=collections.Counter(x);n=len(x);m=mp.mpf(sum(x))/n
        def equations(q,t):
            return (sum(c*(mp.digamma(q+v)-mp.digamma(q)) for v,c in counts.items())/n-t/(-mp.expm1(-q*t)),
                    q*mp.expm1(t)/(-mp.expm1(-q*t))-m)
        oracle_r,t=mp.findroot(equations,(mp.mpf(r),-mp.log(p)),tol=mp.mpf('1e-45'))
        oracle_p=mp.exp(-t)
        errors=[float(abs(mp.mpf(r)/oracle_r-1)),float(abs(mp.mpf(p)/oracle_p-1))]
        assert max(errors)<2e-6,(label,'retry',errors)
        item.update(size=r,prob=p,errors=errors)
    else:assert status in [3,4],(label,status)
    retries.append(item)
payload=dict(seed=1165,precision=mp.mp.dps,numpy=np.__version__,mpmath=mp.__version__,cases=len(results),
             max_errors=max_errors,interval_checks=bounds_checked,results=results,retries=retries,zinb_held_pi_multimodality=profile)
Path(sys.argv[2]).write_text(json.dumps(payload,indent=2,allow_nan=True)+'\n')
print(json.dumps({k:v for k,v in payload.items() if k not in ['results']},indent=2))
