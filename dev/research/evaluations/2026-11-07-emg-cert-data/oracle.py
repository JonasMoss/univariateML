"""Independent 70-digit roots; execute after compiling location-oracle.cpp."""
import csv
import json
import sys
from pathlib import Path
import mpmath as m
m.mp.dps=70
root=Path(__file__).resolve().parent
mode="-double" if "--double" in sys.argv else ""
rows=[]
for row in csv.DictReader((root/f'location{mode}.csv').open()):
    s,l=m.mpf(row['sigma']),m.mpf(row['lambda'])
    def score(mu):
        ans=0
        for x in [-2,-1,0,1,2]:
            t=l*s-(x-mu)/s
            # erfc at arbitrary precision avoids the production Mills fraction.
            r=m.exp(-t*t/2)*m.sqrt(2/m.pi)/m.erfc(t/m.sqrt(2))
            ans+=l*s-r
        return ans
    lo,hi=m.mpf(row['lower']),m.mpf(row['upper'])
    assert row['ok']=='1' and score(lo)>0 and score(hi)<0, row
    for _ in range(130):
        mid=(lo+hi)/2
        if score(mid)>0:lo=mid
        else:hi=mid
    truth=(lo+hi)/2
    error=abs(m.mpf(row['mu'])-truth)/(max(abs(truth),s))
    assert error<=m.mpf('1e-10'),row
    rows.append(dict(sigma=str(s), **{'lambda':str(l)}, mu=str(truth), relative_error=str(error)))
(root/f'oracle{mode}.json').write_text(json.dumps(rows,indent=2)+'\n')
print(f'{len(rows)} independent location enclosures passed')
rows=[]
for row in csv.DictReader((root/f'fixed-shape{mode}.csv').open()):
    assert row['ok']=='1',row
    mask=int(row['mask']); k=m.mpf(row['k'])
    def objective(u,b):
        ans=5*m.log(b)
        for x in [-2,-1,0,1,2]:
            z=b*x-u
            ans+=m.log(k)+k*k/2-k*z+m.log(m.erfc((k-z)/m.sqrt(2))/2)
        return ans
    def gu(u,b):return m.diff(lambda v:objective(v,b),u)
    def gb(u,b):return m.diff(lambda v:objective(u,v),b)
    guessb=m.mpf(row['b']);guessu=m.mpf(row['mu'])*guessb
    if mask&1:
        u=m.mpf(0)
        b=m.mpf(1) if mask&6 else m.findroot(lambda b:gb(0,b),(guessb*.99,guessb*1.01))
    elif mask&6:
        b=m.mpf(1)
        u=m.findroot(lambda u:gu(u,1),(guessu-m.mpf('.01'),guessu+m.mpf('.01')))
    else:
        u,b=m.findroot(lambda u,b:(gu(u,b),gb(u,b)),(guessu,guessb))
    truth=objective(u,b)
    assert m.mpf(row['lower'])<=truth<=m.mpf(row['upper']),row
    assert m.mpf(row['b_lower'])<=b<=m.mpf(row['b_upper']),row
    assert m.mpf(row['u_lower'])<=u<=m.mpf(row['u_upper']),row
    rows.append(dict(mask=mask,k=str(k),mu=str(u/b),b=str(b),logLik=str(truth)))
(root/f'fixed-shape-oracle{mode}.json').write_text(json.dumps(rows,indent=2)+'\n')
print(f'{len(rows)} independent fixed-shape enclosures passed')
