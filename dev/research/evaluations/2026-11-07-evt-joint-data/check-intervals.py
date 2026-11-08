#!/usr/bin/env python3
import json, pathlib, mpmath as mp
mp.mp.dps=60
root=pathlib.Path(__file__).parent
# Use the exact doubles presented to the C++ certificate.
z=[mp.mpf((v-.1)/1.3) for v in [-.7,-.2,0,.4,.8,1.5,3]]
def ll(k):
    ans=mp.mpf(0)
    for v in z:
        L=mp.log1p(k*v)/k if k else v
        ans-=(1+k)*L+mp.exp(-L)
    return ans
checks=0
for row in (root/'interval-bounds.txt').read_text().splitlines():
    lo,hi,*bounds=map(mp.mpf,row.split())
    for k in [lo,(lo+hi)/2,hi]:
        for d in range(3):
            val=mp.diff(ll,k,d)
            assert bounds[2*d]-mp.mpf('1e-10')<=val<=bounds[2*d+1]+mp.mpf('1e-10'),(row,k,d,val)
            checks+=1
print(json.dumps(dict(precision=60,checks=checks,absolute_comparison_margin=1e-10),indent=2))
