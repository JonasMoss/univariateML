#!/usr/bin/env python3
# Deterministic exploratory search; any retained fixture is checked separately.
import math,random
r=random.Random(11633)
grid=[1]+[1+math.exp(-5+j*.1) for j in range(131)]
constants=[]
for p in grid:
    lb=(math.lgamma(1/p)-math.lgamma(3/p))/2
    constants.append((p,lb,-math.log(2)-lb-math.lgamma(1+1/p)))
for it in range(20000):
    a=math.exp(r.uniform(-5,0.5));b=r.uniform(1.,2.1);k=r.randrange(1,51);l=r.randrange(1,51);z=r.randrange(0,101);n=k+l+z
    values=[]
    for p,lb,c in constants:
        e1=p*(math.log(a)-lb);e2=p*(math.log(b)-lb)
        values.append(c-(k*math.exp(min(700,e1))+l*math.exp(min(700,e2)))/n)
    peaks=[j for j in range(1,len(grid)-1) if values[j]>values[j-1] and values[j]>values[j+1]]
    if len(peaks)>=2:
        peaks=sorted(peaks,key=lambda j:values[j],reverse=True)
        if abs(math.log(grid[peaks[0]]/grid[peaks[1]]))>.2 and max(grid[j] for j in peaks[:2])<50:
            print(a,b,k,l,z,[(grid[j],values[j]) for j in peaks[:2]],flush=True);break
else: print('no separated interior maxima found')
