"""Compare valid dispersion bounds from all observed endpoint pairs."""
import json, math
with open('univariateML-cpp/include/cppunivariate/distributions/cmb/golden.json') as f:
    x = json.load(f)['data']
n=len(x)
counts=[x.count(i) for i in range(max(x)+1)]
s=sum(c*math.log(c/n) for c in counts if c)
s+=1e-9*(1+abs(s))
def bound(c,B,lower):
    if not 0<c<n: return -math.inf if lower else 0.
    f=c/n
    lo,hi=(0.,f) if lower else (f,1.)
    for _ in range(100):
        mid=(lo+hi)/2
        if mid<=0 or mid>=1: break
        value=s+c*math.log(mid/f)+(n-c)*(math.log1p(-mid)-math.log1p(-f))
        if lower:
            if value>=B: hi=mid
            else: lo=mid
        else:
            if value>=B: lo=mid
            else: hi=mid
    v=math.log(lo if lower else hi)
    return v-1e-10*(1+abs(v)) if lower else min(0.,v+1e-10*(1+abs(v)))
for candidate in (-797.889642360016,-798.130638928198):
    B=candidate-1e-8*(1+abs(candidate))
    low=[bound(c,B,True) for c in counts]
    up=[bound(c,B,False) for c in counts]
    choices=[]
    for u in range(len(counts)):
        for v in range(u+2,len(counts)):
            if not counts[u] or not counts[v]: continue
            for j in range(u+1,v):
                w=(v-j)/(v-u)
                a=w*math.lgamma(u+1); b=(1-w)*math.lgamma(v+1); c=math.lgamma(j+1)
                delta=a+b-c-1e-10*(1+abs(a)+abs(b)+abs(c))
                numerator=min(up[j], math.log(min(1.,1-math.exp(low[u])-math.exp(low[v])+1e-9)))-w*low[u]-(1-w)*low[v]
                if delta>0: choices.append((max(0.,numerator/delta),u,j,v))
    print('candidate',candidate,'best',min(choices),'current',next(q for q in choices if q[1:]==(0,1,7)))
