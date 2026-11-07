"""Independent 80-digit stationary-root and tied-mode fixtures (no scipy)."""
import json
import mpmath as mp
mp.mp.dps=80
n,k,S=mp.mpf(100),mp.mpf(1),mp.mpf(10)
u,v=S/n,S/k
d=v-u
turns=[u,(u+v-mp.sqrt(d*(d-4)))/2,(u+v+mp.sqrt(d*(d-4)))/2,v]
def bisect(f,a,b):
    fa=f(a)
    for _ in range(300):
        c=(a+b)/2
        if (f(c)>0)==(fa>0):a=c
        else:b=c
    return (a+b)/2
def peaks(p):
    def h(t):
        if t==u:return -mp.inf
        if t==v:return mp.inf
        return -t+mp.log(n*t-S)-mp.log(S-k*t)-mp.log(p/(1-p))
    def ll(t):return (n-k)*mp.log(p+(1-p)*mp.exp(-t))+k*mp.log(1-p)-k*t+S*mp.log(t)-mp.loggamma(S+1)
    return [(r,ll(r)) for a,b in zip(turns,turns[1:]) if h(a)<0<h(b) for r in [bisect(h,a,b)]]
def gap(p):
    q=peaks(p)
    assert len(q)==2
    return q[-1][1]-q[0][1]
assert gap(mp.mpf('.1'))<0<gap(mp.mpf('.78'))
p=bisect(gap,mp.mpf('.1'),mp.mpf('.78'))
cases=[]
for pp in [mp.mpf('.001'),mp.mpf('.1'),mp.mpf('.9'),p]:
    cases.append(dict(pi=str(pp),peaks=[dict(rate=str(a),loglik=str(b)) for a,b in peaks(pp)]))
# A turning point is a stationary-score tangency at this inflation.
t=turns[1]
a=mp.exp(-t)*(n*t-S)/(S-k*t)
signs=[]
for pp in [mp.mpf('.7'),mp.mpf('.75')]:
    hh=lambda t:-t+mp.log(n*t-S)-mp.log(S-k*t)-mp.log(pp/(1-pp))
    h1,h2=hh(turns[1]),hh(turns[2])
    assert h1>mp.mpf('.1') and h2<mp.mpf('-.1')
    signs.append(dict(pi=str(pp),h_turn1=str(h1),h_turn2=str(h2),high_minus_low=str(gap(pp))))
assert gap(mp.mpf('.7'))<0<gap(mp.mpf('.75'))
output=dict(n=100,positive=[10],cases=cases,tangency_pi=str(a/(1+a)),turns=[str(t) for t in turns],tie_existence_signs=signs)
print(json.dumps(output,indent=2))
