#!/usr/bin/env python3
"""Deterministic high-precision diagnostic grids, not optimization certificates."""
import mpmath as m
m.mp.dps = 40

def fmt(x): return m.nstr(x, 12)
def slash(x, mu, s):
    c=1/m.sqrt(2*m.pi)
    def f(v):
        t=(m.mpf(v)-mu)**2/(2*s*s)
        return c/s * (m.mpf('0.5') if not t else -m.expm1(-t)/(2*t))
    return m.fprod(f(v) for v in x)
def collapse(x, v):
    return (1/m.sqrt(2*m.pi))**len(x)*m.power(2,-len(x)/2)*m.fprod(abs(i-v)**-2 for i in x if i!=v)
print('slash')
for x,mu,s in [([0]*10+[-1]+[9]*9,4,3),([0]*10+[-1]+[10]*9,m.mpf(-1)/625,m.mpf(1)/5),([0]*10+[-1]+[10]*9,m.mpf(9)/2,m.mpf(7)/2)]:
    print('  L/B',fmt(slash(x,mu,s)/collapse(x,0)))
print('tlambda endpoint density ratio to lambda=1, support [-1,1]')
for e in ['0.1','0.01','0.001']:
    l=1+m.mpf(e); print(' ', e,fmt(2/l))
print('genbeta mixed law: P(Y > 1), limit exp(-1)/2')
for t in [10,100,1000]:
    # alpha=beta=1/t, tau=t: P(Y>y)=P(Beta<exp(-t*y))
    a=m.mpf(1)/t
    print(' ',t,fmt(m.betainc(a,a,0,m.exp(-t),regularized=True)))
print('  limit',fmt(m.exp(-1)/2))
print('bbinom likelihood ratio to binomial face, N=3')
def bb(x,p,s,N):
    return m.fprod(m.fprod(p+j*s for j in range(v))*m.fprod(1-p+j*s for j in range(N-v))/m.fprod(1+j*s for j in range(N)) for v in x)
for x in [[0,1,2,3],[0,0,3,3,1],[1,1,2,2]]:
    p=m.mpf(sum(x))/(len(x)*3); print(' ',x,fmt(bb(x,p,m.mpf('0.5'),3)/bb(x,p,0,3)))
print('dgamma: grid profiles, maximizing rate by score bisection')
def gp(k,a,r): return m.gammainc(a,r*k,r*(k+1),regularized=True)
def rate_score(x,a,r):
    ans=0
    for k in x:
        lo=r*k; hi=r*(k+1)
        boundary=hi**a*m.exp(-hi)-(lo**a*m.exp(-lo) if lo else 0)
        ans+=boundary/(m.gamma(a)*gp(k,a,r))
    return ans
for x in [[0,1,3],[0,0,1,4,6],[2,3,7,8]]:
    vals=[]
    for i in range(-8,9):
        a=m.exp(m.mpf(i)/3); lo=m.mpf('0.00001'); hi=m.mpf('1000')
        for _ in range(65):
            mid=m.sqrt(lo*hi)
            if rate_score(x,a,mid)>0: lo=mid
            else: hi=mid
        vals.append(sum(m.log(gp(k,a,mid)) for k in x))
    modes=[i-8 for i in range(1,16) if vals[i]>vals[i-1] and vals[i]>vals[i+1]]
    print(' ',x,'grid local peak indexes',modes,'edge slopes',fmt(vals[1]-vals[0]),fmt(vals[-1]-vals[-2]))
print('skellam: equal-rate profile for symmetric samples')
for x in [[-1,0,1],[-4,0,0,4],[-1,-1,1,1]]:
    vals=[]
    for i in range(-40,41):
        t=m.exp(m.mpf(i)/10)
        vals.append(sum(-2*t+m.log(m.besseli(abs(k),2*t)) for k in x))
    modes=[i-40 for i in range(1,80) if vals[i]>vals[i-1] and vals[i]>vals[i+1]]
    print(' ',x,'grid local peak indexes',modes)
