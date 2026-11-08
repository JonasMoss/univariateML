# Central-series radius (ratio and root tests, 61 terms) and the [2,8/3] log-concavity threshold scan.
import mpmath as mp
mp.mp.dps = 60
def Q(u,l): return mp.log(u/(1-u)) if l==0 else (u**l-(1-u)**l)/l
def D(u,l): return u**(l-1)+(1-u)**(l-1)
def mom(k,l): return sum(mp.binomial(k,j)*(-1)**j*mp.beta(l*(k-j)+1,l*j+1) for j in range(k+1))/l**k
print('kurt(1)=',mp.nstr(mom(4,mp.mpf(1))/mom(2,mp.mpf(1))**2,10))
def Rpred(l):
    a=-l; ph=mp.pi/(2*(1+a)); return (2/a)*(2*mp.cos(ph))**a*mp.sin(a*ph)
N=61
def mul(a,b):
    c=[mp.mpf(0)]*(N+1)
    for i,ai in enumerate(a):
        if ai==0: continue
        for j in range(N+1-i): c[i+j]+=ai*b[j]
    return c
def revert(xs):
    v=[mp.mpf(0)]*(N+1); v[1]=1/xs[1]
    for it in range(N+2):
        comp=[mp.mpf(0)]*(N+1); p=[mp.mpf(1)]+[mp.mpf(0)]*N
        for k in range(1,N+1):
            p=mul(p,v)
            for i in range(N+1): comp[i]+=xs[k]*p[i]
        comp[1]-=1
        v=[v[i]-comp[i]/xs[1] for i in range(N+1)]
    return v
for l in ['-1','-0.5','-2','-0.1','-0.01']:
    l=mp.mpf(l)
    xs=mp.taylor(lambda v: Q(mp.mpf(1)/2+v,l), 0, N)
    v=revert(xs)
    est=[abs(v[k])**(-mp.mpf(1)/k) for k in (N-4,N-2,N)]
    # ratio test on odd coefficients: |v_k/v_{k+2}|^(1/2)
    rat=[mp.sqrt(abs(v[k]/v[k+2])) for k in (N-6,N-4,N-2)]
    print('l=',l,'pred',mp.nstr(Rpred(l),7),'root',[mp.nstr(e,5) for e in est],'ratio',[mp.nstr(e,6) for e in rat])
def concave(l,M=600):
    g=lambda u: 1/D(u,l)
    return max(mp.diff(g,u,2) for u in mp.linspace(mp.mpf('1e-5'),mp.mpf('0.5'),M))<=0
lo,hi=mp.mpf('2.6'),mp.mpf('2.9')
for _ in range(25):
    m=(lo+hi)/2
    if concave(m,300): lo=m
    else: hi=m
print('upper log-concave threshold ~', mp.nstr(lo,8), mp.nstr(hi,8))
