"""tie3: inverse Weibull face value I and profiled finite likelihood for b -> 0 at high precision."""
import mpmath as mp
x = [mp.mpf('2.698972628080099')]*2 + [mp.mpf('3.487363739010494')]
n=3
def setdps(T): mp.mp.dps = int(80 + 0.5*T)
# inverse Weibull face: ell_IW(k,s)= sum[log k + k log s -(k+1) log x - (x/s)^-k]
mp.mp.dps=60
ly=[mp.log(v) for v in x]
def Iface(k):
    # profile s: s^k = sum x^-k / n  -> (x/s)^-k = s^k x^-k ; sum = n
    S=sum(mp.e**(-k*l) for l in ly)
    ls=mp.log(n/S)/k
    return n*mp.log(k)+n*k*ls-(k+1)*sum(ly)-n, mp.e**ls
dI=lambda k: mp.diff(lambda kk: Iface(kk)[0], k)
lo,hi=mp.mpf(1),mp.mpf(60)
for _ in range(200):
    mid=(lo+hi)/2
    if dI(mid)>0: lo=mid
    else: hi=mid
kh=(lo+hi)/2
I,s=Iface(kh)
print('I face', mp.nstr(I,25),'k',mp.nstr(kh,15),'s',mp.nstr(s,15))
def lp_mu(mu,b):
    t=[mp.exp(b*(l-mu)) for l in ly]
    H=sum(mp.log(-mp.expm1(-ti)) for ti in t)
    c=-n/H
    ll=n*(mp.log(b)+mp.log(c))-sum(ly)+sum(mp.log(ti)-ti for ti in t)+(c-1)*H
    return ll,c
def score(mu,b):
    t=[mp.exp(b*(l-mu)) for l in ly]
    H=sum(mp.log(-mp.expm1(-ti)) for ti in t); c=-n/H
    return sum(1-ti+(c-1)*ti/mp.expm1(ti) for ti in t)
out=[]
for b in ['0.5','0.2','0.1','0.05','0.02','0.01','0.005','0.002']:
    b=mp.mpf(b); T0=kh/b; setdps(T0)
    b=mp.mpf(b); ly=[mp.log(v) for v in x]
    mu0=mp.log(s)-mp.log(T0)/b
    mu=mp.findroot(lambda m: score(m,b), mu0, tol=mp.mpf(10)**(-mp.mp.dps//3), verify=False)
    ll,c=lp_mu(mu,b)
    row=(mp.nstr(b,4),mp.nstr(mp.log(c),8),mp.nstr(ll,15),mp.nstr(I-ll,6),mp.nstr((I-ll)/b,6))
    print('b=%s T=%s lp=%s I-lp=%s (I-lp)/b=%s'%row,flush=True)
