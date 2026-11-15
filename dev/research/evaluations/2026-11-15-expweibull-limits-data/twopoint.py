"""Two distinct values: affine equivariance in log x reduces the free fit to one parameter, p = share at the lower value.
Data y in {0,1} (weights p, 1-p); l/n = p log g(0) + (1-p) log g(1) - (the x Jacobian constant)."""
import mpmath as mp, sys
import os; sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import core
mp.mp.dps = 40
def Pface(p): return -mp.log(p) - 1
def Iface(p):
    q = 1-p
    f = lambda k: mp.log(k) - k*q - mp.log(p + q*mp.e**(-k)) - 1
    lo,hi=mp.mpf('1e-6'),mp.mpf(1000)
    d = lambda k: mp.diff(f,k)
    for _ in range(200):
        mid=(lo+hi)/2
        if d(mid)>0: lo=mid
        else: hi=mid
    k=(lo+hi)/2
    return f(k), k
def A_of(p):
    q=1-p; v,k=Iface(p)
    S=p+q*mp.e**(-k); ls=-mp.log(S)/k   # zeta=1/S=e^{ks}
    u0,u1=-ls,1-ls
    return k*(p*u0+q*u1)+(k**2/2)*(p*u0**2*(mp.e**(-k*u0)-1)+q*u1**2*(mp.e**(-k*u1)-1)), k, ls
def lpb(p, b, mu):
    t0=mp.e**(b*(0-mu)); t1=mp.e**(b*(1-mu))
    H=p*mp.log(-mp.expm1(-t0))+(1-p)*mp.log(-mp.expm1(-t1))
    c=-1/H
    return (mp.log(b)+mp.log(c)) + p*(mp.log(t0)-t0)+(1-p)*(mp.log(t1)-t1)+(c-1)*H
def finite_sup(p, bs):
    """grid in b, golden in mu around coarse scan; returns best (value, b, mu)."""
    best=(-mp.inf,None,None)
    for b in bs:
        mus=[-3-8/b+ (4+8/b)*i/300 for i in range(301)] if b<30 else [1-8/b+(16/b)*i/300 for i in range(301)]
        mus=[m for m in mus]
        vals=[]
        for m in mus:
            try: vals.append(lpb(p,b,m))
            except Exception: vals.append(-mp.inf)
        i=max(range(len(mus)),key=lambda j:vals[j])
        a,bb=mus[max(i-1,0)],mus[min(i+1,len(mus)-1)]
        g=(mp.sqrt(5)-1)/2
        for _ in range(60):
            c1=bb-g*(bb-a);c2=a+g*(bb-a)
            try: f1=lpb(p,b,c1)
            except Exception: f1=-mp.inf
            try: f2=lpb(p,b,c2)
            except Exception: f2=-mp.inf
            if f1>f2: bb=c2
            else: a=c1
        m=(a+bb)/2
        v=lpb(p,b,m)
        if v>best[0]: best=(v,b,m)
    return best
if __name__=='__main__':
    for p in [mp.mpf(v)/20 for v in range(1,20)]:
        P=Pface(p); I,k=Iface(p); A,k,ls=A_of(p)
        bs=[mp.e**(mp.mpf(i)/4) for i in range(-3,25)]   # b in (0.47, 400) ~ moderate: dps 40 suffices
        fv,fb,fm=finite_sup(p,bs)
        print('p=%.2f P=%s I=%s(k=%s) A=%s  finite grid best=%s at b=%s'%(float(p),mp.nstr(P,8),mp.nstr(I,8),mp.nstr(k,5),mp.nstr(A,6),mp.nstr(fv,8),mp.nstr(fb,5)),flush=True)
