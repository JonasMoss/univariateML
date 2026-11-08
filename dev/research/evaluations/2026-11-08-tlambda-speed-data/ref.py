import mpmath as mp
mp.mp.dps=40
x=[mp.mpf(l) for l in open('golden_data.txt').read().split()]
def logg(z,l):
    # standardized log density via inversion in t=-log p (p smaller tail)
    az=abs(z)
    if az==0: return (l-2)*mp.log(2)
    f=lambda t: ((1-mp.e**(-t))**l-mp.e**(-l*t))/l-az
    t=mp.findroot(f,(mp.log(2)+mp.mpf(10)**-30, mp.mpf(60)),solver='anderson')
    p=mp.e**(-t); u=1-p
    return -mp.log(u**(l-1)+p**(l-1))
def ll(th):
    mu,ls,l=th; s=mp.e**ls
    return sum(logg((xi-mu)/s,l) for xi in x)-len(x)*ls
rows=[r.split() for r in open('golden_fit.txt').read().splitlines()]
legacy=[mp.mpf(v) for v in rows[0][1:4]]; proto=[mp.mpf(v) for v in rows[1][1:4]]
th=[proto[0],mp.log(proto[1]),proto[2]]
for it in range(3):
    h=mp.mpf(10)**-12
    g=[];H=[[0]*3 for _ in range(3)]
    f0=ll(th)
    E=lambda i,d:[th[k]+(d if k==i else 0) for k in range(3)]
    fp=[ll(E(i,h)) for i in range(3)]; fm=[ll(E(i,-h)) for i in range(3)]
    g=[(fp[i]-fm[i])/(2*h) for i in range(3)]
    for i in range(3):
        H[i][i]=(fp[i]-2*f0+fm[i])/h**2
        for j in range(i+1,3):
            pp=ll([th[k]+(h if k in(i,j) else 0) for k in range(3)]); mm=ll([th[k]-(h if k in(i,j) else 0) for k in range(3)])
            H[i][j]=H[j][i]=(pp-fp[i]-fp[j]+2*f0-fm[i]-fm[j]+mm)/(2*h**2)
    step=mp.lu_solve(mp.matrix(H),-mp.matrix(g))
    th=[th[k]+step[k] for k in range(3)]
    print('iter',it,'|grad|=',mp.nstr(max(abs(v) for v in g),5),'step',[mp.nstr(s,5) for s in step])
ref=[th[0],mp.e**th[1],th[2]]
print('reference', [mp.nstr(v,20) for v in ref], 'll', mp.nstr(ll(th),20))
for name,est in (('legacy',legacy),('proto',proto)):
    print(name,'rel err', [mp.nstr(abs(est[k]-ref[k])/abs(ref[k]),3) for k in range(3)])
