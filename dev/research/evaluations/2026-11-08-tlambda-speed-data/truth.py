import mpmath as mp
mp.mp.dps=60
rows=[list(map(mp.mpf,l.split())) for l in open('dump.txt')]
worst={}
for l,z,cur,new in rows:
    lf=float(l)
    f=lambda s: ((1-mp.e**s)**l-mp.e**(l*s))/l-z      # s=log p
    s=mp.findroot(f,(-mp.log(2)-mp.mpf(10)**-40, mp.mpf(-200)),solver='anderson')
    p=mp.e**s; q=1-p
    ref=-mp.log(p**(l-1)+q**(l-1))
    ec=abs(cur-ref)/abs(ref); en=abs(new-ref)/abs(ref)
    w=worst.setdefault(lf,[0,0,0])
    w[0]=max(w[0],ec); w[1]=max(w[1],en); w[2]+=1
for lf,(ec,en,c) in sorted(worst.items()):
    print(f"lambda={lf:5.2f} points={c:3d}  max rel err: dtlambda {float(ec):.2e}   table+Halley {float(en):.2e}")
