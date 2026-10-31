import math, itertools
def ll(cnt,N,p,s):
    t=0
    for xi,c in cnt.items():
        if c: t+=c*(sum(math.log(p+j*s) for j in range(xi))+sum(math.log(1-p+j*s) for j in range(N-xi)))
    n=sum(cnt.values())
    return t-n*sum(math.log(1+j*s) for j in range(N))
tried=found=0
for N in range(3,11):
  for m in range(1,N):
    for a,b,c in itertools.product(range(0,9),range(1,9),range(0,9)):
      cnt={0:a,m:b,N:c}; n=a+b+c; T=m*b+N*c
      if T==0 or T==n*N: continue
      p0=T/(n*N); mean=T/n; v=(a*mean**2+b*(m-mean)**2+c*(N-mean)**2)/n
      if v>=N*p0*(1-p0)-1e-12: continue
      tried+=1; B=ll(cnt,N,p0,0); best=-1e300
      for i in range(1,30):
        p=i/30
        for k in range(-24,24):
          best=max(best,ll(cnt,N,p,math.exp(k/3))-B)
      if best>1e-9: found+=1; print('interior beats face',N,cnt,best,flush=True)
print('three-point underdispersed samples',tried,'interior beats face',found)
