"""Regenerate independent 160-digit d/p fixtures under the shared CPU lock."""
from pathlib import Path
import mpmath as m
m.mp.dps=160
rows=[]
for lf,nf,k in [(.01,.2,3),(2,.5,7),(100,2,12),(1e100,100,10),(.9,0,100),(1e-100,2,0),(8,1.5,5)]:
 l,n=m.mpf(lf),m.mpf(nf)
 if not n:
  ld=k*m.log(l)+m.log1p(-l); lo=m.log1p(-l**(k+1)); up=(k+1)*m.log(l)
 else:
  mode=int(m.floor(m.exp(m.log(l)/n)))
  end=max(k+100,mode+100)
  w=[m.exp(j*m.log(l)-n*m.loggamma(j+1)) for j in range(end+1)]
  while w[-1]>max(w)*m.mpf('1e-90'):
   end+=1; w.append(w[-1]*l/m.mpf(end)**n)
  z=m.fsum(w); ld=m.log(w[k]/z); lo=m.log(m.fsum(w[:k+1])/z); up=m.log(m.fsum(w[k+1:])/z)
 rows.append('{'+','.join(format(float(v),'.17g') for v in (lf,nf,k,ld,lo,up))+'},')
Path(__file__).with_name('reference.inc').write_text('\n'.join(rows)+'\n')
