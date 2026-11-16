"""Large-a profile of genbeta (scale 1, all shapes free) on [c,c,c+] and [c,c+,c+], 60 digits.
For a >~ 50 the beta normalizer is p log q (q astronomically large), so for each a the profile in (p, q)
reduces to one root in p. Prints (profile - power face) against log10(a): a plateau at the log-gamma level
(-0.217, -2.297), a dip, then a monotone rise to 0 (the power face). Run: python3 genbeta_profile_large_a.py"""
import mpmath as mp, math, sys
mp.mp.dps=60
def run(xs,name,ks):
  c=xs
  x=[mp.mpf(v) for v in xs]; n=len(x)
  Lx=[mp.log(v) for v in x]; logM=max(Lx); sl=sum(Lx)
  gap=sum(logM-l for l in Lx)/n; Lp=n*(-mp.log(gap)-1)-sl
  print(name,'Lp',mp.nstr(Lp,15))
  for k in ks:
    a=mp.mpf(10)**mp.mpf(k)
    t=[a*(l-logM) for l in Lx]; T=sum(t); S=sum(mp.exp(v) for v in t)
    r=mp.log(S)-mp.log(n)-T/n
    lo,hi=mp.mpf(-80),mp.mpf(80)
    if not r>0: print(k,'r<=0'); continue
    for _ in range(300):
      mid=(lo+hi)/2; p=mp.exp(mid)
      if mp.log(p)-mp.digamma(p)>r: lo=mid
      else: hi=mid
    p=mp.exp(lo); s=mp.log(n*p/S)
    ll=n*mp.log(a)-sl-n*mp.loggamma(p)+n*p*s+p*T-mp.exp(s)*S
    print(k,mp.nstr(p,5),mp.nstr(ll-Lp,8)); sys.stdout.flush()
if __name__=='__main__':
  cg=0.8364136240866804; nx=math.nextafter(cg,2)
  ks=[2,3,4,6,8,10,12,14,15,16,17,18,20,24]
  run([cg,cg,nx],'A',ks); run([cg,nx,nx],'B',ks)
