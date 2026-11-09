"""Conditional-normal attainment criterion, with numerical sign margin.

After free endpoints move to observed extrema, the natural parameters are
eta1=mu/sd^2 and eta2=-1/(2 sd^2)<0. At eta2=0, match the empirical mean
with the (possibly one-sided) exponential boundary. Strict concavity gives:
empirical variance below its boundary variance -> finite interior maximum;
at/above it -> an unattained exponential boundary. With mu held this is a
one-parameter exponential family with statistic (X-mu)^2 and uniform boundary
on a finite interval. Nonconstant samples exclude zero-scale escape.
The sign decisions below are high precision, not outward-rounded intervals.
"""
import mpmath as mp

def criterion(c):
 data=c['data'];h=c['held']
 if min(data)==max(data):return None,''
 a=h.get('a',min(data));b=h.get('b',max(data))
 if min(data)<a or max(data)>b or not a<b:return None,''
 if 'sd' in h and not(h['sd']>0):return None,''
 if 'sd' in h:return True,'held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema'
 if not mp.isfinite(a) and not mp.isfinite(b):return True,'untruncated normal with nonconstant data has a finite scale/mean maximum'
 with mp.workdps(220):
  x=list(map(mp.mpf,data));a,b=mp.mpf(a),mp.mpf(b);n=len(x)
  if 'mean' in h:
   if not(mp.isfinite(a) and mp.isfinite(b)):return True,'held mean, at least one infinite bound: nonconstant-data likelihood vanishes at both scale escapes'
   width=b-a;m0=(mp.mpf(h['mean'])-a)/width
   precision=220+max(0,int(mp.log10(max(1,abs(m0)))))
   with mp.workdps(precision):
    u=[(v-a)/width for v in x];mean=mp.fsum(u)/n;var=mp.fsum((v-mean)**2 for v in u)/n
    d=var-mp.mpf(1)/12+(mean-mp.mpf('.5'))*(mean+mp.mpf('.5')-2*m0)
    label='held-mean quadratic statistic minus its finite-window uniform-boundary value'
  elif mp.isfinite(a) and mp.isfinite(b):
   u=[(v-a)/(b-a) for v in x];mean=mp.fsum(u)/n;var=mp.fsum((v-mean)**2 for v in u)/n
   if abs(mean-mp.mpf('.5'))<mp.mpf('1e-50'):boundary_var=mp.mpf(1)/12
   else:
    def m(r):return 1/(-mp.expm1(-r))-1/r if r else mp.mpf('.5')
    lo=-max(4,2/min(mean,1-mean));hi=-lo
    for _ in range(300):
     mid=(lo+hi)/2
     if m(mid)<mean:lo=mid
     else:hi=mid
    r=(lo+hi)/2;boundary_var=1/(r*r)-mp.exp(r)/(mp.expm1(r)**2)
   d=var-boundary_var;label='empirical variance minus mean-matched finite-window exponential-boundary variance'
  else:
   mean=mp.fsum(x)/n;var=mp.fsum((v-mean)**2 for v in x)/n;gap=mean-a if mp.isfinite(a) else b-mean
   d=var-gap*gap;label='empirical variance minus one-sided exponential-boundary variance'
  if abs(d)<=mp.mpf('1e-60')*max(1,abs(d)):return None,label+': comparison within the audit margin'
  return bool(d<0),label+' = '+mp.nstr(d,25)+' (strict-concavity boundary score; 120+ digits)'
