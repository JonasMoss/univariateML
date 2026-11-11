"""80-digit stationary-point comparisons, independent log-gamma likelihoods.
All supplied and returned doubles are converted exactly, not via decimal text.
Errors use |estimate-reference|/max(1,|reference|), per public coefficient.
Integer coefficients are exact conditional coordinates, not global certificates.
"""
import collections,json,pathlib,sys,mpmath as mp
mp.mp.dps=80
HERE=pathlib.Path(__file__).resolve().parent
cases=json.loads((HERE/'cases.json').read_text())
def D(v):return mp.mpf(float(v))
def binlog(x,N,p):return mp.loggamma(N+1)-mp.loggamma(x+1)-mp.loggamma(N-x+1)+x*mp.log(p)+(N-x)*mp.log1p(-p)
def solve(c,t):
 f=c['family'];h=c['held'];xs=collections.Counter(c['data']);n=sum(xs.values());S=sum(x*w for x,w in xs.items());z=xs.get(0,0);k=n-z
 if f in ['hyper','nhyper']:return t
 N=t.get('size');ref=t.copy()
 if f=='zib' and N==1:
  mean=mp.mpf(S)/n
  if 'prob' not in h and 'pi' not in h:
   ref['prob']=max(mean,t['prob']);ref['pi']=1-mean/ref['prob'];return ref
  if 'prob' not in h:
   ref['prob']=min(mp.mpf(1),mean/(1-D(h['pi'])));return ref
 if f=='bbinom':
  free=[j for j in ['alpha','beta'] if j not in h]
  if not free:return ref
  def ll(*q):
   p=t.copy();p.update(zip(free,map(mp.exp,q)));a,b=p['alpha'],p['beta']
   return mp.fsum(w*(mp.loggamma(a+x)-mp.loggamma(a)+mp.loggamma(b+N-x)-mp.loggamma(b)-mp.loggamma(a+b+N)+mp.loggamma(a+b)) for x,w in xs.items())
  start=[mp.log(t[j]) for j in free]
 elif f=='bnbinom':
  free=[j for j in ['size','alpha','beta'] if j not in h]
  def ll(*q):
   p=t.copy();p.update(zip(free,map(mp.exp,q)));r,a,b=[p[j] for j in ['size','alpha','beta']]
   return mp.fsum(w*(mp.loggamma(r+x)-mp.loggamma(r)+mp.loggamma(b+x)-mp.loggamma(b)-mp.loggamma(r+a+b+x)+mp.loggamma(r+a)+mp.loggamma(a+b)-mp.loggamma(a)) for x,w in xs.items())
  start=[mp.log(t[j]) for j in free]
 elif f=='skellam':
  free=[j for j in ['mu1','mu2'] if j not in h and t[j]>0]
  def ll(*q):
   p=t.copy();p.update(zip(free,map(mp.exp,q)));a,b=p['mu1'],p['mu2']
   return mp.fsum(w*(-a-b+mp.mpf(x)/2*mp.log(a/b)+mp.log(mp.besseli(abs(x),2*mp.sqrt(a*b)))) for x,w in xs.items())
  start=[mp.log(t[j]) for j in free]
 else:
  # Scalar binomial probability; inflation and zero mass profile in closed form.
  if f=='zmbinom' and 'p0' not in h:ref['p0']=mp.mpf(z)/n
  if 'prob' in h or t['prob'] in (0,1):
   if f=='zib' and 'pi' not in h:
    base=(1-t['prob'])**N;ref['pi']=max(mp.mpf(0),(mp.mpf(z)/n-base)/(1-base)) if base<1 else t['pi']
   return ref
  if f=='zib' and 'pi' in h and h['pi']!=0:
   pi=D(h['pi'])
   # Cleared score avoids cancellation and handles the nearly triple root.
   score=lambda p:pi*(S-k*N*p)+(1-pi)*(1-p)**N*(S-n*N*p)
   # At size two, solve the exact binary64-input cubic at high precision.
   if N==2:
    A=-(1-pi)*2*n;B=(1-pi)*(S+4*n);C=-(1-pi)*(2*S+2*n)-pi*2*k;E=mp.mpf(S)
    if B*B==3*A*C and B*B*B==27*A*A*E:
     ref['prob']=-B/(3*A);return ref
    p=mp.findroot(score,(t['prob']-mp.mpf('1e-4'),t['prob']+mp.mpf('1e-4')),solver='anderson',verify=False)
    # findroot with a bracket containing the simple root, including a flat cubic.
    lo=max(mp.mpf(0),t['prob']-mp.mpf('.01'));hi=min(mp.mpf(1),t['prob']+mp.mpf('.01'))
    if score(lo)*score(hi)<0:
     for _ in range(300):
      mid=(lo+hi)/2
      if score(mid)>0:lo=mid
      else:hi=mid
     p=(lo+hi)/2
   else:p=mp.findroot(score,(t['prob']*mp.mpf('.99'),t['prob']*mp.mpf('1.01')),tol=mp.mpf('1e-65'))
   ref['prob']=p;return ref
  if f=='zib' and (z==0 or t['pi']==0):ref['prob']=mp.mpf(S)/(n*N);ref['pi']=t['pi'];return ref
  mean=mp.mpf(S)/k
  if f=='tbinom':
   lo=max(0,int(mp.floor(t['a']))+1);hi=min(int(mp.floor(t['b'])),int(N))
   def condmean(p):
    if hi-lo<200:
     l=[binlog(v,N,p) for v in range(lo,hi+1)];top=max(l);w=[mp.exp(v-top) for v in l];return mp.fsum((lo+j)*v for j,v in enumerate(w))/mp.fsum(w)
    if lo<=128 and N-hi<=128:
     removed=list(range(lo))+list(range(hi+1,int(N)+1));w=[mp.exp(binlog(v,N,p)) for v in removed]
     return (N*p-mp.fsum(v*q for v,q in zip(removed,w)))/(1-mp.fsum(w))
    # Incomplete beta CDF differences, independent of the production recurrence.
    cdf=lambda v:mp.mpf(1) if v>=N else mp.mpf(0) if v<0 else mp.betainc(N-v,v+1,0,1-p,regularized=True)
    cdf1=lambda v:mp.mpf(1) if v>=N-1 else mp.mpf(0) if v<0 else mp.betainc(N-1-v,v+1,0,1-p,regularized=True)
    return N*p*(cdf1(hi-1)-cdf1(lo-2))/(cdf(hi)-cdf(lo-1))
   target=mp.mpf(S)/n
  else:
   condmean=lambda p:N*p/(-mp.expm1(N*mp.log1p(-p)))
   target=mean
  p=mp.findroot(lambda p:condmean(p)-target,(t['prob']*(1-mp.mpf('1e-7')),min(mp.mpf('0.99999999999999999999999999999'),t['prob']*(1+mp.mpf('1e-7')))),tol=mp.mpf('1e-65'))
  ref['prob']=p
  if f=='zib':ref['pi']=1-mp.mpf(k)/n/(-mp.expm1(N*mp.log1p(-p)))
  return ref
 if not free:return ref
 def score(*q):return tuple(mp.diff(lambda v:ll(*(q[:j]+(v,)+q[j+1:])),q[j]) for j in range(len(q)))
 root=mp.findroot(score,tuple(start),tol=mp.mpf('1e-60'),maxsteps=100)
 for j,q in zip(free,root):ref[j]=mp.exp(q)
 return ref
def main():
 eligible={int(l.split('\t')[0]) for l in (HERE.parent/'2026-11-10-fallback-count-data/after-final.tsv').read_text().splitlines() if l.split('\t')[2]=='uncertified'}
 eligible.update(c['study_index'] for c in cases if c['origin']!='count replay')
 rows=[]
 for line in (HERE/sys.argv[1]).read_text().splitlines():
  if "--start" in sys.argv and int(line.split("\t")[0])<int(sys.argv[sys.argv.index("--start")+1]):continue
  a=line.split('\t');i=int(a[0]);c=cases[i]
  if i not in eligible:continue
  if a[2] not in ('certified','uncertified','unflagged'):continue
  t={j:D(v) for j,v in (field.split('=',1) for field in a[4:] if '=' in field)}
  try:
   ref=solve(c,t);errors={j:float(abs(v-ref[j])/max(1,abs(ref[j]))) for j,v in t.items() if j not in c['held']}
   row=dict(index=i,family=c['family'],route=c.get('sample'),outcome=a[2],errors=errors,max_error=max(errors.values(),default=0),reference={j:str(v) for j,v in ref.items()},tol=c.get('tol',1e-10))
  except Exception as e:row=dict(index=i,family=c['family'],error=str(e))
  rows.append(row);print(i,row.get('max_error',row.get('error')),flush=True)
 (HERE/sys.argv[2]).write_text(json.dumps(dict(digits=mp.mp.dps,rows=rows),indent=2)+'\n')
if __name__=='__main__':main()
