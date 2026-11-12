"""Independent stationary points: mpmath differentiation of log densities.
Run via the one-slot wrapper. Exact binary64 inputs are converted with mp.mpf.
"""
import json,sys,math,signal,os
def deadline(signum,frame):raise TimeoutError("reference solve exceeded 60 seconds")
signal.signal(signal.SIGALRM,deadline)
from pathlib import Path
import mpmath as mp
P=Path(__file__).resolve().parent
cases={str(c['index']):c for c in json.loads((P/'cases.json').read_text())}
def rows(name):
 return {r[0]:r for r in [l.split('\t') for l in (P/(name+'.tsv')).read_text().splitlines()]}
def coeff(r):return {k:float(v) for k,v in (s.split('=',1) for s in r[4:] if not s.startswith('message='))}
def soft(t):return max(t,0)+mp.log1p(mp.exp(-abs(t)))
def logphi(z):
 if z>=-40:return mp.log(mp.erfc(-z/mp.sqrt(2))/2)
 t=-z;term=mp.mpf(1);total=term
 for k in range(1,65):term*=-(2*k-1)/(t*t);total+=term
 return -t*t/2-mp.log(t)-mp.log(2*mp.pi)/2+mp.log(total)
def likelihood(f,x,c):
 if f in ['invburr','invparalogis']:
  f={'invburr':'burr','invparalogis':'paralogis'}[f];c=dict(c);c['scale']=1/c['scale'];x=[1/v for v in x]
 if f in ['pareto3','pareto4','fpareto']:
  x=[v-c['min'] for v in x];f={'pareto3':'llogis','pareto4':'burr','fpareto':'gb2'}[f]
 if f in ['burr','paralogis','llogis','betapr','gb2']:
  if f=='burr':k,b,s=c['shape1'],c['shape2'],c['scale'];return mp.fsum(mp.log(k*b)-mp.log(v)-soft(-b*mp.log(v/s))-k*soft(b*mp.log(v/s)) for v in x)
  if f=='paralogis':a,s=c['shape'],c['scale'];return mp.fsum(2*mp.log(a)-mp.log(v)-soft(-a*mp.log(v/s))-a*soft(a*mp.log(v/s)) for v in x)
  if f=='llogis':a,s=c['shape'],c['scale'];return mp.fsum(mp.log(a)-mp.log(v)-soft(-a*mp.log(v/s))-soft(a*mp.log(v/s)) for v in x)
  q,a,p,b=(c['shape2'],mp.mpf(1),c['shape1'],c['scale']) if f=='betapr' else (c['shape1'],c['shape2'],c['shape3'],c['scale'])
  lb=mp.loggamma(p)+mp.loggamma(q)-mp.loggamma(p+q)
  return mp.fsum(mp.log(a)-mp.log(v)-lb-p*soft(-a*mp.log(v/b))-q*soft(a*mp.log(v/b)) for v in x)
 if f=='ged':
  mu,s,p=c['mean'],c['sd'],c['nu'];L=mp.sqrt(mp.gamma(1/p)/mp.gamma(3/p));return mp.fsum(mp.log(p)-mp.log(2*s*L)-mp.loggamma(1/p)-(abs(v-mu)/(s*L))**p for v in x)
 if f=='bhatt':
  mu,s,a=c['mu'],c['sigma'],c['a']
  if a==0:return mp.fsum(-mp.log(s)-(v-mu)**2/(2*s*s) for v in x)
  return mp.fsum(logphi(a/s-abs((v-mu)/s))+mp.log1p(-mp.exp(logphi(-a/s-abs((v-mu)/s))-logphi(a/s-abs((v-mu)/s))))-mp.log(2*a) for v in x)
 if f=='genbeta':
  p,q,t,b=c['shape1'],c['shape2'],c['shape3'],c['scale'];return mp.fsum(mp.log(t)-mp.log(b)+(p*t-1)*mp.log(v/b)+(q-1)*mp.log1p(-(v/b)**t)-mp.loggamma(p)-mp.loggamma(q)+mp.loggamma(p+q) for v in x)
 if f=='sn':
  mu,s,a=c['xi'],c['omega'],c['alpha'];return mp.fsum(-mp.log(s)-(v-mu)**2/(2*s*s)+logphi(a*(v-mu)/s) for v in x)
 raise ValueError(f)
if __name__=='__main__':
 before=rows('census-before-native' if sys.argv[1].startswith('census') else 'before-native'); after=rows(sys.argv[1]); results=[]
 if sys.argv[1].startswith(('focused','current')):before.update(rows('census-before-native'))
 if not sys.argv[1].startswith('census') and (P/'strict-before-native.tsv').exists():before.update(rows('strict-before-native'))
 selected=set(os.environ.get('UMVL_REFERENCE_IDS','').split(','))-{''}
 cache={}
 if os.environ.get('UMVL_REFERENCE_RESUME') and (P/(sys.argv[1]+'-references.json')).exists():cache={r['id']:r for r in json.loads((P/(sys.argv[1]+'-references.json')).read_text())}
 print('reference start',sys.argv[1],'cache',len(cache),flush=True)
 for key,r in before.items():
  if (r[2]!='uncertified' and after.get(key,['','',''])[2]!='uncertified' and not key.startswith(('strict-default-','strict-seed-coupled-','strict-seed-comparable-'))) or (selected and key not in selected):continue
  if key in cache and 'reference' in cache[key] and float(cache[key].get('normal_H4_sum','1'))>0:results.append(cache[key]);continue
  if key.startswith('strict-seed-bhatt-critical-'):continue # Dedicated 180-digit, scaled raw-CDF reference.
  c=cases[key]; guess=coeff(after[key]) if after[key][2] in ['certified','uncertified'] else coeff(r)
  if not guess:continue
  # Enough working digits to differentiate tiny perturbations at huge offsets.
  mp.mp.dps=max(80,min(700,80+int(2*max(abs(math.log10(abs(v))) for v in list(guess.values())+c['data'] if v))))
  x=list(map(mp.mpf,c['data']));base={k:mp.mpf(v) for k,v in guess.items()};held=c['held'];names=[k for k in base if k not in held and k!='min']
  reported_names=list(names)
  if c['family']=='ged' and base['nu']<1 and 'mean' in names:base['mean']=min(x,key=lambda v:abs(v-base['mean']))
  if c['family']=='ged' and base['nu']<=1:
   names=[k for k in names if k not in ['mean','nu']] # exact knot/median and restriction
  if c['family']=='bhatt' and base['a']==0:names=[k for k in names if k!='a']
  scale=base.get('sd',base.get('sigma',base.get('omega',mp.mpf(1))))
  if c['family'] in ['burr','invburr','pareto4'] and 'shape1' in names and 'shape2' in names and base['shape1']>mp.mpf('1e100'):
   logs=[mp.log(v-base.get('min',0))*( -1 if c['family']=='invburr' else 1) for v in x];meanlog=mp.fsum(logs)/len(logs);sdlog=mp.sqrt(mp.fsum((v-meanlog)**2 for v in logs)/len(logs));base['shape1']=mp.mpf(1);base['shape2']=mp.pi/mp.sqrt(3)/sdlog;base['scale']=mp.exp(meanlog*(-1 if c['family']=='invburr' else 1))
  if c['family']=='bhatt' and 'mu' in names and 'sigma' not in names and 'a' not in names and base['a']<mp.mpf('1e-20')*max(map(abs,x)):
   base['mu']=mp.fsum(x)/len(x)
  if c['family']=='bhatt' and 'sigma' not in names and 'a' in names:
   if 'mu' in names:base['mu']=(min(x)+max(x))/2
   radius=max(abs(v-base['mu']) for v in x)
   if radius>mp.mpf('1e20')*base['sigma']:base['a']=radius+base['sigma']*mp.sqrt(2*mp.log(radius/base['sigma']))
  if c['family'] in ['paralogis','invparalogis'] and 'scale' in names:
   logs=list(map(mp.log,x));sd=mp.sqrt(mp.fsum((v-mp.fsum(logs)/len(logs))**2 for v in logs)/len(logs))
   if base['shape']*sd<mp.mpf('.1'):
    base['shape']=mp.pi/mp.sqrt(6)/sd
    base['scale']=mp.exp(mp.fsum(logs)/len(logs)+(1 if c['family']=='paralogis' else -1)*(mp.log(base['shape'])+mp.euler)/base['shape'])
  closed_normal=c['family']=='bhatt' and base['a']==0
  if closed_normal:
   if 'mu' in names:base['mu']=mp.fsum(x)/len(x)
   if 'sigma' in names:base['sigma']=mp.sqrt(mp.fsum((v-base['mu'])**2 for v in x)/len(x))
   h4=mp.fsum(((v-base['mu'])/base['sigma'])**4-6*((v-base['mu'])/base['sigma'])**2+3 for v in x)
   if 'a' not in held and 'sigma' in names and h4<0:
    ratio=min(mp.sqrt(mp.mpf('2.5')),(-mp.mpf('7.5')*h4/len(x))**mp.mpf('.25'))
    base['a']=base['sigma']*ratio;base['sigma']*=mp.sqrt(1-ratio*ratio/3)
    names.append('a');closed_normal=False
  if c['family']=='ged' and 'mean' in names and sorted(x)==sorted(-v for v in x):
   base['mean']=mp.mpf(0);names.remove('mean')
  def unpack(z):
   d=dict(base)
   for k,t in zip(names,z):d[k]=base[k]+scale*t if k in ['mean','mu','xi'] else base[k]*mp.exp(t)
   if c['family']=='genbeta' and 'shape1' in names and 'shape3' in names:
    d['shape1']=base['shape1']*mp.exp(z[names.index('shape1')]-z[names.index('shape3')])
   return d
  def f(*z):return likelihood(c['family'],x,unpack(z))
  def g(*z):
   if c['family']=='bhatt' and base['a']>0:
    d=unpack(z);mu,s,a=d['mu'],d['sigma'],d['a'];gm=mp.mpf(0);gs=mp.mpf(0);ga=mp.mpf(0)
    for value in x:
     t=(value-mu)/s;k=a/s;u=t+k;l=t-k
     upper=logphi(k-abs(t));lower=logphi(-k-abs(t));lp=upper+mp.log1p(-mp.exp(lower-upper))
     A=mp.exp(-u*u/2-mp.log(2*mp.pi)/2-lp);B=mp.exp(-l*l/2-mp.log(2*mp.pi)/2-lp)
     gm+=(B-A)*scale/s;gs+=l*B-u*A;ga+=k*(A+B)-1
    score={'mu':gm,'sigma':gs,'a':ga}
    return tuple(score[k] for k in names)
   return tuple(mp.diff(lambda t:f(*[t if j==i else z[j] for j in range(len(z))]),z[i]) for i in range(len(z)))
  out={'id':key,'family':c['family'],'digits':mp.mp.dps,'after_status':after[key][2]}
  try:
   signal.alarm(60)
   z=([mp.mpf(0)]*len(names)) if closed_normal else mp.findroot(g,[mp.mpf(0)]*len(names),tol=mp.mpf('1e-55'),maxsteps=70,solver='mdnewton') if names else []
   ref=unpack(z)
   for label,row in [('before',r),('after',after[key])]:
    vals=coeff(row) if row[2] in ['certified','uncertified'] else {}
    errors={k:float(abs(mp.mpf(vals[k])-ref[k])/max(abs(ref[k]),scale if k in ['mean','mu','xi'] else mp.mpf('1e-1000'))) for k in reported_names} if vals else {}
    out[label+'_errors']=errors;out[label+'_max']=max(errors.values(),default=None)
   out['reference']={k:mp.nstr(v,65) for k,v in ref.items()};out['residual']='closed-form normal moments' if closed_normal else mp.nstr(max(map(abs,g(*z)),default=0),8)
   if closed_normal:out['normal_H4_sum']=mp.nstr(mp.fsum(((v-ref['mu'])/ref['sigma'])**4-6*((v-ref['mu'])/ref['sigma'])**2+3 for v in x),40)
  except Exception as e:out['open']=str(e)[:500]
  signal.alarm(0)
  results.append(out);(P/(sys.argv[1]+'-references.json')).write_text(json.dumps(results,indent=2)+'\n');print(key,out.get('before_max'),out.get('after_max'),out.get('open','')[:70],flush=True)

if __name__=='__main__' and sys.argv[1]=='current-native' and not (P/'held-power-edge-probe.json').exists():
 import subprocess
 subprocess.run([sys.executable,str(P/'held_power_probe.py')],check=True)
