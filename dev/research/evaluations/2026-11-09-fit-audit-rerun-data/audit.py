#!/usr/bin/env python3
"""Standalone fit audit. Run with --generate, --fit, --reference, --report."""
import argparse, collections, importlib.util, json, math, os, pathlib, re, signal, subprocess, sys, time
ROOT=pathlib.Path(__file__).resolve().parents[4]
HERE=pathlib.Path(__file__).resolve().parent
WORK=ROOT/'build/scratch/fit-audit'
DISTS=ROOT/'univariateML-cpp/include/cppunivariate/distributions'
SHARDS=int(os.environ.get('AUDIT_SHARDS','1'));SHARD=int(os.environ.get('AUDIT_SHARD','0'))
sys.path.insert(0,str(ROOT/'dev/validation'))
import yaml

def coordinate_unit(ident,params,theta,data,i):
 unit=abs(theta[i]) or 1
 if params[i].get('role')=='location':unit=max(data)-min(data) or 1
 if ident=='tnorm' and params[i]['name']=='mean':
  values={p['name']:v for p,v in zip(params,theta)}
  width=values['b']-values['a'] if math.isfinite(float(values['a'])) and math.isfinite(float(values['b'])) else max(data)-min(data)
  if width>0:unit=max(unit,values['sd']*values['sd']/width)
 return unit
def models():
 frozen={}
 for c in json.loads((HERE/'cases.json').read_text()):
  held=frozen.setdefault(c['family'],{})
  for name,value in c['held'].items():
   assert name not in held or held[name]==value,(c['family'],name,'nonconstant frozen holds')
   held[name]=value
 for f in sorted(DISTS.iterdir()):
  if (f/'golden.json').exists():
   g=json.loads((f/'golden.json').read_text())
   g['estimates'].update(frozen.get(f.name,{}))
   yield f,yaml.safe_load((f/'README.md').read_text().split('---')[1]),g
def number(v): return float(str(v).replace('Inf','inf'))
def cpp(v):
 v=number(v)
 return 'INFINITY' if v==math.inf else '-INFINITY' if v==-math.inf else repr(v)
def generate():
 WORK.mkdir(parents=True,exist_ok=True)
 subprocess.run([sys.executable,str(HERE/'diagnostics_local.py'),'--generate',str(WORK)],check=True)
 src=(WORK/'audit.cpp').read_text().split('int main(){')[0]
 src=src.replace('\"shape1\",\"shape2\",\"shape3\",\"scale\",0,INFINITY', '\"shape1\",r.shape1,0,INFINITY')
 src=src.replace("std::cout<<'\\n';","std::cout<<std::endl;")
 src=src.replace('inline void route_', '#include \"'+str(ROOT/'bindings/common/fit.hpp')+'\"\nnamespace umvl_binding { inline FitResult fail(const std::string& s){return failure(s); }\ninline void route_',1)
 src=re.sub(r'return classify[^\n]*?;', '(void)0;', src)
 src+='\n} // namespace umvl_binding\n'
 # Preserve runtime certificate flags separately from the frozen CSV schema.
 marker='template<class T> void observe('
 extra="""template<class T,class=void> struct cert { static int get(const T&){return -1;} };
template<class T> struct cert<T,std::void_t<decltype(std::declval<T>().certified)>> { static int get(const T&r){return int(r.certified);} };
template<class T,class=void> struct globalcert { static int get(const T&){return -1;} };
template<class T> struct globalcert<T,std::void_t<decltype(std::declval<T>().global_certified)>> { static int get(const T&r){return int(r.global_certified);} };
"""
 src=src.replace(marker,extra+marker).replace('if(seen)return;seen=true;', 'if(seen)return;seen=true;std::cerr<<input<<","<<cert<T>::get(r)<<","<<globalcert<T>::get(r)<<std::endl;')
 blocks=[]; cases=[];omitted=[]
 for folder,m,g in models():
  ident=m['id']; ps=m['params']; names=[p['name'] for p in ps]
  vals=[number(g['estimates'].get(p['name'],p.get('simulate',1))) for p in ps]
  env=dict(inf=math.inf,Inf=math.inf,min=min,max=max,floor=math.floor,ceil=math.ceil);env.update(zip(names,vals))
  def bound(key):
   try:return float(eval(str(m['support'][key]),{'__builtins__':{}},env))
   except Exception:return -math.inf if key=='lower' else math.inf
  lo,hi=bound('lower'),bound('upper'); discrete=m['support']['type']=='integer'
  base=list(map(float,g['data'][:12])); samples=[('interior',base)]
  for n in [2,3,5]:samples.append(('tiny-'+str(n),base[:n]))
  samples.append(('ties',[base[0]]*4+base[1:3]))
  center=sorted(base)[len(base)//2]
  for loc in [center, .25 if lo>=0 and hi<=1 else max(1.,center)]:
   for spread in [1e-4,1e-8,1e-12]:
    samples.append((f'constant-{loc:g}-{spread:g}',[loc+max(abs(loc),1e-150)*spread*t for t in [-2,-1,0,1,3]]))
  if not discrete:
   for scale in [1e-6,1e-12,1e-17,1e-200]:
    if math.isfinite(lo):samples.append((f'lower-{scale:g}',[lo+scale*t for t in [1,2,3,5,9]]))
    if math.isfinite(hi):samples.append((f'upper-{scale:g}',[hi-scale*t for t in [1,2,3,5,9]]))
   for scale in [1e-150,1e150]:samples.append((f'scale-{scale:g}',[v*scale for v in base]))
   if any(p.get('role')=='location' for p in ps):samples.append(('offset-1e8',[v+1e8 for v in base]))
   samples.extend([('limit-outlier',base[:4]+[max(base)+max(1,abs(center))*1e6]),('limit-concentration',[center*(1+t*1e-6) for t in [-2,-1,0,1,2]])])
  else:
   samples.extend([('large-count',[1000000,1000001,1000003,1000008,1000011]),('zero-but-one',[0,0,0,0,1]),('constant-count',[100,100,100,101,100])])
  filtered=[]
  for label,x in samples:
   if label.startswith(('lower-','upper-')) and not all(lo<v<hi for v in x):
    omitted.append(dict(family=ident,sample=label,reason='binary64 endpoint rounding or support incompatibility'));continue
   filtered.append((label,x))
  samples=filtered
  samples=[(label,[int(v) for v in x] if discrete else x) for label,x in samples if all(math.isfinite(v) for v in x)]
  header=(ROOT/'bindings/common/distributions'/ident/'fit.hpp').read_text()
  integer=bool(re.search(r'fit_'+ident+r'\(cppunivariate::ispan',header))
  for mask in range(1<<len(ps)):
   held=[n for i,n in enumerate(names) if mask>>i&1]
   if any(set(h.get('held',[]))==set(held) for h in m.get('holds',[])):continue
   for label,x in samples:
    lower_params=[i for i,n in enumerate(names) if re.search(r'\b'+re.escape(n)+r'\b',str(m['support']['lower']))]
    upper_params=[i for i,n in enumerate(names) if re.search(r'\b'+re.escape(n)+r'\b',str(m['support']['upper']))]
    if min(x)<lo and (not lower_params or all(mask>>i&1 for i in lower_params)):continue
    if max(x)>hi and (not upper_params or all(mask>>i&1 for i in upper_params)):continue
    idx=len(cases); cases.append(dict(index=idx,family=ident,mask=mask,held=dict((n,vals[i]) for i,n in enumerate(names) if mask>>i&1),sample=label,data=x))
  args=','.join('audit::Held{bool(mask&'+str(1<<i)+'),'+cpp(v)+'}' for i,v in enumerate(vals))
  typ='int' if integer else 'double';span='ispan' if integer else 'span'
  blocks.append(f'if(family=="{ident}"){{std::vector<{typ}> x; for(double v:data)x.push_back(v);umvl_binding::route_{ident}(cppunivariate::{span}{{x.data(),x.size()}},{args});}}')
 src+='\n#include <iomanip>\nint main(){std::cout<<std::setprecision(17);std::string family;unsigned mask;int index,n;while(std::cin>>family>>mask>>index>>n){std::vector<double> data(n);for(double& v:data)std::cin>>v;audit::family=family;audit::held=std::to_string(mask);audit::input=std::to_string(index);audit::seen=false;'+''.join(blocks)+'if(!audit::seen)std::cout<<family<<","<<mask<<","<<index<<",-999,0,0,0,0,nan"<<std::endl;}return 0;}\n'
 if not (WORK/'stress.cpp').exists() or (WORK/'stress.cpp').read_text()!=src:(WORK/'stress.cpp').write_text(src)
 (HERE/'omitted.json').write_text(json.dumps(omitted,indent=2))
 if not (HERE/'cases.json').exists() or os.environ.get('AUDIT_RESET_CASES')=='1':(HERE/'cases.json').write_text(json.dumps(cases,allow_nan=True))
 print('generated routes; replay inventory',len(json.loads((HERE/'cases.json').read_text())),'cases',flush=True)
def fit():
 cases=json.loads((HERE/'cases.json').read_text()); out=HERE/(f'fits-{SHARD}.csv' if SHARDS>1 else 'fits.csv')
 # Each individual invocation has a wall-clock guard, with explicit open records.
 completed={int(l.split(',')[2]) for l in out.read_text().splitlines()} if out.exists() else set()
 with out.open('a') as dest:
  for c in cases:
   if c['index'] in completed or c['index']%SHARDS!=SHARD:continue
   line=f"{c['family']} {c['mask']} {c['index']} {len(c['data'])} "+' '.join(map(str,c['data']))+'\n'
   try:
    r=subprocess.run([str(WORK/'stress')],input=line,text=True,capture_output=True,timeout=3)
    
    with (HERE/'certificates.csv').open('a') as cert:cert.write(r.stderr)
    result=r.stdout.strip() or f"{c['family']},{c['mask']},{c['index']},-998,0,0,0,0,nan"
   except subprocess.TimeoutExpired:result=f"{c['family']},{c['mask']},{c['index']},-997,0,0,0,0,nan"
   dest.write(result+'\n');dest.flush()
   if c['index']%100==0:print('fit',c['index'],c['family'],flush=True)
class Deadline(Exception):pass
def alarm(*args):raise Deadline()
def reference():
 import mpmath as mp
 import sympy as sp
 from quality.density import density_expression
 from reference_extra import adapt
 from existence import proof as existence_proof
 from quality.harness.check import interval,r_bound_to_sympy
 mp.mp.dps=70;signal.signal(signal.SIGALRM,alarm)
 cases=json.loads((HERE/'cases.json').read_text()); fits={int(l.split(',')[2]):l.split(',') for l in (HERE/'fits.csv').read_text().splitlines()}
 dest=HERE/os.environ.get('REFERENCE_FILE',f'reference-{SHARD}.jsonl' if SHARDS>1 else 'reference.jsonl'); previous=list(map(json.loads,dest.read_text().splitlines())) if dest.exists() else []
 valid_previous=[]
 for record in previous:
  row=fits.get(record['index']);same=row is not None and int(row[3])==record['status']
  if same and record['status'] in [0,-1] and 'reported_logLik' in record:
   try:same=float(record['reported_logLik'])==float(row[8]) and ('theta' not in record or all(float(v)==float(t.split('=')[1]) for v,t in zip(record['theta'],row[9:])))
   except Exception:same=False
  if same:valid_previous.append(record)
 dest.write_text(''.join(json.dumps(record)+'\n' for record in valid_previous))
 completed={record['index'] for record in valid_previous}
 with dest.open('a') as out:
  for folder,m,g in models():
   ps=m['params']; names=[p['name'] for p in ps]; setup_error=None
   try:
    domains=[interval(p['domain'],{k:number(v) for k,v in g['estimates'].items()}) for p in ps]
    syms,x,expr=density_expression(folder,m,domains,prefer_spec=False)
    expr=expr.replace(lambda e:e.func==sp.log and e.args[0].is_Add and sp.S.One in e.args[0].args,lambda e:sp.Function('log1p')(e.args[0]-1))
    fn=sp.lambdify([*syms,x],expr,[{'log1p':mp.log1p},'mpmath'],cse=True)
    fn=adapt(folder.name,names,fn)
    support=[]
    for k in ['lower','upper']:
     source=r_bound_to_sympy(str(m['support'][k])).replace('inf','oo')
     for name,symbol in sorted(zip(names,syms),key=lambda pair:-len(pair[0])):source=re.sub(r'\b'+re.escape(name)+r'\b',symbol.name,source)
     support.append(sp.lambdify(syms,sp.sympify(source,locals={symbol.name:symbol for symbol in syms}),'mpmath'))
   except Exception as e:setup_error=repr(e)
   print('reference',folder.name,setup_error or '',flush=True)
   for c in (c for c in cases if c['family']==folder.name and c['index'] not in completed and c['index']%SHARDS==SHARD and c['index'] in fits):
    row=fits[c['index']];status=int(row[3]);r=dict(index=c['index'],family=c['family'],status=status,classification='open',observed_fit_signature=[row[3],*row[8:]])
    signal.setitimer(signal.ITIMER_REAL,float(os.environ.get("REFERENCE_SECONDS","2")))
    try:
     if status in [-997,-998,-999]:raise ValueError('core invocation timed out' if status==-997 else 'core observation unavailable')
     if status not in [0,-1]:
      proof=existence_proof(folder.name,c);exists=bool(proof)
      if not exists and len(c['held'])==len(ps) and not setup_error:
       try:
        fixed=[mp.mpf(c['held'][p['name']]) for p in ps]
        lo,hi=[f(*fixed) for f in support]
        if all(lo<=v<=hi for v in c['data']):
         fixed_ll=mp.fsum(fn(*fixed,mp.mpf(v)) for v in c['data'])
         if mp.isfinite(fixed_ll):exists=True;proof='all coordinates held: the sole admissible parameter point has a finite independently evaluated log likelihood'
       except Deadline:raise
       except Exception:pass
      r.update(classification='loud failure' if exists else 'non-Ok',existence=proof or 'open: finite-MLE existence not inferred from a finite candidate');raise StopIteration
     if setup_error:raise ValueError(setup_error)
     mp.mp.dps=70
     theta=[mp.mpf(float(t.split('=')[1])) for t in row[9:]]
     magnitude=max([abs(float(mp.log10(abs(v)))) for v in theta+list(map(mp.mpf,c['data'])) if v and mp.isfinite(v)]+[0])
     mp.mp.dps=max(70,80+int((2 if folder.name=='tnorm' else 1)*magnitude)) if magnitude>20 else 70
     r['precision_digits']=mp.mp.dps
     if len(theta)!=len(ps):raise ValueError('coefficient mapping length')
     from support_geometry import classify as support_classify
     if support_classify(r,m,c,[float(v) for v in theta],float(row[8])):raise StopIteration
     data=list(map(mp.mpf,c['data']))
     def valid(t):
      for i,(p,v) in enumerate(zip(ps,t)):
       env=dict(zip(names,t));lo,hi=interval(p['domain'],env)
       if v<lo or v>hi or v==lo and p['domain'][0]=='(' or v==hi and p['domain'][-1]==')':return False
       if p.get('integer') and v!=int(v):return False
       rules=p.get('fit_domain',[]);rules=[rules] if isinstance(rules,dict) else rules
       if p['name'] not in c['held']:
        for rule in rules:
         if any(set(h).issubset(c['held']) for h in rule.get('restriction',{}).get('exempt_when_held',[])):continue
         if v<number(rule.get('lower',-math.inf)) or v>number(rule.get('upper',math.inf)):return False
      lower,upper=[f(*t) for f in support]
      return all(lower<=v<=upper for v in data)
     def total(t):return mp.fsum(fn(*t,v) for v in data)
     interior=[]
     for j,p in enumerate(ps):
      if p['name'] in c['held'] or any(p.get(k) for k in ['integer','bound','kink']):continue
      scale0=coordinate_unit(folder.name,ps,theta,data,j)
      left=theta[:];right=theta[:];left[j]-=scale0*mp.mpf('1e-8');right[j]+=scale0*mp.mpf('1e-8')
      if valid(left) and valid(right):interior.append(j)
     ll=total(theta);reported=mp.mpf(float(row[8]));den=max(1,abs(reported));err=abs(ll-reported)/den
     if not mp.isfinite(reported):
      r.update(theta=[str(v) for v in theta],reference_logLik=str(ll),reported_logLik=str(reported),relative_error=float('inf'),classification='silent failure',evidence='status Ok with nonfinite reported logLik')
      raise StopIteration
     if not mp.isfinite(ll):raise ValueError('nonfinite independent likelihood at returned point')
     r.update(theta=[str(v) for v in theta],reference_logLik=str(ll),reported_logLik=str(reported),relative_error=float(err),classification='silent failure' if err>mp.mpf('1e-8') else 'ok',evidence='logLik discrepancy' if err>mp.mpf('1e-8') else '70-digit density and feasible coordinate comparisons')
     best=ll;bt=theta[:];scores={};parameter_error=0;score_witness=None
     seed=[mp.mpf(number(g['estimates'].get(p['name'],p.get('simulate',1)))) if p['name'] not in c['held'] else theta[i] for i,p in enumerate(ps)]
     try:
      if valid(seed):
       candidate=total(seed)
       if mp.isfinite(candidate) and candidate>best:best=candidate;bt=seed
     except Deadline:raise
     except Exception:pass
     for i,p in enumerate(ps):
      if p['name'] in c['held']:continue
      scale=coordinate_unit(folder.name,ps,theta,data,i)
      probes=[]
      if p.get('bound'):probes=[min(data) if p['bound']=='lower' else max(data)]
      if p.get('role')=='location':
       probes+=sorted(set(data))[::max(1,len(set(data))//5)]
      for value in probes:
       candidate_theta=theta[:];candidate_theta[i]=value
       try:
        if valid(candidate_theta):
         candidate=total(candidate_theta)
         if mp.isfinite(candidate) and candidate>best:best=candidate;bt=candidate_theta
       except Deadline:raise
       except Exception:pass
      def univariate(v):
       t=theta[:];t[i]=v;return total(t)
      if i in interior and folder.name!='tlambda':
       try:scores[p['name']]=str(mp.diff(univariate,theta[i])*scale/len(data))
       except Deadline:raise
       except Exception:pass
       if p['name'] in scores and abs(mp.mpf(scores[p['name']]))>mp.mpf('1e-3'):
        left=theta[:];right=theta[:];left[i]-=scale*mp.mpf('1e-8');right[i]+=scale*mp.mpf('1e-8')
        if valid(left) and valid(right):
         direction=right if mp.mpf(scores[p['name']])>0 else left
         candidate=total(direction)
         if candidate>ll+mp.mpf('1e-30')*den:
          score_witness=p['name']
          if candidate>best:best=candidate;bt=direction
      for step in ([mp.mpf(-1),mp.mpf(1)] if p.get('integer') else [scale*mp.mpf(s) for s in ['-0.01','-0.0001','0.0001','0.01']]):
       t=theta[:];t[i]+=step
       try:
        if valid(t):
         trial=total(t)
         if mp.isfinite(trial) and trial>best:best=trial;bt=t
       except Deadline:raise
       except Exception:pass
     # Newton refinement in dimensionless free coordinates; a found point
     # is an improvement witness only, never a global certificate.
     smooth=interior
     if folder.name!='tlambda' and smooth and len(smooth)<=5 and any(abs(mp.mpf(v))>mp.mpf('1e-12') for v in scores.values()):
      scales=[coordinate_unit(folder.name,ps,theta,data,i) for i in smooth]
      def decode(z):
       t=theta[:]
       for i,scale,zi in zip(smooth,scales,z):t[i]+=scale*zi
       return t
      def objective(*z):return total(decode(z))
      def equations(*z):return tuple(mp.diff(lambda v:objective(*(z[:j]+(v,)+z[j+1:])),z[j]) for j in range(len(z)))
      try:
       root=mp.findroot(equations,tuple(mp.mpf(0) for i in smooth),tol=mp.mpf('1e-40'),maxsteps=12,verify=False)
       t=decode(list(root))
       if valid(t):
        trial=total(t)
        if mp.isfinite(trial) and trial>best:best=trial;bt=t
        residual=max(abs(v) for v in equations(*list(root)))/len(data)
        r['stationary_residual']=str(residual)
        if residual<mp.mpf('1e-25') and trial>=ll-mp.mpf('1e-45')*den:
         parameter_error=max(float(abs(t[i]-theta[i])/(max(1,abs(t[i])) if domains[i][0]<0 else max(abs(t[i]),mp.mpf('1e-300')))) for i in smooth)
         r['refined_theta']=[str(v) for v in t];r['refined_logLik']=str(trial);r['parameter_relative_error']=parameter_error
      except Deadline:raise
      except Exception:pass
     gain=(best-ll)/den;r['scores']=scores;r['relative_gain']=float(gain)
     if gain>mp.mpf('1e-8'):
      r.update(classification='silent failure',evidence='feasible higher-likelihood point (70 digits)',better_theta=[str(v) for v in bt],better_logLik=str(best))
     if score_witness and r['classification']!='silent failure':
      r.update(classification='silent failure',evidence='interior dimensionless free score exceeds 1e-3 with feasible ascent',score_coordinate=score_witness,better_theta=[str(v) for v in bt],better_logLik=str(best))
     if parameter_error>1e-8 and best>ll and not r.get('classification')=='silent failure':
      r.update(classification='silent failure',evidence='stationary local refinement: coefficient relative error exceeds 1e-8',better_theta=[str(v) for v in bt],better_logLik=str(best))
     r['relative_error']=max(float(err),float(gain),parameter_error)
     if r['classification']=='silent failure':
      with mp.workdps(mp.mp.dps*2):
       ll140=total(theta);best140=total(bt)
       if abs(ll140-ll)>mp.mpf('1e-20')*max(1,abs(ll140)) or abs(best140-best)>mp.mpf('1e-20')*max(1,abs(best140)):
        raise ValueError('base/doubled precision reference disagreement; further precision escalation required')
       if r['evidence'].startswith('stationary') and best140<=ll140:raise ValueError('coefficient witness did not improve at 140 digits')
       r['precision_check']='base/doubled precision agree to relative 1e-20'
       r['logLik_doubled_precision']=str(ll140)

    except StopIteration:pass
    except Exception as e:
     r['open_reason']=type(e).__name__+': '+str(e)
     if r['classification']=='ok' or 'precision' in str(e) or '140 digit' in str(e):r['classification']='open'
    finally:signal.setitimer(signal.ITIMER_REAL,0)
    out.write(json.dumps(r)+'\n');out.flush()
def merge():
 cases=json.loads((HERE/'cases.json').read_text())
 for stem,suffix in [('fits','csv'),('reference','jsonl')]:
  files=list(HERE.glob(stem+'-[0-9].'+suffix))
  if not files:continue
  lines=[l for f in files for l in f.read_text().splitlines()]
  key=(lambda l:int(l.split(',')[2])) if stem=='fits' else (lambda l:json.loads(l)['index'])
  assert len(lines)==len(cases) and {key(l) for l in lines}==set(range(len(cases))),(stem,len(lines),len(cases))
  (HERE/(stem+'.'+suffix)).write_text('\n'.join(sorted(lines,key=key))+'\n')
  print('merged',stem,len(lines),flush=True)
def static():
 patterns={'log complement':r'\blog\s*\(\s*1(?:\.0)?\s*-','special difference':r'(digamma|trigamma|lgamma|log_gamma).*[-]|[-].*(digamma|trigamma|lgamma)','one-pass variance':r'(sum_sq|sum_log_sq|sumsq|sum2|sum_x2|mean_sq).*[-]|[-].*(mean\s*\*\s*mean)','fixed iteration':r'for\s*\([^;]*;[^;]*<\s*\d+','step stop':r'(abs|fabs).*([<]=?|tol)|([<]=?).*(tol|tolerance)','legacy':r'legacy|bit.identical|reproduc','special start':r'(start|init|seed|guess).*(exp|gamma|pow)|(exp|gamma|pow).*(start|init|seed|guess)'}
 hits=[];paths=set()
 def visit(path):
  path=path.resolve()
  if path in paths or not path.exists() or ROOT not in path.parents:return
  paths.add(path);s=path.read_text()
  for line,n in zip(s.splitlines(),range(1,len(s.splitlines())+1)):
   for kind,pat in patterns.items():
    if re.search(pat,line,re.I):
     review='candidate; execution path and safeguards require contextual review'
     if line.strip().startswith('//'):review='comment, not an evaluated operation'
     elif kind=='legacy':review='name/comment only; later acceptance or certificate determines authority'
     elif kind=='one-pass variance':review='one-pass second-moment cancellation' if 'sum_log_sq' in line else 'false positive: score sum, not sample variance'
     elif kind=='fixed iteration':
      context='\n'.join(s.splitlines()[n-1:n+18])
      if re.search(r'for\s*\([^;]+;[^;]+<\s*(?:2|3|4|6|8|9|16)\s*;',line):review='small fixed-size loop; inspect whether coordinate enumeration or numerical iteration'
      elif 'break' in context and re.search(r'tol|epsilon|1e-',context):review='nearby convergence/remainder break; exhaustion handling requires caller review'
      else:review='fixed-cap or pattern nomination; convergence/exhaustion check not established by grep'
     elif kind=='step stop':
      context='\n'.join(s.splitlines()[max(0,n-8):n+10])
      review='nearby score/bracket logic; not established as step-only' if re.search(r'score|bracket|lo|hi',context,re.I) else 'potential step-only condition; caller acceptance check still needs inspection'
     hits.append(dict(file=str(path.relative_to(ROOT)),line=n,kind=kind,text=line.strip(),review=review))
  for inc in re.findall(r'#include "([^"]+)"',s):
   candidate=path.parent/inc
   if not candidate.exists():candidate=ROOT/'univariateML-cpp/include'/inc
   if candidate.name!='dpqr.hpp':visit(candidate)
 for f,m,g in models():visit(f/'mle.hpp')
 (HERE/'static.json').write_text(json.dumps(hits,indent=2));print('static',len(paths),'headers',len(hits),'nominations')
if __name__=='__main__':
 p=argparse.ArgumentParser();p.add_argument('stage',choices=['generate','fit','reference','static','merge']);a=p.parse_args();globals()[a.stage]()
