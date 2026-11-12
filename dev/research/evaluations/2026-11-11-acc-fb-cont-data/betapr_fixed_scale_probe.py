"""Force the uncertified fixed-scale return boundary using loose search nominations.
The final target is 1e-10. References differentiate the raw density at 100 digits.
"""
from pathlib import Path
import json,subprocess,mpmath as m,os,hashlib
P=Path(__file__).resolve().parent;root=P.parents[3];m.mp.dps=100;x=list(map(m.mpf,[.2,.5,1.,2.,4.]));rows=[]
for mode in [0,1,2]:
 held={'scale':1.};base={'shape1':m.mpf(float(1.7)) if mode==1 else m.mpf(2),'shape2':m.mpf(float(3.2)) if mode==2 else m.mpf(2)}
 if mode==1:held['shape1']=float(base['shape1'])
 if mode==2:held['shape2']=float(base['shape2'])
 names=[k for k in base if k not in held]
 def unpack(z):return {k:base[k]*m.exp(z[names.index(k)]) if k in names else base[k] for k in base}
 def ll(*z):
  c=unpack(z);a,b=c['shape1'],c['shape2'];return len(x)*(m.loggamma(a+b)-m.loggamma(a)-m.loggamma(b))+m.fsum((a-1)*m.log(v)-(a+b)*m.log1p(v) for v in x)
 def g(*z):return tuple(m.diff(lambda t:ll(*[t if j==i else z[j] for j in range(len(z))]),z[i]) for i in range(len(z)))
 z=m.findroot(g,[m.mpf(0)]*len(names),tol=m.mpf('1e-75'),maxsteps=50);ref=unpack(tuple(z));ref['scale']=m.mpf(1)
 rows.append(dict(mode=mode,id='fixed-scale-beta-'+str(mode),data=[float(v) for v in x],held=held,digits=100,reference={k:m.nstr(v,85) for k,v in ref.items()},builds={}))
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip();work=root/'build/scratch'
input_text=''.join(' '.join([str(r['mode']),repr(float(r['reference']['shape1'])),repr(float(r['reference']['shape2']))])+'\n' for r in rows)
for build in ['native','double']:
 exe=work/('betapr-fixed-scale-'+build);subprocess.run(['g++','-O2','-std=c++17',*(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if build=='double' else []),'-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(P/'betapr_fixed_scale_probe.cpp'),'-o',str(exe)],check=True)
 out=subprocess.check_output([str(exe)],input=input_text,text=True)
 for r,line in zip(rows,out.splitlines()):
  v=line.split();assert v[0]=='0' and v[1]=='0' and v[5]=='0' and v[6]=='0',line
  record={}
  for label,start in [('before',2),('after',7)]:
   got={k:m.mpf(float(t)) for k,t in zip(['shape1','shape2','scale'],v[start:start+3])};errors={k:float(abs(got[k]-m.mpf(r['reference'][k]))/abs(m.mpf(r['reference'][k]))) for k in got if k not in r['held']};record[label+'_errors']=errors;record[label+'_max']=max(errors.values());assert got['scale']==1
  assert record['after_max']<=1e-10,record
  r['builds'][build]=record;print(r['id'],build,record,flush=True)
(P/'betapr-fixed-scale-probe.json').write_text(json.dumps(rows,indent=2)+'\n')
