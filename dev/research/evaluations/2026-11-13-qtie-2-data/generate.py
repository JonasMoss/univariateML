import json,pathlib
root=pathlib.Path('.'); out=root/'dev/research/evaluations/2026-11-13-qtie-2-data'
folders={'pois':['ztpois','zmpois','zip','tpois'],'binom':['ztbinom','zmbinom','zib','tbinom'],'nbinom':['ztnbinom','zmnbinom','zinb'],'geom':['ztgeom','zmgeom'],'lgser':['zmlogarithmic'],'zeta':['zipf']}
cases=[]; includes=[]; calls=[]; samplers=[]
for base,ids in folders.items():
 for id in ids:
  includes.append(f'#include "cppunivariate/distributions/{base}/{id}_dpqr.hpp"')
  t=json.load(open(f'univariateML-cpp/tests/modifiers/fixtures/{id}/truth.json'))
  for i,pars in enumerate(t['parameters']):
   centre=next((r[4] for r in t['quantiles'] if r[:4]==[i,1,0,.5] and isinstance(r[4],(int,float))),0)
   if abs(centre)>2**53-101: centre=0
   args=','.join(str(v) for v in pars)
   calls.append(f'probe("{id}",{i},{centre},[&](double k,bool l,bool g){{return p{id}(k,{args},l,g);}},[&](double p,bool l,bool g){{return q{id}(p,{args},l,g);}});')
   samplers.append(f'for(int j=0;j<512;++j) std::cout<<"{id},{i},"<<r{id}(rng,{args})<<"\\n";')
   names=t['parameter_names']; d=dict(zip(names,pars)); config={}
   if id=='zipf': config={'truncate':[1,d.pop('N')]}
   elif id in ('tpois','tbinom'): config={'truncate':[d.pop('a')+1,d.pop('b')]}
   else: config={'zero':'truncated' if id.startswith('zt') else 'modified' if id.startswith('zm') else 'inflated'}
   if base=='lgser': d['theta']=d.pop('prob')
   cases.append(dict(id=id,base=base,point=i,args=d,config=config,centre=centre))
# Composition-only windows, parameters from positive-kernel frozen fixtures.
for base,id in [('pois','ztpois'),('binom','ztbinom'),('geom','ztgeom'),('nbinom','ztnbinom')]:
 t=json.load(open(f'univariateML-cpp/tests/modifiers/fixtures/{id}/truth.json'))
 for i,pars in enumerate(t['parameters']):
  for window in [[2,12],[2,'Inf'],[0,5],[0,'Inf']]:
   cases.append(dict(id=base+'-window-'+str(window),base=base,point=i,args=dict(zip(t['parameter_names'],pars)),config={'truncate':window},centre=4))
   if base in ('pois','binom'):
    a=window[0]-1;b='INFINITY' if window[1]=='Inf' else str(window[1])
    arg=f'{pars[0]},{a},{b}' if base=='pois' else f'{pars[1]},{a},{b},{pars[0]}'
    calls.append(f'probe("{base}-window-{window[0]}-{window[1]}",{i},4,[&](double k,bool l,bool g){{return pt{base}(k,{arg},l,g);}},[&](double p,bool l,bool g){{return qt{base}(p,{arg},l,g);}});')
# Geometric inflation is the NB size-one private route.
for i,p in enumerate(json.load(open('univariateML-cpp/tests/modifiers/fixtures/ztgeom/truth.json'))['parameters']):
 for pi in [0,.25,.5,1]: cases.append(dict(id='geom-inflated',base='geom',point=i,args={'prob':p[0],'pi':pi},config={'zero':'inflated'},centre=4))
for base,id in [('pois','ztpois'),('binom','ztbinom'),('nbinom','ztnbinom'),('geom','ztgeom'),('lgser','zmlogarithmic')]:
 includes.append(f'#include "cppunivariate/distributions/{base}/dpqr.hpp"')
 t=json.load(open(f'univariateML-cpp/tests/modifiers/fixtures/{id}/truth.json'))
 for i,pars in enumerate(t['parameters']):
  names=t['parameter_names'];d=dict(zip(names,pars));d.pop('p0',None)
  if base=='lgser':d['theta']=d.pop('prob')
  vals=list(d.values());arg=','.join(str(x) for x in vals)
  cases.append(dict(id=base+'-none',base=base,point=i,args=d,config={'zero':'none'},centre=4))
  calls.append(f'probe("{base}-none",{i},4,[&](double k,bool l,bool g){{return p{base}(k,{arg},l,g);}},[&](double p,bool l,bool g){{return q{base}(p,{arg},l,g);}});')
for i,p in enumerate(json.load(open('univariateML-cpp/tests/modifiers/fixtures/ztgeom/truth.json'))['parameters']):
 for pi in [0,.25,.5,1]:
  calls.append(f'probe("geom-inflated",{i},4,[&](double k,bool l,bool g){{return pzinb(k,1,{p[0]},{pi},l,g);}},[&](double p,bool l,bool g){{return qzinb(p,1,{p[0]},{pi},l,g);}});')
includes.append('#include "cppunivariate/distributions/zeta/truncation.hpp"')
for i,(N,s) in enumerate([(8,0),(8,-1),(20,-2),(20,2)]):
 cases.append(dict(id='zeta-window',base='zeta',point=i,args={'s':s},config={'truncate':[1,N]},centre=N//2))
 calls.append(f'probe("zeta-window",{i},{N//2},[&](double k,bool l,bool g){{return cppunivariate::zeta_window_eval(\'p\',k,{N},{s},l,g);}},[&](double p,bool l,bool g){{return cppunivariate::zeta_window_eval(\'q\',p,{N},{s},l,g);}});')
json.dump(cases,open(out/'cases.json','w'),indent=2)
common='''#include <iostream>
#include <iomanip>
#include <cmath>
#include "cppunivariate/distributions/rng_std.hpp"
'''+ '\n'.join(includes)+'''
using namespace cppunivariate::distributions;
template<class P,class Q> void probe(const char* id,int point,double centre,P prob,Q quant) {
 for(bool l:{true,false})for(bool g:{false,true}) {
 int n=0,up=0,down=0,bad=0,cross=0;
 const int width=std::string(id).find("-window-")!=std::string::npos?2:100;
 for(int j=-width;j<=width;++j){double k=centre+j,p=prob(k,l,g),prev=prob(k-1,l,g);
 if(!(g?std::isfinite(p)&&p<0:p>0&&p<1)||p==prev)continue;
 ++n;double q=quant(p,l,g);if(!std::isfinite(q)){++bad;continue;}if(q>k)++up;if(q<k)++down;
 if(cppunivariate::special::discrete_reached(prob(q,l,g),p,l,g)!=1 || cppunivariate::special::discrete_reached(prob(q-1,l,g),p,l,g)!=0)++cross;
 }
 std::cout<<id<<","<<point<<","<<l<<","<<g<<","<<n<<","<<up<<","<<down<<","<<bad<<","<<cross<<"\\n";
 }}
'''
(out/'probe.cpp').write_text(common+'int main(){std::cout<<"id,point,lower,log,n,up,earlier,invalid,crossing_failure\\n";'+ '\n'.join(calls)+'}\n')
(out/'samplers.cpp').write_text(common+'int main(){StdRng<> rng(215);std::cout<<std::hexfloat;'+ '\n'.join(samplers)+'}\n')
