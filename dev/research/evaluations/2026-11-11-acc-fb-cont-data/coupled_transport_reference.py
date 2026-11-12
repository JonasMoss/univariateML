"""Independent raw-density stationary points and compensated-finish regressions."""
from pathlib import Path
import json,mpmath as m
P=Path(__file__).resolve().parent;root=P.parents[3];m.mp.dps=100;results=[];new=[]
for i,parent in enumerate(json.loads((P/'coupled-burr-probe.json').read_text())):
 for family in ['burr','invburr','pareto4']:
  shift=1 if family=='pareto4' else 0;inverse=family=='invburr'
  raw=[1/v if inverse else v+shift for v in parent['data']];z=[1/m.mpf(v) if inverse else m.mpf(v)-shift for v in raw]
  base=parent['reference'];u0=m.log(m.mpf(base['shape1']));v0=m.log(m.mpf(base['shape2']));D0=u0-m.exp(v0)*m.log(m.mpf(base['scale']))
  def likelihood(u,v,D):
   k=m.exp(u);c=m.exp(v);return m.fsum(v+D+c*m.log(t)-(k+1)*m.log1p(m.exp(D+c*m.log(t)-u)) for t in z)
  def score(u,v,D):return m.exp(2*u)*m.diff(lambda w:likelihood(w,v,D),u),m.diff(lambda w:likelihood(u,w,D),v),m.diff(lambda w:likelihood(u,v,w),D)
  u,v,D=m.findroot(score,(u0,v0,D0),tol=m.mpf('1e-75'),maxsteps=100)
  k=m.exp(u);c=m.exp(v);s=m.exp((u-D)/c);s=1/s if inverse else s
  name='strict-seed-coupled-'+family+'-'+str(i)
  ref=dict(shape1=m.nstr(k,85),shape2=m.nstr(c,85),scale=m.nstr(s,85))
  if shift:ref['min']=str(shift)
  held={'min':shift} if shift else {}
  results.append(dict(id=name,family=family,digits=100,reference=ref,data=raw,held=held))
  control=dict(gradtol=1e-10,iterlim=1000)
  if family=='pareto4':control=dict(reltol=1e-10,iterlim=1000)
  new.append(dict(index=name,family=family,data=raw,held=held,tol=1e-10,iterlim=1000,source='Coupled finite Weibull departure',control=control))
(P/'coupled-transport-references.json').write_text(json.dumps(results,indent=2)+'\n')
for file in ['cases.json','spot-cases.json']:
 rows=json.loads((P/file).read_text());ids={str(c['index']) for c in rows};rows.extend(c for c in new if c['index'] not in ids);(P/file).write_text(json.dumps(rows,indent=2)+'\n')
for family in ['burr','invburr','pareto4']:
 path=root/('univariateML-cpp/include/cppunivariate/distributions/'+family+'/test.cpp');source=path.read_text();name='Coupled Weibull-limit '+family+' fallback retains coefficient accuracy'
 if 'TEST_CASE("'+name+'")' in source:continue
 source+='\nTEST_CASE("'+name+'") {\n'
 for r in results:
  if r['family']!=family:continue
  ref=r['reference'];source+='  { const std::vector<double> x{'+','.join(map(repr,r['data']))+'};\n'
  source+='    double k='+ref['shape1'][:36]+'*(1+1e-5),c='+ref['shape2'][:36]+'*(1+1e-8),s='+ref['scale'][:36]+'*(1+1e-5);\n'
  source+='    REQUIRE(cppunivariate::burr_local_accuracy(mltest::as_span(x),k,c,s,{{true,true,true}},1e-10,1000,'+('true' if family=='invburr' else 'false')+','+('1' if family=='pareto4' else '0')+'));\n'
  for field,key in [('k','shape1'),('c','shape2'),('s','scale')]:source+='    CHECK('+field+'==doctest::Approx('+ref[key][:36]+').scale(0.).epsilon(1e-10));\n'
  source+='  }\n'
 path.write_text(source+'}\n')
p=P/'focused_tests.py';s=p.read_text()
if "'Coupled Weibull-limit*'" not in s:s=s.replace('patterns=[',"patterns=['Coupled Weibull-limit*',");p.write_text(s)
p=P/'reference.py';s=p.read_text().replace("not key.startswith('strict-default-')", "not key.startswith(('strict-default-','strict-seed-coupled-'))");p.write_text(s)
print('Coupled raw transport references and regressions',len(results),flush=True)
