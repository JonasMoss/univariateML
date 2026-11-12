"""100-digit independent Burr weak-coordinate hold regressions."""
from pathlib import Path
import json,mpmath as m
P=Path(__file__).resolve().parent;root=P.parents[3];m.mp.dps=110;raw=[.5,.5,1.,1.,2.];x=list(map(m.mpf,raw));n=len(x)
def weibull(c):return 1/c+m.fsum(m.log(v) for v in x)/n-m.fsum(v**c*m.log(v) for v in x)/m.fsum(v**c for v in x)
c=m.findroot(weibull,(1,3));w=(m.fsum(v**c for v in x)/n)**(1/c)
records=[]
for kind in ['first','scale','inverse-first']:
 inverse=kind=='inverse-first';k=m.mpf(float(1e300));s=m.exp(m.log(k)/c)*w
 if kind=='scale':s=m.mpf(float(1e100));k=(s/w)**c
 def ll(u,v):
  K=k if kind!='scale' else m.exp(u);C=m.exp(v);S=m.exp(u) if kind!='scale' else s
  return m.fsum(m.log(K)+m.log(C)+C*m.log(t/S)-(K+1)*m.log1p(m.exp(C*m.log(t/S))) for t in x)
 if kind=='scale':
  def score(u,v):return m.diff(lambda z:ll(z,v),u),m.diff(lambda z:ll(u,z),v)
  u,v=m.findroot(score,(m.log(k),m.log(c)),tol=m.mpf('1e-85'),maxsteps=100);K=m.exp(u);C=m.exp(v);S=s;mask=[True,True,False]
 else:
  def score(u,v):return m.diff(lambda z:ll(z,v),u),m.diff(lambda z:ll(u,z),v)
  u,v=m.findroot(score,(m.log(s),m.log(c)),tol=m.mpf('1e-85'),maxsteps=100);K=k;C=m.exp(v);S=m.exp(u);mask=[False,True,True]
 if inverse:S=1/S
 records.append(dict(kind=kind,inverse=inverse,data=[1/v if inverse else v for v in raw],free=mask,digits=110,reference=dict(shape1=m.nstr(K,95),shape2=m.nstr(C,95),scale=m.nstr(S,95))))
(P/'weak-hold-references.json').write_text(json.dumps(records,indent=2)+'\n');path=root/'univariateML-cpp/include/cppunivariate/distributions/burr/test.cpp';s=path.read_text();name='Burr weak-coordinate holds preserve native coefficient accuracy'
marker='\nTEST_CASE("'+name+'")'
if marker in s:
 prefix,tail=s.split(marker,1);assert '\nTEST_CASE(' not in tail;s=prefix
if True:
 s+='\nTEST_CASE("'+name+'") {\n'
 for r in records:
  t=r['reference'];s+='  { const std::vector<double> x{'+','.join(map(repr,r['data']))+'};\n'
  for field,key,index in [('k','shape1',0),('c','shape2',1),('scale','scale',2)]:s+='    double '+field+'='+m.nstr(m.mpf(t[key]),36)+('*(1+1e-6)' if r['free'][index] else '')+';\n'
  mask='{{'+','.join('true' if v else 'false' for v in r['free'])+'}}'
  s+='    REQUIRE(cppunivariate::burr_local_accuracy(mltest::as_span(x),k,c,scale,'+mask+',1e-10,1000,'+('true' if r['inverse'] else 'false')+'));\n'
  for field,key in [('k','shape1'),('c','shape2'),('scale','scale')]:s+='    CHECK('+field+'==doctest::Approx('+m.nstr(m.mpf(t[key]),36)+').scale(0.).epsilon(1e-10));\n'
  s+='  }\n'
 path.write_text(s+'}\n')
p=P/'focused_tests.py';s=p.read_text()
if "'Burr weak-coordinate holds*'" not in s:s=s.replace('patterns=[',"patterns=['Burr weak-coordinate holds*',");p.write_text(s)
print('Weak-coordinate hold references and regressions',len(records),flush=True)
