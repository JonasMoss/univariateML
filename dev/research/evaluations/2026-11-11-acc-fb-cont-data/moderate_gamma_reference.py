"""Independent 100-digit original-density roots beyond the small-ratio series."""
from pathlib import Path
import json,mpmath as m
P=Path(__file__).resolve().parent;root=P.parents[3];m.mp.dps=100
cases=[c for c in json.loads((P/'cases.json').read_text()) if str(c['index']).startswith('strict-seed-gamma-moderate')];out=[]
for c in cases:
 x=list(map(m.mpf,c['data']));n=len(x);p=m.mpf(3);top=x[-1]
 def likelihood(u,v):
  q=m.exp(u);s=m.exp(v)
  return -n*(m.loggamma(p)+m.loggamma(q)-m.loggamma(p+q)+p*v)-(p+q)*((n-1)*m.log1p(1/s)+m.log1p(top/s))
 def score(u,v):return m.diff(lambda z:likelihood(z,v),u),m.diff(lambda z:likelihood(u,z),v)
 u,v=m.findroot(score,(m.log(50),m.log(50*m.fsum(x)/n/p)),tol=m.mpf('1e-85'),maxsteps=100)
 q=m.exp(u);s=m.exp(v);assert q>0 and s>0
 H00=-m.diff(lambda z:likelihood(z,v),u,2);H11=-m.diff(lambda z:likelihood(u,z),v,2);H01=-m.diff(lambda z:m.diff(lambda t:likelihood(z,t),v),u)
 assert H00>0 and H00*H11-H01**2>0
 out.append(dict(id=c['index'],digits=100,reference=dict(shape1='3',shape2=m.nstr(q,90),scale=m.nstr(s,90)),maximum_observation_ratio=m.nstr(top/s,40),information_determinant=m.nstr(H00*H11-H01**2,40)))
(P/'moderate-gamma-references.json').write_text(json.dumps(out,indent=2)+'\n')
pth=root/'univariateML-cpp/include/cppunivariate/distributions/betapr/test.cpp';source=pth.read_text();name='Beta prime compensated finish handles non-small observation ratios'
if 'TEST_CASE("'+name+'")' not in source:
 source+='\nTEST_CASE("'+name+'") {\n'
 for c,r in zip(cases,out):
  source+='  { std::vector<double> x(99,1.); x.push_back('+repr(c['data'][-1])+');\n'
  source+='    double p=3,q=64,s=64*(99+x.back())/300,ll=NAN;\n'
  source+='    REQUIRE(cppunivariate::burr_power_one_detail::beta_gamma_fit(mltest::as_span(x),p,q,s,{{false,true,true}},1e-10,1000,ll));\n'
  for field,key in [('q','shape2'),('s','scale')]:source+='    CHECK('+field+'==doctest::Approx('+r['reference'][key][:36]+').scale(0.).epsilon(1e-10));\n'
  source+='  }\n'
 pth.write_text(source+'}\n')
pth=P/'focused_tests.py';s=pth.read_text()
if "'Beta prime compensated finish handles*'" not in s:s=s.replace('patterns=[',"patterns=['Beta prime compensated finish handles*',");pth.write_text(s)
print(json.dumps(out,indent=2),flush=True)
