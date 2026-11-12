"""Append the held-sigma critical-width and signed-normal regressions."""
from pathlib import Path
import json,mpmath as m
P=Path(__file__).resolve().parent;root=P.parents[3];m.mp.dps=180
rows=json.loads((P/'bhatt-held-sigma-critical-references.json').read_text())
for file in ['cases.json','spot-cases.json']:
 a=json.loads((P/file).read_text());ids={str(c['index']) for c in a}
 for r in rows:
  if r['id'] not in ids:a.append(dict(index=r['id'],family='bhatt',data=r['data'],held=r['held'],tol=1e-10,iterlim=1000,source='Held-sigma Bhatt quadratic-moment critical stress',control=dict(reltol=1e-10,iterlim=1000)))
 (P/file).write_text(json.dumps(a,indent=2)+'\n')
f=root/'univariateML-cpp/include/cppunivariate/distributions/bhatt/test.cpp';s=f.read_text();name='Bhatt held-sigma near-normal width uses compensated information'
if 'TEST_CASE("'+name+'")' not in s:
 s+='\nTEST_CASE("'+name+'") {\n'
 for r in rows:
  s+='  { const std::vector<double> x{'+','.join(map(repr,r['data']))+'};\n'
  s+='    const auto r=cppunivariate::bhatt_mle(mltest::as_span(x),1e-10,1000,1.,NAN,'+('0.' if 'mu' in r['held'] else 'NAN')+');\n'
  s+='    REQUIRE(r.status==cppunivariate::BhattStatus::Ok);CHECK_FALSE(r.certified);\n'
  for key in ['mu','sigma','a']:s+='    CHECK(r.'+key+'==doctest::Approx('+m.nstr(m.mpf(r['reference'][key]),40)+').scale('+('1.' if key in ['mu','a'] else '0.')+').epsilon(1e-10));\n'
  s+='  }\n'
 s+='}\n'
name='Bhatt compensated normal members retain a signed physical likelihood'
if 'TEST_CASE("'+name+'")' not in s:
 s+='\nTEST_CASE("'+name+'") {\n'
 s+='  for(bool held:{false,true}) {\n    std::vector<double> x=held?std::vector<double>{-1.,1.}:std::vector<double>(22,0.);\n    if(!held){x.front()=-3.;x.back()=3.;}\n    cppunivariate::BhattResult r{0.,held?2.:1.,0.,0.,0,cppunivariate::BhattStatus::Ok};\n    REQUIRE(cppunivariate::bhatt_detail::pair_variance_accuracy(r,mltest::as_span(x),NAN,1e-10,1000,held));\n    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dbhatt(v,r.mu,r.sigma,r.a,true);\n    CHECK(r.logLik<0);CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-12));\n  }\n}\n'
f.write_text(s)
f=P/'focused_tests.py';s=f.read_text()
for pattern in ['Bhatt held-sigma near-normal*','Bhatt compensated normal*']:
 if repr(pattern) not in s:s=s.replace('patterns=[','patterns=['+repr(pattern)+',')
f.write_text(s)
print('Held-sigma Bhatt fixtures',len(rows),flush=True)
