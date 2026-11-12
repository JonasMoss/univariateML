"""Retain the two deeper Gamma-face cases without changing existing tests."""
from pathlib import Path
import json,mpmath as m
P=Path(__file__).resolve().parent;root=P.parents[3]
items=json.loads((P/'betapr-seam-probe.json').read_text())[-2:]
for name in ['cases.json','spot-cases.json']:
 rows=json.loads((P/name).read_text());ids={str(r['index']) for r in rows}
 for r in items:
  key='strict-seed-'+r['id']
  if key not in ids:rows.append(dict(index=key,family='betapr',data=r['data'],held=r['held'],tol=1e-10,iterlim=1000,source='Deeper Gamma-face coefficient-wise remainder regression',control=dict(gradtol=1e-10,iterlim=1000)))
 (P/name).write_text(json.dumps(rows,indent=2)+'\n')
f=root/'univariateML-cpp/include/cppunivariate/distributions/betapr/test.cpp';s=f.read_text();name='Beta prime deeper Gamma face uses coefficient-specific error bounds'
if 'TEST_CASE("'+name+'")' not in s:
 s+='\nTEST_CASE("'+name+'") {\n'
 for r in items:
  s+='  { const std::vector<double> x{'+','.join(map(repr,r['data']))+'};\n'
  s+='    const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),'+repr(r['held']['shape1'])+',NAN,NAN,1e-10,1000);\n'
  s+='    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);CHECK_FALSE(r.certified);\n'
  for field,key in [('beta','shape2'),('scale','scale')]:s+='    CHECK(r.'+field+'==doctest::Approx('+m.nstr(m.mpf(r['reference'][key]),17)+').scale(0.).epsilon(1e-10));\n'
  s+='  }\n'
 f.write_text(s+'}\n')
f=P/'focused_tests.py';s=f.read_text()
if "'Beta prime deeper Gamma face*'" not in s:f.write_text(s.replace('patterns=[',"patterns=['Beta prime deeper Gamma face*',"))
print('Added deeper Gamma cases',len(items),flush=True)
