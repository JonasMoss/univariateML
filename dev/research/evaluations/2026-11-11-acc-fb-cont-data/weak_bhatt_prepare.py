"""Retain near-normal Bhatt fixtures; append tests without changing the old prefix."""
from pathlib import Path
import json,mpmath as m
P=Path(__file__).resolve().parent;root=P.parents[3];m.mp.dps=180
rows=json.loads((P/'bhatt-critical-width-references.json').read_text())
rows=[r for r in rows if not r.get('held')]
for i,r in enumerate(rows):
 r['id']='strict-seed-bhatt-critical-'+('asym-' if r.get('asymmetric') else 'sym-')+repr(r['e']);r['family']='bhatt';r['held']={}
held=dict(rows[1],id='strict-seed-bhatt-critical-held-mu',held={'mu':0.});held.pop('baseline',None);held.pop('builds',None);rows.append(held)
(P/'bhatt-critical-width-references.json').write_text(json.dumps(rows,indent=2)+'\n')
inputs=[{k:r[k] for k in ['id','family','data','held','e'] if k in r} for r in rows]
(P/'bhatt-critical-width-inputs.json').write_text(json.dumps(inputs,indent=2)+'\n')
for file in ['cases.json','spot-cases.json']:
 a=json.loads((P/file).read_text());ids={str(c['index']) for c in a}
 for r in rows:
  if r['id'] not in ids:a.append(dict(index=r['id'],family='bhatt',data=r['data'],held=r['held'],tol=1e-10,iterlim=1000,source='Near-normal Bhatt fourth-moment critical stress',control=dict(reltol=1e-10,iterlim=1000)))
 (P/file).write_text(json.dumps(a,indent=2)+'\n')
f=root/'univariateML-cpp/include/cppunivariate/distributions/bhatt/test.cpp';s=f.read_text();name='Bhatt near-normal width uses compensated conditional information'
if 'TEST_CASE("'+name+'")' not in s:
 s+='\nTEST_CASE("'+name+'") {\n'
 for r in rows:
  s+='  { const std::vector<double> x{'+','.join(map(repr,r['data']))+'};\n'
  s+='    const auto r=cppunivariate::bhatt_mle(mltest::as_span(x),1e-10,1000,NAN,NAN,'+('0.' if r['held'] else 'NAN')+');\n'
  s+='    REQUIRE(r.status==cppunivariate::BhattStatus::Ok);CHECK_FALSE(r.certified);\n'
  for key in ['mu','sigma','a']:
   s+='    CHECK(r.'+key+'==doctest::Approx('+m.nstr(m.mpf(r['reference'][key]),40)+').scale('+('1.' if key in ['mu','a'] else '0.')+').epsilon(1e-10));\n'
  s+='  }\n'
 f.write_text(s+'}\n')
f=P/'focused_tests.py';s=f.read_text()
if "'Bhatt near-normal width*'" not in s:f.write_text(s.replace('patterns=[',"patterns=['Bhatt near-normal width*',"))
f=P/'reference.py';s=f.read_text()
if "key.startswith('strict-seed-bhatt-critical-')" not in s:s=s.replace('  c=cases[key]; guess=',"  if key.startswith('strict-seed-bhatt-critical-'):continue # Dedicated 180-digit, scaled raw-CDF reference.\n  c=cases[key]; guess=")
f.write_text(s)
f=P/'measure.py';s=f.read_text()
if "'bhatt-critical-width-references.json'" not in s:s=s.replace("'special-references.json',","'special-references.json','bhatt-critical-width-references.json',")
f.write_text(s)
print('Near-normal Bhatt fixtures',len(rows),flush=True)
