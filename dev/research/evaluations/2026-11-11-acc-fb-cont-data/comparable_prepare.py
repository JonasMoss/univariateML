"""Independent dense-shape and small-power transport fixtures (100 digits)."""
from pathlib import Path
import json,mpmath as m
P=Path(__file__).resolve().parent;root=P.parents[3];m.mp.dps=100;items=json.loads((P/'comparable-beta-probe.json').read_text());records=[];public=[]
for i,row in enumerate(items):
 rec=dict(id='strict-seed-comparable-betapr-'+str(i),family='betapr',data=row['data'],held={},reference=row['reference'],digits=100);records.append(rec);public.append(rec)
p=float(items[0]['reference']['shape1']);a=float(1e-4)
for family in ['betapr-held','gb2','fpareto']:
 power=1. if family=='betapr-held' else a;shift= -1e-100 if family=='fpareto' else 0
 raw=items[0]['data'] if family=='betapr-held' else [float(m.exp(m.log(m.mpf(v))/m.mpf(power)))+shift for v in items[0]['data']]
 x=[m.mpf(v)-m.mpf(shift) for v in raw];pp=m.mpf(p);aa=m.mpf(power)
 def ll(u,v):
  q=m.exp(u);return -20*(m.loggamma(pp)+m.loggamma(q)-m.loggamma(pp+q))+m.fsum(pp*aa*(m.log(t)-v)-(pp+q)*m.log1p(m.exp(aa*(m.log(t)-v))) for t in x)
 def score(u,v):return m.diff(lambda t:ll(t,v),u),m.diff(lambda t:ll(u,t),v)
 u,v=m.findroot(score,(m.log(m.mpf(items[0]['reference']['shape2'])),m.log(m.mpf(items[0]['reference']['scale']))/aa),tol=m.mpf('1e-75'),maxsteps=100)
 q=m.exp(u);s=m.exp(v)
 if family=='betapr-held':
  rec=dict(id='strict-seed-comparable-betapr-held',family='betapr',data=raw,held=dict(shape1=p),reference=dict(shape1=m.nstr(pp,85),shape2=m.nstr(q,85),scale=m.nstr(s,85)),digits=100);public.append(rec)
 else:
  ref=dict(shape1=m.nstr(q,85),shape2=m.nstr(aa,85),shape3=m.nstr(pp,85),scale=m.nstr(s,85));held=dict(shape2=power,shape3=p)
  if shift:ref['min']=m.nstr(m.mpf(shift),85);held['min']=shift
  rec=dict(id='direct-comparable-'+family,family=family,data=raw,held=held,reference=ref,digits=100)
 records.append(rec)
(P/'comparable-references.json').write_text(json.dumps(records,indent=2)+'\n')
for file in ['cases.json','spot-cases.json']:
 rows=json.loads((P/file).read_text());ids={str(c['index']) for c in rows}
 for r in public:
  if r['id'] not in ids:rows.append(dict(index=r['id'],family=r['family'],data=r['data'],held=r['held'],tol=1e-10,iterlim=1000,source='Comparable large beta-prime shapes',control=dict(gradtol=1e-10,iterlim=1000)))
 (P/file).write_text(json.dumps(rows,indent=2)+'\n')
path=root/'univariateML-cpp/include/cppunivariate/distributions/betapr/test.cpp';source=path.read_text();name='Beta prime comparable large shapes retain coefficient accuracy'
if 'TEST_CASE("'+name+'")' not in source:
 source+='\nTEST_CASE("'+name+'") {\n'
 for r in public:
  ref=r['reference'];held=r['held'].get('shape1');source+='  { const std::vector<double> x{'+','.join(map(repr,r['data']))+'};\n'
  ps=repr(held) if held else ref['shape1'][:36]+'*(1+1e-5)';qs=ref['shape2'][:36]+'*(1-1e-5)';ss=ref['scale'][:36]+'*(1+1e-5)'
  source+='    cppunivariate::BetaPrimeResult proposal{'+ps+','+qs+','+ss+',0,0,cppunivariate::BetaPrimeStatus::Ok};proposal.certified=false;\n'
  source+='    const auto r=cppunivariate::betapr_detail::accurate_fallback(proposal,mltest::as_span(x),'+(repr(held) if held else 'NAN')+',NAN,NAN,1e-10,1000);\n    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);CHECK_FALSE(r.certified);\n'
  for field,key in [('alpha','shape1'),('beta','shape2'),('scale','scale')]:source+='    CHECK(r.'+field+'==doctest::Approx('+ref[key][:36]+').scale(0.).epsilon(1e-10));\n'
  source+='  }\n'
 path.write_text(source+'}\n')
for family in ['gb2','fpareto']:
 r=next(r for r in records if r['family']==family);ref=r['reference'];path=root/('univariateML-cpp/include/cppunivariate/distributions/'+family+'/test.cpp');source=path.read_text();name='Comparable beta shapes '+family+' transport propagates small-power error'
 if 'TEST_CASE("'+name+'")' in source:continue
 source+='\nTEST_CASE("'+name+'") {\n  const std::vector<double> x{'+','.join(map(repr,r['data']))+'};\n'
 source+='  cppunivariate::Gb2Result r{'+ref['shape1'][:36]+'*(1+1e-5),1e-4,'+repr(p)+','+ref['scale'][:36]+'*(1+1e-5),0,0,cppunivariate::Gb2Status::Ok};r.certified=false;\n'
 if family=='gb2':call='cppunivariate::gb2_transport_accuracy(mltest::as_span(x),0,r,{{true,false,false,true}},1e-10,1000,true)'
 else:call='cppunivariate::fpareto_transport_accuracy(mltest::as_span(x),-1e-100,r,{{true,false,false,true}},1e-10,1000)'
 source+='  REQUIRE('+call+');\n'
 for field in ['shape1','scale']:source+='  CHECK(r.'+field+'==doctest::Approx('+ref[field][:36]+').scale(0.).epsilon(1e-10));\n'
 source+='  CHECK(r.shape2==1e-4);CHECK(r.shape3=='+repr(p)+');\n}\n';path.write_text(source)
pth=P/'focused_tests.py';s=pth.read_text()
for pattern in ['Beta prime comparable large shapes*','Comparable beta shapes*']:
 if repr(pattern) not in s:s=s.replace('patterns=[','patterns=['+repr(pattern)+',')
pth.write_text(s)
pth=P/'reference.py';s=pth.read_text()
if "'strict-seed-comparable-'" not in s:s=s.replace("'strict-seed-coupled-'","'strict-seed-coupled-','strict-seed-comparable-'")
pth.write_text(s)
print('Comparable-shape references and regressions',len(records),flush=True)
