#!/usr/bin/env python3
"""Append frozen high-precision fixtures; never rewrite preceding tests."""
import json,pathlib,math
HERE=pathlib.Path(__file__).resolve().parent;ROOT=HERE.parents[3]
refs=json.loads((HERE/'reference.json').read_text());marker='TASK-101.7.3 raw likelihood regressions'
def number(v):return repr(float(v))
for family,names in {'betapr':['shape1','shape2','scale'],'nsbeta':['shape1','shape2','min','max'],'pareto4':['shape1','shape2','scale','min']}.items():
 rows=[r for r in refs if r['family']==family and r['index']!=272]
 p=ROOT/'univariateML-cpp/include/cppunivariate/distributions'/family/'test.cpp'
 fields=['a','b']+(['s'] if family=='betapr' else ['lo','hi'] if family=='nsbeta' else ['s','lo'])
 s='\n// Raw score roots checked independently at 350 and 700 digits.\n'
 s+=f'TEST_CASE("{family}: {marker}") {{\n'
 s+='  struct Fixture { int index; std::vector<double> x; double '+','.join(fields)+',ll; };\n'
 s+='  const std::vector<Fixture> fixtures{\n'
 for r in rows:s+='    {'+str(r['index'])+',{'+','.join(map(number,r['data']))+'},'+','.join(map(number,r['theta']+[r['logLik']]))+'},\n'
 s+='  };\n  for(const auto& f:fixtures) {\n    CAPTURE(f.index);\n'
 if family=='betapr':s+='    const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(f.x),NAN,NAN,f.s);\n    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);\n';members=['alpha','beta','scale']
 elif family=='nsbeta':s+='    const bool first=f.index==5348 || f.index==10791 || f.index==10792;\n    const auto r=cppunivariate::nsbeta_mle(mltest::as_span(f.x),f.lo,f.hi,1e-10,1000,first?f.a:NAN,first?NAN:f.b);\n    REQUIRE(r.status==cppunivariate::BetaStatus::Ok);\n';members=['shape1','shape2','min','max']
 else:s+='    const auto r=cppunivariate::pareto4_mle(mltest::as_span(f.x),f.lo,1e-10,1000,NAN,f.a,NAN);\n    REQUIRE(r.status==cppunivariate::BurrStatus::Ok);\n';members=['shape1','shape2','scale','min']
 for field,member in zip(fields,members):s+=f'    CHECK(r.{member}==doctest::Approx(f.{field}).epsilon(1e-10));\n'
 s+='    CHECK(r.logLik==doctest::Approx(f.ll).epsilon(1e-10));\n  }\n}\n'
 if marker not in p.read_text():
  with p.open('a') as f:f.write(s)
 p=ROOT/'bindings/r/distributions'/family/'test.R'
 s=f'\ntest_that("{family}: {marker}", {{\n  fixtures <- list(\n'
 literals=[]
 for r in rows:
  literals.append('    list(index='+str(r['index'])+', x=c('+','.join(map(number,r['data']))+'),\n         held=list('+','.join(k+'='+number(v) for k,v in r['held'].items())+'),\n         theta=c('+','.join(k+'='+number(v) for k,v in zip(names,r['theta']))+'), logLik='+number(r['logLik'])+')')
 s+=',\n'.join(literals)+'\n  )\n  for (f in fixtures) {\n    fit <- do.call(ml'+family+', c(list(x=f$x), f$held))\n    expect_equal(as.numeric(fit[names(f$theta)])/unname(f$theta),\n                 rep(1,length(f$theta)), tolerance=1e-10)\n    expect_equal(attr(fit,"logLik")/f$logLik, 1, tolerance=1e-10)\n  }\n})\n'
 # Zero held thresholds have exact-zero comparisons, avoiding 0/0.
 s=s.replace('as.numeric(fit[names(f$theta)])/unname(f$theta),\n                 rep(1,length(f$theta))','as.numeric(fit[names(f$theta)])/pmax(abs(unname(f$theta)),1e-300),\n                 unname(f$theta)/pmax(abs(unname(f$theta)),1e-300)')
 if marker not in p.read_text():
  with p.open('a') as f:f.write(s)

# The replay's additional held-shape/scale coefficient seam.
r=next(r for r in refs if r['index']==272)
extra='TASK-101.7.3 held-shape scale increment regression'
p=ROOT/'univariateML-cpp/include/cppunivariate/distributions/betapr/test.cpp'
s='\nTEST_CASE("betapr: '+extra+'") {\n  const std::vector<double> x{'+','.join(map(number,r['data']))+'};\n'
s+='  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),'+number(r['held']['shape1'])+',NAN,'+number(r['held']['scale'])+');\n'
s+='  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);\n'
for member,value in zip(['alpha','beta','scale'],r['theta']):s+='  CHECK(r.'+member+'==doctest::Approx('+number(value)+').epsilon(1e-10));\n'
s+='  CHECK(r.logLik==doctest::Approx('+number(r['logLik'])+').epsilon(1e-10));\n}\n'
if extra not in p.read_text():
 with p.open('a') as f:f.write(s)
p=ROOT/'bindings/r/distributions/betapr/test.R'
s='\ntest_that("betapr: '+extra+'", {\n  fit <- mlbetapr(c('+','.join(map(number,r['data']))+'),shape1='+number(r['held']['shape1'])+',scale='+number(r['held']['scale'])+')\n'
s+='  expect_equal(unname(as.numeric(fit))/c('+','.join(map(number,r['theta']))+'),rep(1,3),tolerance=1e-10)\n'
s+='  expect_equal(attr(fit,"logLik")/'+number(r['logLik'])+',1,tolerance=1e-10)\n})\n'
if extra not in p.read_text():
 with p.open('a') as f:f.write(s)
