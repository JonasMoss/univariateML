#!/usr/bin/env python3
"""Append frozen raw-data, independent-reference regressions (run once)."""
import json,pathlib,sys
from reference import CASES,NAMES
HERE=pathlib.Path(__file__).resolve().parent;ROOT=HERE.parents[3]
refs=json.loads((HERE/'reference.json').read_text())
if len(sys.argv)>1:refs=[r for r in refs if r['index'] in set(map(int,sys.argv[1:]))]
CPP={'invgamma':('invgamma_mle','GammaStatus',['shape','scale']),'naka':('naka_mle','GammaStatus',['shape','scale']),'lgamma':('lgamma_mle','LgammaStatus',['shapelog','ratelog']),'lnorm':('lognormal_mle','LognormalStatus',['meanlog','sdlog']),'logitnorm':('logitnorm_mle','LogitnormStatus',['mu','sigma']),'llogis':('loglogistic_mle','LogLogisticStatus',['shape','scale']),'invweibull':('invweibull_mle','InvWeibullStatus',['shape','scale']),'invburr':('invburr_mle_holds','InvBurrStatus',['shape1','shape2','scale'])}
def num(v):return repr(float(v))
for family in NAMES:
 if not any(r["family"]==family for r in refs):continue
 fun,status,fields=CPP[family];cpp='\n// Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.\n';r='\n# Independent 180/360-digit raw-model references: TASK-101.7.5 evidence.\n'
 for ref in refs:
  if ref['family']!=family:continue
  i=ref['index'];c=CASES[i];data=', '.join(map(repr,c['data']));held=[num(c['held'][k]) if k in c['held'] else 'NAN' for k in NAMES[family]]
  args='x' if family=='lnorm' else 'x, 1e-10, 200'
  if family=='logitnorm':args='x'
  if family=='invburr':args='x, '+', '.join(held)+', 1e-10, 200'
  if c['held'] and family=='invweibull':args+=', NAN, '+', '.join(held)
  cpp+=f'TEST_CASE("{family}: raw transform audit case {i}") {{\n  const std::vector<double> data{{{data}}};\n  const auto x=mltest::as_span(data);\n  const auto fit=cppunivariate::{fun}({args});\n'
  r+=f'test_that("raw transform audit case {i}", {{\n  x <- c({data})\n'
  controls={'invgamma':'abstol','naka':'abstol','lgamma':'abstol','llogis':'abstol','invweibull':'reltol'}
  rargs='x'+''.join(f', {k} = {repr(v)}' for k,v in c['held'].items())
  if family in controls:rargs+=f', {controls[family]} = 1e-10, iterlim = 200'
  if family=='invburr':rargs+=', abstol = 1e-10, iterlim = 200'
  # Verify the actual public argument name from metadata before running.
  if i==4283:
   cpp+=f'  CHECK(fit.status==cppunivariate::{status}::IterationFailure);\n}}\n'
   r+=f'  expect_error(ml{family}({rargs}))\n}})\n';continue
  cpp+=f'  REQUIRE(fit.status==cppunivariate::{status}::Ok);\n  CHECK(fit.certified);\n'
  r+=f'  fit <- ml{family}({rargs})\n'
  for name,field,value in zip(NAMES[family],fields,ref['theta']):
   cpp+=f'  CHECK(fit.{field}==doctest::Approx({num(value)}).epsilon(1e-10).scale(0));\n'
   r+=f'  expect_lte(abs(unname(fit["{name}"]) / ({num(value)}) - 1), 1e-10)\n'
  cpp+=f'  CHECK(fit.logLik==doctest::Approx({num(ref["logLik"])}).epsilon(1e-10).scale(0));\n}}\n'
  r+=f'  expect_lte(abs(as.numeric(logLik(fit)) - ({num(ref["logLik"])})) / max(1, abs({num(ref["logLik"])})), 1e-10)\n}})\n'
 with (ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{family}/test.cpp').open('a') as f:f.write(cpp)
 with (ROOT/f'bindings/r/distributions/{family}/test.R').open('a') as f:f.write(r)
