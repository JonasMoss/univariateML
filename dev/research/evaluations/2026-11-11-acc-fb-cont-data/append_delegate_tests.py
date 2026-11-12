"""Append independent-reference regressions once; never rewrite prior tests."""
from pathlib import Path
import json,sys
P=Path(__file__).resolve().parent;ROOT=P.parents[3];cases={str(c['index']):c for c in json.loads((P/'cases.json').read_text())};stem=sys.argv[1] if len(sys.argv)>1 else 'current-native';refs={r['id']:r for r in json.loads((P/(stem+'-references.json')).read_text()) if 'reference' in r}
for family in ['gb2','fpareto','burr','betapr']:
 key='strict-seed-'+family+('-nonunit-power' if family=='burr' else '-reverse-rare' if family=='betapr' else '-small-power');c=cases[key];r=refs[key]['reference'];name={'gb2':'GB2 small-power delegate propagates native coefficient accuracy','fpareto':'Feller Pareto shifted small-power delegate preserves coefficient accuracy','burr':'Burr nonunit held-power fallback meets native coefficient accuracy','betapr':'Beta prime inverse gamma fallback meets coefficient accuracy'}[family]
 path=ROOT/('univariateML-cpp/include/cppunivariate/distributions/'+family+'/test.cpp');old=path.read_text()
 if 'TEST_CASE("'+name+'")' in old:continue
 if family=='burr':call='cppunivariate::burr_mle_holds(mltest::as_span(x),NAN,2,NAN,1e-10,1000)'
 elif family=='gb2':call='cppunivariate::gb2_mle(mltest::as_span(x),1e-10,64,NAN,.5,'+repr(c['held']['shape3'])+',NAN)'
 elif family=='betapr':call='cppunivariate::betapr_mle_holds(mltest::as_span(x),NAN,'+repr(c['held']['shape2'])+',NAN,1e-10,64)'
 else:call='cppunivariate::fpareto_mle(mltest::as_span(x),-1,1e-10,64,NAN,.5,'+repr(c['held']['shape3'])+',NAN)'
 status='BurrStatus' if family=='burr' else 'BetaPrimeStatus' if family=='betapr' else 'Gb2Status'
 text='\nTEST_CASE("'+name+'") {\n  const std::vector<double> x{'+','.join(map(repr,c['data']))+'};\n  const auto r='+call+';\n  REQUIRE(r.status==cppunivariate::'+status+'::Ok);CHECK_FALSE(r.certified);\n  // Independent high-precision stationary point of the original density.\n'
 for k in ['shape1','scale']:
  member='alpha' if family=='betapr' and k=='shape1' else k
  text+='  CHECK(r.'+member+'==doctest::Approx('+r[k]+').scale(0.).epsilon(1e-10));\n'
 for k,v in c['held'].items():
  member='beta' if family=='betapr' and k=='shape2' else k
  text+='  CHECK(r.'+member+'=='+repr(v)+');\n'
 path.write_text(old+text+'}\n');print('appended',family,name,flush=True)
