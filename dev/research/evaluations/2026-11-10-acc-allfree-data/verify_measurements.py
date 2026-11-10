"""Require all four captures and matching tightened C++/R/generated JS policy."""
import json,re,sys
from pathlib import Path
D=Path(__file__).resolve().parent
root=D.parents[3]
cpp=(root/'univariateML-cpp/tests/test_support.hpp').read_text()
r=(root/'bindings/r/tests/testthat/test_golden_regression.R').read_text()
cli='--wasm-cli' in sys.argv
js={v['id']:v for v in json.loads((root/'build/js/generated/fit-cases.json').read_text())} if not cli else {}
rows=json.loads((D/'measurements.json').read_text())
for row in rows:
 ident=row['family']
 allowance=float(re.search(r'kGolden_'+ident+r'\s*=\s*([^;]+)',cpp)[1])
 r_allowance=float(re.search(r'^  '+ident+r' = ([^,]+),',r,re.M)[1])
 assert r_allowance==allowance
 if not cli:assert all(v==allowance for v in js[ident]['tolerances'].values())
 assert allowance<1e-10
 errors=[]
 for mode in ['after-native','after-double','after-r','after-wasm-cli' if cli else 'after-wasm']:
  fit=row['modes'][mode]
  assert 'error' not in fit,(ident,mode,fit)
  errors.append(fit['max_relative'])
  assert fit['max_relative']<=allowance,(ident,mode,fit,allowance)
 row['allowance']=allowance
 row['allowance_over_maximum_error']=allowance/max(errors)
 assert row['allowance_over_maximum_error']<6
(D/('golden-cli-policy-check.json' if cli else 'golden-policy-check.json')).write_text(json.dumps(rows,indent=2)+'\n')
print('PASS: 24 default fits, all below 1e-10 and calibrated allowances; allowances below six times measured error.'+(' C++/R/JS policies match.' if not cli else ' WASM CLI preflight; generated JS policy checked separately.'))
