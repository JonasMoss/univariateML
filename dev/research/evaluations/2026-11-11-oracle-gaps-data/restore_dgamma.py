"""Restore only independently measured DGamma rows after the local CDF fix."""
import json,math,sys
from pathlib import Path
import audit
from accounting import payload
from quality.truth import input_sha256
HERE=audit.HERE;folder=audit.DISTS/'dgamma'
for mode in ('native','double'):
 receipt=json.loads((HERE/f'fixed-dgamma-{mode}-summary.json').read_text())
 assert receipt['checks']==11431 and receipt['failures']==0
path=folder/'truth.json';table=json.loads(path.read_text());before=payload(table,'values')
rows=table.get('excluded_values',[])
assert len(rows)==6 and all(r['reason'].startswith('TASK-196 open:') for r in rows)
for entry in rows:table['values'].append(entry['row'])
del table['excluded_values']
assert payload(table,'values')==before
route=folder/'truth.py';source=route.read_text();marker='\n\n# Independently measured production limitations; numerical rows remain archived.\n'
assert source.count(marker)==1
source,tail=source.split(marker);assert tail.startswith('truth_exclusions = {}\n')
route.write_text(source+'\n')
table['input_sha256']=input_sha256(folder);path.write_text(json.dumps(table,indent=2,allow_nan=False)+'\n')
records=json.loads((HERE/'row-exclusions.json').read_text())
removed=[r for r in records if r['family']=='dgamma'];assert len(removed)==6
(HERE/'dgamma-closed-exclusions.json').write_text(json.dumps(removed,indent=2)+'\n')
(HERE/'row-exclusions.json').write_text(json.dumps([r for r in records if r['family']!='dgamma'],indent=2)+'\n')
row=next(e['row'] for e in rows if e['row'][1]==100114683.)
with (folder/'test.cpp').open('a') as out:
 out.write('\nTEST_CASE("dgamma: concentrating bins retain accurate gamma endpoints") {\n')
 out.write(' CHECK(cppunivariate::distributions::ddgamma(99629986,1e8,1,true)==doctest::Approx(-696.3688404012099).epsilon(1e-10));\n')
 out.write(' CHECK(cppunivariate::distributions::pdgamma(100114683,1e8,1,false,false)==doctest::Approx('+repr(math.exp(float(row[4])))+').epsilon(1e-10).scale(0));\n}\n')
with (folder/'README.md').open('a') as out:
 out.write('\nAt shape 1e8 and unit rate, generic incomplete-gamma endpoint errors were\namplified by subtraction over one count bin. The CDF and bin endpoints use\nthe protected unit-gamma CDF; the independent bin integrals resolve the\nprevious double-precision discrepancies without a midpoint approximation.\n')
print('Restored 6 DGamma value rows; exact numerical payload unchanged.')
