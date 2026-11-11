import gzip,json
from pathlib import Path
D=Path(__file__).resolve().parent;E=D.parent
cases=[c for c in json.loads(gzip.decompress((E/'2026-11-07-fit-audit-data/cases.json.gz').read_bytes())) if c['family'] in ('sged','sstd','snorm')]
archived={c['index']:c for c in cases}
cert={r['index'] for r in map(json.loads,(E/'2026-11-11-acc-fb-t-data/after-native.jsonl').read_text().splitlines()) if r['certified']==1}
for c in json.loads((E/'2026-11-11-acc-fb-t-data/cases.json').read_text()):
 if c['family']=='sged' and c['index'] in cert:
  assert c['data']==archived[c['index']]['data'] and c['held']==archived[c['index']]['held']
  c=dict(c);c['reference_index']=c['index'];c['index']+=100000;cases.append(c)
for i,f in enumerate(('sged','sstd','snorm')):
 g=json.loads((E.parents[2]/'univariateML-cpp/include/cppunivariate/distributions'/f/'golden.json').read_text())
 cases.append(dict(index=200000+i,family=f,held={},data=g['data'],sample='golden'))
for j,nu in enumerate((.3,.5,.9)):
 for k,xi in enumerate((.3,1.3,3.)):
  cases.append(dict(index=210000+3*j+k,family='sged',held=dict(mean=.1,nu=nu,xi=xi),data=[-3.,-1.,-.2,0.,.3,.4,2.],sample='subunit-cusp-or-smooth'))
(D/'cases.json').write_text(json.dumps(cases,indent=2)+'\n')
with (D/'inputs.txt').open('w') as out:
 for c in cases:
  keys=['mean','sd']+(['nu'] if c['family']!='snorm' else [])+['xi']
  out.write(' '.join(map(str,[c['index'],c['family'],len(c['data']),*[c['held'].get(k,'nan') for k in keys],*c['data']]))+'\n')
