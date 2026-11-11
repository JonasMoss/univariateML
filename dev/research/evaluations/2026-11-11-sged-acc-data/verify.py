"""Acceptance assertions, independent roots and density values; no widened tolerances."""
import json,sys,math
from pathlib import Path
import mpmath as m
D=Path(__file__).resolve().parent
sys.path.insert(0,str(D));from check import density
m.mp.dps=110
cs={r['index']:r for r in json.loads((D/'cases.json').read_text())}
s=json.loads((D/'summary.json').read_text());roots=json.loads((D/'all-certified-roots.json').read_text())
assert not s['component_error_increases'],s['component_error_increases']
report={}
for mode in ('native','double'):
 before={r['index']:r for r in json.loads((D/f'before-{mode}-checked.json').read_text())}
 after={r['index']:r for r in json.loads((D/f'after-{mode}-checked.json').read_text())}
 assert not s[mode]['new_stops']
 assert all(r['unchanged'] for r in s[mode]['goldens'])
 assert all(cs[i]['held'] for i in s[mode]['changed_parameters'])
 for family,g in s[mode]['groups'].items():
  assert not g['after']['fitter_misses'] and not g['after']['dpqr_misses']
 exceptions=[]
 for i,r in after.items():
  if r['error']:continue
  assert r['dpqr_error'] is not None and r['dpqr_error']<=1e-10
  if r['fitter_error']>1e-10:
   assert r['family']=='sged' and r['cusp']
   value=m.mpf(r['reference']);loss=m.mpf(r['logLik'])-value
   allowance=len(cs[i]['data'])*m.power(64*m.mpf(2)**-52,r['parameters'][2])
   assert loss>=-m.mpf('1e-12')*max(1,abs(value)) and loss<=allowance
   exceptions.append(dict(index=i,nu=r['parameters'][2],xi=r['parameters'][3],loss=float(loss),allowance=float(allowance),dpqr_error=r['dpqr_error']))
 group=[r for r in roots['rows'] if r['mode']==mode and r['phase']=='after']
 assert all(not r['error'] and r['certified']==1 and r['maximum_scaled_error']<=1e-10 for r in group)
 supremum=[]
 for r in group:
  c=cs[r['index']];t=list(map(m.mpf,r['reference']));x=list(map(m.mpf,c['data']))
  value=density('sged',x,t);actual=after[r['index']]['logLik']
  error=float(abs(m.mpf(actual)-value)/max(1,abs(value)))
  assert error<=1e-10,(r['index'],error)
  supremum.append(error)
 report[mode]=dict(datasets=len(after),successes=sum(not r['error'] for r in after.values()),no_new_stops=True,goldens_unchanged=True,all_free_parameters_unchanged=True,all_certified_roots_checked=len(group),unique_certified_roots=len({cs[r['index']].get('reference_index',r['index']) for r in group}),maximum_coefficient_error=max(r['maximum_scaled_error'] for r in group),maximum_certified_supremum_error=max(supremum),cusp_exceptions=exceptions)
(D/'verification.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
