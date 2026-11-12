"""Recheck the append-only payload and record final, report-ready counts."""
import collections,hashlib,json,math,platform,subprocess,sys
from pathlib import Path
import mpmath,sympy
import audit
HERE=audit.HERE;ROOT=audit.ROOT;DISTS=audit.DISTS;BASE='7825e912'

def old_table(path):return json.loads(subprocess.check_output(['git','show',BASE+':'+str(path.relative_to(ROOT))],cwd=ROOT,text=True))
def payload(table,category):
 out=[]
 for row in table.get(category,[]):out.append([table['parameters'][row[0]],*row[1:]])
 for e in table.get('excluded_'+category,[]):
  row=e['row'];out.append([table['parameters'][row[0]],*row[1:]])
 for point in table.get('excluded_parameter_points',[]):
  for row in point.get(category,[]):out.append([point['parameters'],*row[1:]])
 return collections.Counter(json.dumps(v,separators=(',',':')) for v in out)
def total_norm(t):return len(t.get('normalization',[]))+sum(len(p.get('normalization',[])) for p in t.get('excluded_parameter_points',[]))
def point_set(t):return t['parameters']+[p['parameters'] for p in t.get('excluded_parameter_points',[])]

def main():
 manifest=json.loads((ROOT/'dev/research/evaluations/2026-11-11-dpqr-extreme-data/oracle-gaps.json').read_text())
 totals=collections.Counter();families=[];preexisting=[]
 for path in sorted(DISTS.glob('*/truth.json')):
  family=path.parent.name;old=old_table(path);new=json.loads(path.read_text())
  if family=='gengamma':
   previous=subprocess.check_output(['git','show',BASE+':'+str(path.relative_to(ROOT))],cwd=ROOT)
   assert previous==path.read_bytes()
  record=dict(family=family,added_points=len(point_set(new))-len(point_set(old)),added_normalizations=total_norm(new)-total_norm(old))
  for category in ('values','quantiles'):
   before=payload(old,category);after=payload(new,category)
   assert not before-after,(family,category)
   record['added_'+category]=sum((after-before).values())
   totals[category]+=record['added_'+category]
  totals['points']+=record['added_points'];totals['normalizations']+=record['added_normalizations']
  families.append(record)
  for e in manifest:
   if e['family']!=family:continue
   theta=e['gap']['parameters']
   if theta in old['parameters']:
    index=old['parameters'].index(theta)
    v=sum(r[0]==index for r in old['values']);q=sum(r[0]==index for r in old['quantiles'])
    assert v>0 and q>0,(family,theta)
    preexisting.append(dict(family=family,parameters=theta,baseline_values=v,baseline_quantiles=q))
 assert totals['points']==124,totals
 assert len(preexisting)==3,preexisting
 changed=subprocess.check_output(['git','diff','--name-only'],cwd=ROOT,text=True).splitlines()
 forbidden=[p for p in changed if '/gengamma/' in p or p.endswith('/mle.hpp') or p.endswith('/golden.json') or '/special/' in p or '/core/' in p or p.startswith('bindings/') or p.startswith('dev/roadmap/') or p in ('bindings/r/NEWS.md','dev/STATUS.md','dev/KNOWN_ISSUES.md')]
 assert not forbidden,forbidden
 receipts=(ROOT/'build/scratch/oracle-final/receipts.txt').read_text().splitlines()
 required=['cpp-dev workstation 0','r-dev workstation 0','r-full workstation 0','cpp-double workstation 0','site workstation 0','js workstation 0','dgamma-cpp-native workstation 0','dgamma-cpp-double workstation 0','dgamma-r-full workstation 0','dgamma-site workstation 0','dgamma-js workstation 0']
 assert all(r in receipts for r in required),receipts
 failures=[]
 for mode in ('native','double'):
  failures.extend(dict(f,mode=mode) for f in json.loads((HERE/f'final-{mode}-failures.json').read_text()))
 grouped=collections.defaultdict(list)
 for f in failures:grouped[f['family']].append(f)
 production=[]
 for family,fs in sorted(grouped.items()):
  finite=[f['error'] for f in fs if math.isfinite(f['error'])]
  production.append(dict(family=family,points=len({tuple(f['parameters']) for f in fs}),functions=sorted({f['function'] for f in fs}),modes=sorted({f['mode'] for f in fs}),measured_failed_scalars=len(fs),nonfinite_actual=sum(not math.isfinite(f['actual']) for f in fs),maximum_finite_normalized_error=max(finite) if finite else None))
 result=dict(baseline=BASE,totals=dict(totals),preexisting=preexisting,families=families,production=production,
             whole_point_exclusions=8,new_scalar_row_exclusions=len(json.loads((HERE/'row-exclusions.json').read_text())),
             unchanged_gengamma=True,forbidden_changes=forbidden,required_receipts=required,
             environment=dict(python=platform.python_version(),mpmath=mpmath.__version__,sympy=sympy.__version__,compiler=subprocess.check_output(['g++','--version'],text=True).splitlines()[0],R=subprocess.check_output(['R','--version'],text=True).splitlines()[0]))
 (HERE/'accounting.json').write_text(json.dumps(result,indent=2,allow_nan=False)+'\n')
 print(json.dumps({k:v for k,v in result.items() if k not in ('families','production')},indent=2))
if __name__=='__main__':main()
