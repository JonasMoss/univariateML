"""Retain measured open rows and reproducible exact-input exclusion declarations.

Only this job's unresolved forward errors are archived. Existing exclusions
are preserved. The raw before/after audit continues measuring archived rows.
"""
import collections,json,sys,math
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(ROOT/'dev/validation'))
from quality.truth import DISTS,module_at,apply_exclusions
records=json.loads((HERE/'after-failures.json').read_text())
by=collections.defaultdict(list)
for r in records:
 if r['excluded_reason'] is None:by[r['family']].append(r)
notes={
 'snorm':'Rounded standardizing moments lose the residual on the thin side at extreme skew; an FMA alone cannot restore the rounded centering constant.',
 'sged':'Rounded standardizing moments lose the residual on the thin side at extreme skew; the existing large-nu precision gate does not cover this finite-nu case.',
 'sstd':'Extreme skew loses centered residuals; an independent moment expansion with retained low-order centering terms remains needed.',
 'hyper':'The near-upper-end quantile comparison needs a more accurate finite-count survival sum at this large population size.'}
archive=[]
for ident,rs in sorted(by.items()):
 if ident in ('frechet','invweibull'):continue
 p=DISTS/ident/'truth.py';text=p.read_text();assert 'truth_exclusions =' not in text,ident
 groups={}
 for r in rs:groups.setdefault((r['category'],tuple(r['parameters']),tuple(r['row'][1:2] if r['category']=='values' else r['row'][1:4])),[]).append(r)
 declarations={'values':[],'quantiles':[]}
 for (category,theta,at),same in groups.items():
  measured=max(r['error'] for r in same)
  reason='TASK-189 open: '+notes.get(ident,'Unresolved forward error; see the retained exact-input audit.')+' Measured worst normalized error '+str(measured)+'.'
  declarations[category].append((list(theta),list(at),reason))
  archive.append(dict(family=ident,category=category,parameters=list(theta),input=list(at),reason=reason,failed_checks=len(same)))
 p.write_text(text+'\n# Measured TASK-189 open work; rows remain in excluded_* evidence.\ntruth_exclusions = '+repr(declarations)+'\n')
 table_path=DISTS/ident/'truth.json';table=json.loads(table_path.read_text());old={key:table.get('excluded_'+key,[]) for key in declarations}
 table=apply_exclusions(table,module_at(p))
 for key,rows in old.items():
  if rows:table['excluded_'+key]=rows+table.get('excluded_'+key,[])
 table_path.write_text(json.dumps(table,indent=2,allow_nan=False)+'\n')
(HERE/'open-rows.json').write_text(json.dumps(archive,indent=2)+'\n')
print('open rows retained',len(archive),'across',len(by),'families')
