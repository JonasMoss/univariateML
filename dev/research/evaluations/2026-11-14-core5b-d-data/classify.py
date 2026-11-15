"""Inventory every changed snapshot row, including roundoff and core provenance."""
import collections,gzip,json,pathlib
D=pathlib.Path(__file__).resolve().parent
B=D.parent/'2026-11-14-fit-snapshot-data'
F=set('pois binom nbinom geom zeta lgser norm exp gamma laplace'.split())
def rows(path):
 if path.is_dir():
  lines=(line for p in sorted(path.glob('*.jsonl')) for line in p.read_text().splitlines())
 else:lines=gzip.open(path,'rt')
 out={}
 for line in lines:
  if not line.startswith('{'):continue
  row=json.loads(line);out[(row['family'],row['source'],str(row['case']))]=row
 return out
all_changes=[];counts={}
for mode in ['native','double','routes-native','routes-double']:
 old=rows(B/(mode+'.jsonl.gz'));new=rows(pathlib.Path('build/snapshots')/mode)
 if not mode.startswith('routes'):old={k:v for k,v in old.items() if k[0] in F}
 assert old.keys()==new.keys(),(mode,old.keys()-new.keys(),new.keys()-old.keys())
 cats=collections.Counter()
 for key,a in old.items():
  b=new[key]
  if a==b:continue
  ra,rb=a['result'],b['result'];fields=sorted(k for k in ra.keys()|rb.keys() if ra.get(k)!=rb.get(k))
  coefficient_state=lambda r:(r.get('estimates'),r.get('limit_estimates'),[(c.get('model'),c.get('estimates')) for c in r.get('candidates',[])])
  if coefficient_state(ra)!=coefficient_state(rb):category='COEFFICIENT-REGRESSION'
  elif ra.get('kind')!=rb.get('kind'):
   category='evaluation-classification' if rb.get('outcome',{}).get('reason')=='evaluation' else 'estimated-window-collapse' if key[0].startswith('norm ') and rb.get('outcome',{}).get('reason')=='unbounded' else 'boundary-outcome' if key[0] in ['tgeom','tnbinom'] else 'core-classification'
  elif ra.get('unique')!=rb.get('unique'):category='proved-nonidentification'
  elif ra.get('logLik')!=rb.get('logLik'):
   category='strict-loglik-repair' if (key[0],key[1],key[2]) in {('gamma','class','near-constant'),('gamma','class','offset'),('laplace','class','subnormal'),('lgser','class','huge'),('pois+zero=modified','class','huge'),('pois+zero=truncated','class','huge')} else 'returned-law-loglik-roundoff'
  elif ra.get('outcome')!=rb.get('outcome'):category='evaluation-classification' if rb.get('outcome',{}).get('reason')=='evaluation' else 'core-classification'
  elif any(k in fields for k in ['limit_loglik','candidates','loglik_lower','loglik_upper']):category='conditional-poisson-face-loglik'
  elif fields==['core_status']:category='core-provenance'
  elif fields==['core_status','warnings']:category='held-parent-identification'
  elif fields==['message']:category='core-diagnostic'
  else:category='payload-or-provenance'
  reasons={
   'estimated-window-collapse':'An estimated truncation endpoint collapses the window onto the constant sample, giving unbounded likelihood even with held parent coefficients; the core now reports Nonexistence/Unbounded.',
   'evaluation-classification':'A proper held model or finite returned family coefficients failed point likelihood evaluation; the core reports Evaluation rather than invalid input, a convergence failure, or an adapter override.',
   'core-diagnostic':'The core-owned collapse flag selects a diagnostic naming the estimated window rather than incorrectly requiring a vanishing parent scale.',
   'proved-nonidentification':'Algebraic non-identification is now carried by the core unique flag, conditional on which parent coordinates are held.',
   'returned-law-loglik-roundoff':'Likelihood is evaluated at returned binary64 coefficients; inherited positive-Poisson face values follow the same repair.',
   'strict-loglik-repair':'Independent 120-digit density evaluation confirms the repaired likelihood at unchanged returned coefficients.',
   'core-provenance':'Fully held window evaluation now returns a core-owned raw status; its numerical answer and outcome are unchanged.',
   'core-classification':'Raw count validation or empty tbinom rejection now comes from the core as InvalidInput, rather than a pre-core InvalidArgument.',
   'conditional-poisson-face-loglik':'A conditional Poisson face inherits the repaired positive-Poisson point likelihood, including candidate bounds.',
   'held-parent-identification':'A held parent gives core Ok rather than AllZero, removing the AllZero warning; coefficients and likelihood are unchanged.'}
  reason=reasons[category]
  cats[category]+=1
  all_changes.append(dict(mode=mode,family=key[0],source=key[1],case=key[2],category=category,reason=reason,fields=fields,old=ra,new=rb))
 counts[mode]=dict(cases=len(new),differences=sum(cats.values()),categories=dict(cats))
(D/'snapshot-counts.json').write_text(json.dumps(counts,indent=2)+'\n')
with gzip.open(D/'snapshot-differences.jsonl.gz','wt') as out:
 for change in all_changes:out.write(json.dumps(change,sort_keys=True)+'\n')
with (D/'snapshot-cases.tsv').open('w') as out:
 out.write('mode\tfamily\tsource\tcase\tcategory\tchanged fields\treason\n')
 for c in all_changes:out.write('\t'.join(str(c[k]) for k in ['mode','family','source','case','category'])+'\t'+','.join(c['fields'])+'\t'+c['reason']+'\n')
print(json.dumps(counts,indent=2))
assert not any(c['category']=='COEFFICIENT-REGRESSION' for c in all_changes)
