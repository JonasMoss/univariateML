"""Summarize the completed, fixed-seed default-control census (stdlib only)."""
import csv,json,statistics
from collections import Counter,defaultdict
from pathlib import Path
root=Path(__file__).resolve().parent
rows=list(csv.DictReader((root/'review-census.csv').open()))
assert len(rows)==350
assert {int(r['row']) for r in rows}==set(range(350))
assert {int(r['n']) for r in rows}=={10,30,100,300,1000}
for r in rows:
    r['status']=int(r['status']);r['certified']=int(r['certified']);r['seconds']=float(r['seconds'])
    r['outcome']='certified' if r['status']==0 and r['certified'] else 'uncertified' if r['status']==0 else 'stop'
    r['decision']='certified' if r['certified'] and r['status'] in (0,1,2,3) else 'uncertified' if not r['certified'] and r['status'] in (0,1,2) else 'search_or_refutation_stop'
def summarize(group):
    times=sorted(r['seconds'] for r in group);count=Counter(r['outcome'] for r in group);decision=Counter(r['decision'] for r in group)
    return dict(n_fits=len(group),**{k:count[k] for k in ('certified','uncertified','stop')},
        fractions={k:count[k]/len(group) for k in ('certified','uncertified','stop')},
        decisions=dict(decision),statuses=dict(Counter(str(r['status']) for r in group)),
        certified_uniform_limits=sum(r['status']==2 and r['certified']==1 for r in group),
        uncertified_uniform_limits=sum(r['status']==2 and r['certified']==0 for r in group),
        median_seconds=statistics.median(times),p90_seconds=times[(9*len(times)+9)//10-1],max_seconds=max(times),total_seconds=sum(times))
by_model=defaultdict(list);by_n=defaultdict(list);by_cell=defaultdict(list)
for r in rows:
    model=r['model']+((' a='+format(float(r['a']),'.3g')) if r['model']=='bhatt' else '')
    by_model[model].append(r);by_n[r['n']].append(r);by_cell[(model,r['n'])].append(r)
assert all(len(g)==10 for g in by_cell.values())
result={'controls':{'tol':1e-8,'maxit':500},'seed_rule':'11658000 + row, std::mt19937; GCC15/libstdc++ normal/uniform draws',
    'definition':'certified/uncertified count successful finite returns only; stop counts every non-Ok status, including named uniform limits. decisions additionally classify finite and named-face results by their per-fit certificate flag.',
    'overall':summarize(rows),'by_model':{k:summarize(g) for k,g in by_model.items()},'by_n':{k:summarize(g) for k,g in by_n.items()},
    'finite_by_flag_and_width':dict(Counter(('certified' if r['certified'] else 'uncertified')+(' normal' if float(r['fitted_a'])==0 else ' positive width') for r in rows if r['status']==0)),
    'slowest_five':sorted(rows,key=lambda r:r['seconds'],reverse=True)[:5],
    'by_cell':[{"model":k[0],"n":int(k[1]),**summarize(g)} for k,g in by_cell.items()]}
(root/'review-census-summary.json').write_text(json.dumps(result,indent=2)+'\n')
with (root/'review-census-summary.csv').open('w') as out:
    names=['model','n','n_fits','certified','uncertified','stop','certified_uniform_limits','uncertified_uniform_limits','median_seconds','p90_seconds','max_seconds','total_seconds']
    writer=csv.DictWriter(out,fieldnames=names,extrasaction='ignore');writer.writeheader();writer.writerows(result['by_cell'])
print(json.dumps({k:result[k] for k in ('overall','by_model','by_n')},indent=2))
