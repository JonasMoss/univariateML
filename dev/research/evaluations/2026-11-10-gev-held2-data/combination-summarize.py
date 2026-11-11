from pathlib import Path
import csv,json,statistics
D=Path(__file__).resolve().parent
def rows(name):return list(csv.DictReader((D/name).open()))
def summarize(a):
 t=sorted(float(r['milliseconds']) for r in a)
 p=.95*(len(t)-1);i=int(p)
 return dict(fits=len(a),certified=sum(r['status']=='0' and r['certified']=='1' for r in a),uncertified=sum(r['status']=='0' and r['certified']=='0' for r in a),stops=sum(r['status']!='0' for r in a),median_ms=statistics.median(t),p95_ms=t[i]+(t[min(i+1,len(t)-1)]-t[i])*(p-i),maximum_ms=max(t),total_ms=sum(t))
out={'held':{},'allfree':{},'local_accuracy':{},'frechet_modes':{}}
for name in ['held-before.csv','study-local-100.csv','study-local-1000.csv','combination-policy-100.csv','combination-policy-1000.csv']:
 r=rows(name);out['held'][name]={fam+'_'+hold:summarize([x for x in r if x['family']==fam and x['hold']==hold]) for fam,hold in [('gev','mu'),('gev','sigma'),('frechet','sigma')]}
for name in ['allfree-before.csv','allfree-local-100.csv','allfree-local-1000.csv','combination-allfree-100.csv','combination-allfree-1000.csv']:
 r=rows(name)
 if name=='allfree-before.csv':
  for x in r:
   if x['family']=='gev' and x['status']=='0':x['certified']='1'
 out['allfree'][name]={fam:summarize([x for x in r if x['family']==fam]) for fam in ['gev','frechet']}
for name in ['combination-policy-reference.jsonl','combination-allfree-local-reference.jsonl','combination-seam-reference-native.jsonl','combination-seam-reference-double.jsonl','combination-held-reference-native.jsonl','combination-held-reference-double.jsonl','combination-held-public-reference-native.jsonl','combination-held-public-reference-double.jsonl']:
 r=[json.loads(l) for l in (D/name).read_text().splitlines()];out['local_accuracy'][name]={'fits':len(r),'maximum_coefficient_error':max(x['max_error'] for x in r)}
for mode in ['final','all-growth','no-growth']:
 runs=[rows(f'combination-frechet-{mode}-{repeat}.csv') for repeat in [1,2,3]]
 r=runs[0];out['frechet_modes'][mode]={'fits':len(r),'certified':sum(int(x['certified']) for x in r),'evaluations':sum(int(x['evaluations']) for x in r),'edge_evaluations':sum(int(x['evaluations']) for x in r if float(x['lambda'])==1),'interior_evaluations':sum(int(x['evaluations']) for x in r if float(x['lambda'])!=1),'cpu_totals_ms':[sum(float(x['cpu_milliseconds']) for x in r) for r in runs]}
out['scaling']=rows('combination-scaling-growth.csv')
out['remaining_phases']=[r for r in rows('combination-phase-1000.csv') if r['phase']!='complete']
(D/'combination-summary.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out['local_accuracy'],indent=2))
