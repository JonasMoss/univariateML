import sys,json
from pathlib import Path
sys.path.insert(0,'dev/validation')
from quality.reference import reference,write,DISTS
rows=[]
for p in sorted(DISTS.glob('*/golden.json')):
 id=p.parent.name
 if id=='bhatt':continue
 g=json.loads(p.read_text());r=reference(id)
 if 'error' in r:raise RuntimeError((id,r))
 old=g.get('reference',{})
 same=r['input_sha256']==old.get('input_sha256')
 # Preserve independently checked global evidence only when its defining inputs agree.
 if same:r['global']=old['global']
 else:
  print('CHANGED REFERENCE INPUTS',id,flush=True)
  if r['global']=='local only':
   from quality.harness.check import check
   from quality.reference import rounded_estimates
   candidate=dict(g,estimates=rounded_estimates(id,r),logLik=float(r['logLik']))
   oracle=check(id,golden=candidate)['oracle']
   if oracle['status']=='fail':raise RuntimeError((id,oracle))
   r['global']={'kind':'numerical multistart, not a proof',**oracle}
   if oracle.get('gap',0)>1e-12*(1+abs(float(r['logLik']))):
    import mpmath as mp
    from quality.reference import rescore_oracle
    verified=rescore_oracle(id,r,oracle)
    assert mp.mpf(verified['gap'])<=mp.mpf(verified['comparison_margin']), (id,verified)
    r['global']['high_precision_check']=verified
 write(id,r)
 rows.append({'id':id,'input_sha256_unchanged':same,'input_sha256':r['input_sha256'],'score_norm':r.get('score_norm'),'information_negative_definite':r.get('information_negative_definite')})
 print(id,'verified',r.get('score_norm'),flush=True)
 Path('dev/research/evaluations/2026-11-09-golden-ref-port-data/reference-checks.json').write_text(json.dumps(rows,indent=2)+'\n')
