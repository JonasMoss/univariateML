"""Reuse independent values only at byte-identical data and public coefficients."""
import gzip,json,math,sys
import mpmath as mp
from pathlib import Path
HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(HERE.parents[3]/'dev/validation'))
from quality.truth import DISTS,read
from seam_reference import reference
source=HERE/'before.jsonl'
text=source.read_text() if source.exists() else gzip.decompress(source.with_suffix('.jsonl.gz').read_bytes()).decode()
before=[json.loads(l) for l in text.splitlines()]
with gzip.open(HERE/'final.jsonl.gz','rt') as h:after={(r['family'],r['source'],r['index']):r for r in map(json.loads,h)}
rows=[]
for b in before:
    if 'reference' not in b:continue
    k=b['family'],b['source'],b['index'];a=after[k]
    assert a['data']==b['data']
    same=[float(v).hex() for v in a['parameters']]==[float(v).hex() for v in b['parameters']]
    if same:ref=b['reference']
    else:
        meta,_=read(DISTS/k[0]);theta=dict(zip([p['name'] for p in meta['params']],a['parameters']))
        mp.mp.dps=200;ref=reference(DISTS/k[0],meta,theta,a['data'])
        mp.mp.dps=250;assert ref==reference(DISTS/k[0],meta,theta,a['data'])
    err=abs(a['reported']-ref)/max(1,abs(ref))
    rows.append(dict(family=k[0],source=k[1],index=k[2],reference=ref,
                     before_fitter_error=b['fitter_error'],after_fitter_error=err,reference_mode='identical-input reuse' if same else 'recomputed at polished coefficients'))
(HERE/'reference-reconciliation.json').write_text(json.dumps(rows,indent=2)+'\n')
print('32 independent exact-input values reconciled; changed points recomputed:',len(rows)==32)
for ident in sorted({r['family'] for r in rows}):
    rr=[r for r in rows if r['family']==ident]
    print(ident,max(r['before_fitter_error'] for r in rr),max(r['after_fitter_error'] for r in rr))
