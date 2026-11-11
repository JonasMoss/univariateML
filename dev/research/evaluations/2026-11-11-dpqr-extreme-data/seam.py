"""Check archived fit payloads and frozen golden estimates against scalar d.

The rerun explicitly reuses the November 7 frozen cases, as documented by its
cache-validation.json. No fit algorithms or goldens are changed by this replay.
"""
import csv,gzip,inspect,json,math,sys,signal,collections
from pathlib import Path
import mpmath as mp
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(HERE));import audit
from quality.truth import DISTS,read,module_at,declarative,number
from seam_reference import reference,InvalidFit,ExcludedModel
OLD=ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data'
RERUN=ROOT/'dev/research/evaluations/2026-11-09-fit-audit-rerun-data'

def main(label):
 cases=json.loads(gzip.decompress((OLD/'cases.json.gz').read_bytes()))
 records=[]
 ads=audit.adapters(); metas={i:read(DISTS/i)[0] for i in ads}
 for ident in ads:
  if not (DISTS/ident/'golden.json').exists():continue
  meta,g=read(DISTS/ident)
  names=[p['name'] for p in meta['params']]
  records.append(dict(family=ident,source='golden',index=-1,data=g['data'],parameters=[g['estimates'][n] for n in names],reported=g['logLik']))
 with gzip.open(RERUN/'fits.csv.gz','rt') as h:
  for r in csv.reader(h):
   if r[0] not in ads or r[4]!='1':continue
   case=cases[int(r[2])]; assert case['family']==r[0]
   pars=dict(s.split('=',1) for s in r[9:]);meta=metas[r[0]];names=[p['name'] for p in meta['params']]
   if any(n not in pars for n in names):continue
   theta=[float(pars[n]) for n in names]
   if any(math.isnan(v) for v in theta):continue
   records.append(dict(family=r[0],source='fit-audit-rerun',index=int(r[2]),data=case['data'],parameters=theta,reported=float(r[8]),archived_status=r[3],archived_flags=r[5:8]))
 jobs=[]
 for r in records:jobs += [(r['family'],0,x,True,True,r['parameters']) for x in r['data']]
 vals=iter(audit.evaluate(jobs)); mismatches=[];counts=collections.Counter()
 for r in records:
  r['dpqr']=sum(next(vals) for x in r['data']);r['error']=audit.error(r['dpqr'],r['reported'])
  counts[r['family']]+=1
  if r['error']<=1e-10:continue
  def timeout(*args):raise TimeoutError('60-second reference budget')
  signal.signal(signal.SIGALRM,timeout);signal.alarm(60)
  try:
   folder=DISTS/r['family'];meta,g=read(folder);route=module_at(folder/'truth.py')
   maxexp=max([abs(math.log10(abs(v))) for v in r['parameters'] if v and math.isfinite(v)]+[0])
   mp.mp.dps=min(1600,max(100,int(4*maxexp)+100))
   theta=dict(zip([p['name'] for p in meta['params']],r['parameters']))
   value=reference(folder,meta,theta,r['data'])
   r.update(reference=value,dpqr_error=audit.error(r['dpqr'],value),fitter_error=audit.error(r['reported'],value))
   r['attribution']='both' if r['dpqr_error']>1e-10 and r['fitter_error']>1e-10 else 'dpqr' if r['dpqr_error']>1e-10 else 'fitter' if r['fitter_error']>1e-10 else 'summation margin'
   if r['family']=='sged' and math.isnan(r['dpqr']) and theta['nu']>=1e6:
    r['attribution']='named precision-region NaN; fitter' if r['fitter_error']>1e-10 else 'named precision-region NaN'
    r['reason']='The dossier and dsged forward-error gate explicitly reject this unresolved large-nu skew standardization.'
   if r['family'] in ('gb2','fpareto','genbeta') and math.isnan(r['dpqr']):
    r['attribution']='named precision-region NaN; fitter' if r['fitter_error']>1e-10 else 'named precision-region NaN'
    r['reason']='The concentrating beta log-odds coordinate exceeds the practical forward-error margin named in the dossier.'
  except InvalidFit as e:r.update(attribution='invalid fit parameters',reason=str(e))
  except ExcludedModel as e:r.update(attribution='dossier-excluded model',reason=str(e))
  except Exception as e:r.update(attribution='open reference',reason=f'{type(e).__name__}: {e}')
  finally:signal.alarm(0)
  mismatches.append(r)
 (HERE/('seam-'+label+'.json')).write_text(json.dumps(dict(records=len(records),by_family=dict(counts),mismatches=mismatches),indent=2)+'\n')
 print('seam',len(records),'mismatches',len(mismatches),collections.Counter(r['attribution'] for r in mismatches))
if __name__=='__main__':main(sys.argv[1])
