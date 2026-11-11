"""Resumable per-point regeneration. Failures are explicit oracle gaps.

Historical rows are retained byte-for-value. Each added point is independently
normalized/differentiated by the shared generator before its rows are appended.
The full shared CLI also regenerates historical points; this driver avoids
repeating their previously validated, sometimes costly quadratures.
"""
import concurrent.futures, json, os, signal, sys, time, subprocess, resource, gc
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
sys.path.insert(0,str(ROOT/'dev/validation'))
from quality.truth import *
import quality.truth as truth_module
truth_module.DIGITS=int(os.environ.get('TRUTH_EXTREME_DIGITS','80'))
HERE=Path(__file__).resolve().parent

def run(ident):
    print(ident,'START',flush=True)
    memory=int(os.environ.get('TRUTH_REFERENCE_MEMORY_MB','1024'))*1024**2
    resource.setrlimit(resource.RLIMIT_AS,(memory,memory))
    reserve=bytearray(8*1024**2)
    folder=DISTS/ident
    meta,g=read(folder); route=module_at(folder/'truth.py')
    destination=folder/'truth.json'
    if ident in ('frechet','invweibull'):
        destination=HERE/'measured-tables'/(ident+'.json')
        destination.parent.mkdir(exist_ok=True)
    table=json.loads((destination if destination.exists() else folder/'truth.json').read_text())
    before=dict(bytes=(folder/'truth.json').stat().st_size, points=len(table['parameters']), values=len(table['values']),quantiles=len(table['quantiles']))
    records=[]
    budget=int(os.environ.get('TRUTH_EXTREME_TIMEOUT','30'))
    fingerprint=input_sha256(folder)
    previous_path=HERE/'generation'/(ident+'.json')
    previous=json.loads(previous_path.read_text()) if previous_path.exists() else {}
    reusable=(os.environ.get('TRUTH_FORCE_GAPS')!='1' and previous.get('input_sha256')==fingerprint and
              previous.get('digits',0)>=truth_module.DIGITS and
              previous.get('timeout',0)>=budget and
              previous.get('memory_bytes',0)>=memory)
    previous_gaps={tuple(r['parameters']):r for r in previous.get('records',[])
                   if r['status']=='oracle gap'} if reusable else {}
    for theta in extreme_points(meta,route):
        encoded=[encode(theta[p['name']]) for p in meta['params']]
        if encoded in table['parameters'] or any(encoded==r['parameters'] for r in table.get('excluded_parameter_points',[])):continue
        if tuple(encoded) in previous_gaps:
            records.append(dict(previous_gaps[tuple(encoded)],reused=True));continue
        start=time.monotonic()
        def timeout(*args):raise TimeoutError(f'independent reference exceeded {budget} seconds per point')
        signal.signal(signal.SIGALRM,timeout);signal.alarm(budget)
        try:
            result=make(folder,[theta])
            offset=len(table['parameters'])
            table['parameters']+=result['parameters']
            table.setdefault('excluded_parameter_points',[]).extend(result.get('excluded_parameter_points',[]))
            if not table['excluded_parameter_points']:table.pop('excluded_parameter_points')
            table['normalization'] += [dict(r,point=r['point']+offset) for r in result['normalization']]
            for key in ('values','quantiles'):
                table[key]+=[[r[0]+offset,*r[1:]] for r in result[key]]
                if 'excluded_'+key in result:
                    table.setdefault('excluded_'+key,[]).extend(dict(r,row=[r['row'][0]+offset,*r['row'][1:]]) for r in result['excluded_'+key])
            records.append(dict(parameters=encoded,status='generated',digits=truth_module.DIGITS,seconds=time.monotonic()-start))
        except Exception as e:
            if isinstance(e,MemoryError):
                reserve=None;e.__traceback__=None;e.__context__=None;gc.collect()
                reason='MemoryError: reference exceeded the recorded address-space limit'
            else: reason=f'{type(e).__name__}: {e}'
            records.append(dict(parameters=encoded,status='oracle gap',reason=reason,seconds=time.monotonic()-start))
        finally:signal.alarm(0)
    gaps=[dict(parameters=r['parameters'],reason=r['reason']) for r in records if r['status']=='oracle gap']
    if gaps: table['extreme_reference_gaps']=gaps
    else: table.pop('extreme_reference_gaps',None)
    baseline=json.loads((HERE/'baseline-sizes.json').read_text())['families'][ident]['points']
    table['historical_sampler_points']=[[i] for i in range(baseline)]
    table['input_sha256']=input_sha256(folder)
    temporary=destination.with_suffix('.json.tmp')
    temporary.write_text(json.dumps(table,indent=2,allow_nan=False)+'\n');temporary.replace(destination)
    out=dict(id=ident,input_sha256=fingerprint,digits=truth_module.DIGITS,timeout=budget,memory_bytes=memory,before=before,after=dict(bytes=destination.stat().st_size,points=len(table['parameters']),values=len(table['values']),quantiles=len(table['quantiles'])),records=records)
    (HERE/'generation'/f'{ident}.json').write_text(json.dumps(out,indent=2)+'\n')
    print(ident,'DONE',len(records),flush=True)
    return len(gaps)
def isolated(ident):
    result=subprocess.run([sys.executable,__file__,'--one',ident])
    manifest=HERE/'generation'/(ident+'.json')
    if result.returncode not in (0,1) or not manifest.exists():
        status=dict(id=ident,status='worker failed',exit=result.returncode,
                    reason='isolated reference worker failed; no success inferred')
        manifest.write_text(json.dumps(status,indent=2)+'\n')
        return 1
    return sum(r['status']=='oracle gap' for r in json.loads(manifest.read_text()).get('records',[])) or int('records' not in json.loads(manifest.read_text()))

if __name__=='__main__':
    if len(sys.argv)>1 and sys.argv[1]=='--one':
        sys.exit(int(run(sys.argv[2])>0))
    (HERE/'generation').mkdir(exist_ok=True)
    ids=sys.argv[1:] or sorted(p.parent.name for p in DISTS.glob('*/truth.json'))
    with concurrent.futures.ThreadPoolExecutor(max_workers=min(6,max(1,int(os.environ.get("TRUTH_EXTREME_WORKERS","1"))))) as pool: gaps=sum(pool.map(isolated,ids))
    print('ORACLE GAPS',gaps,flush=True)
    sys.exit(int(gaps>0))
