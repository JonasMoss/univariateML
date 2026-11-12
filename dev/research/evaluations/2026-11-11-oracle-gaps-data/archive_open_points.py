"""Measure unresolved members and retain their complete independent laws."""
import collections,hashlib,json,math,subprocess,sys
from pathlib import Path
import audit
from quality.truth import DISTS,module_at,apply_point_exclusions,input_sha256
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
REQUESTS=[
 ('bnbinom',[1e8,1e16,1e8],'Near-Poisson beta-mixture evaluation needs a cancellation-free mass and a closed mixture remainder.'),
 ('cmp',[1.,1e-8],'Diffuse factorial-power normalization and count tails need a protected Euler--Maclaurin evaluator.'),
 ('pig',[1.,1e-8],'Positive heavy count tails exceed the 10000000-cell remainder guard; a bounded tail integral is missing.'),
 ('pig',[1e8,1.],'Positive heavy count tails exceed the 10000000-cell remainder guard; a bounded tail integral is missing.'),
 ('lgser',[0.9999999999999999],'Near-unit logarithmic-series survival exceeds the geometric-series guard; a uniform bounded tail evaluator is missing.'),
 ('zmlogarithmic',[0.9999999999999999,.5],'Near-unit logarithmic-series survival exceeds the geometric-series guard; a uniform bounded tail evaluator is missing.'),
 ('st',[0.,1.,-1e8,1e8],'The joint large-skew/large-degree angular tail is not resolved by the public evaluator.'),
 ('st',[0.,1.,1e8,1e8],'The joint large-skew/large-degree angular tail is not resolved by the public evaluator.'),
]

def payload(t):
    out=[]
    for category in ('values','quantiles'):
        for r in t[category]:out.append(json.dumps([t['parameters'][r[0]],category,*r[1:]],separators=(',',':')))
        for e in t.get('excluded_'+category,[]):
            r=e['row'];out.append(json.dumps([t['parameters'][r[0]],category,*r[1:]],separators=(',',':')))
        for p in t.get('excluded_parameter_points',[]):
            for r in p[category]:out.append(json.dumps([p['parameters'],category,*r[1:]],separators=(',',':')))
    return collections.Counter(out)

def main():
    if (HERE/'open-point-probes.json').exists():raise FileExistsError('Point probes already recorded')
    previous=[]
    for mode in ('native','double'):
        previous+=json.loads((HERE/f'interim-{mode}-failures.json').read_text())
    selected=[]
    for name,params,missing in REQUESTS:
        table=json.loads((DISTS/name/'truth.json').read_text());index=table['parameters'].index(params)
        old=json.loads(subprocess.check_output(['git','show','7825e912:'+str((DISTS/name/'truth.json').relative_to(audit.ROOT))],cwd=audit.ROOT,text=True))
        assert params not in old['parameters'],(name,params)
        candidates=[f for f in previous if f['family']==name and f['parameters']==params and f['function']=='p'
                    and math.isnan(f['actual']) and math.isfinite(float(f['input']))
                    and (name!='pig' or math.floor(float(f['input']))!=0)]
        assert candidates,(name,params)
        f=candidates[0]
        row=next(r for r in table['values'] if r[0]==index and r[1]==f['input'])
        log_expected=float(row[3 if f['lower'] else 4])
        expected=log_expected if f['log'] else math.exp(log_expected)
        assert math.isfinite(expected)
        job=(name,1,f['input'],f['lower'],f['log'],params)
        selected.append(dict(family=name,parameters=params,missing=missing,job=job,row=row,
                             expected=expected,category='values',function='p',input=f['input'],lower=f['lower'],log=f['log']))
    probes=[];bad=set()
    for mode,binary in [('native','probe-fixed'),('double','probe-double-fixed')]:
        audit.EXE=audit.WORK/binary
        actual=audit.evaluate([x['job'] for x in selected])
        digest=hashlib.sha256(audit.EXE.read_bytes()).hexdigest()
        for entry,value in zip(selected,actual):
            failed=not math.isfinite(value)
            if failed:bad.add((entry['family'],tuple(entry['parameters'])))
            probes.append(dict(entry,mode=mode,binary_sha256=digest,
                               actual=value if math.isfinite(value) else str(value),failed=failed))
    (HERE/'open-point-preflight.json').write_text(json.dumps(probes,indent=2,allow_nan=False)+'\n')
    assert len(bad)==len(selected),'A proposed exclusion is no longer supported by its measured probe'
    changes=collections.defaultdict(list)
    for entry in selected:
        name=entry['family'];params=entry['parameters']
        reason=("TASK-196 open: public CDF returns NaN at x="+repr(entry['input'])+
                ', lower='+str(entry['lower'])+', log='+str(entry['log'])+
                '; independent value '+repr(entry['expected'])+'. '+entry['missing'])
        changes[name].append((params,reason))
        for p in probes:
            if p['family']==name and p['parameters']==params:p['exclusion_reason']=reason
    evidence=[]
    for name,declarations in changes.items():
        folder=DISTS/name;path=folder/'truth.py';source=path.read_text();route=module_at(path)
        label='truth_point_exclusions'
        addition=('\n\n'+label+('.extend(' if hasattr(route,label) else ' = ')+repr(declarations)+(')' if hasattr(route,label) else '')+'\n')
        path.write_text(source+addition)
        table_path=folder/'truth.json';table=json.loads(table_path.read_text());before=payload(table)
        route=module_at(path);apply_point_exclusions(table,route)
        assert payload(table)==before,name
        old=json.loads(subprocess.check_output(['git','show','7825e912:'+str(table_path.relative_to(audit.ROOT))],cwd=audit.ROOT,text=True))
        for key in ('parameters','values','quantiles','normalization','excluded_values','excluded_quantiles','excluded_parameter_points'):
            assert table.get(key,[])[:len(old.get(key,[]))]==old.get(key,[]),(name,key)
        for key in ('sampler_points','historical_sampler_points'):assert table.get(key)==old.get(key),(name,key)
        table['input_sha256']=input_sha256(folder)
        table_path.write_text(json.dumps(table,indent=2,allow_nan=False)+'\n')
        evidence.append(dict(family=name,points=len(declarations),numeric_payload_unchanged=True))
    (HERE/'open-point-probes.json').write_text(json.dumps(probes,indent=2,allow_nan=False)+'\n')
    (HERE/'point-archive-preservation.json').write_text(json.dumps(evidence,indent=2)+'\n')
    print('Measured unsupported points archived without changing any independent number:',len(selected),flush=True)

if __name__=='__main__':main()
