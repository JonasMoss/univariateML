"""Validate and reconcile finished census/retries; write evidence, never a draft report."""
import collections,gzip,importlib.util,json,math,sys
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
sys.path.insert(0,str(ROOT/'dev/validation'))
spec=importlib.util.spec_from_file_location('seam',ROOT/'dev/validation/seam-check.py')
s=importlib.util.module_from_spec(spec);spec.loader.exec_module(s)
def read(name):
    p=HERE/name
    if not p.exists():p=p.with_suffix(p.suffix+'.gz')
    text=gzip.decompress(p.read_bytes()).decode() if p.suffix=='.gz' else p.read_text()
    return [json.loads(l) for l in text.splitlines()]
def key(r):return r['family'],r['source'],r['index']
before={key(r):r for r in read('before.jsonl')}
after={key(r):r for r in read('after.jsonl')}
assert len(before)==len(after)==11284
assert before.keys()==after.keys()
for r in read('after-bbinom-final.jsonl'):after[key(r)]=r
invgauss=read('after-invgauss-final.jsonl')
assert len(invgauss)==101 and all(r['family']=='invgauss' for r in invgauss)
for r in invgauss:after[key(r)]=r
late=read('after-current-beta.jsonl')
assert len(late)==1658
dependents=set(json.loads((HERE/'beta-dependent-families.json').read_text()))
assert {r['family'] for r in late}==dependents
for r in late:after[key(r)]=r
retry=[]
for name in ['retry-120.jsonl','retry-additional.jsonl','retry-golden.jsonl']:
    for r in read(name):
        retry.append(r)
        if r['family'] not in dependents and after[key(r)]['outcome']=='open fit':after[key(r)]=r
owned_elsewhere=set('sged sstd snorm burr invburr pareto4 paralogis invparalogis betapr gb2 fpareto pareto3 ged genbeta bhatt sn ztpois gengamma trgamma invtrgamma'.split())
miss=lambda r:r.get('attribution') in ('fitter','both','support')
dpqr=lambda r:r.get('attribution') in ('dpqr','both')
owned_misses=[r for r in after.values() if miss(r) and r['family'] not in owned_elsewhere]
assert not owned_misses,owned_misses
assert not [r for r in after.values() if r.get('attribution')=='open reference']
support={};changes=[];golden_changes=[];transitions=collections.Counter()
for label,rows in [('before',before),('after',after)]:
    checked=0;violations=[]
    for r in rows.values():
        if 'parameters' not in r:continue
        meta,_=s.read(s.DISTS/r['family']);checked+=1
        outside=s.support_violations(meta,r['parameters'],r['data'])
        if outside:violations.append(dict(key=key(r),outside=outside))
    assert not violations,violations
    support[label]=dict(checked=checked,outside_support=violations)
for k,b in before.items():
    a=after[k];transitions[b['outcome']+' -> '+a['outcome']]+=1
    if 'parameters' in a and 'parameters' in b:
        if [float(v).hex() for v in a['parameters']] != [float(v).hex() for v in b['parameters']]:
            changes.append(dict(key=k,before=b['parameters'],after=a['parameters']))
        if k[1]=='golden' and (a['parameters']!=b['parameters'] or a['reported']!=b['reported']):
            golden_changes.append(dict(family=k[0],before_parameters=b['parameters'],after_parameters=a['parameters'],before_logLik=b['reported'],after_logLik=a['reported']))
opens=[r for r in after.values() if r['outcome']=='open fit']
retried={key(r) for r in retry}
assert not [key(r) for r in opens if key(r) not in retried and '120s' not in r.get('reason','')], 'new open case needs a 120-second retry'
summary=dict(records=len(after),families=len({k[0] for k in after}),before=dict(collections.Counter(r['outcome'] for r in before.values())),after=dict(collections.Counter(r['outcome'] for r in after.values())),before_fitter=sum(map(miss,before.values())),after_fitter=sum(map(miss,after.values())),before_owned_fitter=sum(miss(r) and r['family'] not in owned_elsewhere for r in before.values()),after_owned_fitter=len(owned_misses),before_dpqr=sum(map(dpqr,before.values())),after_dpqr=sum(map(dpqr,after.values())),support=support,parameter_changes=changes,golden_payload_changes=golden_changes,transitions=dict(transitions),open_cases=opens,remaining_mismatches=[r for r in after.values() if r['outcome']=='mismatch'])
summary['by_family']={}
for ident in sorted({k[0] for k in after}):
    b=[r for r in before.values() if r['family']==ident];a=[r for r in after.values() if r['family']==ident]
    summary['by_family'][ident]=dict(records=len(a),before_fitter=sum(map(miss,b)),after_fitter=sum(map(miss,a)),after_dpqr=sum(map(dpqr,a)),after=dict(collections.Counter(r['outcome'] for r in a)),owned_elsewhere=ident in owned_elsewhere)
(HERE/'final-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
with gzip.open(HERE/'final.jsonl.gz','wt') as dest:
    for k in before:dest.write(json.dumps(after[k])+'\n')
print({k:v for k,v in summary.items() if k not in ['by_family','remaining_mismatches','open_cases','golden_payload_changes','parameter_changes']})
print('parameter changes',len(changes),'golden payload changes',len(golden_changes),'open cases',len(opens))
