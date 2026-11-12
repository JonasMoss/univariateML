"""Repeat frozen fit preservation in the double-width mode, baseline from git."""
import study,os,json,collections
os.environ['DPQ_SKEW_BASELINE']='1';study.build();os.environ.pop('DPQ_SKEW_BASELINE')
os.environ['DPQ_SKEW_FIT_MODE']='double';os.environ['DPQ_SKEW_BINARY']='probe-baseline';study.replay('before-double')
os.environ['DPQ_SKEW_BINARY']='probe';study.replay('after-double')
a=json.loads((study.H/'before-double-fits.json').read_text());b=json.loads((study.H/'after-double-fits.json').read_text());assert [r['id'] for r in a]==[r['id'] for r in b]
changes=[dict(id=x['id'],before=x['result'],after=y['result']) for x,y in zip(a,b) if x['result']!=y['result']]
r=dict(checks=len(a),families=dict(collections.Counter(x['id'][0] for x in a)),changes=changes,timeouts=sum(x['result']=='timeout' for x in a+b),baseline='217ab8b533cac25ccc8eba8d66eccde1b376f0b4')
(study.H/'fit-preservation-double.json').write_text(json.dumps(r,indent=2)+'\n');print('Double fit preservation',r,flush=True)
assert not changes and not r['timeouts']
