"""Run under one workstation slot after the final JS build."""
import study,check,subprocess,json,collections,math,os
H=study.H
study.build()
study.replay('after')
before=json.loads((H/'before-fits.json').read_text());after=json.loads((H/'after-fits.json').read_text())
assert [r['id'] for r in before]==[r['id'] for r in after]
changes=[]
for a,b in zip(before,after):
 if a['result']!=b['result']:
  av=a['result'].split();bv=b['result'].split();changes.append(dict(id=a['id'],before=a['result'],after=b['result'],status_changed=av[0]!=bv[0],coefficients_changed=av[2:]!=bv[2:]))
(H/'fit-preservation.json').write_text(json.dumps(dict(checks=len(before),families=dict(collections.Counter(r['id'][0] for r in before)),changes=changes,timeouts=sum(r['result']=='timeout' for r in after)),indent=2)+'\n')
print('fit preservation',len(before),'rows',len(changes),'changes',flush=True)
assert not any(r['status_changed'] or r['coefficients_changed'] for r in changes)
check.main('final')
for name in ['native','double']:assert not json.loads((H/('final-'+name+'.json')).read_text())['failures']
for script in ['margins.py','edge.py','constants.py']:
 subprocess.run(['python3',str(H/script)],check=True)
rows=check.jobs()
def safe(v):
 if isinstance(v,float) and not math.isfinite(v):return str(v).replace('inf','Infinity').replace('nan','NaN')
 if isinstance(v,list):return list(map(safe,v))
 if isinstance(v,dict):return {k:safe(x) for k,x in v.items()}
 return v
(H/'consumer-jobs.json').write_text(json.dumps(safe(rows),separators=(',',':'))+'\n')
if os.environ.get('DPQ_SKEW_SKIP_JS')!='1':
 subprocess.run(['node','--v8-pool-size=1',str(H/'js-consumer.mjs')],check=True)
 print('Final evidence green',flush=True)
else:print('Native evidence green; JS consumer awaits the JS build',flush=True)
