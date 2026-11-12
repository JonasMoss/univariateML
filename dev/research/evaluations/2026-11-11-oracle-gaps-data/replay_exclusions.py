"""Freshly replay every remaining excluded witness with the final binaries."""
import collections,hashlib,json,math
import audit
from classify_rows import canonical
HERE=audit.HERE
probes=json.loads((HERE/'open-point-final-probes.json').read_text());updated_probes=[]
for mode,binary in [('native','probe-fixed'),('double','probe-double-fixed')]:
 audit.EXE=audit.WORK/binary
 digest=hashlib.sha256(audit.EXE.read_bytes()).hexdigest()
 active=json.loads((HERE/f'measured-recovered-{mode}-summary.json').read_text())
 assert active['failures']==0 and active['binary_sha256']==digest
 fs=[f for f in json.loads((HERE/f'final-{mode}-failures.json').read_text()) if f['family']!='dgamma']
 fs.sort(key=lambda f:f['family'])
 jobs=[(f['family'],{'d':0,'p':1,'q':2}[f['function']],f['input'],f['lower'],f['log'],f['parameters']) for f in fs]
 actual=audit.evaluate(jobs)
 for f,a in zip(fs,actual):
  relative=f['function']=='q' or f['function']=='p' and not f['log']
  error=audit.error(a,f['expected'],relative)
  tolerance=1e-9 if f['function']=='q' else 1e-10
  assert error>tolerance,(mode,f['family'],f['input'],a,f['expected'])
  f['actual']=a;f['error']=error
  for p in probes:
   if p['mode']==mode and p['family']==f['family'] and p['parameters']==f['parameters'] and p['input']==f['input'] and p['lower']==f['lower'] and p['log']==f['log'] and f['function']=='p':
    assert not math.isfinite(a)
    updated_probes.append(dict(p,binary_sha256=digest,actual=str(a)))
 (HERE/f'final-{mode}-failures.json').write_text(json.dumps(fs,indent=2)+'\n')
 final=dict(active,classification='Fresh final active-table measurement; excluded witnesses separately replayed with the same binary.',source_measurement=f'measured-recovered-{mode}',active_failures=0,failures=len(fs),excluded_witnesses=len(fs))
 (HERE/f'final-{mode}-summary.json').write_text(json.dumps(final,indent=2)+'\n')
 print('Fresh excluded-witness replay:',mode,len(fs),flush=True)
assert len(updated_probes)==16,len(updated_probes)
(HERE/'open-point-final-probes.json').write_text(json.dumps(updated_probes,indent=2,allow_nan=False)+'\n')
