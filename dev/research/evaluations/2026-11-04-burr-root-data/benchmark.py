"""Time unchanged C++ fits; compare research Python overhead without claiming a C++ port speed."""
import json
import subprocess
from isolate import HERE, PREV

samples=json.loads((PREV/'samples.json').read_text())
payload=''.join(str(len(x))+' '+' '.join(format(v,'.17g') for v in x)+'\n' for x in samples.values())
proc=subprocess.run(['build/scratch/burr-root-benchmark'],input=payload,text=True,capture_output=True,check=True)
lines=proc.stdout.splitlines();assert len(lines)==len(samples)
results=[]
prototype={a['sample']:a for a in json.loads((HERE/'global.json').read_text())}
for (name,x),line in zip(samples.items(),lines):
    status,k,c,s,ll,seconds=line.split()
    results.append(dict(sample=name,n=len(x),existing_status=int(status),k=float(k),c=float(c),scale=float(s),logLik=float(ll),
                        existing_seconds=float(seconds),prototype_global_seconds=prototype[name]['seconds']))
(HERE/'benchmark.json').write_text(json.dumps(results,indent=2)+'\n')
print('Timed',len(results),'unchanged fits; 10 repetitions each.')
