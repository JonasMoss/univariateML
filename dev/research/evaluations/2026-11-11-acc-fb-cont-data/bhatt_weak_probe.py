"""Check public weak-width fallbacks against independent raw-CDF references."""
from pathlib import Path
import json,subprocess,mpmath as m
P=Path(__file__).resolve().parent;root=P.parents[3];m.mp.dps=160;results=[]
for r in json.loads((P/'bhatt-critical-width-references.json').read_text())+json.loads((P/'bhatt-held-sigma-critical-references.json').read_text()):
 item=dict(id=r['id'],builds={});held=r.get('held',{})
 line=['bhatt',r['id'],str(len(r['data'])),str(len(held)),'1000','1e-10',*map(repr,r['data'])]
 for key,v in held.items():line += [key,repr(v)]
 for mode in ['native','double']:
  out=subprocess.check_output([str(root/'build/scratch'/('acc-strict-probe-'+mode))],input=' '.join(line)+'\n',text=True).strip();v=out.split('\t');assert v[2]=='uncertified',out
  got={a:m.mpf(float(b)) for a,b in (c.split('=',1) for c in v[4:])};ref={a:m.mpf(b) for a,b in r['reference'].items()}
  errors={a:float(abs(got[a]-b)/max(abs(b),ref['sigma'] if a=='mu' else 1 if a=='a' else 0)) for a,b in ref.items() if a not in held}
  assert max(errors.values())<=1e-10,(r['id'],mode,errors)
  item['builds'][mode]=dict(row=out,errors=errors,max_error=max(errors.values()));print(r['id'],mode,max(errors.values()),flush=True)
 results.append(item)
(P/'bhatt-critical-width-final.json').write_text(json.dumps(results,indent=2)+'\n')
