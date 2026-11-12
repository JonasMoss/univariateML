"""100-digit unit-cell quadrature for the two changed reported values."""
import json,math
from pathlib import Path
import mpmath as mp
mp.mp.dps=100
here=Path(__file__).resolve().parent
read=lambda file:{r['index']:r for r in map(json.loads,(here/file).read_text().splitlines()) if r['family']=='dgamma'}
before=read('before.jsonl');after=read('after.jsonl');out=[]
for index in [1025,1027]:
 r=after[index];a,b=map(mp.mpf,r['parameters']);ref=0
 for x in r['data']:
  mid=mp.mpf(x)+mp.mpf('.5');base=(a-1)*mp.log(mid)+a*mp.log(b)-b*mid-mp.loggamma(a)
  ref+=base+mp.log(mp.quad(lambda u:mp.exp((a-1)*mp.log1p(u/mid)-b*u),[-mp.mpf('.5'),mp.mpf('.5')]))
 out.append(dict(index=index,reference=str(ref),before=before[index]['reported'],after=r['reported'],before_absolute_error=float(abs(ref-before[index]['reported'])),after_absolute_error=float(abs(ref-r['reported']))))
 assert out[-1]['after_absolute_error']<out[-1]['before_absolute_error']
(here/'changed-likelihoods.json').write_text(json.dumps(out,indent=2)+'\n');print(out)
