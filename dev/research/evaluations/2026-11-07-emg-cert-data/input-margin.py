"""Independent 80-digit reference for the natural-coordinate cancellation case."""
import json
from pathlib import Path
import mpmath as m
m.mp.dps=80
b=m.mpf(.7);u=m.mpf(7000000000);k=m.mpf(1)
x=[10000000001,10000000005,10000000009]
value=sum(m.log(k*b)+k*k/2-k*(b*y-u)+m.log(m.erfc((k-b*y+u)/m.sqrt(2))/2) for y in x)
out=dict(precision_digits=80,x=x,b=str(b),u=str(u),k=str(k),loglik=str(value))
Path(__file__).with_suffix('.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
