"""100-digit upper incomplete gamma inversion at the binary64 probe."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=100
out=Path(__file__).resolve().parent
params=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/gamma/golden.json').read_text())['estimates']
a=mp.mpf(params['shape']); scale=mp.mpf(params['scale']); p=mp.mpf(1-1e-12)
lo,hi=mp.mpf(0),mp.mpf(100)
for _ in range(400):
    mid=(lo+hi)/2
    if mp.gammainc(a,mid/scale,mp.inf)/mp.gamma(a)>1-p: lo=mid
    else: hi=mid
reference=(lo+hi)/2
probe=json.loads((out/'conventions.json').read_text())['gamma q near_integer_tiny']
result={'probability_hex':float(p).hex(),'reference':mp.nstr(reference,50)}
for name in ('core','stats'):
    value=mp.mpf(probe[name]['value'][0])
    result[name+'_relative_error']=mp.nstr(abs((value-reference)/reference),20)
(out/'gamma-reference.json').write_text(json.dumps(result,indent=2)+'\n')
print(result)
