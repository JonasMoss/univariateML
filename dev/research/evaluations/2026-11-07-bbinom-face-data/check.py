"""Independent rational saturated witnesses and 70-digit face comparisons."""
import json
from fractions import Fraction
import mpmath as mp
mp.mp.dps = 70
cases = []
f = [25001, 50000, 25001]
h = sum(f)
rho = 4 * Fraction(f[0], h) - 1
a = (1/rho - 1)/2
finite = sum(mp.mpf(n)*mp.log(mp.mpf(n)/h) for n in f)
face = (f[0]+f[2])*mp.log(mp.mpf('0.25'))+f[1]*mp.log(mp.mpf('0.5'))
assert a == 25000
assert 0 < finite-face < mp.mpf('1e-8')*h
cases.append({'size':2, 'counts':f,'alpha':str(a),'beta':str(a),'rho':str(rho),
              'gain':str(finite-face),'old_preference':str(mp.mpf('1e-8')*h)})
f = [12501,37499,37499,12501]
h = sum(f)
rho = Fraction(8,3)*(Fraction(f[0],h)-Fraction(1,8))
a = (1/rho-1)/2
assert a == Fraction(37499,2)
probs = [(a+2)/(4*(2*a+1)),3*a/(4*(2*a+1))]
assert probs[0] == Fraction(f[0],h)
assert probs[1] == Fraction(f[1],h)
gain = sum(mp.mpf(n)*mp.log(mp.mpf(n)/h) for n in f)-sum(mp.mpf(n)*mp.log(p) for n,p in zip(f,[mp.mpf(1)/8,mp.mpf(3)/8,mp.mpf(3)/8,mp.mpf(1)/8]))
assert 0 < gain < mp.mpf('1e-8')*h
cases.append({'size':3,'counts':f,'alpha':str(a),'beta':str(a),'rho':str(rho),
              'gain':str(gain),'old_preference':str(mp.mpf('1e-8')*h)})
print(json.dumps(cases,indent=2))
