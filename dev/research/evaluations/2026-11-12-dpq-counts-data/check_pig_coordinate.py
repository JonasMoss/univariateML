"""Independent log-tail enclosure for the newly admitted large coordinate.
For this law C<3A/2, so all positive-tail mass ratios are <=A.
The true log survival lies between log p(n) and log p(n)-log(1-A).
The inverse-gamma Bessel Taylor next term bounds the mass error.
"""
import json,re
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent
mp.mp.dps=80
q=mp.mpf(float(2.0004e19));n=q+1;m=mp.mpf(10);s=mp.mpf(float(.1))
A=2*m*m/(s+2*m*m);C=m/mp.sqrt(1+2*m*m/s)
assert C<3*A/2
c=s/2+(s/m)**2/4
term=mp.mpf(1);series=term
for j in range(1,100):
 nxt=-term*c/(j*(n-mp.mpf('.5')-j))
 if abs(nxt)<mp.mpf('1e-60'):break
 series+=nxt;term=nxt
else:raise ArithmeticError('mass remainder did not close')
logmass=(mp.log(s/(2*mp.pi))/2+s/m-mp.log(A)/2+n*mp.log(A)+
         mp.loggamma(n-mp.mpf('.5'))-mp.loggamma(n+1)+mp.log(series))
error=2*abs(nxt)/series+mp.mpf('1e-55')
lo=logmass-error;hi=logmass-mp.log1p(-A)+error
actual=mp.mpf(re.search(r'log_upper ([^\s]+)',(HERE/'pig-large-coordinate.log').read_text())[1])
relative_bound=max(abs(actual-lo),abs(actual-hi))/min(abs(lo),abs(hi))
assert relative_bound<mp.mpf('1e-10')
result=dict(log_tail_interval=[mp.nstr(lo,75),mp.nstr(hi,75)],actual=str(actual),normalized_error_bound=mp.nstr(relative_bound,30),mass_relative_remainder=mp.nstr(error,30))
(HERE/'pig-large-coordinate-reference.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
