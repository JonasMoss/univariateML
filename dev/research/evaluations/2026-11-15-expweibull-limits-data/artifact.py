"""The 0.42 of expweibull_probe.py is binary64 cancellation, not a likelihood above the inverse Weibull face.
Evaluate the probe's best point (a,b,c) = (1.8774493789920255e-4, 0.3753218375200164, 1.3510798882111488e16)
in binary64 (probe formula) and with mpmath (50 and 200 digits); then the mp profile maximum over mu at that b."""
import numpy as np, mpmath as mp, sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import core
xs = [2.698972628080099, 2.698972628080099, 3.487363739010494]
a, b, c = 1.8774493789920255e-4, 0.3753218375200164, 1.3510798882111488e16
x = np.array(xs); n = 3
t = (x/a)**b; h = np.log(-np.expm1(-t))
print('binary64: t =', t, ' sum h =', h.sum(), ' c_hat = -n/sum h =', -n/h.sum())
ll = n*(np.log(b)+np.log(c)) - n*np.log(a) + np.sum((b-1)*np.log(x/a) - t + (c-1)*h)
print('binary64 ll at the probe point:', ll)
for dps in (50, 200):
    mp.mp.dps = dps
    y = [mp.log(mp.mpf(v)) for v in xs]
    v, chat = core.lp(y, mp.log(mp.mpf(a)), mp.mpf(b))
    print('dps', dps, ': profile ll at (a,b) =', mp.nstr(v, 12), ' c_hat =', mp.nstr(chat, 8), '  (float c was 1.351e16)')
    # ll at the float c, not profiled
    n_ = 3; tt = [mp.e**(mp.mpf(b)*(yy-mp.log(mp.mpf(a)))) for yy in y]
    cc = mp.mpf(c)
    l2 = n_*(mp.log(b)+mp.log(cc)) - sum(y) + sum(mp.log(ti)-ti for ti in tt) + (cc-1)*sum(core.h(ti) for ti in tt)
    print('        ll at the stored float c =', mp.nstr(l2, 12))
mp.mp.dps = 50
y = [mp.log(mp.mpf(v)) for v in xs]
P, I, k, ls = core.faces(y)
print('P =', mp.nstr(P, 12), ' I =', mp.nstr(I, 12))
