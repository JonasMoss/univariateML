#!/usr/bin/env python3
"""fatigue, mu = 0 held, on the near-constant class: the maximizer over (alpha, beta), at 120 digits.

log f = log(t+b) - 3/2 log t - 1/2 log b - log(2 a) - 1/2 log(2 pi) - (t/b + b/t - 2)/(2 a^2).
alpha profiles out in closed form: a^2 = mean(t/b + b/t - 2).
"""
import mpmath as mp
from common import *
mp.mp.dps = 120
x, held = sample('fatigue'); r = record('fatigue')
n = 3; eps = mp.log(x[2]/x[0]); d = delta(x)

def ll(a, b):
    return mp.fsum(mp.log(t+b) - mp.mpf(3)/2*mp.log(t) - mp.log(b)/2 - mp.log(2*a) - mp.log(2*mp.pi)/2
                   - (t/b + b/t - 2)/(2*a*a) for t in x)
def a_of(b): return mp.sqrt(mp.fsum(t/b + b/t - 2 for t in x)/n)
def prof(v):                       # v = log(b/x0)
    b = x[0]*mp.exp(v); return ll(a_of(b), b)

print('data', [float(t).hex() for t in x], 'held', held)
print('delta =', mp.nstr(d, 12), ' eps = log(x3/x0) =', mp.nstr(eps, 12))
# global scan of the profile in log beta (beta from 1e-3 x0 to 1e3 x0): where is the max?
vs = [mp.mpf(k)/4 for k in range(-28, 29)]
vals = [(prof(v), v) for v in vs]
print('coarse scan, best v =', mp.nstr(max(vals)[1], 5), ' (profile is unimodal on the grid:',
      all(vals[i][0] <= vals[i+1][0] for i in range(0, 28)) and all(vals[i][0] >= vals[i+1][0] for i in range(28, 56)), ')')
# fine scan on the scale eps (beta between and around the data)
fine = [(prof(eps*mp.mpf(k)/20), eps*mp.mpf(k)/20) for k in range(-100, 101)]
print('fine scan on v = k eps/20, k in [-100,100]: best k =', max(fine)[1]/eps*20)
ps = lambda s_: prof(eps*s_)
v_hat = eps*mp.findroot(lambda s_: mp.diff(ps, s_), mp.mpf(1)/3, tol=mp.mpf(10)**-50)
b_hat = x[0]*mp.exp(v_hat); a_hat = a_of(b_hat)
print('v_hat/eps =', mp.nstr(v_hat/eps, 20), '(prediction 1/3)')
print('alpha_hat =', mp.nstr(a_hat, 15), ' alpha_hat/eps =', mp.nstr(a_hat/eps, 15), '(prediction sqrt(2)/3 =', mp.nstr(mp.sqrt(2)/3, 15), ')')
print('beta_hat  =', mp.nstr(b_hat, 20), '  (beta_hat - x0)/(x3 - x0) =', mp.nstr((b_hat-x[0])/(x[2]-x[0]), 12))
print('loglik_hat =', mp.nstr(prof(v_hat), 20))
# curvature of the profile in log beta and in the log-alpha direction (2x2 Hessian in (log a, log b))
H = mp.matrix(2, 2)
f2 = lambda la, lb: ll(mp.exp(la), mp.exp(lb))
la0, lb0 = mp.log(a_hat), mp.log(b_hat)
H[0,0] = mp.diff(f2, (la0, lb0), (2, 0)); H[1,1] = mp.diff(f2, (la0, lb0), (0, 2)); H[0,1] = H[1,0] = mp.diff(f2, (la0, lb0), (1, 1))
print('Hessian in (log alpha, log beta):', [[mp.nstr(H[i,j], 8) for j in range(2)] for i in range(2)], ' eigenvalues', [mp.nstr(e, 8) for e in mp.eigsy(H)[0]])
print('profile curvature d2/dv2 =', mp.nstr(mp.diff(prof, v_hat, 2), 12), ' (prediction -3*(1+ ... ) approx -9/ (2*eps^2/9... see report)')
# binary64 representability and the rounding problem
bd = float(b_hat)
print('alpha_hat representable:', representable(a_hat), ' beta_hat representable:', representable(b_hat),
      '  beta_hat strictly between the doubles', float(x[0]).hex(), 'and', float(x[2]).hex())
for name, bval in (('round(beta_hat) = x0', x[0]), ('x3', x[2])):
    av = a_of(bval)
    print('beta =', name, ': conditional best alpha =', mp.nstr(av, 12), ' loglik =', mp.nstr(ll(av, bval), 15),
          ' loss vs sup =', mp.nstr(prof(v_hat)-ll(av, bval), 8), ' (log(3/2)*3/2 =', mp.nstr(mp.mpf(3)/2*mp.log(mp.mpf(3)/2), 8), ')')
# the fit's candidate
cand = r['result']['candidates'][0]['estimates']
ac, bc = hexval(cand['alpha']), hexval(cand['beta'])
print('fit candidate: alpha =', mp.nstr(ac, 12), 'beta = x0+%d ulp' % int((bc-x[0])/(mp.mpf(2)**-51)), ' loglik exact =', mp.nstr(ll(ac, bc), 15),
      ' reported loglik_lower/upper', float.fromhex(r['result']['candidates'][0]['loglik_lower']))
print('candidate alpha / alpha_hat =', mp.nstr(ac/a_hat, 12))
print('loglik at (alpha_hat, beta = x0):', mp.nstr(ll(a_hat, x[0]), 15), ' loss', mp.nstr(prof(v_hat)-ll(a_hat, x[0]), 8))
