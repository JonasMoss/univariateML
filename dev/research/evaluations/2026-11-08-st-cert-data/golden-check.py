"""Independent conditional location optimum on the frozen st golden sample.

This is not a claim about the all-free golden's global optimality.
The other three coefficients are held at their stored double values.
"""
from pathlib import Path
import json
import mpmath as mp

mp.mp.dps = 60
root = Path(__file__).resolve().parent
g = json.loads((root.parents[3] / 'univariateML-cpp/include/cppunivariate/distributions/st/golden.json').read_text())
# Round-trip through binary64 so the independent calculation uses the public
# inputs of probe.cpp, rather than decimal approximations to different data.
x = [mp.mpf(v) for v in g['data']]
xi, omega, alpha, nu = [mp.mpf(g['estimates'][p]) for p in ('xi', 'omega', 'alpha', 'nu')]
m = nu + 1
def pdf(z, df):
    return mp.gamma((df + 1)/2)/mp.gamma(df/2)/mp.sqrt(df*mp.pi)*(1 + z*z/df)**(-(df + 1)/2)
def cdf(z, df):
    b = mp.betainc(df/2, mp.mpf('.5'), 0, df/(df + z*z), regularized=True)/2
    return b if z < 0 else 1-b
def score(t):
    result = mp.mpf(0)
    for v in x:
        z = (v-t)/omega
        h = alpha*z*mp.sqrt(m/(nu + z*z))
        result += m*z/(nu + z*z) - pdf(h,m)/cdf(h,m)*alpha*nu*mp.sqrt(m)/(nu + z*z)**mp.mpf('1.5')
    return result/omega
optimum = mp.findroot(score, (xi-mp.mpf('1e-6'), xi+mp.mpf('1e-6')))
ll = sum(mp.log(2*pdf((v-optimum)/omega,nu)*cdf(alpha*(v-optimum)/omega*mp.sqrt(m/(nu+((v-optimum)/omega)**2)),m)/omega) for v in x)
row = next(line.split() for line in (root/'probe.txt').read_text().splitlines() if line.startswith('golden '))
estimate = mp.mpf(float(row[3]))
assert abs(estimate-optimum) < mp.mpf('1e-10')
out = dict(precision=mp.mp.dps, scope='xi free; omega, alpha, nu held at frozen golden binary64 coefficients',
           xi=str(optimum), logLik=str(ll), stored_xi=str(xi),
           stored_xi_difference=str(optimum-xi), certified_xi=str(estimate),
           certified_xi_error=str(abs(estimate-optimum)), stored_score=str(score(xi)))
(root/'golden-conditional-check.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out))
