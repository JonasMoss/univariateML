"""Independent symbolic identities and high-precision finite checks for TASK-139.

No fitting, searches for optima, or calls to the package. Run at repo root.
Numerical quadrature and finite grids illustrate the proofs; they are not
interval certificates. The BB tail bound is analytic (a Markov bound).
"""
import json
import platform
from pathlib import Path
import mpmath as mp
import sympy as sp

mp.mp.dps = 60
x, mu = sp.symbols('x mu', positive=True)
c2 = x*(x-1)-2*mu*x+mu**2
c3 = x*(x-1)*(x-2)-3*mu*x*(x-1)+3*mu**2*x-mu**3
a2 = -x*(x-1)*(2*x-1)/6+mu*x*(x+1)/2-mu**3/6-mu**2/2
assert sp.expand(a2+c3/3+(mu+1)*c2/2) == 0

def poisson_expect(expr):
    poly = sp.Poly(sp.expand(expr), x)
    return sp.simplify(sum(coef * sum(sp.functions.combinatorial.numbers.stirling(power[0], j, kind=2)*mu**j
                                    for j in range(power[0]+1))
                           for power, coef in poly.terms()))
assert poisson_expect(c2) == 0
assert poisson_expect(a2) == 0
assert poisson_expect(c2*c3) == 0
assert poisson_expect(c2**2) == 2*mu**2
assert poisson_expect(c3**2) == 6*mu**3
assert sp.simplify(poisson_expect(c2**2)*poisson_expect(a2**2)-poisson_expect(c2*a2)**2) == 4*mu**5/3

def enc(v):
    return mp.nstr(v, 24)

out = {'environment': {'python': platform.python_version(), 'mpmath': mp.__version__,
                       'sympy': sp.__version__, 'precision_decimal_digits': mp.mp.dps},
       'symbolic': {'mean_C2': '0', 'mean_A2': '0', 'gram_determinant': '4*mu**5/3'},
       'bb': [], 'leakage_counterexample': [], 'burr': []}

# Fixed-count square-root vectors plus a separately bounded omitted tail.
M = mp.mpf(2)
cutoff = 160
for N in (100, 1000, 10000):
    k = 1/mp.mpf(N)
    for route in ('positive', 'negative', 'diagonal', 'cubic'):
        u = {'positive': 2*k, 'negative': k/2,
             'diagonal': k, 'cubic': k+(M+1)*k*k}[route]
        a = 1/u
        b = a*(N/M-1)
        logp0 = mp.loggamma(b+N)-mp.loggamma(b)+mp.loggamma(a+b)-mp.loggamma(a+b+N)
        pp, gg = mp.exp(logp0), mp.exp(-M)
        scale = k if route in ('positive', 'negative') else k*k
        norm_error, norm_target, hell2 = mp.mpf(0), mp.mpf(0), mp.mpf(0)
        for j in range(cutoff+1):
            C2 = j*(j-1)-2*M*j+M*M
            A2 = -mp.mpf(j*(j-1)*(2*j-1))/6+M*j*(j+1)/2-M**3/6-M**2/2
            C3 = j*(j-1)*(j-2)-3*M*j*(j-1)+3*M*M*j-M**3
            target = {'positive': C2/4, 'negative': -C2/8,
                      'diagonal': A2/2, 'cubic': -C3/6}[route]*mp.sqrt(gg)
            delta = mp.sqrt(pp)-mp.sqrt(gg)
            norm_error += (delta/scale-target)**2
            norm_target += target*target
            hell2 += delta*delta
            pp = pp*(N-j)*(a+j)/((j+1)*(b+N-j-1)) if j < N else mp.mpf(0)
            gg = gg*M/(j+1)
        # E[2^X] <= (1-mu*u)^(-1/u), uniform NB factorial-moment bound.
        tail = ((1-M*u)**(-1/u)+mp.exp(M))*2**(-mp.mpf(cutoff+1))
        relative_error = mp.sqrt(norm_error/norm_target)
        assert relative_error < {100: mp.mpf('.20'), 1000: mp.mpf('.025'), 10000: mp.mpf('.003')}[N]
        assert tail/scale**2 < mp.mpf('1e-25')
        out['bb'].append({'N': N, 'route': route, 'relative_L2_error_on_counts_0_160': enc(relative_error),
                          'H_squared_on_counts_0_160': enc(hell2),
                          'omitted_squared_norm_bound_divided_by_scale_squared': enc(tail/scale**2)})

for t in map(mp.mpf, ('0.01', '0.0001', '0.000001')):
    p0 = (1-t*t)*(1+t**mp.mpf('1.5'))/2
    p1 = (1-t*t)*(1-t**mp.mpf('1.5'))/2
    inside = (mp.sqrt(p0)-mp.sqrt(mp.mpf('.5')))**2+(mp.sqrt(p1)-mp.sqrt(mp.mpf('.5')))**2
    ratio = t*t/(t*t+inside)
    gain = mp.log(2*p0)
    assert gain > 0 and ratio > mp.mpf('.99')
    out['leakage_counterexample'].append({'t': enc(t), 'R': enc(ratio), 'log_likelihood_gain_at_first_atom': enc(gain)})

for k in map(mp.mpf, ('0.001', '0.00001', '0.0000001')):
    sig = -mp.expm1(-k*mp.log1p(1/k))
    for route in ('fixed_shape', 'varying_shape', 'mixed'):
        alpha = {'fixed_shape': mp.mpf(1), 'varying_shape': 1+k**mp.mpf('.25'),
                 'mixed': 1+2*mp.sqrt(sig)}[route]
        def integrand(y):
            logr = mp.log(alpha)-(alpha-1)*y+k*mp.log(k)-(k+1)*mp.log1p(k*mp.exp(-alpha*y/k))
            return mp.exp(-y)*mp.expm1(logr/2)**2
        inside = mp.quad(integrand, [0, k, 1, mp.inf])
        ratio = sig/(sig+inside)
        assert 0 < ratio < 1
        if route == 'fixed_shape':
            assert ratio > mp.mpf('.99')
        if route == 'mixed':
            assert abs(ratio-mp.mpf('.5')) < mp.mpf('.1')
        out['burr'].append({'k': enc(k), 'route': route, 'sigma': enc(sig), 'A': enc(inside), 'R': enc(ratio)})

Path(__file__).with_name('checks.json').write_text(json.dumps(out, indent=2)+'\n')
print('PASS: symbolic means, Charlier identity and Gram determinant; 12 BB expansions;')
print('3 dominant-leakage likelihood counterexamples; 9 Burr leakage integrations.')
print(json.dumps(out, indent=2))
