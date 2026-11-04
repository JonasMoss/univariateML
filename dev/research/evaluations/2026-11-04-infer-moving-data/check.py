"""Independent research calculations, not production fitter or coverage tests.

Run from the repository root through /tmp/umvl-cpu/run python3 <this file>.
No source/fixture regeneration. JSON is deterministic apart from environment.
"""
import hashlib
import json
import math
import platform
import subprocess
import sys
import warnings
from pathlib import Path

import numpy as np
import scipy
import sympy as sp
from scipy import integrate, optimize, special, stats

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
SEED = 20261104
rng = np.random.default_rng(SEED)
checks = []
quad_warnings = []


def check(name, actual, expected, atol=2e-8, rtol=2e-7):
    error = float(np.max(np.abs(np.asarray(actual) - expected)))
    ok = bool(np.allclose(actual, expected, atol=atol, rtol=rtol))
    checks.append(dict(name=name, passed=ok, max_absolute_error=error,
                       atol=atol, rtol=rtol))
    if not ok:
        raise AssertionError(checks[-1])


def quad(fun, points, absolute=1e-22):
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter('always', integrate.IntegrationWarning)
        pairs = [integrate.quad(fun, a, b, epsabs=absolute, epsrel=2e-8,
                                limit=250)
                 for a, b in zip(points[:-1], points[1:]) if b > a]
        quad_warnings.extend(dict(message=str(w.message), points=points,
                                  absolute_tolerance=absolute) for w in caught)
    return sum(v for v, e in pairs), sum(e for v, e in pairs)


def sqdiff(l0, l1):
    # Stable squared difference of square roots, with no subtraction of affinities.
    return math.exp(max(l0, l1)) * math.expm1(-abs(l0-l1)/2)**2


def distances():
    rows = []
    hs = [0.01, 0.003, 0.001, 0.0003, 0.0001, 0.00003]
    for family in ['gpd', 'gev', 'ged']:
        shapes = [1, 4/3, 2, 3, 5] if family != 'ged' else [.3, .5, .6, 1, 1.2, 2]
        for p in shapes:
            vals = []
            for h in hs:
                cuts = sorted(set([0, h, 10*h, .1, 1]))
                if family == 'gpd':
                    # mu=0, endpoint 1 -> 1+h, xi=-1/p, sigma=endpoint/p.
                    def fun(d):
                        l0 = math.log(p)+(p-1)*math.log(d)
                        l1 = math.log(p)+(p-1)*math.log(d+h)-p*math.log1p(h)
                        return sqdiff(l0, l1)
                    v, err = quad(fun, cuts)
                    v += (h/(1+h))**p
                    gamma = min(2, p)
                elif family == 'gev':
                    # Reverse Weibull: endpoint 0 -> h, b=1, xi=-1/p.
                    def fun(d):
                        l0 = math.log(p)+(p-1)*math.log(d)-d**p
                        l1 = math.log(p)+(p-1)*math.log(d+h)-(d+h)**p
                        return sqdiff(l0, l1)
                    v, err = quad(fun, cuts+[np.inf])
                    v += -math.expm1(-h**p)
                    gamma = min(2, p)
                else:
                    # Exponential-power scale a=1; sd=sqrt(Gamma(3/p)/Gamma(1/p)).
                    norm = math.log(p)-math.log(2)-special.gammaln(1/p)
                    v, err = quad(lambda x: sqdiff(norm-abs(x)**p,
                                                  norm-abs(x-h)**p),
                                  [-np.inf, -1, -h, 0, h, 2*h, 1, np.inf])
                    gamma = min(2, 1+2*p)
                vals.append(v)
                critical = p == (0.5 if family == 'ged' else 2)
                rows.append(dict(family=family, shape=p, h=h, H2=v,
                                 quad_error_estimate=err, expected_power=gamma,
                                 scaled=v/(h**gamma*(math.log(1/h) if critical else 1)),
                                 critical=critical))
            slope = math.log(vals[-1]/vals[-2])/math.log(hs[-1]/hs[-2])
            target = gamma - (1/math.log(1/math.sqrt(hs[-1]*hs[-2])) if critical else 0)
            check(f'{family} shape {p}: Hellinger slope', slope, target,
                  atol=.13, rtol=0)
    return rows


def snorm(x, th):
    # Raw mode, log geometric side scale, log xi (a smooth invertible chart).
    c, a, b = th
    s = math.exp(a)
    z = (x-c)/s
    sign = 1 if z > 0 else -1
    w = math.exp(-2*sign*b)
    v = z*z*w
    ld = .5*math.log(2/math.pi)-a-np.logaddexp(b, -b)-v/2
    score = np.array([z*w/s, -1+v, -math.tanh(b)+sign*v])
    H = np.array([[w/s**2, 2*z*w/s, 2*sign*z*w/s],
                  [2*z*w/s, 2*v, 2*sign*v],
                  [2*sign*z*w/s, 2*sign*v, 1/math.cosh(b)**2+2*v]])
    return ld, score, H


def huber(x, th):
    mu, a, b = th
    s, c = math.exp(a), math.exp(b)
    z = (x-mu)/s
    r = abs(z)
    tail = r > c
    Z = math.sqrt(2*math.pi)*special.erf(c/math.sqrt(2))+2*math.exp(-c*c/2)/c
    B = 2*math.exp(-c*c/2)/(c*c*Z)
    Bp = B*(-c-2/c+B)
    psi = np.clip(z, -c, c)
    ld = -a-math.log(Z)-(c*r-c*c/2 if tail else z*z/2)
    score = np.array([psi/s, z*psi-1, c*(B-max(r-c, 0))])
    H = np.array([[0 if tail else 1/s**2,
                   c*np.sign(z)/s if tail else 2*z/s,
                   -c*np.sign(z)/s if tail else 0],
                  [c*np.sign(z)/s if tail else 2*z/s,
                   c*r if tail else 2*z*z, -c*r if tail else 0],
                  [-c*np.sign(z)/s if tail else 0, -c*r if tail else 0,
                   -c*B-c*c*Bp+(c*r-2*c*c if tail else 0)]])
    return ld, score, H


def matrices(family, model, th, breaks):
    pts = [-np.inf]+breaks+[np.inf]
    I = np.zeros((3, 3))
    EH = np.zeros((3, 3))
    mean = np.zeros(3)
    mass = quad(lambda x: math.exp(model(x, th)[0]), pts, 1e-11)[0]
    for i in range(3):
        mean[i] = quad(lambda x: math.exp(model(x, th)[0])*model(x, th)[1][i],
                       pts, 1e-11)[0]
        for j in range(i, 3):
            I[i, j] = I[j, i] = quad(
                lambda x: math.exp(model(x, th)[0])*np.prod(model(x, th)[1][[i, j]]),
                pts, 1e-11)[0]
            EH[i, j] = EH[j, i] = quad(
                lambda x: math.exp(model(x, th)[0])*model(x, th)[2][i, j],
                pts, 1e-11)[0]
    check(f'{family} {th}: normalization', mass, 1)
    check(f'{family} {th}: mean score', mean, 0)
    check(f'{family} {th}: Bartlett', I, EH)
    eig = np.linalg.eigvalsh(I)
    if eig[0] <= 0:
        raise AssertionError((family, th, eig))
    # Check the hand derivatives independently by differentiating the density/score.
    for x in [-2.123, -.123, .347, 2.467]:
        if any(abs(x-k) < 1e-4 for k in breaks):
            continue
        score = np.zeros(3)
        H = np.zeros((3, 3))
        for j in range(3):
            up, dn = np.array(th), np.array(th)
            up[j] += 1e-5
            dn[j] -= 1e-5
            score[j] = (model(x, up)[0]-model(x, dn)[0])/2e-5
            H[:, j] = -(model(x, up)[1]-model(x, dn)[1])/2e-5
        check(f'{family} {th} x={x}: score derivative', score, model(x, th)[1], atol=1e-7)
        check(f'{family} {th} x={x}: Hessian derivative', H, model(x, th)[2], atol=1e-7)
    return dict(family=family, coordinates=('mode,log_side_scale,log_xi' if family == 'snorm'
                                          else 'mu,log_sigma,log_epsilon'),
                theta=th, I=I.tolist(), expected_H=EH.tolist(),
                eigenvalues=eig.tolist(), mean_score=mean.tolist(), mass=mass)


def tlambda_information(lam):
    def score(u):
        v = 1-u
        lu, lv = math.log(u), math.log1p(-u)
        A = u**(lam-1)+v**(lam-1)
        B = (lam-1)*(u**(lam-2)-v**(lam-2))
        Al = u**(lam-1)*lu+v**(lam-1)*lv
        if lam == 0:
            q, ql = lu-lv, (lu*lu-lv*lv)/2
        else:
            q = (u**lam-v**lam)/lam
            ql = (u**lam*lu-v**lam*lv-q)/lam
        return np.array([B/(A*A), -1+B*q/(A*A), -Al/A+B*ql/(A*A)])
    # Symmetry separates the odd location score from even scale/shape scores.
    I = np.zeros((3, 3))
    mean = np.zeros(3)
    for i in range(3):
        mean[i] = integrate.quad(lambda t: (0 if i == 0 else 2*score(math.exp(-t))[i])*math.exp(-t),
                                 math.log(2), 150, epsabs=1e-9)[0]
        for j in range(i, 3):
            I[i, j] = I[j, i] = (0 if (i == 0) != (j == 0) else integrate.quad(
                lambda t: 2*score(math.exp(-t))[i]*score(math.exp(-t))[j]*math.exp(-t),
                math.log(2), 150, epsabs=1e-8)[0])
    check(f'tlambda {lam}: mean score', mean, 0, atol=1e-7)
    if np.linalg.eigvalsh(I)[0] <= 0:
        raise AssertionError(I)
    return dict(family='tlambda', lambda_=lam, coordinates='mu,log_sigma,lambda at mu=0,sigma=1',
                I=I.tolist(), eigenvalues=np.linalg.eigvalsh(I).tolist(),
                integration='u=exp(-t), t in [log(2),150], symmetry; omitted tail not certified')


def tlambda_distances():
    rows = []
    for lam in [.25, .5, .75, 1, 1.5, 2]:
        def D(u):
            return (u**lam-math.expm1(lam*math.log1p(-u)))/lam
        def inv(target):
            if target > 1/lam:
                return 1-inv(2/lam-target)
            if target <= 0:
                return 0.
            return math.exp(optimize.brentq(lambda t: D(math.exp(t))-target,
                                            -740, math.log(.5), xtol=1e-12))
        def logA(u):
            return np.logaddexp((lam-1)*math.log(u), (lam-1)*math.log1p(-u))
        for h in [.01, .003, .001]:
            if lam in [1, 2]:
                val, err = lam*h, 0.
            else:
                uc = inv(h)
                def fun(u):
                    v = inv(D(u)-h)
                    if v <= 0 or v >= 1:
                        return 1.
                    return math.expm1((logA(u)-logA(v))/2)**2
                cuts = sorted(set([uc, max(uc, 1e-8), max(uc, 1e-5),
                                   max(uc, .001), .1, .5]))
                # Flatten the nonanalytic overlap boundary before quadrature.
                transformed = [((u-uc)/(.5-uc))**.25 for u in cuts]
                val, err = quad(lambda w: fun(uc+(.5-uc)*w**4)*4*(.5-uc)*w**3,
                                transformed, 1e-12)
                # The upper quantile endpoint is integrably singular too. Retain
                # its small tail explicitly instead of rounding 1-exp(-t) to one.
                def upper(t):
                    r = math.exp(-t)
                    v = inv(2/lam-D(r)-h)
                    log_au = np.logaddexp((lam-1)*math.log1p(-r), (lam-1)*(-t))
                    return math.expm1((log_au-logA(v))/2)**2*r
                top, top_err = quad(upper, [math.log(2), 5, 15, 40, 80, 150], 1e-12)
                val += top
                err += top_err
                val += 2*uc
            power = min(2, 1/lam) if lam < 1 else 1
            rows.append(dict(lambda_=lam, h=h, H2=val, quad_error_estimate=err,
                             expected_power=power, critical=lam == .5,
                             scaled=val/(h**power*(math.log(1/h) if lam == .5 else 1))))
        last = rows[-3:]
        slope = math.log(last[-1]['H2']/last[-2]['H2'])/math.log(.001/.003)
        target = power-(1/math.log(1/math.sqrt(.001*.003)) if lam == .5 else 0)
        check(f'tlambda {lam}: Hellinger slope', slope, target, atol=.13, rtol=0)
    return rows


def calibration(info):
    rows = []
    reps = 3000
    for row in info:
        family, th = row['family'], row['theta']
        # Moderate interior cells only. These are tests at a fully specified null.
        if not ((family == 'snorm' and th[2] == 0) or
                (family == 'huber' and abs(math.exp(th[2])-1.345) < 1e-10)):
            continue
        I = np.asarray(row['I'])
        for n in [100, 1000]:
            accepted = 0
            for batch in range(0, reps, 100):
                if family == 'snorm':
                    z = rng.normal(size=(100, n))
                    scores = np.stack([z, z*z-1, np.sign(z)*z*z], axis=-1)
                else:
                    c = math.exp(th[2])
                    Z = math.sqrt(2*math.pi)*special.erf(c/math.sqrt(2))+2*math.exp(-c*c/2)/c
                    pc = math.sqrt(2*math.pi)*special.erf(c/math.sqrt(2))/Z
                    u = rng.random((100, n))
                    core = u < pc
                    z = np.empty_like(u)
                    z[core] = special.ndtri(special.ndtr(-c)+u[core]/pc*
                                            (special.ndtr(c)-special.ndtr(-c)))
                    # Conditional tails are independent exponential excesses.
                    z[~core] = (c+rng.exponential(1/c, size=np.count_nonzero(~core)))*rng.choice([-1, 1], size=np.count_nonzero(~core))
                    B = 2*math.exp(-c*c/2)/(c*c*Z)
                    psi = np.clip(z, -c, c)
                    scores = np.stack([psi, z*psi-1,
                                       c*(B-np.maximum(abs(z)-c, 0))], axis=-1)
                totals = np.sum(scores, axis=1)
                statistic = np.einsum('bi,ij,bj->b', totals, np.linalg.inv(I), totals)/n
                accepted += int(np.sum(statistic <= stats.chi2.ppf(.95, 3)))
            rate = accepted/reps
            rows.append(dict(family=family, n=n, reps=reps, accepted=accepted,
                             coverage=rate, MCSE=math.sqrt(rate*(1-rate)/reps),
                             hypothesis='all three parameters equal fully specified null; chi-square(3) score test'))
    # A monotone location solve with held shape and a=1, separate from production fits.
    for nu in [1, 1.2, 1.5, 2]:
        I = nu*nu*math.exp(special.gammaln(2-1/nu)-special.gammaln(1/nu))
        for n in [100, 1000]:
            reps_ged = 1200
            locs, hs, js = [], [], []
            for b in range(reps_ged):
                x = rng.gamma(1/nu, size=n)**(1/nu)*rng.choice([-1, 1], size=n)
                if nu == 1:
                    mu = float(np.median(x))
                else:
                    # Strictly decreasing continuous total score; bracketed by sample extremes.
                    mu = optimize.brentq(lambda m: np.sum(np.sign(x-m)*abs(x-m)**(nu-1)),
                                          np.min(x), np.max(x), xtol=1e-12)
                residual = x-mu
                locs.append(mu)
                hs.append(0 if nu == 1 else np.mean(nu*(nu-1)*abs(residual)**(nu-2)))
                js.append(np.mean(nu*nu*abs(residual)**(2*nu-2)))
            rate = float(np.mean(np.abs(locs)*math.sqrt(n*I) <= stats.norm.ppf(.975)))
            rows.append(dict(family='ged', nu=nu, scale_a=1, n=n, reps=reps_ged,
                             coverage=rate, MCSE=math.sqrt(rate*(1-rate)/reps_ged),
                             median_H_over_I=float(np.median(hs)/I),
                             median_OPG_over_I=float(np.median(js)/I),
                             hypothesis='location=0, scale and shape known; model Fisher Wald for exact location MLE'))
    return rows


def extreme_information():
    x, mu, a, xi = sp.symbols('x mu a xi', real=True)
    tv = sp.Symbol('t', positive=True)
    z = (x-mu)*sp.exp(-a)
    t = 1+xi*z
    rows = []
    for family in ['gpd', 'gev']:
        parameters = [a, xi] if family == 'gpd' else [mu, a, xi]
        ld = -a-(1+1/xi)*sp.log(t)
        if family == 'gev':
            ld -= t**(-1/xi)
        expansion = -a-z+xi*(z*z/2-z)+xi*xi*(z*z/2-z**3/3)
        if family == 'gev':
            expansion -= sp.exp(-z)*(1+xi*z*z/2+xi*xi*(-z**3/3+z**4/8))
        for shape in [-.4, -.2, 0., .2, .8]:
            # Symbolically differentiate independently of the closed GPD Fisher formula.
            expression = expansion if shape == 0 else ld
            def stable(expr):
                if shape != 0:
                    # Transform AFTER differentiating at fixed x. Evaluating 1+xi*z
                    # directly loses the endpoint distance far into the Exp tail.
                    expr = sp.simplify(expr.subs(x, mu+sp.exp(a)*(tv-1)/xi))
                return expr.subs({mu: 0, a: 0})
            args = (x if shape == 0 else tv, xi)
            sf = sp.lambdify(args, [stable(sp.diff(expression, p)) for p in parameters], 'numpy')
            hf = sp.lambdify(args, [[stable(-sp.diff(expression, p, q)) for q in parameters]
                                   for p in parameters], 'numpy')
            d = len(parameters)
            def values(v):
                argument = (math.exp(shape*v) if shape else v) if family == 'gpd' else (
                    math.exp(-shape*math.log(v)) if shape else -math.log(v))
                return np.asarray(sf(argument, shape), float), np.asarray(hf(argument, shape), float)
            def integral(f):
                # v is Exp(1); split at 1 and avoid overflow in the irrelevant far tail.
                return quad(lambda v: math.exp(-v)*f(*values(v)), [0, .01, .1, 1, 10, 100], 1e-8)[0]
            I = np.array([[integral(lambda s, H: s[i]*s[j]) for j in range(d)] for i in range(d)])
            H = np.array([[integral(lambda s, H: H[i, j]) for j in range(d)] for i in range(d)])
            mean = np.array([integral(lambda s, H: s[i]) for i in range(d)])
            check(f'{family} {shape}: symbolic Bartlett', I, H, atol=3e-6, rtol=3e-6)
            check(f'{family} {shape}: mean score', mean, 0, atol=3e-6)
            if family == 'gpd':
                expected = np.array([[1/(1+2*shape), 1/((1+shape)*(1+2*shape))],
                                     [1/((1+shape)*(1+2*shape)), 2/((1+shape)*(1+2*shape))]])
                check(f'gpd {shape}: closed Fisher', I, expected, atol=3e-6, rtol=3e-6)
            if np.linalg.eigvalsh(I)[0] <= 0:
                raise AssertionError((family, shape, I))
            rows.append(dict(family=family, xi=shape, coordinates='log_sigma,xi' if d == 2 else 'mu,log_sigma,xi',
                             mu=0, sigma=1, I=I.tolist(), expected_H=H.tolist(),
                             eigenvalues=np.linalg.eigvalsh(I).tolist(), mean_score=mean.tolist(),
                             tail='Exp(1) variable truncated at 100; no interval error certificate'))
    return rows


def transition_checks():
    rows = []
    for nu in [1.2, 1.05, 1.01, 1.001]:
        for n in [1000, 1000000, 10**12]:
            # Fraction of E H_mumu supplied by distances >=1/n, with a=1.
            fraction = special.gammaincc(1-1/nu, (1/n)**nu)
            rows.append(dict(family='ged', nu=nu, n=n,
                             expected_H_fraction_outside_1_over_n=float(fraction)))
    # At nu=1/2 raw OPG has twice the critical-experiment information.
    f0 = .25  # scale a=1
    K = f0/8
    for n in [1000, 1000000, 10**12]:
        h = 1/math.sqrt(n*math.log(n))
        truncated = f0*special.exp1(math.sqrt(h))
        rows.append(dict(family='ged-critical', n=n, h=h,
                         raw_OPG_limit_over_n_log_n=f0/2,
                         critical_LAN_information=2*K,
                         truncated_OPG_expectation_over_log_n=float(truncated/math.log(n))))
    return rows


def main():
    info = [matrices('snorm', snorm, [0., 0., math.log(xi)], [0.])
            for xi in [.5, 1, 1.5, 3]]
    info += [matrices('huber', huber, [0., 0., math.log(c)], [-c, c])
             for c in [.1, .5, 1.345, 3, 6]]
    # Moment-coordinate normal member: mode=mean-2*m*log(xi)+o(log(xi)).
    J = np.array([[1, 0, -2*math.sqrt(2/math.pi)], [0, 1, 0], [0, 0, 1]])
    moment_I = J.T @ np.array(info[1]['I']) @ J
    check('snorm symmetry: moment information', moment_I, np.diag([1, 2, 3-8/math.pi]))
    dists = distances()
    td = tlambda_distances()
    ti = [tlambda_information(lam) for lam in [-1, -.3, 0, .25, .4]]
    cal = calibration(info)
    extreme = extreme_information()
    source = {}
    for fam in ['tlambda', 'ged', 'gpd', 'gev', 'snorm', 'huber']:
        for filename in ['README.md', 'mle.hpp']:
            p = ROOT/'univariateML-cpp/include/cppunivariate/distributions'/fam/filename
            source[str(p.relative_to(ROOT))] = hashlib.sha256(p.read_bytes()).hexdigest()
    result = dict(seed=SEED, revision=subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
                  environment=dict(python=sys.version, numpy=np.__version__, scipy=scipy.__version__,
                                   sympy=sp.__version__, platform=platform.platform()),
                  source_sha256=source, script_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                  H2_definition='integral (sqrt(f)-sqrt(g))^2 dx, no factor 1/2',
                  distances=dists, tlambda_distances=td, information=info,
                  snorm_symmetry_moment_information=moment_I.tolist(), tlambda_information=ti,
                  calibration=cal, checks=checks, quadrature_warnings=quad_warnings)
    result['extreme_information'] = extreme
    result['transition_checks'] = transition_checks()
    (HERE/'results.json').write_text(json.dumps(result, indent=2, allow_nan=False)+'\n')
    print(f'{len(checks)} assertions passed; {len(quad_warnings)} quadrature warnings')
    assert not quad_warnings, quad_warnings
    for row in cal:
        print(row)


if __name__ == '__main__':
    main()
