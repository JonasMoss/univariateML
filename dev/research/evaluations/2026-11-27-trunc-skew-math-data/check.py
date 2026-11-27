"""Replay selected joint likelihood limits. The report supplies the proofs."""
import csv
from pathlib import Path

import mpmath as mp


mp.mp.dps = 80
digits = (3, 6, 12, 20)
rows = []


def record(case, step, value, limit, tolerance):
    error = abs(value / limit - 1)
    assert error < tolerance, (case, step, value, limit, error, tolerance)
    rows.append((case, str(step), *(mp.nstr(v, 35) for v in
                                  (value, limit, error, tolerance))))


def product(kernel, cuts, sample):
    return mp.fprod(kernel(x) for x in sample) / mp.quad(kernel, cuts)**len(sample)


def Phi(z):
    return mp.erfc(-z / mp.sqrt(2)) / 2


def two_piece(skew, mean_abs):
    D = mp.sqrt((1 - mean_abs**2) * (skew**2 + skew**-2) + 2 * mean_abs**2 - 1)
    return -mean_abs * (skew - 1/skew) / D, skew / D, 1 / (D*skew)


for digits_step in digits:
    e = mp.mpf(10)**-digits_step
    ged = lambda x: mp.exp(2*x - e*x*x)
    record('GED remote Gaussian, increasing left-ray tilt', e,
           product(ged, [-mp.inf, 0], [-1, 0]), 4/mp.exp(2), 5*e)
    huber = lambda x: mp.exp(-x*x/(2*e) if abs(x) <= e else -abs(x)+e/2)
    laplace = lambda x: mp.exp(-abs(x))
    record('Huber finite-center Laplace', e, product(huber, [-1, -e, e, 2], [0, 1]),
           product(laplace, [-1, 0, 2], [0, 1]), 2*e)

    sn = lambda x: mp.exp(-x*x/2)*Phi(x/e)
    sn_z = mp.quad(lambda x: mp.exp(-x*x/2), [0, 1])
    record('SN half Gaussian at join', e, product(sn, [-1, -e, 0, e, 1], [0, 1]),
           mp.exp(-mp.mpf('0.5'))/(2*sn_z**2), e**2)

    m1 = mp.sqrt(2/mp.pi)
    A = 1-m1*m1
    c0 = -m1/mp.sqrt(A)
    S = 1/mp.sqrt(A)
    H = m1/(2*A)
    center, plus, minus = two_piece(1/mp.sqrt(e), m1)
    snorm = lambda x: mp.exp(-((x-center)/(plus if x >= center else minus))**2/2)
    limit_z = mp.quad(lambda x: mp.exp(-((x-c0)/S)**2/2), [c0, c0+1])
    record('snorm skew half law at moving join', e,
           product(snorm, [c0-1, center-30*minus, center, c0+1], [c0, c0+1]),
           mp.exp(-(H*H+1/S**2)/2)/limit_z**2, 30*e)

    df = mp.mpf(3)
    m1_t = mp.gamma((df-1)/2)/mp.gamma(df/2)*mp.sqrt((df-2)/mp.pi)
    A_t = 1-m1_t*m1_t
    c0_t = -m1_t/mp.sqrt(A_t)
    H_t = m1_t/(2*A_t)
    center, plus, minus = two_piece(1/mp.sqrt(e), m1_t)
    t = lambda z: (1+z*z)**-2
    zcut = (c0_t-center)/minus
    zbound = (c0_t-1-center)/minus
    # Three ties and one off-cut value give the critical gamma=4 product.
    height = t(zcut)**3*t(zbound)/(minus*mp.quad(t, [-mp.inf, zcut])
                                     - minus*mp.quad(t, [-mp.inf, zbound]))**4
    critical = t(-H_t)**3/mp.quad(t, [-mp.inf, -H_t])**4
    record('sstd critical skew contact', e, height, critical, 20*e)

    slash = lambda z: -mp.expm1(-z*z/2)/(z*z) if z else mp.mpf('0.5')
    z_slash = mp.quad(slash, [0, 1, 1/e])
    j_slash = mp.sqrt(mp.pi/2)
    record('Slash critical held-center half-window', e,
           slash(0)*slash(1/e)/(e*z_slash)**2, slash(0)/j_slash**2, 3*e)

    # Tukey shape 1/2, with lower support edge held at zero along the path.
    lam = mp.mpf('0.5')
    Q = lambda u: (u**lam-(1-u)**lam)/lam
    root = lambda x: mp.findroot(lambda z: Q(z*z)+2-e*x, (0, e*x), tol=mp.eps)**2
    u1, u2 = root(1), root(2)
    kernel = lambda u: 1/(u**(lam-1)+(1-u)**(lam-1))
    record('Tukey broad lower edge, lambda 1/2', e,
           e*e*kernel(u1)*kernel(u2)/(u2-u1)**2, mp.mpf(8)/9, 5*e)

    remote_df = 2/e
    remote_center = 1/e
    denom = remote_df + remote_center**2
    remote_student = lambda x: mp.exp(-(remote_df+1)/2 * mp.log1p(
        (x*x-2*x*remote_center)/denom))
    record('LST joint large df and remote center', e,
           product(remote_student, [-mp.inf, -1, 0], [-1, 0]), 4/mp.exp(2), 10*e)
    s = mp.sqrt(e)
    normalizer = s**(-e)*mp.sqrt(mp.pi)*mp.gamma(e/2)/(2*mp.gamma((e+1)/2))
    small_df_numerator = s**(-1-e)*(1+s*s)**(-(e+1)/2)
    record('HT small df with a tie on the ray', e,
           small_df_numerator/(normalizer**2*e**mp.mpf('1.5')),
           mp.mpf(1), 5*e*abs(mp.log(e)))
    # Inward distance x=s sinh(t) leaves a nearly flat integral of log length.
    log_length = -mp.log(s)
    bounded_integral = mp.quad(lambda t: (s*mp.cosh(t))**(-e), [0, mp.asinh(1/s)])
    record('HT small df with a tie on a bounded window', e,
           small_df_numerator*s*(log_length/bounded_integral)**2,
           mp.mpf(1), 3/log_length)

    emg = lambda x: mp.exp(-x)*Phi(x/e-e)
    cuts = [-1, -30*e, 0, 30*e, 1]
    record('EMG translated exponential with half join', e,
           product(emg, cuts, [0, 1]), mp.exp(-1)/(2*(-mp.expm1(-1))**2), 3*e)
    small_rate = lambda x: mp.exp(-e*x)*Phi(x-e)
    limit_k = lambda x: Phi(x)
    record('EMG zero rate on left ray', e,
           product(small_rate, [-mp.inf, -1, 0], [-1, 0]),
           product(limit_k, [-mp.inf, -1, 0], [-1, 0]), 5*e)

    bhatt = lambda x: Phi((x+1)/e)-Phi((x-1)/e)
    record('Bhattacharjee uniform with half endpoints', e,
           product(bhatt, [-2, -1, -1+30*e, 0, 1-30*e, 1, 2], [-1, 1]),
           mp.mpf(1)/16, e**2)

    record('Double Pareto infinite left rate on finite window', e,
           (mp.exp(-1)/(-e*mp.expm1(-1/e)-mp.expm1(-1)))**2,
           (mp.exp(-1)/(-mp.expm1(-1)))**2, 5*e)
    p = 1/e
    ged_edge = mp.exp(-mp.exp(p*mp.log(3)/2+(p/2)*(mp.loggamma(3/p)-mp.loggamma(1/p))))
    record('GED at limiting uniform endpoint', e, ged_edge, mp.exp(-mp.exp(-mp.euler)), 5*e)

    logit_ratio = lambda y: mp.exp(-e*y*y)
    logit_cut = mp.log(3)
    record('logit-normal logit-uniform escape', e,
           (mp.mpf(16)/3)**2*product(logit_ratio, [-logit_cut, 0, logit_cut],
                                     [-logit_cut, logit_cut]),
           (mp.mpf(16)/3/(2*logit_cut))**2, 5*e)
    loggamma_kernel = lambda y: y**(e-1)*mp.exp(-e*y)
    record('log-gamma corner', e,
           mp.exp(-5)*product(loggamma_kernel, [1, 2, 4], [1, 4]),
           mp.exp(-5)/(4*mp.log(4)**2), 5*e)

path = Path(__file__).with_name('checks.csv')
with path.open('w', newline='') as stream:
    writer = csv.writer(stream)
    writer.writerow(('case', 'step', 'likelihood_or_kernel', 'limit', 'relative_error', 'tolerance'))
    writer.writerows(rows)
print(f'{len(rows)} checks passed at {mp.mp.dps} decimal digits')
