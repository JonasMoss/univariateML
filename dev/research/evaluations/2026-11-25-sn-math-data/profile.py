"""Independent high-precision profile probes and deterministic bound checks."""
import mpmath as mp

mp.mp.dps = 70
X = tuple(map(mp.mpf, (-1, 0, 0, 0, 0, 0, 1)))
N = len(X)
LOG_TWO = mp.log(2)
B = mp.sqrt(2 / mp.pi)
C = N * mp.log(B)


def log_cdf(t):
    return mp.log(mp.erfc(-t / mp.sqrt(2)) / 2)


def mills(t):
    return mp.exp(-t*t/2 - mp.log(2*mp.pi)/2 - log_cdf(t))


def likelihood(alpha, eta, h):
    residuals = [eta*x-h for x in X]
    return C + N*mp.log(eta) + sum(
        -z*z/2 + log_cdf(alpha*z) for z in residuals
    )


def profile(alpha, eta_start, h_start):
    def score(eta, h):
        terms = []
        for x in X:
            z = eta*x-h
            terms.append(-z + alpha*mills(alpha*z))
        return N/eta + sum(x*g for x, g in zip(X, terms)), -sum(terms)

    eta, h = mp.findroot(score, (eta_start, h_start), tol=mp.mpf('1e-62'))
    assert eta > 0 and max(map(abs, score(eta, h))) < mp.mpf('1e-58')
    return eta, h, likelihood(alpha, eta, h)


def tangent_upper(left, right, reference):
    alpha, eta, h = reference
    args = [alpha*(eta*x-h) for x in X]
    ratios = [mills(t) for t in args]
    intercept = C + sum(log_cdf(t)-r*t for t, r in zip(args, ratios))
    q = sum(x*r for x, r in zip(X, ratios))
    s = sum(ratios)
    ss = sum(x*x for x in X)

    def upper(a):
        eta = (a*q + mp.sqrt((a*q)**2+4*ss*N))/(2*ss)
        h = -a*s/N
        return intercept + N*mp.log(eta) - (eta*eta*ss+N*h*h)/2 + a*(eta*q-h*s)

    return max(upper(left), upper(right))


def cover_example():
    # For |alpha| >= 8, the existing tail quadratic gives Q* = 9-49/71 > 8.
    # The Gaussian fit has Q = 2, so the tail bound is strictly below it.
    assert 9 - mp.mpf(49)/71 > 8
    nodes = [(mp.mpf(0), *profile(mp.mpf(0), mp.sqrt(mp.mpf(N)/2), 0))]
    for j in range(1, 33):
        _, eta, h, _ = nodes[-1]
        a = mp.mpf(j)/4
        nodes.append((a, *profile(a, eta, h)))
    best = max(node[3] for node in nodes)
    pending = list(zip(nodes[:-1], nodes[1:]))
    splits = 0
    while pending:
        left, right = pending.pop()
        a, b = left[0], right[0]
        upper = min(tangent_upper(a, b, left[:3]), tangent_upper(a, b, right[:3]))
        if upper <= best + mp.mpf('1e-5'):
            continue
        splits += 1
        assert splits < 4096
        mid = (a+b)/2
        middle = (mid, *profile(mid, left[1], left[2]))
        best = max(best, middle[3])
        pending.extend(((left, middle), (middle, right)))
    print('sn symmetric half-cover subdivisions:', splits, 'gap <= 1e-5 numerically')


def student_cdf(z, nu):
    if z == 0:
        return mp.mpf('0.5')
    thin = mp.betainc(nu/2, mp.mpf('0.5'), 0, nu/(nu+z*z), regularized=True)/2
    return thin if z < 0 else 1-thin


def student_log_base(z, nu):
    return (mp.loggamma((nu+1)/2) - mp.loggamma(nu/2) - mp.log(mp.pi*nu)/2
            - (nu+1)*mp.log1p(z*z/nu)/2)


def st_normal_checks():
    count = 0
    for nu in map(mp.mpf, (2, 3, 8, 32, 128, 1024)):
        for z in map(mp.mpf, ('-4', '-2', '-0.5', '0', '0.5', '2', '4')):
            base_diff = abs(student_log_base(z, nu) + (mp.log(2*mp.pi)+z*z)/2)
            base_bound = -mp.log1p(-1/nu) + z*z/(2*nu) + (nu+1)*z**4/(4*nu**2)
            assert base_diff <= base_bound
            q = z*mp.sqrt((nu+1)/(nu+z*z))
            for alpha in map(mp.mpf, ('-1000', '-3', '-1', '0', '1', '3', '1000')):
                arg = alpha*q
                cdf_diff = abs(student_cdf(arg, nu+1) - mp.exp(log_cdf(alpha*z)))
                cdf_bound = (8/(nu+1) + mp.sqrt(2/(nu+1))/(mp.sqrt(2*mp.pi*mp.e))
                             + abs(mp.log((nu+1)/(nu+z*z)))/(2*mp.sqrt(2*mp.pi*mp.e)))
                assert cdf_diff <= cdf_bound
                count += 1
    print(f'st normal-layer checks: {count} points')


def main():
    normal = likelihood(0, mp.sqrt(mp.mpf(N)/2), 0)
    face = C + N*(mp.log(mp.sqrt(mp.mpf(N)/9))-mp.mpf('0.5'))
    print('sn right/left face minus normal:', mp.nstr(face-normal, 25))
    eta, h = mp.sqrt(mp.mpf(N)/2), mp.mpf(0)
    for a in map(mp.mpf, ('0', '0.25', '0.5', '1', '1.5', '2', '3', '5', '10')):
        eta, h, ll = profile(a, eta, h)
        print('alpha', a, 'eta', mp.nstr(eta, 12), 'h', mp.nstr(h, 12),
              'profile minus normal', mp.nstr(ll-normal, 20))
    ref = (mp.mpf('0.5'), mp.mpf('1.748'), mp.mpf('-0.357'))
    assert likelihood(*ref) > normal
    print('rational witness gain:', mp.nstr(likelihood(*ref)-normal, 25))
    eta_max = mp.mpf(10)  # Gaussian envelope at eta=10 is below face and decreasing.
    assert C + N*mp.log(eta_max) - eta_max**2 < face
    cutoff = eta_max*7/B
    for a in (cutoff, 2*cutoff, 4*cutoff):
        eta, h, ll = profile(a, mp.sqrt(mp.mpf(N)/9), -mp.sqrt(mp.mpf(N)/9)*mp.mpf('1.03'))
        assert ll < face
        print('tail alpha', mp.nstr(a, 10), 'profile minus face', mp.nstr(ll-face, 15))
    for center in (mp.mpf(-2), mp.mpf(0), mp.mpf(2)):
        eta, h, _ = profile(center, mp.mpf('1.5'), -center/3)
        left, right = center-mp.mpf('0.2'), center+mp.mpf('0.2')
        upper = tangent_upper(left, right, (center, eta, h))
        for j in range(11):
            a = left+(right-left)*j/10
            assert profile(a, eta, h)[2] <= upper
    print('sn tangent checks: 33 profile points')
    cover_example()
    st_normal_checks()


if __name__ == '__main__':
    main()
