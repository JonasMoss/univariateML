"""High-precision checks and exact-arithmetic bounds for the TASK-264 examples."""
import json
import mpmath as mp

mp.mp.dps = 80
D = mp.mpf
EPS = D('1e-65')


def ged_terms(t, z):
    g1, g2, g3 = (mp.loggamma(1 + j*t) for j in (1, 2, 3))
    extra = mp.log1p(-3*z*z*mp.expm1(2*g2-g1-g3))/2
    return (g3-g1)/2 + extra, (g3-3*g1)/2 + extra


def entropy(t, z):
    w = (1+z)/2
    if t == 1:
        return -mp.log(w)
    alpha = 1/(1-t)
    return -mp.log(w**alpha+(1-w)**alpha)/(alpha-1)


def balanced_cover(fixed_z=None):
    """Upper bounds for log likelihood relative to the unpenalized height."""
    l, h = (D(0), D(1)) if fixed_z is None else (fixed_z, fixed_z)
    pending = [(D('.01'), D(1), l, h)]
    count = 0
    largest_upper = -mp.inf
    limit = -mp.exp(-mp.euler)
    if fixed_z is not None:
        assert fixed_z == 0  # At symmetry the entropy is constant in t.
        limit *= 2
    while pending:
        a, b, l, h = pending.pop()
        count += 1
        assert count < 200000
        # C_sym(t) and Delta(t) increase; symmetry lets z range over [0,1].
        height = ged_terms(b, h)[1]
        # (log W_sym(t)+log sqrt(3))/t increases by concavity of B.
        extra = ged_terms(a, l)[0] - ged_terms(a, 0)[0]
        penalty = mp.exp(ged_terms(a, 0)[0]/a + extra/b + entropy(b, h))
        upper = 2*height - penalty + EPS
        if upper < limit:
            largest_upper = max(largest_upper, upper)
            continue
        if b-a >= h-l:
            c = (a+b)/2
            pending.extend([(a, c, l, h), (c, b, l, h)])
        else:
            c = (l+h)/2
            pending.extend([(a, b, l, c), (a, b, c, h)])
    # The relative analytic bound is monotone in the required directions.
    cutoff = D('.01')
    assert 2*(D(15)/4)*cutoff < 2*mp.exp(-mp.euler)/(1+3*cutoff)**2
    return {'boxes': count, 'largest_leaf_upper': mp.nstr(largest_upper, 26),
            'limit': mp.nstr(limit, 26), 'minimum_comparison_gap':
            mp.nstr(limit-largest_upper, 16)}


def student_held_mean(nu, xi, ordinary_scale, counts):
    x = [D(0)]*counts[0] + [D(1)]*counts[1]
    moment = 2*mp.sqrt(nu)/((nu-1)*mp.beta(nu/2, D('.5')))
    shift = (xi-1/xi)*moment
    kappa = [xi if v/ordinary_scale+shift >= 0 else 1/xi for v in x]
    log_height = mp.log(2/((xi+1/xi)*ordinary_scale*mp.sqrt(nu)
                           *mp.beta(nu/2, D('.5'))))
    return len(x)*log_height - (nu+1)/2*sum(
        mp.log1p((v/ordinary_scale+shift)**2/(nu*u*u))
        for v, u in zip(x, kappa))


def student_collapse(nu, xi, counts):
    k, m = counts
    moment = 2*mp.sqrt(nu)/((nu-1)*mp.beta(nu/2, D('.5')))
    shift = (xi-1/xi)*moment
    side = xi if shift >= 0 else 1/xi
    height = 2/((xi+1/xi)*mp.sqrt(nu)*mp.beta(nu/2, D('.5')))
    return ((k+m)*mp.log(height) + m*(nu+1)*mp.log(mp.sqrt(nu)*xi)
            - k*(nu+1)/2*mp.log1p(shift**2/(nu*side*side)))


def student_examples():
    result = {}
    for name, nu, xi, counts in [('finite', D(3), D('.1'), (3, 1)),
                                 ('collapse', D(3), D(10), (3, 1))]:
        collapse = student_collapse(nu, xi, counts)
        result[name] = [mp.nstr(student_held_mean(nu, xi, D(10)**-j, counts)
                               - collapse, 24) for j in [2, 4, 8]]
        assert all((D(v) > 0) == (name == 'finite') for v in result[name])
    xi = D('.1')
    nu = D(2) + D('1e-12')
    moment = 2*mp.sqrt(nu)/((nu-1)*mp.beta(nu/2, D('.5')))
    v = nu/(nu-2)*(1+(xi-1/xi)**2) - ((xi-1/xi)*moment)**2
    delta = student_held_mean(nu, xi, D('.01')/mp.sqrt(v), (2, 1))
    delta -= student_collapse(D(2), xi, (2, 1))
    assert delta > 0
    result['held_sd_finite_minus_collapse'] = mp.nstr(delta, 24)
    return result


def contact_grid():
    result = []
    for a,b,z in [(1,1,D(0)), (1,1,D('.9')), (1,1000,D(0)), (3,17,D('-.7'))]:
        w = (1-z)/2
        avg = w*mp.log(w/a)+(1-w)*mp.log((1-w)/b)
        variance = w*(1-w)*mp.log(w*b/((1-w)*a))**2
        coefficient = (2+D('1.5')*z*z)*mp.zeta(2)-variance/2
        for p in (D(1000),D(100000),D(10000000)):
            t = 1/p
            log_s = mp.log(w*(w/a)**(1/(p-1)) + (1-w)*((1-w)/b)**(1/(p-1)))
            lr = ged_terms(t,z)[0]/t - (p-1)*log_s
            scaled = (lr+mp.euler+avg)*p
            assert abs(scaled-coefficient) < D('.05')*1000/p
            result.append([a,b,str(z),int(p),mp.nstr(coefficient,18),mp.nstr(scaled,18)])
    return result


def both_moment_corner_grid():
    cases = 0
    min_log_ratio_gap = mp.inf
    for p in map(D, [1, 2, 10, 1000, 1000000]):
        t = 1/p
        lo = mp.exp(-mp.euler*t + 3*mp.zeta(2)*t*t/(2*(1+2*t)**2))
        b = mp.exp(mp.loggamma(1+2*t)-mp.loggamma(1+t))
        assert b >= lo
        for tail in map(D, ['0.1','1e-3','1e-6','1e-12','1e-24']):
            for sign in (-1, 1):
                z = sign*(1-tail)
                a = mp.exp(ged_terms(t, z)[0])
                for side in (-1, 1):
                    ratio = (a+side*z*b)/(1+side*z)
                    assert ratio >= b
                    min_log_ratio_gap = min(min_log_ratio_gap, mp.log(ratio/lo))
                    cases += 1
    return {'cases': cases, 'smallest_log_ratio_gap': mp.nstr(min_log_ratio_gap, 12)}


def finite_contact_example():
    height = mp.log(3/(2*mp.pi))/2
    ll = 102*height-3
    assert ll > -102*mp.log(2)
    return mp.nstr(ll + 102*mp.log(2), 24)


def location_grid():
    worst = D(0)
    cases = 0
    for p in map(D, ['1.01','1.1','2','5','20']):
        for xi in map(D, ['.01','.7','1','2','100']):
            for a,b in [(1,1),(1,11),(11,1)]:
                w = 1/(1+xi*xi)
                ratio = (mp.log(D(a)/b)+2*p*mp.log(xi))/(p-1)
                left = 1/(1+mp.exp(ratio))
                right = 1/(1+mp.exp(-ratio))
                residual = abs(mp.log(a)+(p-1)*mp.log(left)-p*mp.log(w)
                               -mp.log(b)-(p-1)*mp.log(right)+p*mp.log(1-w))
                worst = max(worst,residual)
                cases += 1
    assert worst < D('1e-74')
    return {'cases': cases, 'largest_absolute_log_score_balance': mp.nstr(worst, 7)}


if __name__ == '__main__':
    print(json.dumps({'digits':mp.mp.dps, 'balanced_contact':balanced_cover(),
                      'balanced_held_symmetric':balanced_cover(D(0)),
                      'finite_contact_above_unpenalized_uniform':finite_contact_example(),
                      'student':student_examples(), 'contact_grid':contact_grid(),
                      'both_moment_corner_grid':both_moment_corner_grid(),
                      'location_grid':location_grid()}, indent=2))
