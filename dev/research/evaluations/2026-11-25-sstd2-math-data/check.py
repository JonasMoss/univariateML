"""Replay the contact and collar comparisons in the TASK-347 report."""
import json

import mpmath as mp

mp.mp.dps = 80
M = mp.mpf


def moments(family, q):
    if family == "student":
        peak = 2 * mp.gamma((q + 1) / 2) / (mp.sqrt(mp.pi) * mp.gamma(q / 2))
        return peak / (q - 1), 1 / (q - 2), mp.log(peak)
    t = 1 / q
    return mp.gamma(2*t)/mp.gamma(t), mp.gamma(3*t)/mp.gamma(t), -mp.loggamma(1+t)


def geometry(family, q, w, mu, sd):
    k, v, peak = moments(family, q)
    z = 1 - 2*w
    width = 2*sd/mp.sqrt(v+(3*v-4*k*k)*z*z)
    return mu-k*z*width, width, peak


def penalty(family, q, r):
    if family == "student":
        return (q+1)/2*mp.log1p(r*r)
    return r**q


def likelihood(family, q, w, mu, sd, sample, relaxed=False):
    c, width, peak = geometry(family, q, w, mu, sd)
    loss = 0
    for x in sample:
        if x < c and not relaxed:
            loss += penalty(family, q, (c-x)/(w*width))
        elif x > c:
            loss += penalty(family, q, (x-c)/((1-w)*width))
    return len(sample)*(peak-mp.log(width))-loss


def contact_replay():
    rows = []
    for family, q in (("student", M(3)), ("ged", M(2)), ("ged", M(1))):
        k, v, _ = moments(family, q)
        mu, sd = k, mp.sqrt(v-k*k)
        c0 = lambda shape: geometry(family, shape, 0, mu, sd)[0]
        slope = mp.diff(c0, q)
        join_drift = 2*k*v/(4*(v-k*k))
        # width is one at this normalization
        samples = ([0, 1], [0, 3], [0, 0, 1, 3])
        if family == "ged" and q == 1:
            samples += ([0]+[M("2.5")]*5,)
        for sample in samples:
            F = lambda shape: likelihood(family, shape, 0, mu, sd, sample, True)
            Fq = mp.diff(F, q, direction=1)
            A = mp.diff(lambda w: likelihood(family, q, w, mu, sd, sample, True),
                        M(0), direction=1)
            C = A-join_drift/slope*Fq
            # At GED p=1 the matched path has exactly zero contact loss;
            # the relaxed derivative in w uses the unsupported side.
            errors = []
            for exponent in (8, 12, 20):
                w = M(10)**(-exponent)
                root = mp.findroot(lambda shape: geometry(family, shape, w, mu, sd)[0],
                                   (q, q-join_drift/slope*w), tol=M("1e-76"))
                got = likelihood(family, root, w, mu, sd, sample, True)
                errors.append(abs((got-F(q))/w-C))
                fixed = likelihood(family, q, w, mu, sd, sample)
                assert abs(fixed-F(q)+sample.count(0)*penalty(family,q,join_drift)) < 1000*w
            # At p=1 Fq includes the contact broad-side derivative,
            # which is canceled on the exactly aligned path.
            if family == "ged" and q == 1:
                C += sample.count(0)*join_drift
                # errors above are differences from the uncorrected expression
                assert abs(errors[-1]-sample.count(0)*join_drift) < M("1e-16")
            else:
                assert errors[-1] < M("1e-16")
            rows.append(dict(family=family, shape=str(q), sample=list(map(str, sample)),
                             endpoint_slope=mp.nstr(slope, 14),
                             aligned_derivative=mp.nstr(C, 14),
                             fixed_loss=mp.nstr(sample.count(0)*penalty(family,q,join_drift), 14)))
    return rows


def normal_replay():
    g = 1/mp.sqrt(2*mp.pi)
    mu, sd = 2*g, mp.sqrt(1-4*g*g)
    rows = []
    for ratio in (M(0), M(1), M(10), M(100)):
        for exponent in (6, 10, 20):
            w = M(10)**(-exponent)
            h = ratio*w
            if h == 0:
                z = 1-2*w
                k = 2*g
                width = 2*sd/mp.sqrt(1+(3-4*k*k)*z*z)
                c = mu-k*z*width
                value = (c/(w*width))**2/2
            else:
                q = 1/h
                c, width, _ = geometry("student", q, w, mu, sd)
                value = penalty("student", q, c/(w*width))
            # At h=0, T=1, J= k/(1-4k²).  c_h from the prior moment derivative.
            J = g/(1-4*g*g)
            H = mu*( (2-6*g*g)/(2*(1-4*g*g)) - M(3)/4 )
            target = (J+ratio*H)**2/2
            if exponent == 20:
                assert abs(value-target) < M("1e-8")
            rows.append([str(ratio), exponent, mp.nstr(value, 15), mp.nstr(target, 15)])
    return rows


def width_replay():
    rows = []
    C = M(15)/4*mp.zeta(2)
    for exponent in (2, 8, 16, 32):
        eps = M(10)**(-exponent)
        t = eps/(8-4*mp.log(eps))
        q = 1/t
        for side in (-1, 1):
            sample = [-1, 0, 1]
            sd = mp.exp(-side*eps)/mp.sqrt(3)
            value = likelihood("ged", q, M(".5"), M(0), sd, sample)
            base = -len(sample)*mp.log(2*mp.sqrt(3)*sd)
            gain = value-base
            if side < 0:
                assert gain > len(sample)*t*t/2
            else:
                upper = len(sample)*C*t*t-mp.exp(side*eps/t-mp.euler)
                assert gain < upper < 0
            rows.append(dict(width_log_order=side, exponent=exponent,
                             shape=mp.nstr(q, 14), gain=mp.nstr(gain, 14)))
    return rows


def signs_replay():
    rows = []
    for sample in ([1, 2], [4, 5]):
        coefficient = M(3)*len(sample)/2-M(sum(sample))/2
        face = likelihood("ged", M(1), M(0), M(1), M(1), sample)
        for exponent in (5, 10, 16):
            w = M(10)**(-exponent)
            quot = (likelihood("ged", M(1), w, M(1), M(1), sample)-face)/w**2
            assert abs(quot-coefficient) < 100*w
        rows.append(dict(sample=sample, second_order=mp.nstr(coefficient, 10)))
    return rows


if __name__ == "__main__":
    print(json.dumps({"digits": mp.mp.dps, "contacts": contact_replay(),
                      "normal_contact": normal_replay(), "near_width": width_replay(),
                      "laplace_half_signs": signs_replay()}, indent=2))
