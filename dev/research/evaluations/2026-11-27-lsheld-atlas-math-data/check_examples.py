"""Check selected held-mask identities and witnesses independently of the fitters."""

import mpmath as mp
import sympy as sp

mp.mp.dps = 80
m = mp.mpf


def emg(rate, location, scale, x):
    residual = x - location
    return (
        rate * mp.exp((rate * scale) ** 2 / 2 - rate * residual)
        * mp.erfc((rate * scale - residual / scale) / mp.sqrt(2)) / 2
    )


def half_t(shape, scale, residual):
    return (
        2 * mp.gamma((shape + 1) / 2)
        / (scale * mp.sqrt(shape * mp.pi) * mp.gamma(shape / 2))
        * (1 + residual**2 / (shape * scale**2)) ** (-(shape + 1) / 2)
    )


def huber_normalizer(shape):
    return mp.sqrt(2 * mp.pi) * mp.erf(shape / mp.sqrt(2)) + (
        2 * mp.exp(-shape**2 / 2) / shape
    )


def huber_tail_score(shape, distance):
    return shape + 2 * mp.exp(-shape**2 / 2) / (
        shape**2 * huber_normalizer(shape)
    ) - distance


def gpd_objective(b, c, ratios):
    return -(1 / c + b) * sum(mp.log1p(b * v) / b for v in ratios)


def feller_log_density(y, power, left_shape, right_shape, mode):
    log_odds = power * (y - mode) + mp.log(left_shape / right_shape)
    return (
        mp.log(power) - mp.log(mp.beta(left_shape, right_shape))
        + left_shape * log_odds
        - (left_shape + right_shape) * mp.log1p(mp.exp(log_odds))
    )


def fatigue_rational(gaps):
    delta = sp.Symbol("delta", positive=True)
    shifted = [delta + gap for gap in gaps]
    mean_loss = sum(t + 1 / t - 2 for t in shifted) / len(shifted)
    profile = sp.cancel(
        sp.prod((t + 1) ** 2 / t**3 for t in shifted) / mean_loss**len(shifted)
    )
    return delta, profile, sp.limit(profile, delta, 0, dir="+")


def check_singleton_identities():
    # At an EMG stationary point with common positive residual.
    z = sp.Symbol("z", positive=True)
    k = z / (z**2 - 1)
    mills = 1 / (z * (z**2 - 1))
    weight = sp.simplify(mills * (z - k + mills))
    hessian = sp.Matrix([
        [-1 / z**2 - weight, -1 + weight],
        [-1 + weight, -1 / k**2 + 1 - weight],
    ])
    assert weight == 1 / z**2
    assert sp.simplify(hessian.det() - (z**2 - 1) * (z**2 - 3) / z**4) == 0
    argument = z - k
    root_slope = -weight * sp.diff(argument, z) - sp.diff(mills, z)
    assert sp.simplify(root_slope + (z**2 - 3) / (z**2 * (z**2 - 1))) == 0

    # Held-scale half-t score with zero and positive residuals.
    for shape, squared, fraction in ((m(2), m(16), m(".5")), (m(".5"), m(3), m(1))):
        def kernel(t):
            return (
                2 / (1 + mp.exp(-t)) - (1 - fraction)
                + fraction * ((squared - 1) * mp.exp(-squared * t)
                              + mp.expm1(-squared * t) / t)
            )

        transform = mp.quad(lambda t: mp.exp(-shape * t) * kernel(t), [0, 1, mp.inf])
        score = 2 * mp.diff(lambda nu: (
            (1 - fraction) * mp.log(half_t(nu, m(1), m(0)))
            + fraction * mp.log(half_t(nu, m(1), mp.sqrt(squared)))
        ), shape)
        assert abs(transform - score) < m("1e-75")

    # Fatigue damped threshold score and exact held-scale sample.
    u, precision = sp.symbols("u precision", positive=True)
    score = 1 / (u + 1) - 3 / (2 * u) - precision * (1 - 1 / u**2)
    damped = u / (u + 1)**2 + 3 * (1 - u) / (2 * u**2) - precision * (
        1 - 1 / u**2 + 2 / u**3
    )
    assert sp.simplify(sp.diff(score, u) + score - damped) == 0
    delta, profile, _ = fatigue_rational((0, 1, 1))
    numerator = sp.fraction(sp.factor(sp.diff(profile, delta)))[0]
    polynomial = sp.Poly(numerator / (-54 * (delta + 2)**3), delta)
    assert polynomial.all_coeffs() == [9, 36, 19, -11, -4]
    assert polynomial.count_roots(0, sp.oo) == 1
    print("Scoped singleton proofs: EMG and fatigue identities, half-t score integral")


def main():
    scale = m("1e-8")
    rate = m(2)
    for contact in (m(-3), m(0), m(2)):
        prob = mp.erfc(-contact / mp.sqrt(2)) / 2
        value = emg(rate, -contact * scale, scale, m(0))
        assert abs(value - rate * prob) < m("1e-6")
    assert emg(m(1), 0, m(1), m(9)) * emg(m(1), 0, m(1), m(-10)) > (
        mp.exp(-m(181) / 2) / (2 * mp.pi)
    )
    print("EMG: three exponential contacts and a strict finite witness")

    for residuals in ((m(0), m(0)), (m(0), m(4)), (m(1), m(2))):
        shape = m("1e6")
        normal = mp.fprod(
            mp.sqrt(2 / mp.pi) * mp.exp(-r**2 / 2) for r in residuals
        )
        ratio = mp.fprod(half_t(shape, m(1), r) for r in residuals) / normal
        coefficient = sum((r**4 - 2 * r**2 - 1) / 4 for r in residuals)
        assert abs(shape * mp.log(ratio) - coefficient) < m(".001")
    print("Half-t: normal inward coefficient at three held-scale samples")

    root = mp.findroot(lambda c: huber_tail_score(c, 10), (m(".08"), m(".2")))
    assert 0 < root < m(".5")
    assert abs(huber_tail_score(root, 10)) < m("1e-70")
    assert mp.exp(-1 + m(".005")) / huber_normalizer(m(".1")) > mp.exp(-5)
    assert mp.exp(-m("4.875")) / mp.sqrt(2 * mp.pi) < mp.exp(-5)
    print("Huber: all-tail held-scale root", mp.nstr(root, 22))

    for c, ratios in (
        (m(".3"), (m(0), m(".4"), m(1), m(1))),
        (m(".8"), (m(0), m(".01"), m(".7"), m(1))),
    ):
        for b in (m("-.9"), m("-.2"), m(2), m(50)):
            derivative = sum(
                mp.quad(lambda s: (s / c - 1) / (1 + b * s) ** 2, [0, v])
                for v in ratios if v
            )
            assert abs(derivative - mp.diff(
                lambda t: gpd_objective(t, c, ratios), b
            )) < m("1e-75")
    print("GPD: eight held-scale score integral checks")

    delta, profile, face = fatigue_rational((0, 1, 1))
    assert face == 432 and profile.subs(delta, sp.Rational(1, 2)) == 2880
    delta, profile, face = fatigue_rational((0, sp.Rational(1, 10), 1))
    assert face == 130680
    numerator = sp.Poly(sp.fraction(sp.cancel(profile - face))[0], delta)
    numerator = sp.div(numerator, sp.Poly(delta, delta))[0]
    assert numerator.eval(0) != 0 and numerator.count_roots(0, sp.oo) == 0
    print("Fatigue: both exact critical-exponent examples and zero positive crossings")

    power = m(10**6)
    left_rate, right_rate = m(2), m(3)
    for y in (m(-1), m(0), m(1)):
        asymmetric_laplace = (
            mp.log(left_rate * right_rate / (left_rate + right_rate))
            - left_rate * max(-y, 0) - right_rate * max(y, 0)
        )
        finite = feller_log_density(
            y, power, left_rate / power, right_rate / power, m(0)
        )
        assert abs(finite - asymmetric_laplace) < m("1e-5")
    for shape in (m(".3"), m(1), m(3)):
        # Move the log-mode outward to recover the full closed face density.
        mode = 1 / mp.sqrt(power)
        for y in (m(-1), m(0)):
            finite = feller_log_density(y, power, left_rate / power, shape, mode)
            assert abs(finite - (mp.log(left_rate) + left_rate * y)) < m(".003")
        assert feller_log_density(m(1), power, left_rate / power, shape, mode) < -100
    print("Feller–Pareto: large-power two-rate and moved-endpoint traces")


if __name__ == "__main__":
    main()
    check_singleton_identities()
