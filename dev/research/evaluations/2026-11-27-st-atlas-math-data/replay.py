"""High-precision replays of the boundary layers and scale crossing."""

from itertools import product

import mpmath as mp

mp.mp.dps = 65


def peak(df):
    return mp.gamma((df + 1) / 2) / (mp.sqrt(df * mp.pi) * mp.gamma(df / 2))


def student(z, df):
    return peak(df) * (1 + z**2 / df) ** (-(df + 1) / 2)


def cdf(z, df):
    tail = mp.betainc(df / 2, mp.mpf("0.5"), 0, df / (df + z**2), regularized=True) / 2
    return tail if z < 0 else 1 - tail


def density(z, slant, df):
    return 2 * student(z, df) * cdf(slant * z * mp.sqrt((df + 1) / (df + z**2)), df + 1)


def likelihood(sample, location, scale, slant, df):
    return mp.fprod(density((x - location) / scale, slant, df) / scale for x in sample)


def critical(sample, shift, slant):
    ties = sum(x == 0 for x in sample)
    off_ties = (x for x in sample if x != 0)
    return density(-shift, slant, mp.mpf(1)) ** ties * mp.fprod(
        2 / (mp.pi * x**2) * cdf(mp.sign(x) * slant * mp.sqrt(2), mp.mpf(2))
        for x in off_ties
    )


def check_layers():
    samples = [(0, 0, 1, 3), (0, 0, -1, 3)]
    scale = mp.mpf("1e-10")
    checks = 0
    for sample, shift, slant in product(samples, (-1, 0, 1), (-4, 0, 4)):
        observed = likelihood(sample, scale * shift, scale, slant, mp.mpf(1))
        expected = critical(sample, shift, slant)
        assert abs(observed / expected - 1) < mp.mpf("1e-8")
        checks += 1
    for sample, slant in product(samples, (-12, -2, 0, 2, 12)):
        values = [likelihood(sample, 0, mp.mpf(10) ** j, slant, mp.mpf(1))
                  for j in (-8, -4, -2, 0, 1)]
        assert all(a > b for a, b in zip([critical(sample, 0, slant)] + values, values))
        checks += len(values)
    for distance, orientation in product((0, 1), (-1, 1)):
        # An approaching location restores the closed-endpoint factor.
        sample = (distance, distance, distance + orientation)
        slant = orientation * mp.mpf("1e12")
        held = likelihood(sample, distance, 1, slant, mp.mpf(3))
        moving = likelihood(sample, distance - orientation * mp.mpf("1e-6"),
                            1, slant, mp.mpf(3))
        assert abs(moving / held - 4) < mp.mpf("1e-5")
        checks += 1
    for rate in (0, 1, 3):
        sample = (0, 0, -1, 3)
        scale = mp.exp(-10000)
        df = 1 + mp.mpf(rate) / 10000
        observed = likelihood(sample, 0, scale, 2, df)
        expected = critical(sample, 0, 2) * mp.exp(-2 * rate)
        assert abs(observed / expected - 1) < mp.mpf("0.003")
        checks += 1
    print(f"boundary layers: {checks} checks at {mp.mp.dps} decimal digits")


def log_scale_profile(log_scale, log_distance):
    p = 100
    slant = mp.mpf(2)**20
    far = mp.mpf(2)**50000
    w = mp.exp(log_scale)
    distance = mp.exp(log_distance)

    def log_skew(point):
        return mp.log1p(slant / mp.sqrt(1 + slant**2 + (w / point)**2))

    return (2 * log_scale - p * mp.log1p(1 / w**2)
            - p * mp.log1p((w / distance)**2) + p * log_skew(1)
            + p * (log_skew(distance) - mp.log(2))
            - mp.log1p((w / far)**2) + log_skew(far) - mp.log(2))


def check_crossing():
    def score(t, d):
        return mp.diff(lambda s: log_scale_profile(s, d), t)

    def equations(early, late, distance):
        return (score(early, distance), score(late, distance),
                log_scale_profile(early, distance) - log_scale_profile(late, distance))

    early, late, distance = mp.findroot(equations, (12, 45, 48))
    assert mp.log(2) * 30 < distance < mp.log(2) * 120
    separator = 23 * mp.log(2)
    assert early < separator < late
    for loc in (early, late):
        assert mp.diff(lambda s: log_scale_profile(s, distance), loc, 2) < 0
        assert log_scale_profile(loc, distance) > log_scale_profile(separator, distance) + 10
    log_max = log_scale_profile(early, distance)
    grid = [mp.log(2) * j / 4 for j in range(4 * 122 + 1)]
    assert max(log_scale_profile(s, distance) for s in grid) < log_max
    print("scale crossing grid:", len(grid), "points at", mp.mp.dps, "decimal digits")
    print("numerical log2(D), log2(scales):",
          *(mp.nstr(v / mp.log(2), 18) for v in (distance, early, late)))


if __name__ == "__main__":
    check_layers()
    check_crossing()
