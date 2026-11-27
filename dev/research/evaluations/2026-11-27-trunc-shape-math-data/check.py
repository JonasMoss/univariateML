"""Check conditional likelihood identities and finite-grid face comparisons."""
import csv
import sys

import mpmath as mp


def likelihood(kernel, lower, upper, sample):
    return mp.fprod(map(kernel, sample)) / mp.quad(kernel, [lower, upper]) ** len(sample)


def frechet_contact(d, cut_at_minimum, q):
    shape, m, upper = 1, 1, 3
    location = m - d

    def cdf(x):
        return mp.exp(-q * (d / (x - location)) ** shape)

    def density(x):
        return shape * q * d**shape * (x - location) ** (-shape - 1) * cdf(x)

    den = cdf(upper) - (cdf(m) if cut_at_minimum else 0)
    return density(1) * density(2) / den**2


def checks():
    writer = csv.writer(sys.stdout, lineterminator="\n")
    writer.writerow(["check", "dps", "grid", "computed", "reference", "absolute_gap"])

    for dps in (80, 120):
        mp.mp.dps = dps

        def record(name, grid, value, ref, tol=None):
            gap = abs(value - ref)
            if tol is not None:
                assert gap < tol, (name, grid, value, ref)
            writer.writerow([name, dps, grid, *[mp.nstr(x, 24) for x in (value, ref, gap)]])

        for exponent in (4, 8, 16):
            d = mp.mpf(10) ** -exponent
            grid = f"1e-{exponent}"
            for cut in (False, True):
                ref = 4 * mp.exp(-2) / ((1 - mp.exp(-2)) ** 2 if cut else 1)
                label = "frechet-contact-at-cut" if cut else "frechet-contact-below-cut"
                record(label, grid, frechet_contact(d, cut, mp.mpf(2)), ref, 10 * d)
            attained_limit = frechet_contact(d, True, d)
            assert attained_limit < 1
            record("frechet-half-cut-sharp-envelope", grid, attained_limit, 1, 10 * d)
            pareto = (1 + d) ** -2 / (1 - d / (2 + d)) ** 2
            record("pareto-critical-on-[1,3]", grid, pareto, 1, 10 * d)
            ly = tuple(map(mp.mpf, (1, 2)))
            first = lambda y: mp.exp(-d * y) * (-mp.expm1(-d * y) / d) ** 2
            record("kumar-a-zero-on-y-[1,3]", grid, likelihood(first, 1, 3, ly),
                   likelihood(lambda y: y**2, 1, 3, ly), 10 * d)
            second = lambda y: mp.exp(-y) * (-mp.expm1(-y)) ** (d - 1)
            record("kumar-b-zero-on-y-[1,3]", grid, likelihood(second, 1, 3, ly),
                   likelihood(lambda y: 1 / mp.expm1(y), 1, 3, ly), 10 * d)
            weibull = lambda y: mp.exp(y - d * mp.exp(y))
            record("weibull-positive-power-on-y-[0,1]", grid, likelihood(weibull, 0, 1, (0, 1)),
                   likelihood(mp.exp, 0, 1, (0, 1)), 10 * d)
            gompertz = lambda x: mp.exp(d * x - 2 * mp.expm1(d * x) / d)
            record("gompertz-held-a-on-[0,1]", grid, likelihood(gompertz, 0, 1, (0, 1)),
                   likelihood(lambda x: mp.exp(-2 * x), 0, 1, (0, 1)), 10 * d)

            def gumbel_scaled(x):
                return mp.exp(-mp.log1p(d * x) / d)

            f = lambda x: gumbel_scaled(x) * mp.exp(-gumbel_scaled(x)) / (1 + d * x)
            g = lambda x: mp.exp(-x - mp.exp(-x))
            record("frechet-gumbel-on-[0,2]", grid, likelihood(f, 0, 2, (0, 1)),
                   likelihood(g, 0, 2, (0, 1)), 10 * d)

        beta = 2 / mp.log(2)
        for upper in (2, 3, 10):
            effective = min(upper, 2)
            fitted = likelihood(lambda x: x ** (beta - 1), 0, effective, (1, 2))
            record("power-endpoint-plateau", str(upper), fitted, beta**2 * mp.exp(-2) / 2,
                   mp.mpf(10) ** (5 - dps))

        for threshold in (mp.mpf("0.75"), mp.mpf("0.875"), mp.mpf(1)):
            scale = threshold - mp.mpf("0.5")
            kernel = lambda x: (x - threshold + scale) ** -3
            record("pareto-threshold-plateau", str(threshold), likelihood(kernel, 1, mp.inf, (1, 2)),
                   mp.mpf(16) / 27, mp.mpf(10) ** (5 - dps))
        trial = lambda y: mp.exp(4 * y - mp.exp(4 * y))
        pair = (mp.mpf("-0.01"), mp.mpf("0.01"))
        value = likelihood(trial, -1, 1, pair)
        assert value > mp.mpf("0.3")
        record("weibull-finite-witness-vs-best-power", "y=+-0.01;s=4,c=1;I=[-1,1]", value, mp.mpf(1) / 4)


if __name__ == "__main__":
    checks()
