"""Numerical checks of the zero-cell bounds and sample-only criterion."""
import itertools
import mpmath as mp


def deviance(x):
    return x - 1 - mp.log(x)


def cutoff(sample):
    best = mp.mpf(0)
    for (low, wl), (high, wh) in itertools.combinations(sample, 2):
        if high <= low + 1:
            continue
        mean = (wl * (low + 1) + wh * high) / (wl + wh)
        bound = wl * deviance((low + 1) / mean) + wh * deviance(high / mean)
        best = max(best, bound)
    return 1 / best


def criterion(counts):
    total = sum(counts.values())
    sample = [(k, mp.mpf(n) / total) for k, n in sorted(counts.items())]
    mean = sum(k * w for k, w in sample)
    upper = cutoff(sample)
    zero = sum(w for k, w in sample if k == 0)
    spread = sum(w * max(abs(1 / mp.mpf(k) - 1 / (mean + 1)),
                         abs(1 / mp.mpf(k + 1) - 1 / mean))**2
                 for k, w in sample if k)
    lower = (1 + 1 / (upper + 1)**2) / 2
    trace = (zero + upper**2 * spread / 4) / lower + upper / (4 * mean**2)
    return upper, trace


def check_zero_moments():
    checked = 0
    for a, mean in itertools.product(
            map(mp.mpf, ('.03', '.2', '1', '5', '20')),
            map(mp.mpf, ('.1', '.49', '.5', '1', '10', '200'))):
        rate = a / mean
        # Integral with kernel y**(a-1)*exp(-rate*y) on (0, 1).
        def mass(s):
            return mp.gammainc(s, 0, rate) / rate**s

        z = mass(a)
        var_t = mp.diff(lambda s: mp.log(mass(s)), a, 2)
        var_y = mass(a + 2) / z - (mass(a + 1) / z)**2
        cov_log_y = mp.diff(lambda s: mass(s + 1) / mass(s), a)
        variance = var_t + var_y / mean**2 - 2 * cov_log_y / mean
        assert 0 < variance < 1 / a**2
        lower = (1 + 1 / (a + 1)**2) / 2
        assert a**2 * (mp.polygamma(1, a) - 1 / a) > lower
        checked += 1
    print(f'{checked} zero-cell moment checks')


def main():
    with mp.workdps(70):
        check_zero_moments()
        for counts, affirmative in [
            ({0: 1, 2: 1}, False),
            ({0: 1, 1000: 1}, True),
            ({0: 1, 10: 1, 100: 1}, True),
            ({0: 1, 100: 9}, True),
            ({0: 998, 10: 1, 100: 1}, False),
            ({10: 1, 100: 1}, True),
        ]:
            upper, trace = criterion(counts)
            assert bool(trace < 1) == affirmative, counts
            print(counts, 'cutoff', mp.nstr(upper, 12), 'trace', mp.nstr(trace, 12))


if __name__ == '__main__':
    main()
