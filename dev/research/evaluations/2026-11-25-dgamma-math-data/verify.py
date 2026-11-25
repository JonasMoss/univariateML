"""High-precision cell probabilities, profile derivatives and saddle replays."""
import mpmath as mp


def log_cell(shape, rate, cell):
    return mp.log(mp.gammainc(shape, rate * cell, rate * (cell + 1))) - mp.loggamma(shape)


def rate_score(shape, rate, sample):
    def boundary(x, log_p):
        return mp.exp(shape * mp.log(x) - x - mp.loggamma(shape) - log_p) if x else 0

    return sum(
        weight * (boundary(rate * (cell + 1), lp) - boundary(rate * cell, lp))
        for cell, weight in sample
        for lp in [log_cell(shape, rate, cell)]
    )


def profile(shape, sample):
    mean = sum(cell * weight for cell, weight in sample)
    lower, upper = shape / (mean + 1), shape / mean
    assert rate_score(shape, lower, sample) > 0 > rate_score(shape, upper, sample)
    for _ in range(120):
        mid = (lower + upper) / 2
        if rate_score(shape, mid, sample) > 0:
            lower = mid
        else:
            upper = mid
    rate = (lower + upper) / 2
    value = sum(weight * log_cell(shape, rate, cell) for cell, weight in sample)
    slope = sum(weight * mp.diff(lambda a: log_cell(a, rate, cell), shape)
                for cell, weight in sample)
    return value, slope


def replay(shape, mean):
    rate = shape / mean
    columns = []
    covariances = []
    for cell in range(3):
        def lp(a, r):
            return log_cell(a, r, cell)

        columns.append([1, mp.diff(lp, (shape, rate), (1, 0)),
                        mp.diff(lp, (shape, rate), (0, 1))])
        covariances.append([mp.diff(lp, (shape, rate), order)
                            for order in [(2, 0), (1, 1), (0, 2)]])
    weights = mp.lu_solve(mp.matrix(columns).T, mp.matrix([1, 0, 0]))
    aa, ar, rr = [sum(weights[i] * covariances[i][j] for i in range(3))
                  for j in range(3)]
    assert all(w > 0 for w in weights) and rr < 0
    return shape**2 * (aa - ar**2 / rr)


def main():
    samples = [
        ('spread positive', [(10, 1), (100, 1)], False),
        ('spread with zero', [(0, 1), (2, 1)], False),
        ('rare separated cell', [(0, 999998), (1, 1), (2, 1)], False),
        ('mostly adjacent', [(0, 200), (1, 200), (2, 1)], False),
        ('adjacent', [(0, 3), (1, 7)], True),
        ('constant positive', [(5, 1)], True),
    ]
    for precision in (60, 90):
        with mp.workdps(precision):
            for name, counts, increasing in samples:
                total = sum(weight for _, weight in counts)
                sample = [(cell, mp.mpf(weight) / total) for cell, weight in counts]
                start = -36 if name == "rare separated cell" else -16
                stop = 8 if name == "adjacent" else 13
                shapes = [mp.power(10, mp.mpf(i) / 4) for i in range(start, stop)]
                values, slopes = zip(*(profile(a, sample) for a in shapes))
                signs = ''.join('+' if s > 0 else '-' for s in slopes)
                print(precision, name, signs, flush=True)
                assert all(s != 0 for s in slopes)
                if increasing:
                    assert all(a < b for a, b in zip(values, values[1:]))
                    assert signs == '+' * len(shapes)
                else:
                    assert signs[0] == '+' and signs[-1] == '-' and '-+' not in signs
            half = mp.mpf('0.5')
            gap = [(1, half), (3, half)]
            values = [profile(mp.mpf(a), gap)[0] for a in (100, 200, 300)]
            deficit = (values[0] + values[2]) / 2 - values[1]
            assert deficit > 0
            print(precision, 'concavity deficit', mp.nstr(deficit, 20), flush=True)
            check_replay('0.372759372031494', '0.025')

    # One direction loses about 435 digits near this nearly constant fit.
    with mp.workdps(550):
        check_replay('6105.402296585326', '1.1834318555465324')


def check_replay(shape, mean):
    curvature = replay(mp.mpf(shape), mp.mpf(mean))
    assert curvature < 0
    print(mp.mp.dps, shape, mean, mp.nstr(curvature, 20), flush=True)



if __name__ == '__main__':
    main()
