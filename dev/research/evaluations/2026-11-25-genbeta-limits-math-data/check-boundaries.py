"""Replay selected GB1 boundary strata using independent beta probabilities."""

import mpmath as mp


def log_density(y, p, q, a):
    return (
        mp.log(a) - (mp.loggamma(p) + mp.loggamma(q) - mp.loggamma(p + q)) - p * a * y
        + (q - 1) * mp.log(-mp.expm1(-a * y))
    )


def survivor(y, p, q, a):
    return mp.betainc(p, q, 0, mp.exp(-a * y), regularized=True)


def mean_log_density(sample, p, q, a):
    return mp.fsum(log_density(y, p, q, a) for y in sample) / len(sample)


def check_convergence(label, values, target, tolerance):
    errors = [abs(value - target) for value in values]
    assert errors[-1] < mp.mpf(tolerance), (label, errors)
    assert errors[-1] < errors[0], (label, errors)
    print(f"{label}: final absolute error {mp.nstr(errors[-1], 8)}")


def main():
    mp.mp.dps = 150
    sample = [mp.mpf(1) / 8, mp.mpf(1) / 4, mp.mpf(1) / 2]
    minimum = min(sample)
    mean = mp.fsum(sample) / len(sample)
    gap = mean - minimum
    rate = 1 / gap
    power = mp.log(rate) - 1

    for q in map(mp.mpf, ("0.25", "1", "3")):
        gamma_rate = q / mean
        gamma = (
            q * mp.log(gamma_rate) - mp.loggamma(q) - q
            + (q - 1) * mp.fsum(map(mp.log, sample)) / len(sample)
        )
        values = [
            mean_log_density(sample, gamma_rate / a, q, a)
            for a in map(mp.mpf, ("1e-3", "1e-6", "1e-9"))
        ]
        if q != 1:
            check_convergence(f"small power, q={q}", values, gamma, "1e-8")
        if q == 1:
            assert max(abs(value - gamma) for value in values) < mp.mpf("1e-140")
            print("q=1 ridge: agreement within 1e-140")

    powers = list(map(mp.mpf, (100, 400, 1200)))
    values = [
        mean_log_density(sample, rate / a, mp.exp(a * minimum) / a, a)
        for a in powers
    ]
    check_convergence("moving endpoint, vanishing penalty", values, power, ".04")
    penalty = mp.mpf("0.75")
    values = [
        mean_log_density(sample, rate / a, penalty * len(sample) * mp.exp(a * minimum), a)
        for a in powers
    ]
    check_convergence("same weak limit, positive likelihood penalty", values, power - penalty, ".02")

    for ratio in map(mp.mpf, ("0.25", "1", "3")):
        weight = 1 / (1 + ratio)
        for y in map(mp.mpf, ("0.125", "0.5", "1.25")):
            p = [1 / a for a in powers]
            values = [survivor(y, shape, shape / ratio, a) for shape, a in zip(p, powers)]
            check_convergence(f"mixed tail, p/q={ratio}, y={y}", values, weight * mp.exp(-y), "6e-6")
        values = [mean_log_density(sample, 1 / a, 1 / (ratio * a), a) for a in powers]
        check_convergence("mixed interior likelihood", values, mp.log(weight) - mean, "6e-6")

    grid = list(map(mp.mpf, (10, 40, 100)))
    for y in map(mp.mpf, ("0.25", "1", "2")):
        values = [survivor(y, 1 / t, 1 / t, mp.exp(-t)) for t in grid]
        check_convergence(f"two atoms from both shapes small, y={y}", values,
                          1 - mp.exp(-1) / 2, ".004")
        values = [survivor(y, mp.mpf(2), 1 / t, mp.exp(-t)) for t in grid]
        check_convergence(f"two atoms from q alone small, y={y}", values,
                          1 - mp.exp(-1), ".008")

    values = []
    for t in map(mp.mpf, (10, 100, 100000)):
        offset = 1 / t**3
        sample_y = (offset + mp.log(2), offset)
        log_likelihood = mp.fsum(log_density(y, 1 / t, 1 / t, t) for y in sample_y) + mp.log(2)
        values.append(log_likelihood - 2 * mp.log(t))
    check_convergence("free endpoint counterexample, sample (1/2, 1)", values, -mp.log(4), ".001")

    y = mp.mpf("0.5")
    p = mp.mpf("2.25")
    values = [log_density(y, p, p * mp.exp(a * y), a) - mp.log(a) for a in grid]
    check_convergence("held p, constant sample", values, p * mp.log(p) - p - mp.loggamma(p), "1e-18")

    # For p=q=t, the observed beta argument is its center, u=1/2.
    y = mp.log(2)
    values = [log_density(y, t, t, 1) - mp.log(t) / 2 for t in grid]
    check_convergence("held a, constant sample", values, -mp.log(mp.pi) / 2, ".002")
    for q in map(mp.mpf, ("0.25", "0.75", "1.25", "4")):
        for a in map(mp.mpf, ("0.01", "1", "10")):
            for p in map(mp.mpf, ("0.01", "0.5", "5", "100")):
                y = mp.mpf("0.5")
                if q < 1:
                    upper = -1 - mp.log(y)
                else:
                    upper = q * mp.log(q) - q - mp.loggamma(q) - mp.log(y)
                assert log_density(y, p, q, a) < upper, (p, q, a)
    print("held q, constant sample: 48 strict envelope comparisons")
    print(f"PASS at {mp.mp.dps} decimal digits")


if __name__ == "__main__":
    main()
