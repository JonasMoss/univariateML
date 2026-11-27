"""Independent high-precision probes for TASK-439."""
import csv
import sys

import mpmath as mp


def probe():
    results = {}
    # Non-steep boundary after retaining the even integers.
    results["nonsteep_face_mean"] = mp.zeta(2) * 2 / mp.zeta(3)
    assert results["nonsteep_face_mean"] < 4

    def bracket(function, lo, hi):
        lo, hi = mp.mpf(lo), mp.mpf(hi)
        assert function(lo) < 0 < function(hi)
        for _ in range(mp.mp.dps * 3):
            mid = (lo + hi) / 2
            if function(mid) < 0:
                lo = mid
            else:
                hi = mid
        return (lo + hi) / 2

    sample_mean = mp.mpf(2)
    q = bracket(lambda z: z / ((1-z)*(-mp.log1p(-z))) - sample_mean,
                "0.1", "0.95")
    rate = bracket(lambda z: z / (-mp.expm1(-z)) - sample_mean, "0.1", 4)
    for r in (0, 1, 10, 100):
        if r:
            z = bracket(
                lambda z: r*z / ((1-z)*(-mp.expm1(r*mp.log1p(-z)))) - 2,
                mp.mpf("0.01")/(r+1), mp.mpf("2")/(r+2))
            denominator = -mp.expm1(r*mp.log1p(-z))
            mass = lambda x: mp.rf(r, x) * z**x * (1-z)**r / (
                mp.factorial(x)*denominator)
        else:
            mass = lambda x: q**x / (x*(-mp.log1p(-q)))
        for top in (3, 10, 50):
            results[f"nb_r{r}_sample_top{top}"] = (
                (top-2)*mp.log(mass(1)) + mp.log(mass(top)))/(top-1)
    for top in (3, 10, 50):
        mass = lambda x: rate**x / (mp.factorial(x)*mp.expm1(rate))
        results[f"nb_poisson_sample_top{top}"] = (
            (top-2)*mp.log(mass(1)) + mp.log(mass(top)))/(top-1)

    # Gamma corner (0,0), interval [1,4], sample (1,4).
    w = mp.log(4)
    results["gamma_corner_shape_score"] = w/2 - w/2
    results["gamma_corner_rate_score"] = 3/w - mp.mpf("2.5")
    assert results["gamma_corner_rate_score"] < 0

    # Fixed n=3 binomial inflation: 19 zeros, 11 ones and 8 twos.
    p = mp.mpf(1)/3
    empirical_zero = mp.mpf(1)/2
    results["binom_free_pi"] = (empirical_zero-(1-p)**3)/(1-(1-p)**3)
    assert 0 < results["binom_free_pi"] < 1

    # Symmetric interior normal optimum extremely close to the uniform face.
    radius = mp.mpf(57735)/100000
    integrals = lambda t, power: mp.quad(
        lambda x: x**power * mp.exp(t*x*x), [0, 1])
    root = bracket(lambda t: integrals(t, 2)/integrals(t, 0)-radius**2,
                   "-0.01", "0")
    results["normal_quadratic_tilt"] = root
    results["normal_per_observation_gap"] = root*radius**2-mp.log(integrals(root, 0))
    assert 0 < results["normal_per_observation_gap"] < mp.mpf("1e-8")

    # NB at held p=1/2, all ones: exact size-zero logseries value.
    results["nb_logseries_all_ones_loglik_each"] = -mp.log(2*mp.log(2))
    return results


def main():
    with mp.workdps(80):
        first = probe()
    with mp.workdps(120):
        second = probe()
        writer = csv.writer(sys.stdout, lineterminator="\n")
        writer.writerow(["probe", "value"])
        for key, value in second.items():
            assert abs(first[key]-value) < mp.mpf("1e-68")
            writer.writerow([key, mp.nstr(value, 30)])


if __name__ == "__main__":
    main()
