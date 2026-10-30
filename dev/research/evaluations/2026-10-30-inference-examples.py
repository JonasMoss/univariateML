"""Independent checks of the inference audit's illustrative calculations.

Run: python3 dev/research/evaluations/2026-10-30-inference-examples.py
Uses mathematical densities, not the package implementation. Quadrature and
simulation illustrate the derivations; neither proves DQM or exact coverage.
"""

import json
from pathlib import Path
import platform

import numpy as np
import scipy
from scipy.integrate import quad
from scipy.stats import chi2, kstest


def integrate(fn, breaks):
    return sum(quad(fn, lo, hi, epsabs=1e-11, epsrel=1e-11)[0]
               for lo, hi in zip(breaks, breaks[1:]))


def main():
    b = 1.7
    laplace = lambda x: np.exp(-abs(x)/b)/(2*b)
    score = lambda x: np.array([np.sign(x)/b, -1/b+abs(x)/b**2])
    laplace_mean = np.array([
        integrate(lambda x: score(x)[i]*laplace(x), [-np.inf, 0, np.inf])
        for i in range(2)])
    laplace_information = np.array([[
        integrate(lambda x: score(x)[i]*score(x)[j]*laplace(x),
                  [-np.inf, 0, np.inf]) for j in range(2)] for i in range(2)])
    assert np.max(abs(laplace_mean)) < 1e-10
    assert np.max(abs(laplace_information-np.eye(2)/b**2)) < 1e-10

    a, c, upper = -1.0, 0.25, 2.0
    def triangular(x):
        if x <= c:
            return 2*(x-a)/((upper-a)*(c-a))
        return 2*(upper-x)/((upper-a)*(upper-c))
    mode_score = lambda x: -1/(c-a) if x < c else 1/(upper-c)
    mode_second = lambda x: 1/(c-a)**2 if x < c else 1/(upper-c)**2
    mean = integrate(lambda x: mode_score(x)*triangular(x), [a, c, upper])
    opg = integrate(lambda x: mode_score(x)**2*triangular(x), [a, c, upper])
    negative_hessian = -integrate(
        lambda x: mode_second(x)*triangular(x), [a, c, upper])
    expected = 1/((c-a)*(upper-c))
    assert abs(mean) < 1e-10 and abs(opg-expected) < 1e-10
    assert abs(negative_hessian+expected) < 1e-10
    # Sensitivity includes the derivative of the moving split point.
    prob_left = (c-a)/(upper-a)
    sensitivity = (2/(upper-a))*(1/(c-a)+1/(upper-c)) - (
        prob_left/(c-a)**2+(1-prob_left)/(upper-c)**2)
    assert abs(sensitivity-expected) < 1e-10

    seed, replicates, n, shape = 20261030, 20000, 12, 2.3
    rng = np.random.default_rng(seed)
    # log(X/b) are exponential: subtracting their minimum removes log(b).
    log_ratios = rng.exponential(1/shape, size=(replicates, n))
    pivot = 2*shape*np.sum(log_ratios-log_ratios.min(axis=1)[:, None], axis=1)
    df = 2*(n-1)
    bounds = chi2.ppf([0.025, 0.975], df)
    coverage = np.mean((pivot >= bounds[0]) & (pivot <= bounds[1]))
    result = {
        'purpose': 'independent illustrative calculations; no production inference tested',
        'environment': {'python': platform.python_version(),
                        'numpy': np.__version__, 'scipy': scipy.__version__},
        'laplace': {'scale': b, 'mean_score': laplace_mean.tolist(),
                    'score_information': laplace_information.tolist(),
                    'pointwise_location_second_derivative': 0},
        'triangular': {'a': a, 'b': upper, 'c': c, 'mean_score': mean,
                       'opg': opg, 'negative_expected_pointwise_hessian': negative_hessian,
                       'sensitivity_with_kink': sensitivity},
        'pareto_shape_pivot_simulation': {
            'seed': seed, 'replicates': replicates, 'sample_size': n,
            'shape': shape, 'chi_square_df': df, 'coverage': float(coverage),
            'coverage_mc_standard_error': float(np.sqrt(0.95*0.05/replicates)),
            'kolmogorov_smirnov_distance': float(kstest(pivot, chi2(df).cdf).statistic)},
    }
    output = Path(__file__).with_suffix('.json')
    output.write_text(json.dumps(result, indent=2, allow_nan=False)+'\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
