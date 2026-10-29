---
id: pig
name: Poisson-inverse Gaussian
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: mean
  role: mean
  domain: (0, inf)
  simulate: 2.0
  kind: regular
- name: shape
  role: shape
  domain: '[0, inf)'
  simulate: 3.0
  kind: regular
r:
  density: actuar::dpoisinvgauss
  sampler: actuar::rpoisinvgauss
  control:
    reltol: "Absolute tolerance on the log parameter in the scalar search."
    iterlim: "Maximum refinement iterations per bracketed scalar peak."
log_density: "log(2*shape/pi)/2 - loggamma(x+1) + shape/mean + (x-1/2)*log(shape/(2+shape/mean**2))/2 + log(besselk(x-1/2, sqrt(shape*(2+shape/mean**2))))"
limits:
  - model: pois
    when: '$\mathrm{shape} \to \infty$ at finite mean'
    detect: Poisson maximised likelihood wins, including ties within 1e-8 times sample size
    status: PoissonLimit
  - model: pois
    when: 'mean tends to zero with positive shape held'
    detect: all observations are zero, mean is free, and positive shape is held
    fixed: [shape]
    status: ZeroLimit
references:
  - "Willmot, G. E. (1987). The Poisson-inverse Gaussian distribution as an alternative to the negative binomial. Scandinavian Actuarial Journal, 1987(3-4), 113-127. DOI: 10.1080/03461238.1987.10413823."
examples: |
  mlpig(c(0, 0, 0, 0, 10))
holds: []
---

The Poisson-inverse Gaussian distribution mixes a Poisson count with an inverse Gaussian mean. It models overdispersed nonnegative counts.

## Details

Either `mean` or `shape`, or both, can be held fixed. With both free the fitted mean is exactly the sample mean. The fitter searches all bracketed peaks on a broad log-parameter grid and compares the Poisson likelihood; it does not assume a unique peak. A search reaching its range boundary without a winning Poisson face reports failure.

With both parameters free and a positive sample mean, a population-form sample variance no greater than the mean gives the Poisson supremum, including equality. Strict overdispersion guarantees a finite maximum; a gain smaller than the numerical tie tolerance still selects Poisson.

Shape zero is the attained zero point mass. All-zero data with free shape return shape zero, preserving held mean or choosing mean one when unidentified. With positive shape held and mean free, all-zero data still have an unattained mean-zero supremum. With both parameters fixed the likelihood is evaluated directly. The infinite-mean Poisson-Levy face is strictly dominated for every finite sample when mean is free, including with shape fixed.

The C++ fitter uses stable log masses. R evaluation through `dml`, `pml` and `qml` uses actuar, whose recurrence can underflow for large counts and whose starting probability loses precision near the Poisson limit. These failures can affect fitted models; `actuar::dpoisinvgauss(2, 2, 1e20, log = TRUE)` can return a positive log probability, and `actuar::dpoisinvgauss(1000, 1000, 1e12, log = TRUE)` can return minus infinity instead of about -4.3729. No replacement R density is installed.
