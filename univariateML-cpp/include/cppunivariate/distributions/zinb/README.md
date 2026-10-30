---
id: zinb
name: Zero-inflated negative binomial
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: size
  symbol: 'r'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
- name: prob
  symbol: 'p'
  role: probability
  domain: (0, 1]
  simulate: 0.5
  kind: regular
- name: pi
  symbol: '\pi'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: dzinb
  sampler: rzinb
  control:
    reltol: "Tolerance on the scalar log-size search interval."
    iterlim: "Maximum number of scalar search iterations."
    size: "Optional fixed finite nonnegative size, including fractional values; NULL estimates size."
    prob: "Optional held prob; NULL estimates it."
    pi: "Optional held pi; NULL estimates it."
log_density: "Piecewise((log(pi + (1 - pi)*prob**size), Eq(x, 0)), (log(1 - pi) + loggamma(x + size) - loggamma(size) - loggamma(x + 1) + size*log(prob) + x*log(1 - prob), True))"
limits:
  - model: zip
    when: '$\mathrm{size}\to\infty$ with NB mean fixed and positive inflation'
    detect: size and prob are free and the maximized ZIP likelihood wins within 1e-8 times sample size
    status: ZipLimit
  - model: pois
    when: '$\mathrm{size}\to\infty$ with NB mean fixed and inflation tending to zero'
    detect: size and prob are free and the winning ZIP face has zero inflation
    status: PoissonLimit
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley-Blackwell."
examples: |
  set.seed(1)
  x <- rml(200, mlzinb())
  mlzinb(x)
---

The zero-inflated negative binomial distribution mixes negative binomial counts with an additional point mass at zero.

## Details

Zero inflation may be estimated as zero, an attained negative binomial member.
Holding a finite nonnegative `size` fixed removes the infinite-size escape; no
unattained limit remains for that subset. Fractional sizes are allowed.
All-zero data with free size return `size = 0`, preserving holds and using `prob = 0.5`, `pi = 0` for unidentified free coordinates. With positive size held, a free probability is one; if probability is held below one, free inflation is one. Boundary fits are silent.
Held probability removes the infinite-size ZIP escape. Held inflation compares against the ZIP face with the same inflation. With both probability and inflation held below one, all-zero data attain size zero. Endpoint holds incompatible with positive observations stop. Scalar profile peaks are searched numerically; this is not a certified global optimization.
