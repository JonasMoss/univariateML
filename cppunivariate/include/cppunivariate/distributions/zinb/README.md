---
id: zinb
name: Zero-inflated negative binomial
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: size
  role: shape
  domain: (0, inf)
  simulate: 2.0
  kind: regular
- name: prob
  role: probability
  domain: (0, 1]
  simulate: 0.4
  kind: regular
- name: pi
  role: probability
  domain: '[0, 1]'
  simulate: 0.3
  kind: regular
r:
  density: extraDistr::dzinb
  sampler: extraDistr::rzinb
  control:
    reltol: "Tolerance on the scalar log-size search interval."
    iterlim: "Maximum number of scalar search iterations."
    size: "Optional fixed finite positive size, including fractional values; NULL estimates size."
    prob: "Optional held prob; NULL estimates it."
    pi: "Optional held pi; NULL estimates it."
log_density: "Piecewise((log(pi + (1 - pi)*prob**size), Eq(x, 0)), (log(1 - pi) + loggamma(x + size) - loggamma(size) - loggamma(x + 1) + size*log(prob) + x*log(1 - prob), True))"
limits:
  - model: pois
    name: zero point mass
    when: '$size \to 0$ on the current positive-size domain'
    detect: size is free, prob and pi are held below one, and all observations are zero
    fixed: [prob, pi]
    status: ZeroLimit
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
  mlzinb(simulate_default("mlzinb", 1000))
---

The zero-inflated negative binomial distribution mixes negative binomial counts with an additional point mass at zero.

## Details

Zero inflation may be estimated as zero, an attained negative binomial member.
Holding a finite positive `size` fixed removes the infinite-size escape; no
unattained limit remains for that subset. Fractional sizes are allowed.
All-zero data have unidentified parameters: the fitter warns and returns
`size = 1` (or the supplied size), `prob = 1`, `pi = 0`.
`extraDistr::dzinb(log = TRUE)` and the upper-tail log CDF underflow to `-Inf` far in the tail (see `KNOWN_ISSUES.md`); `logLik` is unaffected.

Held probability removes the infinite-size ZIP escape. Held inflation compares against the ZIP face with the same inflation. With both probability and inflation held below one, all-zero data approach size zero, outside the current positive-size domain; the fitter stops. Endpoint holds incompatible with positive observations stop. Scalar profile peaks are searched numerically; this is not a certified global optimization.
