---
id: binom
name: Binomial
support: {type: integer, lower: "0", upper: "size", closed: [true, true]}
params:
- name: size
  symbol: 'n'
  role: trials
  domain: '[0, inf)'
  simulate: 10.0
  integer: yes
  kind: design
- name: prob
  symbol: 'p'
  role: probability
  domain: '[0, 1]'
  simulate: 0.5
  kind: regular
r:
  density: stats::dbinom
  sampler: stats::rbinom
  control:
    size: "Optional fixed number of trials; NULL estimates size."
    abstol: "Unused by the free-size search (an exact integer bracket, not a continuous one); kept for the held-probability integer search's tail bound."
    iterlim: "Iteration budget for the free-size bracket-and-bisect search (no fixed magnitude limit: raise this to search further) and for the held-probability integer search."
    prob: "Optional fixed probability in `[0, 1]`; NULL estimates it."
log_density: "loggamma(size + 1) - loggamma(x + 1) - loggamma(size - x + 1) + x*log(prob) + (size - x)*log(1 - prob)"
limits:
  - model: pois
    when: '$\mathrm{size} \to \infty$ with mean fixed'
    detect: size is free and the positive sample mean is no larger than the population-form sample variance
    status: Underdispersed
references:
  - "Johnson, N. L., Kemp, A. W., and Kotz, S. (2005). Univariate Discrete Distributions (3rd ed.). Wiley. doi:10.1002/0471715816."
  - "Olkin, I., Petkau, A. J., and Zidek, J. V. (1981). A comparison of n estimators for the binomial distribution. Journal of the American Statistical Association, 76(375), 637-642. doi:10.1080/01621459.1981.10477697."
  - "Carroll, R. J. and Lombard, F. (1985). A Note on N Estimators for the Binomial Distribution. Journal of the American Statistical Association, 80(390), 423-426."
  - "DasGupta, A. and Rubin, H. (2005). Estimation of binomial parameters when both n,p are unknown. Journal of Statistical Planning and Inference, 130(1-2), 391-404."
fit_args:
  size: ~
examples: |
  set.seed(1)
  x <- rml(200, mlbinom())
  mlbinom(x, size = 10)

summary: The binomial distribution counts successes in a fixed number of independent trials with a common success probability.
classification: classical
classification_reason: A standard model for success counts from independent trials with a common success probability.
aliases: []
key_references:
- 1
- 2

group: Poisson, binomial and negative binomial
construction:
- base
relations: {}
fit:
  method: integer-search
  certified: 'yes'
  note: 'binom/README.md, Details; binom/mle.hpp (public fitter and held overloads); binom/test.cpp: Free-size profile
    is unimodal (DeRiggi result cited in Details); held-probability search excludes the entire integer tail. Fixed-size
    probability is analytic; work-budget exhaustion stops.'
areas:
- counts and ecology
---

The binomial distribution models the number of successes in a fixed number of independent Bernoulli trials. Both size and success probability are estimated unless size is supplied.

## Details

Free trial-size estimation brackets and bisects the integer profile likelihood, which DeRiggi (1983) proves unimodal whenever a finite MLE exists (checked first: the escape to `mlpois` below). There is no fixed magnitude limit on the search; it fails only by exhausting `iterlim`, which a larger value always resolves for any sample with a finite MLE.

The estimate of `size` can be unstable; alternatives are discussed by Carroll and Lombard (1985) and DasGupta and Rubin (2005). Supplying `size` fixes the number of trials and estimates only `prob`; it must be at least `max(x)`. The support is the integers from zero to `size`, and `size` is a nonnegative integer.

A held probability is preserved exactly. With probability strictly between zero and one, the fixed-probability integer search bounds the remaining tail before returning; exhausting its work budget is an error. Boundary probabilities must give positive likelihood to the observations.

### Diagnostic status contract (2026-10-31)

Invalid and empty samples carry non-success statuses, including delegate paths.
See the [shared diagnostic contract](../../../../../dev/validation/diagnostics.md)
and the appended TASK-132 cases in `test.cpp`.
