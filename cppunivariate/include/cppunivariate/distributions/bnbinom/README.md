---
id: bnbinom
name: Beta negative binomial
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: size
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: alpha
  role: shape
  domain: (0, inf)
  simulate: 2
  kind: regular
- name: beta
  role: shape
  domain: (0, inf)
  simulate: 5
  kind: regular
r:
  density: dbnbinom
  sampler: rbnbinom
  control:
    gradtol: "Tolerance on the average log likelihood score in log parameters."
    iterlim: "Maximum BFGS iterations per deterministic start."
log_density: "loggamma(size+x)-loggamma(size)-loggamma(x+1)+loggamma(alpha+size)+loggamma(beta+x)-loggamma(alpha+size+beta+x)-loggamma(alpha)-loggamma(beta)+loggamma(alpha+beta)"
limits:
  - model: nbinom
    when: '$\alpha,\beta\to\infty$ with their ratio fixed, or the mirror path exchanging size and beta'
    detect: alpha and at least one interchangeable shape are free, and the reachable negative binomial face wins within 1e-8 times sample size
    status: NegativeBinomialLimit
  - model: pois
    when: '$r,\beta,\alpha\to\infty$ with $r\beta/\alpha$ fixed'
    detect: all parameters free and the Poisson face wins within 1e-8 times sample size
    status: PoissonLimit
  - model: pois
    when: '$r\to0$, $\beta\to0$, or $\alpha\to\infty$'
    detect: all observations zero and at least one parameter free
    status: PointMass
references:
  - "Irwin, J. O. (1975). The Generalized Waring Distribution. Part I. JRSS A 138(1), 18-31. doi:10.2307/2345247."
  - "Xekalaki, E. (1984). The Bivariate Generalized Waring Distribution and its Application to Accident Theory. JRSS A 147(3), 488-498. doi:10.2307/2981580."
examples: |
  set.seed(4)
  mlbnbinom(univariateML:::rbnbinom(n = 1000, size = 1, alpha = 2, beta = 5))
holds: []
---

The beta negative binomial mixes a negative binomial success probability with a beta distribution. Also called the generalized Waring distribution, it models counts with power law tails.

## Details

Any subset of `size`, `alpha` and `beta` may be held fixed. With `size` and `beta` both free, the returned representative satisfies `size <= beta`: swapping these two coefficients gives exactly the same distribution. No ordering is imposed when either is held.

A deterministic multistart search compares finite stationary points with every reachable negative binomial and Poisson face. Faces win likelihood ties within `1e-8 * length(x)`. This search does not certify global optimality or assume unimodality. An unresolved score or range endpoint raises an error. With all coefficients supplied, the likelihood is evaluated directly.
