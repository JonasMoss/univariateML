---
id: slash
name: Slash
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
- name: mu
  symbol: '\mu'
  role: location
  domain: (-inf, inf)
  simulate: 0
  kind: regular
- name: sigma
  symbol: '\sigma'
  role: scale
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: dslash
  sampler: rslash
  control:
    mu: Optional held mu; NULL estimates it.
    sigma: Optional positive held sigma; NULL estimates it.
    reltol: Relative search tolerance in standardized coordinates.
    iterlim: Maximum refinement iterations per search start.
log_density: -log(sigma)-log(2*pi)/2+log(1-exp(-((x-mu)/sigma)**2/2))-2*log(Abs((x-mu)/sigma))
limits:
  - name: point mass collapse
    when: '$\sigma \to 0$ at a half-mass observation'
    detect: finite collapse likelihood within 1e-8*n of the best profile value
    status: CollapseLimit
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlslash())
  mlslash(x)

group: skewed and robust location-scale
construction:
- location-scale
relations: {}
fit:
  method: multistart
  certified: partial
  note: 'slash/README.md, Details; slash/mle.hpp (public fitter and held overloads); slash/test.cpp: Held location
    has a unique scale root; free-location profiles are demonstrably multimodal and observation-gap scans do not
    certify mode isolation.'
areas:
- robust modelling
classification: specialist
classification_reason: Used to model robust errors obtained by randomly magnifying Gaussian deviations.
summary: Slash provides a model for robust errors obtained by randomly magnifying Gaussian deviations.
aliases: []
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
---

The slash distribution is a normal variate divided by an independent uniform variate, with location and scale.

## Details

A value with more than half the sample mass makes the free-scale likelihood unbounded. With every multiplicity strictly below half, the maximum is attained. At exactly half, the finite collapse likelihood must be compared with the interior profile; ties within $10^{-8}n$ select collapse. Holding sigma removes collapse. The scale score has a unique root for every location away from the observations. The location profile can be multimodal; a grid over every observation gap nominates all numerical local maxima for refinement. This search is numerical, not a proof that every mode is isolated.

For an exact existence criterion in the half-mass case, put $c=1/\sqrt{2\pi}$ and $B_v=c^n2^{-n/2}\prod_{x_i\ne v}|x_i-v|^{-2}$ at each half-mass value $v$. For a location $m$ away from such values, let $s(m)$ be the unique positive zero of $\sum_i[1-2t_i/(e^{t_i}-1)]$, $t_i=(x_i-m)^2/(2s^2)$. An MLE exists if and only if some interior profile value $L(m,s(m))$ is at least $\max_v B_v$; if the maximum multiplicity is below half, the comparison is vacuous. This is an exact variational criterion; the implementation evaluates it numerically, with the stated tie preference. It is not a multiplicity-only rule: `c(0, 1)` has an interior maximum, whereas `c(0, 0, -1, 1)` numerically selects collapse. Location lies in the sample range; the likelihood tends to zero at all other escaping paths. The density decreases strictly with absolute standardized distance. Together these facts prove attainment below half. At a half-mass point, $L(v,s)<B_v$ for every positive scale. If $\sum_{x_i\ne v}1/(x_i-v)\ne0$, choosing $m-v$ proportional to $s^2$ increases the likelihood above $B_v$ for sufficiently small $s$, proving existence (subject to comparison with any other half-mass point).

Location and scale are identified by symmetry and any fixed positive quantile width. Density and CDF formulas follow the [extraDistr slash documentation](https://search.r-project.org/CRAN/refmans/extraDistr/html/Slash.html); the existence argument above is direct, not attributed to the Cauchy theorem.

Multimodality is already forced with sigma held: for `c(-100, 100)` and sigma 1, the likelihood near each observation exceeds its value at the midpoint. Symmetry then gives two separated location maxima. The global location search is used for held-scale fits as well as the free-scale profile.

Zero reciprocal sum is not sufficient for collapse: `c(rep(0, 10), -1, rep(9, 9))` has $L(4,3)/B>1.456$. Free-scale multimodality is explicit for `c(rep(0, 10), -1, rep(10, 9))`: $L(-1/625,1/5)/B>1.00012$ and $L(9/2,7/2)/B>1.066$. At location zero every finite-scale likelihood is below B; all other escaping paths on either side have likelihood at most B. Each side therefore contains an attained finite maximum. These strict bounds follow from degree-12 Taylor lower bounds on $1-e^{-t}$ and Bernoulli's inequality; they were certified using exact rational arithmetic.


With mu held, only its tie multiplicity k matters: 2k<n gives a unique positive scale maximum; 2k>n is unbounded, and equality selects an unattained collapse supremum. Holding sigma removes collapse even for constant samples.
