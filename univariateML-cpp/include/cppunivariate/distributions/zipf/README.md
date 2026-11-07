---
id: zipf
inference_numeric:
  - regime: "size one"
    available: false
    condition: "known N = 1; finite s > 0 free; the law is a point mass, so information is zero"
    reason: "The free information is structurally singular; neither observed nor sandwich covariance can be inverted"
    classification: "univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels"
  - regime: "primary interior"
    available: true
    condition: "known integer N >= 2 held and finite s > 0 free"
    classification: "dev/roadmap/2026-10-30-inference-and-dqm.md#discrete-coordinates"
name: Zipf
support: {type: integer, lower: "1", upper: "N", closed: [true, true]}
params:
- name: 'N'
  symbol: 'N'
  role: upper
  domain: '[1, inf)'
  simulate: 10
  integer: yes
  bound: upper
  kind: endpoint
- name: s
  symbol: 's'
  role: shape
  domain: '[0, inf)'
  simulate: 1
  kind: regular
r:
  density: dzipf
  sampler: rzipf
  control:
    "N": "Optional integer upper endpoint; NULL estimates it as max(x)."
    s: "Optional finite nonnegative shape; NULL estimates it."
    reltol: "Relative tolerance on the Newton step, with an absolute floor of the same size."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
log_density: "-s*log(x) - log(Sum(k**(-s), (k, 1, N)))"
references:
- Johnson, N. L., Kemp, A. W. and Kotz, S. (2005). Univariate Discrete Distributions, third edition. Wiley. doi:10.1002/0471715816.
limits:
- name: point mass at one
  firmness: proved
  when: 'The shape satisfies $s\to\infty$ with $N>1$ held.'
  detect: 'The shape is free, every observation equals one, and the held endpoint satisfies $N>1$.'
  status: PointMass
  fixed: ['N']
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mlzipf())
  mlzipf(x)

group: discrete heavy-tailed
construction:
- base
relations:
  truncation_of:
  - family: zeta
    at:
      s: s
    when: s > 1
fit:
  method: scalar-solve
  certified: 'yes'
  note: 'zipf/README.md, Details; zipf/mle.hpp (public fitter and held overloads); zipf/test.cpp: Strictly monotone
    one-parameter exponential-family or log-shape score gives a bracketed unique root or the documented boundary;
    finite Zipf endpoint and modified zero mass profile exactly.'
areas:
- rank-frequency
classification: specialist
classification_reason: Used to model rank frequencies with a finite power-law cutoff.
summary: Zipf provides a model for rank frequencies with a finite power-law cutoff.
aliases:
- "finite Zipfian"
key_references:
- 1
existence:
  uniqueness: proved
  statement: >-
    With both coefficients free, a finite MLE exists for every nonempty
    positive integer sample. If any observation exceeds one, the endpoint
    $\hat N=\max_i x_i$ and the shape estimate are unique. All-ones data
    give $\hat N=1$ with every finite $s\ge0$ maximizing the likelihood.
    With $N>1$ held, the shape MLE is finite and unique unless all
    observations are one, when only an unattained point-mass supremum
    exists. With $N=1$ held the shape is unidentified. With shape held,
    the unique endpoint MLE is the sample maximum. Holding both
    coefficients leaves only likelihood evaluation on the retained support.
  reason: >-
    At every finite shape, the normalizing sum strictly increases with
    $N$, so the smallest data-compatible endpoint maximizes the likelihood.
    For $N>1$, the shape score per observation is
    $E_{N,s}[\log X]-\overline{\log x}$, with derivative
    $-\operatorname{Var}_{N,s}(\log X)<0$. Its model mean tends to zero
    at infinity. The maximum is $s=0$ if
    $\overline{\log x}\ge\log(N!)/N$, and otherwise is the unique
    positive root, unless the empirical log mean is zero.
---

The finite Zipf distribution models power-law frequencies on the integers from one to a finite endpoint $N$. The shape $s$ controls the decrease in frequency. Shape zero is the attained uniform distribution.

## Details

At every finite shape, increasing the endpoint adds unobserved probability mass and lowers the likelihood. A free endpoint therefore has the unique estimate $\hat N=\max_i x_i$. With $N>1$, the shape log likelihood is strictly concave. The maximum is the uniform boundary when the empirical mean of $\log X$ is at least $\log(N!)/N$. Otherwise the strictly decreasing shape score has one positive root, provided an observation exceeds one.

All-ones data with a free endpoint select $N=1$. Every finite shape then gives the same point mass, so the shape is unidentified. With $N>1$ held, these data instead have an unattained point-mass supremum as $s\to\infty$. Holding the shape still selects the sample maximum as the endpoint. Holding both coefficients evaluates the likelihood on their support.

For fixed $s>1$, increasing $N$ without bound gives the zeta distribution. The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-discrete2.md) discusses the held-parameter cases.

## R notes

With all observations equal to one and a free shape on singleton support, the fitter returns `s = 1` as a representative and warns that the MLE is not unique. With `N > 1` held, a free-shape fit to all-ones data stops. Supply `N` or `s` to hold that coefficient. The function fits observations directly. For rank-abundance data, see `sads::fitzipf`. The infinite-support model is fitted by `mlzeta`.

The endpoint is capped at `.Machine$integer.max`. Both probability tails are summed directly. Large supports use bounded-memory Euler--Maclaurin harmonic sums.

## Stop details

The held-N point-mass escape retains its degenerate face. The existing ShapeNegative non-existence stop has kind nomle and no candidate estimates.
