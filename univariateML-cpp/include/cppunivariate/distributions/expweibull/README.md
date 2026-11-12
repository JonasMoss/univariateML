---
id: expweibull
name: Exponentiated Weibull
support: {type: real, lower: 0, upper: inf, closed: [true, false]}
params:
- {name: scale, symbol: a, role: scale, domain: '(0, inf)', simulate: 2, kind: regular}
- {name: shape, symbol: b, role: shape, domain: '(0, inf)', simulate: 1.5, kind: regular}
- {name: exponent, symbol: c, role: shape, domain: '(0, inf)', simulate: 2.5, kind: regular}
r:
  density: dexpweibull
  sampler: rexpweibull
  control:
    scale: Optional held positive scale; NULL estimates it.
    shape: Optional held positive Weibull shape; NULL estimates it.
    exponent: Optional held positive CDF exponent; NULL estimates it.
    tol: Requested relative coefficient accuracy (default 1e-10).
    iterlim: 'Per solve, min(iterlim, 100000) iterations; each line search uses the same work allowance.'
log_density: log(shape) + log(exponent) - log(scale) + (shape - 1)*log(x/scale) - (x/scale)**shape + (exponent - 1)*log(1 - exp(-(x/scale)**shape))
inference_numeric:
  - regime: finite positive interior
    available: false
    condition: 'scale, shape, exponent finite and positive; any coordinate subset free'
    reason: 'Regular with full-rank information; no validated numerical inference adapter.'
    classification: 'dev/research/evaluations/2026-11-10-ew-theory-report.md#interior-regularity-and-inference'
holds: []
limits:
  - kind: nonexistence
    name: unbounded concentration
    firmness: proved
    when: 'Concentration at a common positive observation'
    detect: 'Constant data with scale free and either shape or exponent free, or shape free at the held scale, or both shapes free above the held scale'
    status: Unbounded
    message: unbounded
  - model: power
    firmness: proved
    fixed: [scale]
    when: 'shape tends to infinity and shape * exponent tends to a positive constant'
    detect: 'Both shapes free and all observations strictly below the held scale'
    status: PowerBoundary
    message: power
  - name: power envelope with endpoint ties
    firmness: proved
    fixed: [scale]
    when: 'The power path has a factor 1/(e-1) for each observation at the held scale'
    detect: 'Both shapes free, all observations at or below held scale, with observations both below and equal to it'
    status: PowerBoundaryTied
    message: endpoint
summary: A Weibull CDF raised to a positive power permits a second shape coordinate.
classification: specialist
classification_reason: Flexible lifetime model with both inverse-Weibull and power closure faces.
group: gamma and Weibull
construction: [generalisation]
areas: [reliability and survival, waiting times and queues]
relations: {}
fit:
  method: profile-search
  certified: partial
  note: 'Held-exponent fits and single-free fits have uniqueness certificates; other locally checked fits are uncertified. Held-scale power envelopes and constant-data concentration have proved stops.'
existence:
  uniqueness: unknown
  statement: 'For nonconstant samples, holding exponent gives a unique finite fit and holding shape gives a finite maximum; with scale held and both shapes free a finite maximum exists precisely when an observation exceeds scale. Constant-data concentration depends on the free coordinates. Free-fit boundary candidates are power and inverse Weibull; their global comparison remains open.'
  reason: 'Strict concavity in affine log-data coordinates with exponent held; joint boundary exhaustion and the held-endpoint envelope in the linked analyses.'
examples: |
  set.seed(1)
  x <- rml(200, mlexpweibull())
  mlexpweibull(x)
---

For $x>0$, the CDF is $[1-\exp\{-(x/a)^b\}]^c$.
`scale` and `shape` retain the Weibull coefficient names; `exponent`
distinguishes the CDF power from the Weibull shape. The scalar C++ order is
`scale`, `shape`, `exponent`. Setting `exponent = 1` gives Weibull.


### Exact-ratio power evaluation

When an FMA verifies that x/scale is exactly representable, the probability kernel evaluates its power directly. This removes an intermediate rounded logarithm and product whose error in the Weibull hazard is amplified by a large CDF exponent.

## Details

Writing $y=\log x$ and $t=\exp\{b(y-\log a)\}$ profiles the CDF
exponent exactly as $c=-n/\sum\log(1-e^{-t})$. A damped Newton search
uses this two-coordinate profile; observed-information refinement checks all
free log coefficients before accepting a finite candidate. With exponent held,
strict concavity in $(b,-b\log a)$ makes the stationary fit unique. Single-free
scale and shape scores are monotone; the exponent has a closed form.
A held exponent of one uses the dedicated Weibull solver, with a check that
its likelihood is restored at the returned public coefficients. For a tiny
held shape, a weighted-variance bound encloses log scale around the sample
mean log observation, avoiding division of a subnormal reduction by shape.

The continuous likelihood-retaining faces are power and inverse Weibull.
With scale held and both shapes free, data entirely below the endpoint have a
strictly winning power envelope. Endpoint ties subtract $\log(e-1)$ each
from that envelope; the tied stop therefore has no automatic `mlpower`
likelihood delegate. Constant data can instead have unbounded concentration,
with the free coordinates determining whether that path is available.

Global comparison for the free model and uniqueness for the two-free
held-scale and held-shape fits remain open. Successful local fits in those
regimes have `certified = FALSE`. A reachable face strictly above a local
candidate refutes it and causes a numerical stop; reaching a search boundary
does not establish a winning face.

The [first analysis](../../../../../dev/research/evaluations/2026-11-10-ew-theory-report.md)
and [boundary exhaustion](../../../../../dev/research/evaluations/2026-11-11-ew-theory2-report.md)
provide the proofs, including regular full-rank interior information. The
[evaluation driver](../../../../../dev/research/evaluations/2026-11-12-ew-fit-data/run-study.py),
[held-subset references](../../../../../dev/research/evaluations/2026-11-12-ew-fit-data/reference-check.json),
and [stress summary](../../../../../dev/research/evaluations/2026-11-12-ew-fit-data/archive-check.json)
record the implemented outcomes and independent numerical comparisons. Numerical inference is not yet validated for this family.
