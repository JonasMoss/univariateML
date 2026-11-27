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
    detect_firmness: proved
    proof: ["dev/research/evaluations/2026-11-25-expweibull-math-report.md#held-masks-and-sample-predicates"]
    detect_proof: ["dev/research/evaluations/2026-11-25-expweibull-math-report.md#held-masks-and-sample-predicates"]
  - model: power
    firmness: proved
    fixed: [scale]
    when: 'shape tends to infinity and shape * exponent tends to a positive constant'
    detect: 'Both shapes free and all observations strictly below the held scale'
    status: PowerBoundary
    message: power
    detect_firmness: proved
    proof: ["dev/research/evaluations/2026-11-25-expweibull-math-report.md#held-masks-and-sample-predicates"]
    detect_proof: ["dev/research/evaluations/2026-11-25-expweibull-math-report.md#held-masks-and-sample-predicates"]
  - name: power envelope with endpoint ties
    firmness: proved
    fixed: [scale]
    when: 'The power path has a factor 1/(e-1) for each observation at the held scale'
    detect: 'Both shapes free, all observations at or below held scale, with observations both below and equal to it'
    status: PowerBoundaryTied
    message: endpoint
    detect_firmness: proved
    proof: ["dev/research/evaluations/2026-11-25-expweibull-math-report.md#held-masks-and-sample-predicates"]
    detect_proof: ["dev/research/evaluations/2026-11-25-expweibull-math-report.md#held-masks-and-sample-predicates"]
  - model: invweibull
    firmness: proved
    when: 'Exponent tends to infinity, shape times log exponent tends to a positive constant $k$, and scale times $(\log c)^{1/b}$ tends to $s$'
    detect: 'All coefficients free, finite search falls below the faces within the comparison rule, and the inverse-Weibull face is at least as high as power'
    status: InvweibullBoundary
    message: invweibull
    detect_firmness: numerical
    proof: ["dev/research/evaluations/2026-11-25-expweibull-math-report.md#faces-and-finite-attainment"]
  - model: power
    firmness: proved
    when: 'Shape tends to infinity, shape times exponent tends to $n/\sum_i\log(x_{\max}/x_i)$, and scale approaches the maximum from above with $b\log(a/x_{\max})$ diverging'
    detect: 'All coefficients free, finite search falls below the faces within the comparison rule, and the power face is higher'
    status: PowerBoundary
    message: power
    detect_firmness: numerical
    proof: ["dev/research/evaluations/2026-11-25-expweibull-math-report.md#faces-and-finite-attainment"]
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
  statement: 'For nonconstant samples, holding exponent gives a unique finite fit and holding shape gives a finite maximum; with scale held and both shapes free a finite maximum exists precisely when an observation exceeds scale. Constant-data concentration depends on the free coordinates. A finite free maximum exists exactly when a finite parameter reaches the larger of the power and inverse-Weibull face values.'
  reason: 'Strict concavity in affine log-data coordinates with exponent held; joint boundary exhaustion and the held-endpoint envelope in the linked analyses.'
  equivariance:
    group: power-scale
    map: {scale: "A*scale**B", shape: "shape/B"}
    evidence: >-
      The density depends on $x$ only through
      $(x/\mathrm{scale})^{\mathrm{shape}}$ and the factor $1/x$.
      dev/validation/equivariance.py checks the density identity at 24 seeded
      points on the dossier density (40 digits, largest relative difference
      below $10^{-39}$) and dpqr.hpp (binary64); SymPy did not simplify it
      within its guard.
  two_point:
    outcome: limit
    evidence: 'Equivariance reduces samples of size two to a single standardized pair. The high-precision profile search places its supremum on the power face.'
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
from that envelope. The power-envelope limit then carries the endpoint penalty. With shape held, nearly constant data can require an
exponent above the largest representable double. Constant data can have unbounded concentration,
with the free coordinates determining whether that path is available.

With all coefficients free, the power endpoint approaches the largest
observation from above, so its limiting density includes every observation
at that endpoint without the held-scale penalty. Near the inverse-Weibull
face, write $T=\log c$, $k=bT$, and $u_i=\log(x_i/s)$ for Weibull shape $b$,
CDF exponent $c$, and inverse-Weibull scale $s$. The leading likelihood
correction to the face is $A/T$, where
$A=k\sum_i u_i+\tfrac12 k^2\sum_i u_i^2(e^{-ku_i}-1)$.
A positive correction at the face optimum motivates an interior search seed.

Face selection compares the normalized likelihood with locally checked
finite candidates. Without a global interior bound, free-fit limit choices
are flagged. The [limit analysis](../../../../../dev/research/evaluations/2026-11-15-expweibull-limits-report.md)
derives the correction and gives high-precision comparisons for the boundary samples.

The [first analysis](../../../../../dev/research/evaluations/2026-11-10-ew-theory-report.md)
and [boundary exhaustion](../../../../../dev/research/evaluations/2026-11-11-ew-theory2-report.md)
provide the proofs, including regular full-rank interior information. The
[evaluation driver](../../../../../dev/research/evaluations/2026-11-12-ew-fit-data/run-study.py),
[held-subset references](../../../../../dev/research/evaluations/2026-11-12-ew-fit-data/reference-check.json),
and [stress summary](../../../../../dev/research/evaluations/2026-11-12-ew-fit-data/archive-check.json)
record the implemented outcomes and independent numerical comparisons. Numerical inference is not yet validated for this family.
