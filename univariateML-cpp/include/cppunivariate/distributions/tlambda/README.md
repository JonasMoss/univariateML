---
id: tlambda
inference_numeric:
  - regime: unreviewed or unavailable
    available: false
    condition: "interior joint regime"
    reason: "DQM and support-dependent regularity remain open in the cited classification"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Tukey lambda
support: {type: real, lower: "if (lambda > 0) mu - sigma/lambda else -Inf", upper: "if (lambda > 0) mu + sigma/lambda else Inf", closed: [true, true], endpoint_log_density: {lower: "if (lambda < 1) -Inf else if (lambda == 1) -log(2*sigma) else -log(sigma)", upper: "if (lambda < 1) -Inf else if (lambda == 1) -log(2*sigma) else -log(sigma)"}}
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
- name: lambda
  symbol: '\lambda'
  role: shape
  domain: '(-inf, inf)'
  simulate: 0
  fit_domain:
  - lower: -1.0
    restriction:
      mechanism: collapse
      exempt_when_held:
      - - mu
      reason: The lower bound on shape prevents scale collapse at a freely estimated location under the strict tie condition. Holding location removes this bound, but ties at the held location require a separate collapse check.
  - upper: 1.0
    restriction:
      mechanism: branch
      exempt_when_held: []
      reason: The upper bound on shape selects the principal branch, including its uniform endpoint. No exemption for held coefficients has been established.
  kind: regular
r:
  density: dtlambda
  sampler: rtlambda
  control:
    sigma: Optional held sigma; NULL estimates it.
    mu: Optional held mu; NULL estimates it.
    lambda: "Optional finite held shape; NULL estimates it under the applicable fitting bounds."
    reltol: "Tolerance on the standardized search simplex."
    iterlim: "Maximum simplex iterations per search start."
log_density: "-log(sigma)-log(u**(lambda-1)+(1-u)**(lambda-1))"
references:
- Joiner, B. L. and Rosenblatt, J. R. (1971). Some properties of the range in samples from Tukey's symmetric lambda
  distributions. Journal of the American Statistical Association 66, 394-399.
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
holds: []
limits:
  - name: scale collapse
    firmness: proved
    when: The free scale $\sigma$ tends to zero at a location occupied by observations.
    detect: With held location $\mu$, held negative shape $\lambda$, and free scale, the tie count $k$ satisfies $(n-k)/|\lambda|-k=0$. Other collapse configurations may be unbounded or rejected by the default tie guard.
    status: CollapseLimit
    message: "collapse"
  - name: infeasible held support
    firmness: proved
    fixed: [sigma, lambda]
    when: The held scale $\sigma$ and positive shape $\lambda$ leave no positive-likelihood support containing the sample.
    detect: The smallest radius about an eligible location containing the data exceeds $\sigma/\lambda$, or equals it when $0<\lambda<1$ and endpoint density is zero.
    status: InvalidSupport
    message: support cannot contain
examples: |
  set.seed(1)
  x <- rml(200, mltlambda())
  mltlambda(x)

group: skewed and robust location-scale
construction:
- location-scale
- generalisation
relations: {}
fit:
  method: multistart
  certified: 'no'
  note: 'tlambda/README.md, Details; tlambda/mle.hpp (public fitter and held overloads); tlambda/test.cpp: Default
    and nontrivial conditional fits use multistart, finite grids or local profile refinement without a global certificate;
    all-held density evaluation is not counted as certification.'
areas:
- robust modelling
classification: specialist
classification_reason: Used to model symmetric error shapes specified through a flexible quantile function.
summary: Tukey lambda provides a model for symmetric error shapes specified through a flexible quantile function.
aliases: []
key_references:
- 2
existence:
  uniqueness: unknown
  statement: >-
    With all coefficients free over all finite shapes, the likelihood is unbounded. On the fitted branch $-1\le\lambda\le1$, a maximum is attained for at least three observations with every tie multiplicity strictly below half the sample size; uniqueness is unknown. Holding location removes the lower shape bound, and a tie at that location allows collapse when scale and shape are free. Holding scale prevents scale collapse. Held positive shape requires support compatibility. General conditional existence and uniqueness beyond these cases are not established.
  reason: >-
    On the fitted branch the standardized density and its product with squared distance are uniformly bounded. Near a collapsing location of multiplicity $k$, likelihood is bounded by a constant times $\sigma^{n-2k}$. Under the strict tie condition it vanishes at collapse, and it also vanishes at diverging scale or location. Upper semicontinuity, including the uniform endpoint member, then gives attainment. The unrestricted spike path and conditional collapse exponent are given below; numerical searches do not establish uniqueness.
---

The Tukey lambda family describes symmetric measurements through location $\mu$, scale $\sigma$, and shape $\lambda$. Shape controls tail weight and whether support is bounded. Its quantile function spans heavy-tailed, logistic, approximately normal and uniform shapes.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) describes the limiting paths and the scope of the boundary comparisons.

Evaluation permits every finite shape. With location free, estimated shape is restricted to $-1\le\lambda\le1$, including the attained bounds. Holding location removes the lower bound. A held shape can lie outside this branch. In particular, $\lambda=1$ and $\lambda=2$ both represent uniform laws, with scales differing by a factor of two, so unrestricted shape is not identifiable.

The unrestricted likelihood is unbounded. Put location at an observation and take $\sigma=\lambda A$ as $\lambda\to\infty$, with $A$ greater than every distance to that observation. Central density is $2^{\lambda-2}/(\lambda A)$, while at nonzero distance $d$ it is asymptotic to $1/(\lambda|d|)$. With $k$ central observations, log likelihood grows as $k\lambda\log2-n\log\lambda+O(1)$.

The fitted branch excludes the U-shaped interval $1<\lambda<2$ and the subsequent increasingly concentrated branch. Capping shape at two would leave an endpoint-density discontinuity at one: the limit from above is twice the uniform density. On the chosen branch, endpoint density is zero below one and the uniform value at one gives upper semicontinuity. For the sample $(-1,1)$ and shapes in $(1,2]$, support containment forces $\sigma\ge\lambda$, and each density is at most $1/\sigma$. Likelihood stays below one, but $\mu=0$, $\sigma=\lambda\downarrow1$ approaches one without attaining it. See the [junction argument](../../../../../dev/research/evaluations/2026-10-31-open-existence-report.md).

For held $\lambda=-a<0$, collapse at a location of multiplicity $k$ has likelihood proportional to $\sigma^{(n-k)/a-k}$. A negative exponent gives unbounded likelihood. At held location, equality gives an unattained collapse supremum. With shape free and location held, even one observation at that location permits collapse through a sufficiently negative shape. Holding scale prevents scale collapse and permits small or tied samples. Positive held shape requires support containment, strictly inside the endpoints when $0<\lambda<1$; endpoints with positive limiting density are allowed for $\lambda\ge1$.

On the fitted branch with location and scale free, the default tie guard is sufficient for existence, not necessary. It also rejects some tied configurations with shape held. Fitting requires a finite sample range and a standardization that preserves distinct values.

The fitter standardizes the observations and searches multiple location, scale and shape starts, including a separate optimization of the lower shape face. It compares the exact uniform fit with every candidate and selects it for ties within $10^{-8}n$ in log likelihood. This is a numerical search without a certificate that every mode has been found. The uniform is an attained member. The existence argument for the declared branch requires no further limit model, but conditional searches outside that branch do not have a general existence certificate.

The log density uses $u=F(x)$, defined implicitly by the quantile function. The quality specification differentiates this inverse implicitly and uses independent SciPy and mpmath evaluations. The $\lambda=0$ member is logistic. Quantile inversion uses the smaller logarithmic tail in safeguarded Newton steps.

## R notes

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. The declared hold exemptions apply to free coefficients. A search start converges when its simplex diameter and log-likelihood spread meet the `reltol` criteria. Fitting stops if no start converges, or if an unresolved start exceeds the best converged likelihood by more than $10^{-8}n$. This family has no score-based rescue of failed starts.


## Decision history

The 2026-10-31 fit-speed revision shares the smaller-tail complement log and
quantile magnitude between Newton value and derivative calculations. Brackets,
Newton steps, stopping rules and objective reduction order are unchanged. A
sorted warm-start trial was rejected after pointwise agreement failed to
translate into a useful speed-up of the slowest held-scale fit; a pilot also
exceeded the coefficient comparison tolerance. See the [paired evidence and
rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

### Affine overflow portability (2026-11-01)

Quantiles whose unscaled displacement overflows use
`2 * (mu/2 + signed_displacement/2)`. At finite positive-shape endpoints,
the half displacement is `(sigma/2)/lambda`; at interior probabilities it is
`(sigma/2)*exp(logmag)`. This algebraic identity avoids relying on Windows
`fma` to cancel an overflowing product. The ordinary finite-product path is
unchanged. Symmetric binary64 regression cases use `sigma=1e308`,
`mu=+/-1e308`, `lambda=.5`, endpoints and probabilities .001/.999;
these are numerical checks, not general floating-point error bounds.

Sigma-held fits reuse evaluations at exactly repeated search coordinates; seeded decisions are identical to the uncached search ([held-subset comparison](../../../../../dev/research/evaluations/2026-11-06-slow-held-report.md)).
