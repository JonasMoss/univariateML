---
id: tlambda
inference_numeric:
  - regime: "regular interior model"
    available: false
    condition: "finite mu, sigma > 0 and lambda < 1/2; any nonempty supported free subset; if lambda and mu are free require lambda > -1"
    reason: "DQM and full-rank information are established, but support/join-aware numerical derivatives and fitted covariance validation for this expanded scope are not yet implemented"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#tukey-lambda"
  - regime: "active lower fitted shape bound"
    available: false
    condition: "lambda = -1 with both lambda and mu free"
    reason: "the model is DQM with full rank, but the fitted shape bound gives a Gaussian cone experiment rather than ordinary joint Wald calibration"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#tukey-lambda"
  - regime: "critical moving endpoints"
    available: false
    condition: "lambda = 1/2; any nonempty supported free subset"
    reason: "endpoint directions use sqrt(n log n) and the endpoint-preserving complement uses sqrt(n); a dedicated mixed-rate procedure is required"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#tukey-lambda"
  - regime: "Poisson moving endpoints"
    available: false
    condition: "lambda > 1/2; any nonempty supported free subset; lambda > 1 must be held"
    reason: "endpoint directions have non-Gaussian Poisson limits, with an additional root-n endpoint-preserving shape direction when sigma and lambda are both free; lambda = 1 also constrains free shape"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#tukey-lambda"
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
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations."
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
  certified: 'partial'
  note: 'tlambda/certificate.hpp and Details: held nonnegative shapes (concavity below 1, exact uniforms at 1 and 2,
    support-contact reductions above 1), scalar negative-shape fits and shape-only fits
    have conditional arithmetic certificates or structured stops. Free-shape fits with free
    nuisance coordinates and negative-shape joint location-scale fits retain uncertified searches. All-held evaluation is not counted as fitting certification.'
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
    With all coefficients free over all finite shapes, the likelihood is unbounded. On the fitted branch $-1\le\lambda\le1$, a maximum is attained for at least three observations with every tie multiplicity strictly below half the sample size; uniqueness is unknown. Holding location removes the lower shape bound, and a tie at that location allows collapse when scale and shape are free. Holding scale prevents scale collapse. Held positive shape requires support compatibility. Fixed nonnegative sub-uniform shapes have unique nuisance fits when a finite maximum exists. Uniform plateaus, convex support-contact ties and negative-shape multiple modes preclude a general uniqueness claim. The linked theory inventory gives the remaining conditional existence regimes.
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

On the fitted branch with location and scale free, the default tie guard is sufficient for existence, not necessary. The legacy negative-shape joint search still rejects some tied configurations. Held nonnegative shapes instead use their exact existence conditions, including small samples and repeated observations. Standardization must preserve distinct values.

The [certificate theory](../../../../../dev/research/evaluations/2026-11-07-tlambda-theory-data/proofs.md) supplies the reductions in `certificate.hpp`:

* At held $0\le\lambda<1$, the log likelihood is strictly concave in
  $A=1/\sigma,B=-\mu/\sigma$. Newton steps only supply proposals; a tangent
  upper bound over an explicitly bounded superlevel region certifies the
  likelihood gap. Held location is an affine constraint. Acceptance also
  requires a relative inverse-scale and standardized-location Newton step
  below $10^{-9}$. Constant samples collapse unless location is held away
  from the observed value or scale is held.
* At held $\lambda=1,2$, the halfwidth is $\sigma/\lambda$ and the exact
  support solution is used, including location plateaus when scale is held.
* At held $1<\lambda<2$, held-location scale is
  $\lambda\max_i|x_i-\mu|$ for a positive radius. Held-scale location compares the two support
  contacts, with indistinguishable endpoints producing a structured tie.
  Joint fitting searches the contact curve
  $\sigma=\lambda\max_i|x_i-\mu|$ using scalar boxes.
  Above two the same scale and contact reductions hold, while held-scale
  location uses scalar boxes instead of the convex endpoint comparison.
* Held negative shape with either location or scale held uses scalar boxes.
  With both nuisances held, shape uses scalar boxes with an incumbent-derived
  negative cutoff from $g_{-a}\le2^{-a-2}$. The inverse brackets bound
  densities on each box; a local proposal does not establish globality.

The scale reduction above two follows without a shape-profile assumption.
For $u+v=1$, $u,v>0$, and $\lambda>2$,

$$
D^2-(u^\lambda-v^\lambda)(u^{\lambda-2}-v^{\lambda-2})
=2u^{\lambda-1}v^{\lambda-1}+u^\lambda v^{\lambda-2}
 +v^\lambda u^{\lambda-2}>0.
$$

Thus $Q D'/D^2<(\lambda-1)/\lambda<1$, so the log-scale
score of each density, $-1+Q D'/D^2$, is negative. Its closed-support
maximum is at the smallest feasible scale. On a contact curve the envelope
$g\le2^{\lambda-2}$ gives a finite location interval from an incumbent.
Here $D$ is smallest at the largest admissible inverse tail (towards the
centre), whereas for $1<\lambda<2$ it is smallest towards an endpoint.

For held $\lambda=-a<0$, the density envelope is
$\min(M/\sigma,C\sigma^{1/a}/|x-\mu|^{1+1/a})$, where
$M=2^{-a-2}$ and $C=a^{-1-1/a}$. It bounds the scalar search region from a
feasible likelihood. With held location, collapse exponent zero is an
unattained supremum. With free location and held $\lambda=-1$, pairing a
half-tie with the other observations proves strict domination by the
collapse face; other held negative-shape equality comparisons stop
numerically with a face witness. Free-shape half ties retain the collapse face
and a uniform incumbent but stop numerically because the global comparison
is not implemented. A tolerance tie is never used to declare
nonattainment in these certificate routes.

Certification is in exact arithmetic conditional on the computed comparisons.
The practical padding is $512\epsilon(1+|v|)$, with compensated per-observation likelihood
sums padded by the uncancelled normalization/density magnitudes, compensated
score sums and inverse-bracket score uncertainty for tangents. Box geometry
margins are propagated through inverse brackets, so a nearly contacting
observation cannot lose its endpoint density upper bound through rounding. This is not
outward-rounded interval arithmetic. Fitted positive-shape proposals must contain
the observed extrema under the affine support residual, rather than relying on
rounded endpoint aliases. Uniform candidates are compared with the ideal
span/radius likelihood; if a represented midpoint needs a materially wider
interval, the fitter stops numerically. Inverse roots retain finite analytic
brackets in $-\log p$ and contract both bracket width and log-density width;
a small density variation alone is insufficient near the mode to certify a
score. Numerical enclosure failures stop. Scalar-box acceptance bounds the
log-likelihood gap by $n\,\texttt{reltol}$, subject to the stated arithmetic
floor; it does not prove unique coordinates. Certificate evaluations use
$128\min(\texttt{iterlim},100000)$ and solver/inverse iterations use
$\min(\texttt{iterlim},100000)$. The box allowance stops with budget witnesses when exhausted; a solver cap stops as numerical non-convergence.

The other fits retain the standardized multistart search, including the
lower shape face and exact uniform candidate. Its $10^{-8}n$ preference for
the uniform is historical numerical behavior, without a global certificate.
In particular, free shape with a free nuisance, negative-shape joint nuisance
fits remain uncertified. The
[implementation validation](../../../../../dev/research/evaluations/2026-11-07-tlambda-impl-report.md)
records the scope, coefficient comparisons, performance and open work.

The log density uses $u=F(x)$, defined implicitly by the quantile function. The quality specification differentiates this inverse implicitly and uses independent SciPy and mpmath evaluations. The $\lambda=0$ member is logistic. Quantile inversion uses the smaller logarithmic tail in safeguarded Newton steps.

## R notes

Estimated coefficients on an applicable `fit_domain` bound carry `restricted`
and emit a warning of class `univariateML_restricted`. Held coefficients are
not flagged. The declared hold exemptions apply to free coefficients. In the remaining multistart routes, a search start converges when its simplex diameter and log-likelihood spread meet the `reltol` criteria. Fitting stops if no start converges, or if an unresolved start exceeds the best converged likelihood by more than $10^{-8}n$. This family has no score-based rescue of failed starts.


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
