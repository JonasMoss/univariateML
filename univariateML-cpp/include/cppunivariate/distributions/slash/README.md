---
id: slash
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite mu and sigma > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
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
    firmness: proved
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
relations:
  mixture_of:
  - family: norm
    at:
      mean: mu
      sd: sigma/mix
    mixed: sd
    mixing:
      family: unif
      at:
        min: 0
        max: 1
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

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) derives these boundary conditions, including the relevant held parameters.

The 2026-10-31 fit-speed revision skips only the suffix of scale bisections
after the midpoint rounds to an endpoint. That suffix cannot change the
represented final midpoint or scale. Double location residuals are also
computed once per profile and reused in the scale bisections, preserving the
division and squaring order. The location grid, refinement budget and
boundary/failure comparisons are unchanged; no global mode-isolation claim is
added. See the [paired evidence and rejected
trials](../../../../../dev/research/evaluations/2026-10-31-fit-speed-report.md).

The 2026-11-01 slash-speed revision also omits `expm1(t)` in the scale
score for $t\ge42$. The function $2t/(e^t-1)$ is decreasing for positive
$t$, and at 42 it is below $4.83\times10^{-17}<2^{-54}$. Thus
`1-2*t/expm1(t)` rounds to exactly one in binary64 round-to-nearest,
with ample margin for ordinary libm rounding. The previous shortcut at
$t>700$ is extended to this earlier saturation point. The density pass caches `log(sigma)` once per profile, reusing it in
the distance and normalization terms without changing their arithmetic order.
Division, squaring, score summation and bisection midpoints stay unchanged;
no tolerance or search change is involved. See the [paired profile and
validation report](../../../../../dev/research/evaluations/2026-11-01-slash-speed-report.md).

A value with more than half the sample mass makes the free-scale likelihood unbounded. With every multiplicity strictly below half, the maximum is attained. At exactly half, the finite collapse likelihood must be compared with the interior profile; ties within $10^{-8}n$ select collapse. Holding sigma removes collapse. The scale score has a unique root for every location away from the observations. The location profile can be multimodal; a grid over every observation gap nominates numerical local maxima for refinement. With free scale this search is numerical, not a proof that every mode is isolated. With scale held, the search also applies the conditional subdivision certificate below.

For an exact existence criterion in the half-mass case, put $c=1/\sqrt{2\pi}$ and $B_v=c^n2^{-n/2}\prod_{x_i\ne v}|x_i-v|^{-2}$ at each half-mass value $v$. For a location $m$ away from such values, let $s(m)$ be the unique positive zero of $\sum_i[1-2t_i/(e^{t_i}-1)]$, $t_i=(x_i-m)^2/(2s^2)$. An MLE exists if and only if some interior profile value $L(m,s(m))$ is at least $\max_v B_v$; if the maximum multiplicity is below half, the comparison is vacuous. This is an exact variational criterion; the implementation evaluates it numerically, with the stated tie preference. It is not a multiplicity-only rule: `c(0, 1)` has an interior maximum, whereas `c(0, 0, -1, 1)` numerically selects collapse. Location lies in the sample range; the likelihood tends to zero at all other escaping paths. The density decreases strictly with absolute standardized distance. Together these facts prove attainment below half. At a half-mass point, $L(v,s)<B_v$ for every positive scale. If $\sum_{x_i\ne v}1/(x_i-v)\ne0$, choosing $m-v$ proportional to $s^2$ increases the likelihood above $B_v$ for sufficiently small $s$, proving existence (subject to comparison with any other half-mass point). It suffices to check the sum at a half-mass value with the largest $B_v$; a sample of two values with equal counts always passes, since both sums are nonzero and the two $B_v$ agree. A zero sum is not decided by this test ([TASK-117 report](../../../../../dev/research/evaluations/2026-10-31-open-existence-report.md)).

Location and scale are identified by symmetry and any fixed positive quantile width. Density and CDF formulas follow the [extraDistr slash documentation](https://search.r-project.org/CRAN/refmans/extraDistr/html/Slash.html); the existence argument above is direct, not attributed to the Cauchy theorem.

Multimodality is already forced with sigma held: for `c(-100, 100)` and sigma 1, the likelihood near each observation exceeds its value at the midpoint. Symmetry then gives two separated location maxima. The global location search is used for held-scale fits as well as the free-scale profile.

Zero reciprocal sum is not sufficient for collapse: `c(rep(0, 10), -1, rep(9, 9))` has $L(4,3)/B>1.456$. Free-scale multimodality is explicit for `c(rep(0, 10), -1, rep(10, 9))`: $L(-1/625,1/5)/B>1.00012$ and $L(9/2,7/2)/B>1.066$. At location zero every finite-scale likelihood is below B; all other escaping paths on either side have likelihood at most B. Each side therefore contains an attained finite maximum. These strict bounds follow from degree-12 Taylor lower bounds on $1-e^{-t}$ and Bernoulli's inequality; they were certified using exact rational arithmetic.


With mu held, only its tie multiplicity k matters: 2k<n gives a unique positive scale maximum; 2k>n is unbounded, and equality selects an unattained collapse supremum. Holding sigma removes collapse even for constant samples.

## TASK-116: outer search evidence

This model has no free shape parameter: its outer search is over location
$m$, with scale $s$ held or profiled. For held $s>0$, the location log
likelihood has the global Lipschitz bound $n\sqrt{2}/s$. To see this, write
the standardized density as a constant times
$\int_0^1 u\exp(-u^2z^2/2)\,du$. Differentiation gives the absolute
standardized log-density score at most $|z|$. The closed form gives the
same score at most $2/|z|$ for nonzero $z$; the minimum of these two bounds
is at most $\sqrt2$. Divide by $s$ and sum over observations. The score is
continuous at zero. The sample range contains a maximizer because the
density decreases with absolute residual.

A second, local exact-arithmetic bound can sharpen subdivision near a
stationary point. With $h(z)=\log\int_0^1 u e^{-u^2z^2/2}\,du$,
let expectation be under the normalized integrand. Differentiating gives
$h''(z)=-E(u^2)+z^2\operatorname{Var}(u^2)$. Since $0\le u^2\le1$,
$-1\le h''(z)\le z^2/4$. Thus on a location interval $I$, with
$B_i=\max_{m\in I}|x_i-m|/s$, the total likelihood satisfies
$\ell''(m)\le\sum_i B_i^2/(4s^2)$. Taylor's theorem gives the
corresponding quadratic upper bound from any point of $I$. This bound can
be loose for distant observations. The implementation also uses the closed
form $h''(z)\le 2/z^2+1/(e^{z^2/2}-1)$ away from zero, taking the smaller
valid bound for each observation. Its slope and likelihood evaluations are
ordinary floating-point computations.

For held scale, the existing grid and its golden-section refinements supply
an incumbent. Each interval between consecutive grid points is pruned using
the endpoint Lipschitz cones, or bisected and pruned using the midpoint score
and the local curvature bound. Any better subdivision point becomes the new
incumbent and is polished by golden-section refinement. The exact-arithmetic
global gap is at most $10^{-9}(1+|\ell_*|)$, conditional on the computed
likelihood, score and bound comparisons. The magnitude uses the likelihood
in the original coordinates. This margin is a practical floating-point
allowance, not a proved error bound or outward-rounded enclosure. Subdivision
has a budget of one million intervals; exhausted budget, a nonfinite required
quantity, or a midpoint that cannot be represented returns `IterationFailure`
rather than an unchecked partial maximum. The same status is used when a
required golden-section refinement exhausts its iteration budget.

For the free-scale inner problem, an explicit exact-arithmetic root bracket
is available at any location with $k<n/2$ exact ties. Let $d$ and $D$ be
the smallest positive and the largest absolute residual, respectively, and
put $T=4n/(n-2k)$. The unique scale-score root lies strictly between
$d/\sqrt{2T}$ and $\sqrt2D$. Indeed, the summand
$g(t)=1-2t/(e^t-1)$ is increasing, $g(0)=-1$, and
$g(t)\ge(t-2)/(t+2)$ by $e^t\ge1+t+t^2/2$. At the lower endpoint,
the score is at least $-k+(n-k)(T-2)/(T+2)>0$. At the upper
endpoint all $t\le1/4$ and $g(t)\le-1+2t\le-1/2$, using
$e^t-1\le t/(1-t)$ for $0\le t<1$. This bracket is not used by
the implementation; evaluating its endpoints and score signs with rounding
control remains necessary. Moreover $d$ tends to zero near an observation,
so the bracket alone does not control a half-mass collapse neighborhood.
See the [TASK-116 investigation](../../../../../dev/research/evaluations/2026-11-01-slash-cert-report.md).

| Question | Evidence |
|---|---|
| Inner scale | The score has the unique root under the multiplicity conditions above, and the exact-arithmetic bracket above encloses it. The implementation uses numerical bisection, with a fixed lower bracket, rather than a verified error enclosure. |
| Location on bounded intervals | Held scale uses the Lipschitz and curvature bounds with complete subdivision, conditional on computed comparisons; multimodality is demonstrated above. No uniform positive scale bound is established here for the free-scale profile near half-mass collapse. |
| Faces | Range containment and collapse comparisons are established above; the implemented comparisons have numerical tolerances. |
| Global certificate | Held-scale objective gap bounded by the stated margin in exact arithmetic conditional on computed comparisons; the margin is not a proved rounding bound. Free scale remains a numerical grid search: a scale-root enclosure and uniform bounds near collapse remain open. |
