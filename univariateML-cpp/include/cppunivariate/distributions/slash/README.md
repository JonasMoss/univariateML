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
    reltol: "Requested relative coefficient accuracy for all-free fits (default 1e-10); checked by the final observed-information Newton correction."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations (default iterlim 500); per certificate: 128 * min(iterlim, 100000) certificate evaluations."
log_density: -log(sigma)-log(2*pi)/2+log(1-exp(-((x-mu)/sigma)**2/2))-2*log(Abs((x-mu)/sigma))
limits:
  - name: point mass collapse
    firmness: proved
    when: 'Scale satisfies $\sigma\to0$ as location approaches a value occurring exactly half the time.'
    detect: 'A value occurs exactly half the time, and its collapse log likelihood is at least the best numerically found interior log likelihood minus $10^{-8}n$. With location held at that value, the collapse supremum is always unattained.'
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
  certified: 'yes'
  note: 'slash/README.md, Details: conditional scale roots are unique, and held-scale and free-scale location searches use branch-and-bound with an objective-gap margin; exhausted numerical searches fail closed.'
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
existence:
  uniqueness: unknown
  statement: >-
    With location and scale free, an MLE exists if every value occurs fewer
    than half the time. A value occurring more than half the time makes
    likelihood unbounded. At exactly half, attainment depends on comparison
    with the finite collapse limits described below. Uniqueness is not
    guaranteed. With $\mu$ held and scale free, the scale maximum is unique
    if fewer than half the observations equal $\mu$; equality gives an
    unattained supremum and more than half gives divergence. Holding
    $\sigma>0$ guarantees attainment, but location can have multiple maxima.
  reason: >-
    Collapse at a value of multiplicity $k$ has order $\sigma^{n-2k}$.
    All other escaping paths have zero likelihood, and location can be
    confined to the sample range. For held location the scale score is
    strictly monotone through its root when $2k<n$. Symmetric separated
    observations give multiple location maxima when scale is held.
---

The slash distribution is a normal variate divided by an independent uniform variate, with location $\mu$ and positive scale $\sigma$. The random denominator
produces heavy tails; $\sigma$ describes scale rather than a standard deviation.

## Details

The all-free location uses the fitted scale as its accuracy floor. Final joint
location/log-scale refinement differentiates the slash log density analytically, with
a power series at zero; this avoids resolving a shallow location maximum by
likelihood comparisons alone. See the [accuracy
validation](../../../../../dev/research/evaluations/2026-11-10-acc-allfree-report.md).

When every coefficient is held, the likelihood is evaluated at those values
without optimization.

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) derives these boundary conditions, including the relevant held parameters.

With location and scale free, a value occurring more than half the time makes likelihood unbounded. With every multiplicity strictly below half, the maximum is attained. At exactly half, the finite collapse likelihood must be compared with the interior profile; ties within $10^{-8}n$ select collapse. Holding sigma removes collapse. The scale score has a unique root for every location away from the observations. The location profile can be multimodal; a grid over every observation gap nominates numerical local maxima for refinement. Both held-scale and free-scale searches apply the conditional subdivision certificates below; these bound the objective gap without requiring every mode to be isolated.

For an exact existence criterion in the half-mass case, put $c=1/\sqrt{2\pi}$ and $B_v=c^n2^{-n/2}\prod_{x_i\ne v}|x_i-v|^{-2}$ at each half-mass value $v$. For a location $m$ away from such values, let $s(m)$ be the unique positive zero of $\sum_i[1-2t_i/(e^{t_i}-1)]$, $t_i=(x_i-m)^2/(2s^2)$. An MLE exists if and only if some interior profile value $L(m,s(m))$ is at least $\max_v B_v$; if the maximum multiplicity is below half, the comparison is vacuous. This is an exact variational criterion; the implementation evaluates it numerically, with the stated tie preference. It is not a multiplicity-only rule: $(0,1)$ has an interior maximum, whereas $(0,0,-1,1)$ numerically selects collapse. Location lies in the sample range; the likelihood tends to zero at all other escaping paths. The density decreases strictly with absolute standardized distance. Together these facts prove attainment below half. At a half-mass point, $L(v,s)<B_v$ for every positive scale. If $\sum_{x_i\ne v}1/(x_i-v)\ne0$, choosing $m-v$ proportional to $s^2$ increases the likelihood above $B_v$ for sufficiently small $s$, proving existence (subject to comparison with any other half-mass point). It suffices to check the sum at a half-mass value with the largest $B_v$; a sample of two values with equal counts always passes, since both sums are nonzero and the two $B_v$ agree. A zero sum is not decided by this test ([TASK-117 report](../../../../../dev/research/evaluations/2026-10-31-open-existence-report.md)).

Location and scale are identified by symmetry and any fixed positive quantile width. Density and CDF formulas follow the [extraDistr slash documentation](https://search.r-project.org/CRAN/refmans/extraDistr/html/Slash.html); the existence argument above is direct, not attributed to the Cauchy theorem.

Multimodality is already forced with sigma held: for $(-100,100)$ and sigma 1, the likelihood near each observation exceeds its value at the midpoint. Symmetry then gives two separated location maxima. The global location search is used for held-scale fits as well as the free-scale profile.

Zero reciprocal sum is not sufficient for collapse: a sample of ten zeros, one $-1$ and nine $9$s has $L(4,3)/B>1.456$. Free-scale multimodality is explicit for a sample of ten zeros, one $-1$ and nine $10$s: $L(-1/625,1/5)/B>1.00012$ and $L(9/2,7/2)/B>1.066$. At location zero every finite-scale likelihood is below B; all other escaping paths on either side have likelihood at most B. Each side therefore contains an attained finite maximum. These strict bounds follow from degree-12 Taylor lower bounds on $1-e^{-t}$ and Bernoulli's inequality; they were certified using exact rational arithmetic.

With mu held, only its tie multiplicity k matters: 2k<n gives a unique positive scale maximum; 2k>n is unbounded, and equality selects an unattained collapse supremum. Holding sigma removes collapse even for constant samples. The total log-scale score decreases strictly from $n-2k$ to $-n$, so under $2k<n$ its unique root is the global conditional maximum. Failed scale profiles are excluded from candidate selection, and a search in which every profile fails stops as a numerical failure.

## Conditional search certificate

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
has a budget of `128 * min(iterlim, 100000)` intervals; exhausted budget, a nonfinite required
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
$e^t-1\le t/(1-t)$ for $0\le t<1$. The implementation instead checks score signs and bisects a log-scale
bracket, as described below. See the [TASK-116 investigation](../../../../../dev/research/evaluations/2026-11-01-slash-cert-report.md).

For a free-scale interval $I=[a,b]$, put $d_i=\min_{m\in I}|x_i-m|$
and $D_i=\max_{m\in I}|x_i-m|$. The scale score increases with absolute
residual, so any positive $s_-$ whose score using $d_i$ is positive is a
uniform lower bound on the profile root over $I$; similarly a scale $s_+$
whose score using $D_i$ is negative is a uniform upper bound. Where such a
lower bound is available, the profile is $n\sqrt2/s_-$-Lipschitz by the
envelope theorem, allowing the same endpoint-cone test as for held scale.

The quadratic test further bounds the profile curvature. Let $h(z)$ be the
log standardized density up to a constant, $z=(x-m)/s$, and $t=z^2/2$.
On the rectangle $m\in I$, $s\in[s_-,s_+]$, write
$z_{i,-}=d_i/s_+$, $z_{i,+}=D_i/s_-$ and
$t_{i,\pm}=z_{i,\pm}^2/2$. From the mixture representation,
$|h''(z)|\le 1+z^2/4$ and $|h'(z)|\le\sqrt2$. A second bound follows by
differentiating the closed form for $h''$: for $t_{i,-}>0$, with
$q=\max(t_{i,-},1)$, $|h''|$ is at most
$2/z_{i,-}^2+e^{-t_{i,-}}/(1-e^{-t_{i,-}})+2q e^{-q}/(1-e^{-t_{i,-}})^2$.
Use the smaller bound $H_i$. Then $|\ell_{mm}|\le A=\sum H_i/s_-^2$
and $|\ell_{m,\log s}|\le C=\sum(\sqrt2+z_{i,+}H_i)/s_-$.
The negative log-scale curvature is at least
$B=\sum_i\min\{2t_{i,-}e^{-2t_{i,-}},2t_{i,+}e^{-2t_{i,+}}\}$:
for the scale-score summand $g(t)=1-2t/(e^t-1)$,
$g'(t)\ge e^{-2t}$, and $t e^{-2t}$ has a single maximum. When $B>0$,
implicit differentiation yields $P''=\ell_{mm}+\ell_{m,\log s}^2/
(-\ell_{\log s,\log s})\le A+C^2/B$. Taylor's theorem then bounds $P$
from the midpoint and its score. The log-scale score has absolute value at
most $n$, which bounds the objective error from the finite inner-root
bracket; the cross-derivative bound controls its effect on the location score.

If a positive uniform lower scale is unavailable, a separate residual bound
also covers collapse. The density decreases with absolute residual, hence
$\ell(m,s)\le U_I(s)$, the log likelihood of the residuals $d_i$ at scale
$s$. If $k$ of these residuals are zero and $2k<n$, its monotone score gives
a unique finite maximum; if $2k=n$, its supremum is
$n\log c-(n-k)\log 2-2\sum_{d_i>0}\log d_i$, $c=1/\sqrt{2\pi}$;
if $2k>n$ the bound is infinite. This applies to odd and even samples and
intervals adjacent to a half-mass value. A finite scale root is enclosed by
score-sign bisection, and the $n$-Lipschitz bound in log scale bounds its
remaining objective error. An earlier [numerical prototype](../../../../../dev/research/evaluations/2026-11-02-slash-free-data/residual-bound-probe.py)
illustrates why this residual bound alone is inefficient near an interior
maximum.

Subdivision starts with the existing grid and incumbent. It compares the
bounds to the half-mass boundary plus $10^{-8}n$ while the incumbent is
within that collapse preference, and otherwise to the best interior
likelihood plus its margin. The
practical margin is $10^{-9}(1+|\ell_*|)$ in original
coordinates, capped at $10^{-9}n$ when a half-mass boundary is present,
so that a change of units does not enlarge the collapse decision tolerance.
The final comparison applies the stated $10^{-8}n$ collapse preference.
These are exact-arithmetic bounds conditional on the computed comparisons;
the margin is not a proof of floating-point rounding error. Exhausting `128 * min(iterlim, 100000)` intervals reports a budget stop. A
nonfinite required value or unrepresentable midpoint reports
`IterationFailure`; neither path returns an unchecked estimate.

| Question | Evidence |
|---|---|
| Inner scale | The score has the unique root under the multiplicity conditions above, and the exact-arithmetic bracket above encloses it. The implementation expands a log-scale bracket until opposite score signs are verified, bisects, and accepts the scale only if its total log-scale score is below 1e-3 after back-transformation; otherwise it reports a numerical failure. This is conditional on computed comparisons, not an outward-rounded enclosure. |
| Location on bounded intervals | Held scale uses the Lipschitz and curvature bounds with complete subdivision, conditional on computed comparisons; multimodality is demonstrated above. Free scale uses uniform score bounds away from collapse and a residual likelihood bound near half-mass collapse. |
| Faces | Range containment and collapse comparisons are established above; the implemented comparisons have numerical tolerances. |
| Global certificate | Held-scale objective gap bounded by the stated margin in exact arithmetic conditional on computed comparisons; the margin is not a proved rounding bound. Free scale uses the same conditional objective-gap convention, including the collapse bound above. |

## Decision history

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

The free-scale profile caches each observation's log distance from the
current location before bracketing and bisecting its scale root. The cached
distances are reused only at that location; score signs, brackets, stopping
rules and the location certificate are unchanged.

Subtracting zero from a cached log distance is exact in binary64; the
subsequent subtraction of log scale and ordered score sum therefore reproduce
the uncached computation. At `log(t) >= 4`, the previous clamped argument gives
`exp(4) > 42`, so the score term is exactly one without exponentiation.
[Paired records](../../../../../dev/research/evaluations/2026-11-09-slash-perf-data/benchmark.py)
test this arithmetic identity and its fit-level consequences for every held
subset.

With scale held, the final location error is measured using the observed location curvature; the likelihood cover alone does not bound coefficient error near a flat maximum.
