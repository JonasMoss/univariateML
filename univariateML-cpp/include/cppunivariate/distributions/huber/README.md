---
id: huber
inference_numeric:
  - regime: "regular finite interior"
    available: true
    condition: "finite mu, sigma > 0 and finite epsilon > 0; any free subset; normal and Laplace limits excluded; analytic piecewise natural-coordinate derivatives; successful interior fit and nonsingular information required"
    classification: "dev/research/evaluations/2026-11-04-infer-moving-report.md#huber"
name: Huber
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
- name: epsilon
  symbol: '\epsilon'
  role: shape
  domain: (0, inf)
  simulate: 1.345
  kind: design
  advice: Hold epsilon at a positive tuning value, such as epsilon = 1.345. Set epsilon = NULL to estimate it, with possible normal or Laplace limit diagnostics.
r:
  density: dhuber
  sampler: rhuber
  control:
    sigma: Optional held sigma; NULL estimates it.
    mu: Optional held mu; NULL estimates it.
    epsilon: Positive finite tuning constant (required, e.g. 1.345), or NULL to estimate it.
    gradtol: Tolerance on the mean standardized location and scale scores.
    iterlim: "Work limit: min(iterlim, 100000) iterations per solve and 128 times that many certificate evaluations."
log_density: -log(sigma)-log(sqrt(2*pi)*erf(epsilon/sqrt(2))+2*exp(-epsilon**2/2)/epsilon)-Piecewise(((x-mu)**2/(2*sigma**2), Abs((x-mu)/sigma)<=epsilon), (epsilon*Abs((x-mu)/sigma)-epsilon**2/2, True))
limits:
  - name: scale collapse
    firmness: proved
    when: The free scale $\sigma$ tends to zero at the fitted or held location.
    detect: Scale is free and every observation equals the fitted or held location.
    status: UnboundedLikelihood
    message: "unbounded"
  - model: norm
    firmness: proved
    when: 'The free shape satisfies $\epsilon\to\infty$, leaving the normal law at the eligible fitted or held location and scale.'
    fixed: []
    status: NormalLimit
    numerical: true
  - model: laplace
    firmness: proved
    when: 'The free shape and scale tend to zero with $b=\sigma/\epsilon$ fixed, giving a Laplace law.'
    fixed: []
    status: LaplaceLimit
    numerical: true
holds: []
fit_args:
  epsilon: 1.345
examples: |
  set.seed(1)
  x <- rml(200, mlhuber())
  mlhuber(x, epsilon = 1.345)

group: skewed and robust location-scale
construction:
- location-scale
relations: {}
fit:
  method: profile-search
  certified: 'yes'
  note: 'Supporting-plane nuisance bounds certify held-epsilon fits; adaptive epsilon subdivision bounds the profiled likelihood, including both limit faces. Conditional on computed comparisons with the stated floating-point margin; unresolved comparisons and work exhaustion stop.'
areas:
- robust modelling
classification: specialist
classification_reason: Used to model robust errors with a Gaussian centre and exponential tails.
summary: Huber provides a model for robust errors with a Gaussian centre and exponential tails.
aliases: []
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
existence:
  uniqueness: unknown
  statement: >-
    With location, scale and shape free, a finite MLE need not exist: the supremum can lie at a normal or Laplace limit, and constant data permit unbounded scale collapse. No general uniqueness theorem is asserted. With shape held, a finite maximum is attained for nonconstant data, including ties. For constant data it is attained when scale is held or location is held away from the observation. With shape free, held location and scale are respected in the eligible limit comparisons; all supported subsets use global likelihood bounds or stop with an unresolved comparison or work budget.
  reason: >-
    For fixed shape, write inverse scale $\eta=1/\sigma$ and scaled location $\nu=\mu/\sigma$. The log-likelihood is $n\log\eta-\sum_i\rho_\epsilon(\eta x_i-\nu)$ plus a constant, with convex Huber loss $\rho_\epsilon$. It is concave, but linear tails can give an interval of maximizing locations. Coercivity gives attainment except for the stated zero-residual collapse. Concavity in shape of the loss gives endpoint bounds for the profiled search; both limit likelihoods are retained.
---

The Huber density models symmetric measurements with a Gaussian core and exponential tails. Location $\mu$ sets the centre, scale $\sigma$ sets the residual unit, and shape $\epsilon$ sets the transition from quadratic to linear log-density loss.

## d/p/q/r

### Extreme d/p/q arithmetic

Within the Gaussian core, the survival numerator is evaluated as a log-domain difference of normal tails plus the exponential-tail mass. Materializing erfc first underflows at standardized arguments around 38, although its logarithm is finite; inversion of that zero previously saturated the quantile at the underflow point even when epsilon was much larger.

## Details

The [family analysis](../../../../../dev/research/evaluations/2026-10-29-p0-locscale.md) derives these boundary conditions, including the relevant held parameters.

Shape may be held as a tuning constant or estimated. The Laplace scale is
$b=\sigma/\epsilon$; this face requires free $\sigma$. The normal face is
eligible with any held location or scale. Both faces require free $\epsilon$.
For two distinct observations the free location-scale-shape MLE does not exist;
see the [free-shape analysis](../../../../../dev/research/evaluations/2026-10-29-huber-free-epsilon-report.md).

The scale is the joint maximum-likelihood scale, not Huber's
Proposal 2 scale. When the maximising location is an interval (the sample
lies in the linear tails), its midpoint is returned. For held $\epsilon$ and nonconstant data
the maximum is attained, ties included; constant data have an unbounded
likelihood unless $\mu$ is held at a different value or $\sigma$ is held.

## Certificate

For fixed $c=\epsilon$, the nuisance likelihood is concave in inverse scale
and scaled location. At a candidate, put $z_i=(x_i-\mu)/\sigma$,
$G=\sum z_i\psi_c(z_i)$ and $H=\sum\psi_c(z_i)$. A free location is
confined to the sample hull. Let $M$ be the maximum of
$H(\mu'-\mu)/\sigma$ over that hull, or zero when location is held.
The supporting plane to the loss gives the global likelihood gap bound
$M+G-M-n-n\log((G-M)/n)$ when scale is free and $G-M>0$;
with held scale the bound is $M$. Nonfinite or insufficiently tight bounds
stop. Flat location intervals retain the midpoint convention; for example,
with observations $(-10,10)$, held scale 1 and shape 1, every location in
$[-9,9]$ maximizes the likelihood. Uniqueness is therefore false in general.

For free shape, the grid/refinement fitter supplies a proposal only.
Each endpoint of the adaptive subdivision receives a supporting-plane nuisance
upper bound $U$. Since $\rho_c(z)$ is concave in $c$, on $[l,u]$ the
profile likelihood is bounded above by the interpolated endpoint upper bounds
plus $n$ times the chord error of $\log Z(c)$. With
$q=2e^{-c^2/2}/(cZ(c))\in(0,1)$,
$(\log Z)''=q+(2q-q^2)/c^2\le1+2/l^2$.
Thus $\max(U_l,U_u)+n(1+2/l^2)(u-l)^2/8$ bounds the whole interval.
The largest bound is bisected geometrically until the global gap is at most
$10^{-8}n$, or the certificate allowance is exhausted. This is a value
certificate, not a uniqueness or parameter separation theorem. A finite free-shape
answer additionally passes the nuisance solver gates $|H|/n\le\max(\mathrm{gradtol},10^{-7})$
when location is free and $|G/n-1|\le\mathrm{gradtol}$ when scale is free,
and the total log-shape score gate
$|2n e^{-c^2/2}/(cZ(c))-c\sum_{|z_i|>c}(|z_i|-c)|\le10^{-3}$.
Subdivision continues when these gates fail; no unchecked failed proposal is
returned merely because its value enclosure is small.

Above $\max_i|x_i-\mu_N|/\sigma_N$, the exact constrained normal solution is
inside the quadratic core and solves the concave nuisance problem; only the
normalizer changes, monotonically toward the normal face. With scale free,
the excluded lower tail has upper bound $\ell_L+nc^2/2$; with scale held,
use $n\{\log c-\log(2\sigma)+c^2/2\}$. A lower-tail value envelope alone
does not establish that Laplace wins.

A strict median cluster supplies an exact small-shape alternative. Let $k$
observations equal the median $m$, $D$ be the right count minus the left
count, and $b=n^{-1}\sum|x_i-m|$. For sufficiently small $c$, the inner
solution is $\sigma=cb$, $\mu=m+c^2bD/k$, with
$\ell(c)-\ell_L=Bc^2/2-n\log(cZ(c)/2)$ and
$B=n-k+D^2/k$. For held location, the same formula uses its zero-residual
count and $D=0$. The implementation checks that no other residual enters the
core. Since $\log(cZ(c)/2)\ge c^2/2-c^4/6$ for $c<1$, the entire tail
is below Laplace when $B<n$ and $c^2<3(n-B)/n$. Without this proof, an
unresolved Laplace comparison stops with both witnesses.

Certificates are exact-arithmetic statements conditional on the computed
comparisons. The practical supporting-plane padding is
$256\,\mathrm{eps}(1+|\ell|+\sum|z_i\psi_i|+|M|+Q\sum|\psi_i|)(1+n\,\mathrm{eps})$.
Here $Q$ is the largest absolute standardized distance from the candidate
location to a hull endpoint, or zero with location held. Held-epsilon acceptance adds $1024\,\mathrm{eps}(1+|\ell|)$ to the
$10^{-8}n$ allowance. Free-shape face comparisons use
$1024\,\mathrm{eps}(1+|\ell_{\rm incumbent}|+n)$; if that padding exceeds
$10^{-8}n/8$, the fit stops. These are practical floating-point margins,
not outward-rounded intervals. All point likelihoods are reevaluated at the
returned coefficients in the original coordinates. Gaussian face scores must
satisfy $|H_N|/n\le10^{-7}$ and $|G_N/n-1|\le10^{-10}$ for free coordinates;
the free Laplace scale must satisfy $|\sum|x_i-m|/(nb)-1|\le10^{-10}$.
These gates bound the ideal face improvements by $6\times10^{-15}n$ and
$6\times10^{-21}n$, respectively. Each face upper bound adds
$128\,\mathrm{eps}(1+n+|\ell_{\rm face}|)$. The normal core endpoint is
inflated to $(\max|z_{N,i}|+10^{-6})(1+10^{-6})$ to contain the exact
Gaussian optimizer allowed by those score gates. The median-cluster geometry
uses a factor $1+10^{-8}$ for its scale arithmetic. Unavailable face scores
produce a numerical stop even when a finite face point can be retained.
A finite winner must exceed both face upper bounds by
the arithmetic allowance, in addition to passing the global value bound.
A face is selected only when the compact
profile upper bound is below it by the arithmetic allowance; an overlapping
finite profile bound produces a tie stop, with the unresolved segment endpoints
retained. If a face calculation underflows or overflows, its malformed
witness is discarded and the numerical stop retains the remaining proper
points with an unknown global upper bound. Laplace witnesses avoid forming
unscaled absolute sums or the product $2b$, preserving large finite scales.
A nonrepresentable fixed-shape
objective can leave no finite witness; its lower bound remains unavailable.
Work exhaustion retains finite, normal and
eligible Laplace witnesses with a global upper bound and advises raising
`iterlim`. An unresolved comparison retains the same evidence as a tie stop.

Exact-prefix nominations remain a large-sample proposal optimization; the
full-reduction polish reproduces the default fitter's coefficients. That
fitter's proposal is returned when it lies within
$64\,\mathrm{eps}(1+|\ell_{\rm incumbent}|)$ of the best evaluated value
and the global bound accepts that value, adding this
rounding allowance to the finite-fit value tolerance. The
nomination grid requires finite ordered bounds; unavailable face arithmetic
stops the proposal before grid-size conversion or indexing. The observation-count
recovery threshold selects a recovery path and does not shorten the cold
Newton solve. Global acceptance is independent of those proposals.

## R notes

Supply `epsilon = 1.345` for a held tuning value, or `epsilon = NULL` to
estimate shape. Certified normal and Laplace faces stop with advice to use
`mlnorm` or `mllaplace`. Failed nuisance solves stop; work-budget and comparison
stops carry structured evidence through the shared R/JavaScript fit layer.

Both Gaussian-face comparisons obtain their conditional location and residual scale from the normal fitter; physical residuals are never squared to obtain that face scale.
