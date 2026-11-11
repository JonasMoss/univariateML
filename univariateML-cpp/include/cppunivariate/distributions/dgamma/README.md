---
id: dgamma
inference_numeric:
  - regime: primary interior
    available: true
    condition: "finite shape > 0 and rate > 0; either or both free"
    classification: "dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification"
name: Discretized gamma
support: {type: integer, lower: 0, upper: inf, closed: [true, false]}
params:
- name: shape
  symbol: 'k'
  role: shape
  domain: (0, inf)
  simulate: 1
  kind: regular
- name: rate
  symbol: '\lambda'
  role: rate
  domain: (0, inf)
  simulate: 1
  kind: regular
r:
  density: ddgamma
  sampler: rdgamma
  control:
    shape: "Optional held value; NULL estimates it."
    rate: "Optional positive held value; NULL estimates it."
    reltol: "Relative coefficient accuracy (default 1e-10)."
    iterlim: "Work limit per solve: 1 * min(iterlim, 100000) solver iterations; per certificate: 128 * min(iterlim, 100000) certificate evaluations."
log_density: "log((lowergamma(shape, rate*(x+1)) - lowergamma(shape, rate*x))/gamma(shape))"
limits:
  - name: point mass
    firmness: proved
    when: 'The latent shape satisfies $k\to\infty$ while the latent mean $k/\lambda$ is fixed strictly inside the observed cell, so the latent variance $k/\lambda^2$ tends to zero.'
    detect: 'Both coefficients are free and all observations are equal.'
    status: PointMass
  - model: binom
    firmness: proved
    name: shifted Bernoulli
    when: 'The latent shape satisfies $k\to\infty$ and the latent mean approaches the shared integer boundary of two adjacent cells at a rate that preserves their observed proportions.'
    detect: 'Both coefficients are free and the integer sample range is one.'
    status: TwoPointLimit
  - name: point mass at zero
    firmness: proved
    when: 'The rate satisfies $\lambda\to\infty$ with shape held, or shape satisfies $k\to0$ with rate held.'
    detect: 'All observations are zero and exactly one coefficient is free.'
    status: PointMass
existence:
  uniqueness: unknown
  statement: >-
    With both coefficients free, a finite MLE exists if and only if the
    integer sample range is at least two. Constant samples and samples on
    two adjacent integers have unattained suprema. With either coefficient
    held, a finite MLE exists if and only if at least one observation is
    positive. With shape held this maximum is unique. Uniqueness with
    rate held or both coefficients free has not been established.
    Holding both coefficients evaluates the likelihood without fitting.
  reason: >-
    At small shape, every positive cell probability vanishes uniformly in
    rate. At large shape, only one cell or two adjacent cells can retain
    positive mass. The remaining rate boundaries lose an occupied cell.
    These bounds give compact positive likelihood level sets in each
    stated existence regime. Continuity then gives
    attainment. Concentration in one cell or across two adjacent cells
    approaches the saturated multinomial bound, which every finite fit
    strictly misses. With shape held, strict concavity in log rate gives
    uniqueness. Identification of both coefficients does not establish
    uniqueness of a sample likelihood maximum.
holds: []
examples: |
  set.seed(1)
  x <- rml(200, mldgamma())
  mldgamma(x)

group: gamma and Weibull
construction:
- discretisation
relations:
  discretisation_of:
  - family: gamma
    at:
      shape: shape
      scale: 1/rate
fit:
  method: multistart
  certified: 'yes'
  note: 'dgamma/README.md, Details; held shape uses a strictly concave rate solve; free shape uses finite tail bounds
    and branch-and-bound, conditional on computed comparisons and a practical 1e-8 n likelihood margin.'
areas:
- counts and ecology
classification: specialist
classification_reason: Used to model nonnegative counts obtained by grouping gamma waiting times into unit intervals.
summary: Discretized gamma provides a model for nonnegative counts obtained by grouping gamma waiting times into
  unit intervals.
aliases:
- "discrete gamma"
references:
- 'Wolodzko, T. and Klinke, S. (2026). extraDistr: Additional Univariate and Multivariate Distributions. R package
  reference manual, version 1.10.0.5. https://cran.r-project.org/web/packages/extraDistr/extraDistr.pdf.'
key_references:
- 1
---

The discretized gamma distribution is the integer part of a continuous gamma variable. Each observation records the latent variable in an interval from the observed integer to the next integer. The coefficients $k$ and $\lambda$ are the shape and rate of the latent gamma variable. Its mean is $k/\lambda$ and its variance is $k/\lambda^2$, rather than the mean and variance of the observed counts. Holding shape fixes the relative dispersion of the latent variable. Holding rate fixes its inverse scale while shape remains free.

## Details

The [existence analysis](../../../../../dev/research/evaluations/2026-10-29-p0-discrete2.md) and the boundary argument in this dossier establish the existence conditions. Numerical face searches alone do not certify global optimality.

With both parameters free, constant samples and samples on two adjacent integers have no finite maximum likelihood estimate. The limiting adjacent-cell probabilities equal the observed proportions.
Both held parameters evaluate the likelihood without fitting. With one parameter held the fitter handles its distinct boundary conditions explicitly.

Every unrestricted sample with range at least two has an attained maximum. Holding either parameter gives an attained maximum if any observation is positive; all-zero data escape to a point mass. The fitter initializes from a 41-point logarithmic shape grid with improving-edge expansion and Nelder–Mead refinement. It then subdivides the finite shape interval using the documented score bounds; exhaustion or nonfinite calculations report numerical failure. Joint uniqueness and uniqueness with rate held have not been established. No additional proper limit model occurs as shape tends to zero: every positive fixed cell loses its probability uniformly over rate. Some mass can escape to infinity, but that does not give a positive limiting likelihood for a sample containing a positive count.

## Existence and identification

For $a>0,r>0$, $p_k=\Pr(k\le Y<k+1)$ with $Y\sim\mathrm{Gamma}(a,r)$.
The positive-cell tail satisfies
$p_k\sim C(a,r) k^{a-1}e^{-rk}$; the successive ratio identifies $r$,
and the power of $k$ then identifies $a$.
For adjacent cells the saturated multinomial likelihood is a strict upper
bound for every finite parameter value. To approach any lower-cell fraction
$p$, put $b=k+1$, $m=b-s\Phi^{-1}(p)$,
$a=m^2/s^2$, $r=m/s^2$ and send $s\downarrow0$.
The standardized gamma converges to normal, giving cell probabilities $p$
and $1-p$. Constants similarly approach their point masses by concentration
inside the cell (at zero, also by $r\to\infty$).

The potentially troublesome small-shape boundary cannot retain a positive
fixed cell. Indeed, for integer $k\ge1$, maximizing $t^a e^{-t}$ over $t=ry$
gives the uniform-in-rate bound

$$
p_k\le\frac{a^a e^{-a}}{\Gamma(a)}\log(1+1/k)\longrightarrow0
\qquad(a\downarrow0).
$$

Thus even joint small-shape/small-rate paths cannot give positive limiting
likelihood to a sample containing a positive count. Such paths can leave a
subprobability concentrated at zero with mass escaping to infinity; they do
not yield a log-series or zero-modified proper count law without an additional
conditioning/rescaling operation.
For shape in a compact positive interval, rate approaching zero loses every
fixed cell and rate approaching infinity loses every positive cell.
For $a\to\infty$, write $m=a/r$: the coefficient of variation is $a^{-1/2}$.
If $m\to\infty$, fixed cells lose all probability; if $m$ is bounded, variance
$m^2/a\to0$, so positive limiting probabilities can occur only in one cell
or the two cells meeting a common integer boundary. These cases exhaust
noncompact parameter sequences. A sample with range at least two therefore
has compact positive likelihood level sets and an attained maximum.

With shape held, both rate boundaries lose a positive cell unless the sample
is all zero. With rate held, shape tending to infinity loses all fixed cells,
and shape tending to zero loses any positive cell. This proves the stated
held-subset existence conditions. In an all-zero sample, rate increasing or
shape decreasing makes its cell probability tend to one, strictly exceeding
every finite fit. All log likelihoods are bounded above by zero.
These arguments establish existence, not joint uniqueness or absence of local
maxima. The outer subdivision does not assume uniqueness.

With shape held, put $T=\log Y$ and $u=\log r$. Its density is proportional
to $\exp(a(t+u)-e^{t+u})$, strictly log-concave in the location coordinate.
Integrating over each fixed interval in $t$ preserves log concavity. The resulting log likelihood is analytic in $u$. For a sample containing a positive count it tends to minus infinity at both ends. A concave analytic function with two distinct maximizers would be constant on an interval and hence everywhere, contradicting these tails. Thus the maximum is unique, and the inner log-rate score has a unique zero. The fitter brackets and solves this score with safeguarded Newton
iteration before the outer shape search.

## Search certificate

This is **discretized** gamma, not the continuous double-gamma family.
Write $a$ for shape and $r$ for rate. For a cell $[k,k+1)$,
differentiating under the integral gives
$$
\partial_a\log p_k(a,r)=\log r-\psi(a)+E[\log Y\mid k\leq Y<k+1],
$$
where $Y$ has gamma shape $a$ and rate $r$. For $k\geq1$ the conditional
expectation lies between $\log k$ and $\log(k+1)$. The zero-cell lower
bound needs a separate integrable bound near zero. These inequalities are
valid with fixed rate and, at an accurately solved interior rate optimum,
the envelope derivative has the same expression summed over cells.

### Finite shape bounds and a profile derivative bound

The following bounds are exact-arithmetic bounds; no floating-point error
bound for the special-function evaluations is asserted. Let $w_k$ be the
frequency of cell $k$, $n=\sum w_k$, and $\bar k=\sum kw_k/n>0$.
Let $L_0<0$ be the log likelihood of any finite candidate. For any occupied
positive cell $k$, the small-shape bound above and
$\Gamma(1+a)\geq 1/(2e)$ for $0<a\leq1$ give
$$
L(a,r)\leq w_k\log\{2ea\log(1+1/k)\}.
$$
The gamma bound follows by restricting its integral to $[0,1]$. Hence every
$a$ below the smaller of 1 and
$\exp(L_0/w_k)/\{2e\log(1+1/k)\}$ has likelihood at most $L_0$,
uniformly in $r$.

For the other face, suppose occupied cells $l<h$ satisfy $h>l+1$.
Put $b=l+1$, $q=\sqrt{h/b}>1$, and
$c=\min\{q-1-\log q,\;q^{-1}-1+\log q\}>0$.
For $m=a/r\leq\sqrt{bh}$, the gamma Chernoff upper-tail bound gives
$p_h\leq e^{-ac}$; for $m\geq\sqrt{bh}$, the corresponding lower-tail
bound gives $p_l\leq e^{-ac}$. Thus
$$
L(a,r)\leq -a c\min(w_l,w_h),
$$
and $a\geq-L_0/\{c\min(w_l,w_h)\}$ is excluded. This applies to
both free and held rate. For held $r$, any sample also has the cutoff
$a\geq\max\{2r(k_{\max}+1),-L_0/[n(\log 2-1/2)]\}$: the lower-tail
Chernoff bound applies to every observed cell with
$r(k_{\max}+1)/a\leq1/2$.

For a bounded shape interval $[A,B]$, the rate score is
$n(a-r\,\overline{E[Y\mid K]})$. At its root,
$$
a/(\bar k+1)\leq r\leq a/\bar k.
$$
For the zero cell, let $y_0=\min(1,1/r)$ and $d=\log(1/y_0)$.
The kernel integral on $[0,1]$ is at least $e^{-1}y_0^a/a$, while
the integral of $(-\log y-d)_+$ against that kernel is at most
$y_0^a/a^2$. Consequently
$-d-e/a\leq E[\log Y\mid K=0]\leq0$.
Therefore the absolute derivative of the mean profile likelihood with
respect to $t=\log a$ is bounded on $[A,B]$ by
$$
B\{\max(|\log r_-|,|\log r_+|)
 +\max(|\psi(A)|,|\psi(B)|)
 +\max(\max_{k\geq1,w_k>0}|\log(k+1)|,\log\max(1,r_+)+e/A)\},
$$
where $r_-=A/(\bar k+1)$ and $r_+=B/\bar k$. For held rate use
$r_-=r_+=r$. The bound can be very loose and does not by itself establish
a practical search budget.

To account for the inner solve, a retained bracket $[u_-,u_+]$ for the
log-rate maximizer and concavity give the upper bound
$L(a,e^u)+\max\{g(u)(u_--u),g(u)(u_+-u)\}$, with $g$ the total
log-rate score. The certificate uses a separate bracketed rate solve and
includes this objective allowance; the initializer's score-only stopping
rule alone does not provide it.

The following sharper enclosure may be useful for interval subdivision. Put
$H(a,r)=n^{-1}\sum_k w_k\log p_k(a,r)+\log\Gamma(a)-a\log r$.
For each cell, the logarithm in $H$ is the log of an integral of
$y^{a-1}e^{-ry}$ over that cell. Its derivative in $a$ is the conditional
mean of $\log Y$, and its second derivative is the conditional variance of
$\log Y$. Moreover the derivative of $H_a$ with respect to $r$ is minus the
conditional covariance of $Y$ and $\log Y$, with the covariance nonnegative. Thus
$H_a$ is nondecreasing in $a$ and nonincreasing in $r$, including for the
zero cell. If $a\in[A,B]$, $r\in[R,S]$, and $0<h<A$, convex secants give
$$
\frac{H(A,S)-H(A-h,S)}{h}\leq H_a(a,r)
\leq\frac{H(B+h,R)-H(B,R)}{h}.
$$
Combining these with the monotonicity of digamma and the bounds on $r$
encloses the profile score in log shape on the entire interval.

For free rate the crude rate interval above can be sharpened without
assuming that the maximizing rate is monotone in shape. Write the mean
log-rate score as $g(a,r)=a-rM(a,r)$, where $M$ is the weighted conditional
mean of $Y$. Since each conditional mean increases with shape,
$$
g(B,r)-(B-A)\leq g(a,r)\leq g(A,r)+(B-A).
$$
Strict concavity in log rate then lets signs of these two expressions discard
parts of the rate interval containing no optimum for any $a\in[A,B]$.
The implementation subdivides log shape, uses these bounds to enclose the
profile slope, and applies the midpoint likelihood plus its rate-solve
allowance and the worst permitted slope over each half interval. It discards
an interval only when this upper bound is within $5\times10^{-9}$ per
observation of the best candidate. The returned parameter's recomputed
likelihood must be within another $5\times10^{-9}$ per observation of that
candidate. The subdivision budget is one thousand times the iteration limit in intervals. Invalid or
nonfinite bounds, collapsed secants and an exhausted budget report
numerical failure. The secant step is $10^{-4}A$; its computed bounds are
widened by 64 machine epsilons times the magnitude of the summed terms,
divided by that step. These are exact-arithmetic bounds conditional on the
computed comparisons, with a practical $10^{-8}n$ likelihood allowance,
not outward-rounded interval arithmetic or a uniform special-function error
guarantee.

## R notes

For adjacent values `j` and `j + 1`, `mlbinom(x - j, size = 1)` fits the limiting Bernoulli probabilities. Passing `NULL` estimates a coefficient; supplying a positive finite value holds it. Supplying both coefficients evaluates the likelihood without optimization. The subdivision budget is `128 * min(iterlim, 100000)` intervals, and numerical failure stops the fit rather than returning an unchecked candidate.

## Decision history

2026-10-30 (TASK-125): the shared gamma upper-tail helper switches to its asymptotic expansion before the backend tail becomes subnormal. The existing high-precision truth rows test the affected rate/shape cases; the expansion is not a uniform error guarantee.

2026-11-02 (TASK-116): the outer search uses finite tail cutoffs and branch-and-bound rather than relying on the multistart initializer for global optimality.

Numerical checks of these bounds are retained in
[`check_bounds.py`](../../../../../dev/research/evaluations/2026-11-02-dgamma-cert-data/check_bounds.py)
and [`check_secants.py`](../../../../../dev/research/evaluations/2026-11-02-dgamma-cert-data/check_secants.py).
On base revision `77a0aa214e1a51094204a72d6b9672c16fb1f27b`, with these
documentation changes, both passed locally under `/tmp/umvl-cpu/run python3`:
300 seeded checks of the first bounds and 100 seeded quadrature checks of
the secant and score enclosures. The scripts specify seeds and tolerances.
They check sampled inequalities and do not validate the proposed search or
prove special-function error bounds.

| Question | Evidence |
|---|---|
| Inner log-rate | Strict concavity and uniqueness are proved above. The numerical solver accepts a score below $10^{-11}\min(1,a)$ per observation or a relative log-rate bracket width below $10^{-12}$. |
| Shape on bounded intervals | The bounds above include the zero cell and the profiled rate. Convex secants provide the score enclosure used by subdivision. |
| Shape faces | Finite cutoffs are proved above for the unrestricted and held-rate existence regimes. |
| Global certificate | The subdivision uses the stated practical margin and returns numerical failure when it cannot complete. It is conditional on computed comparisons, not outward-rounded arithmetic. |

The shape score differentiates the incomplete-gamma integral over the whole
unit cell. The gamma density at a cell midpoint is not a substitute, and an
accurate conditional rate does not remove the shape/rate cross-information.
This distinction matters when likelihood rounding obscures shape errors.
