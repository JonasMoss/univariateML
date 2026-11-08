# EMG certificates — TASK-116.36

Completed 2026-11-08 in `codex/emg-cert`, based on
`c118a003f111b0d145f8f8b9385ee0a87c1fd4cd`.

The public fitter now certifies finite estimates for every supported nonempty
free subset. Location-only uses a signed score bracket; the other six use a
production outer shape certificate. Finite acceptance requires a validated
physical-coefficient enclosure and exclusion of every competing shape region
and compatible face. Budget, numerical, unresolved and comparison-tie outcomes
stop with representable witnesses. An unexcluded normal face without a better
finite witness stops unresolved. Proved domination cases retain their limit
stops.

The dossier is **fit.certified: 'yes'**. This describes finite-fit acceptance
across all supported subsets, including controlled stops when certification
cannot finish. It does not assert a complete analytic existence criterion or
successful numerical classification of every input. The default controls are
unchanged. Frozen goldens remain within tolerance; none were regenerated.
All final validation commands finished successfully.

**Public routes and implementation.**

| Free coefficients | Public acceptance route |
|---|---|
| mu | Strictly decreasing location score, signed root bracket and coordinate acceptance |
| sigma | Conditional shape profile with held physical mu and lambda |
| lambda | Conditional shape profile with held physical mu and sigma |
| mu, sigma | Fixed-rate shape certificate in `(a,k)=(lambda*mu,lambda*sigma)` |
| mu, lambda | Shape profile with held sigma |
| sigma, lambda | Shape profile with held mu |
| mu, sigma, lambda | Shape profile with the jointly concave `(u,b)` inner problem |

With all coefficients held, the fitter evaluates the proper finite point.
`holds: []` remains correct: there are no structural exclusions of free subsets.
Data-dependent non-existence is reported through the fitter.

The controller is
[certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/emg/certificate.hpp).
It calls the constrained inner certificate, conditional interval bounds, both
shape-tail bounds, local score-root validation, uniform nuisance inclusions,
and the signed fixed-location/scale normal-tail test. Their implementations are
[fixed_shape_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/emg/fixed_shape_certificate.hpp),
[shape_bounds.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/emg/shape_bounds.hpp),
[certificate_math.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/emg/certificate_math.hpp),
[winner_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/emg/winner_certificate.hpp),
and [normal_tail_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/emg/normal_tail_certificate.hpp).
The location route uses
[location_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/emg/location_certificate.hpp).
Every new library helper is on a public fitting path. The previous EMG
multistart, shape-grid and polishing machinery was removed from `mle.hpp`.
The helper shared with Bhattacharjee is byte-identical to the base and remains
used by Bhattacharjee's public fitter.

Routing and structured error mapping are in the
[binding-neutral EMG adapter](../../../bindings/common/distributions/emg/fit.hpp),
so R and JS use the same acceptance and stopping decisions. No shared header
needed modification. No changes were made to `dpqr.hpp`, `special/`, truth
files, code generators, frozen goldens, NEWS, status files or roadmap files.
C++ and shared JS test cases were appended. No commits or board edits were made.

**Exact-arithmetic basis.** Put
$k=\lambda\sigma$, $b=1/\sigma$, $u=b\mu$, $z_i=bx_i-u$, and
$r(t)=\phi(t)/\Phi(-t)$. Write $w(t)=r'(t)$ and $h(t)=r(t)-t$.
Conditional-normal moments give

$$
w(t)=r(t)\{r(t)-t\}=1-\operatorname{Var}(Z\mid Z>t)\in(0,1),
\qquad h'(t)=w(t)-1<0.
$$

The location score is $\sigma L_\mu=\sum_i\{k-r(k-z_i)\}$.
It is strictly decreasing, positive at the left end and negative at the right
end. Thus location-only has one finite global maximum, including for constant
data. The solver requires signed enclosing endpoints and accepts only when
every enclosed location is within `tol * max(abs(mu_star), sigma)` of its
returned value. An unresolved sign or unrepresentable precision stops
numerically.

At fixed k the inner objective is

$$
L=n\log b+n\log k+nk^2/2-k\sum_i z_i+
  \sum_i\log\Phi(z_i-k).
$$

Its nuisance curvature satisfies

$$
D^2_{u,b}L[du,db]= -n\,db^2/b^2-
  \sum_i w(k-z_i)(x_i\,db-du)^2<0
$$

for every nonzero direction. Physical holds impose `u=b*mu`, `b=1/sigma`, or,
with rate held at fixed k, `b=lambda/k`. Strict concavity survives these affine
constraints. For fixed positive k, use a Gaussian exponential envelope with decay
`min(k/2, 1/2)`: its convolution bound makes `n*log(b) - c*b*range(x)`
go to minus infinity as b increases, uniformly over location. A bounded
standardized density makes `n*log(b)` dominate as b goes to zero; bounded b
and escaping u also make the likelihood vanish. This gives a finite inner
maximum for nonconstant data. Constant-data collapses are handled before this route. Held location
different from a constant observation also prevents that collapse.

Validated damped Newton supplies an inner root enclosure. A signed profile
scale bracket with location-root enclosures is the fallback. In that fallback,
implicit differentiation gives

$$
u'(b)=\frac{\sum_i x_iw_i}{\sum_iw_i}\in[x_{\min},x_{\max}]
$$

when location is free, and $u'(b)=\mu$ when held. Thus the scale bracket and
midpoint location enclosure bound the optimizing `(u,b)`. The concavity tangent
over that enclosure gives a global inner likelihood upper bound. Both routes
supply bounds on the true constrained inner maximum.

**Bounds over shape intervals.** With rate free, holding nuisance coordinates
fixed gives

$$
L_{kk}=n-n/k^2-\sum_iw_i\geq-n/k^2,
\qquad L_k=n/k-\sum_i h(k-z_i)\leq n/k.
$$

On $[l,h]$, adding $nk^2/(2l^2)$ makes each nuisance objective convex. Taking
a supremum preserves convexity because the nuisance constraint set is fixed
across k. Certified endpoint profile upper bounds therefore give

$$
\sup_{[l,h]}P(k)\leq\max\{U_l,U_h\}
       +\frac{n(h-l)^2}{8l^2}.
$$

Integrating the first-derivative inequality also gives
$P(k)\leq U_l+n\log(h/l)$. The executor uses the smaller applicable bound.

Holding physical lambda changes the constraint as k varies. The executor uses
a separate bound in $a=\lambda\mu$, $d_i=\lambda x_i-a$:

$$
L=n\log\lambda-\sum_i d_i+nk^2/2+
       \sum_i\log\Phi(d_i/k-k),
$$

$$
L_{kk}=n-\sum_iw_i(1+d_i/k^2)^2+
            \sum_i2r_i d_i/k^3.
$$

The standardized mode lies in $(0,2/k)$. One proof uses $w<1$, which implies
$r(t)<(t+\sqrt{t^2+4})/2$, and checks the mode score at 0 and $2/k$.
For the small-shape bound on its location, put
$T=\sqrt{\max(0,2\log(2/l))}$. The mode is also below $k+T$ throughout
$[l,h]$: $r(-T)<l\leq k$. Consequently the optimizing free location obeys

$$
\lambda x_{\min}-h(h+T)\leq a\leq\lambda x_{\max}.
$$

For $l\geq1$, the lower endpoint can be tightened to
$\lambda x_{\min}-2$. These bounds define a common nuisance interval for every
k in the shape segment. Let Z bound every $|d_i|$ on this interval; for held
mu use the actual maximum of $|\lambda(x_i-\mu)|$. With
$R=h+Z/l+1$, the identities above give

$$
L_{kk}\geq-M,\qquad
M=n\left\{2Z/l^2+Z^2/l^4+2RZ/l^3\right\}.
$$

The resulting chord upper bound adds $M(h-l)^2/8$ to the larger conditional
endpoint upper bound. This argument retains the physical rate throughout.

**Uniform small-shape tail.** Let
$D=\sum_i(x_i-x_{\min})>0$ and $E=n\{\log(n/D)-1\}$.
Retain one minimum's Phi factor and put
$t=\lambda(x_{\min}-\mu)/k-k$. Then

$$
L\leq n\log\lambda-\lambda D-nkt-nk^2/2+\log\Phi(t).
$$

For $t\geq0$ the last terms are nonpositive. For $t<0$, use
$\log\Phi(t)\leq-\log2-t^2/2$ and optimize the quadratic and rate. Uniformly
over nuisance coordinates and $0<k\leq K$,

$$
L\leq E+\max\{0,n(n-1)K^2/2-\log2\}.
$$

With lambda held, replace E by $n\log\lambda-\lambda D$.
For held $\mu\leq x_{\min}$ let $j=\#\{i:x_i=\mu\}$ and

$$
E_\mu=n\log\lambda-\lambda\sum_i(x_i-\mu)-j\log2,
$$

optimizing lambda only when free. The tangent bound
$\log\Phi(-k)\leq-\log2-\sqrt{2/\pi}\,k$ gives

$$
L\leq E_\mu+\max\{0,nK^2/2-j\sqrt{2/\pi}\,K\}.
$$

This retains one half-density factor per held endpoint observation. Signed
small-tail domination is available with a free location when $nK\leq1$, or
with held location at the minimum when $nK\leq j$.

For held $\mu>x_{\min}$ put $d=\mu-x_{\min}>0$. The leftmost density gives

$$
L\leq n\log\lambda-\log2-d^2\lambda^2/(2k^2).
$$

Optimizing lambda if free gives
$n\log K+(n/2)\log n-n\log d-n/2-\log2$, which tends to minus infinity.
With sigma held, $f\leq\lambda=k/\sigma$ gives $n\log(K/\sigma)$.
Thus held sigma removes the exponential escape.

Tail refinement evaluates feasible profiles whenever its current bound cannot
close. This matters with held location strictly below the minimum: for a fixed
positive rate all CDF arguments tend to positive infinity, so
$L(k)=E_\mu+nk^2/2+o(k^2)$. The face is approached from above. A finite witness
can supply the improvement needed to exclude a smaller tail. The regression
`x={.01}, mu=0, lambda=1, iterlim=10` reaches the global subdivision budget
with a proper finite witness strictly above its exponential face, rather than
stopping at the initial shape-one comparison.

These inequalities also prove finite existence when mu is held strictly below
all observations and scale is free. With held rate, the opposite tail goes to
minus infinity. With rate free, the optimized exponential face exceeds the
conditional normal supremum by
$n\{\log(\operatorname{RMS}/\operatorname{mean})+(\log(2\pi)-1)/2\}>0$
for the positive residuals. A small positive shape beats both compatible
faces; compact nuisance coercivity then supplies a finite maximum.

**Uniform large-shape tail.** For $k\geq1$, the convolution integral gives

$$
f_k(z)\leq C e^{-|z|/2},\qquad C=2e^{1/8}/\sqrt{2\pi}.
$$

Indeed, bound the Gaussian kernel by
$e^{1/8}e^{-|z|/2}/\sqrt{2\pi}$ and integrate using
$k/(k-1/2)\leq2$. Hence

$$
L\leq n\log b+n\log C-bD_0/2,
$$

where $D_0=x_{\max}-x_{\min}$ for free location, and
$D_0=\sum_i|x_i-\mu|$ for held location. On a superlevel set $L\geq J$,
doubling past the envelope's maximum bounds b above by B. Held sigma fixes
$B=1/\sigma$. At an optimizing free location the mode bound gives
$z_i\leq Z=B(x_{\max}-x_{\min})+2/K$ for $k\geq K$; with held mu use
$Z=B\max(0,x_{\max}-\mu)$.

For $K>Z$, $M(t)=\Phi(-t)/\phi(t)<1/t$ implies
$f_k(z)\leq\phi(z)/(1-Z/K)$. With N the conditional normal supremum,

$$
\sup_{k\geq K}P(k)\leq\max\{J,N-n\log(1-Z/K)\}.
$$

This is uniform over optimized nuisance coordinates and retains the actual
mu/sigma holds. Normal supremum bounds include the rounding allowance for the
computed mean. A subnormal optimized RMS is kept as a proper witness but is
not used as a finite supremum upper bound.

With rate held, the Gaussian peak bound directly gives
$L\leq n\{\log\lambda-\log K-\tfrac12\log(2\pi)\}\to-\infty$.
Thus held rate removes the normal escape.

The generic normal bound approaches N from above. When no strictly better
finite witness excludes N, the fitter stops unresolved. A supplementary signed
proof is available with held mu, power-of-two held sigma, and exactly standardized integer data.
For $t>0$,

$$
M(t)<\frac{t^4+9t^2+8}{t^5+10t^3+15t}.
$$

Substitute $t=k-z_i$, $q=1/k$, and compare the product of the rational upper
bounds for $kM(k-z_i)$ with 1. Polynomial coefficients are computed exactly as
binary64 integers, with every intermediate strictly below $2^{52}$. The first
nonzero coefficient of denominator minus numerator must be positive; the sum
of all adverse higher coefficients over $0<q\leq1/K$ must be smaller. Failure
of exact representability or of this comparison supplies no domination claim.
A successful signed tail proof, together with exclusion of all compact
intervals, proves the normal limit. This preserves the normal-face regression
for `-2:2, mu=0, sigma=1` through a global proof.

**Finite coefficient certificate and global execution.** The controller keeps
profile upper bounds in a priority queue. It subdivides the remaining compact
shape interval at geometric midpoints and verified winner-region edges.
Intervals leave the queue through an upper bound below the incumbent or
containment in a validated winner region. Small-tail enlargement uses the same
proof before subdivision, avoiding unnecessarily tiny starting endpoints.

A proposed full score root uses scaled coordinates and a numerical
preconditioner C. If the interval Hessian is negative definite throughout a
box, the map $F(y)=y-Cg(y)$ has derivative bounded by
$q=\|I-CH\|_\infty<1/2$, and
$d+qR<R$, its unique score root lies in that box. Its distance from the center
is at most $d/(1-q)$. Back-transformation encloses every physical free
coefficient. Acceptance checks the requested relative tolerance against every
point in each enclosure; location uses the stated normal-scale floor.

A uniform nuisance inclusion is also required throughout a shape interval.
Its stationary nuisance solution is the global conditional solution by the
fixed-shape concavity theorem. The negative-definite full Hessian then gives
a strictly concave profile through its Schur complement. The enclosed full
score root is the unique maximizer of that region. Overlapping certified
regions describe the same root and are merged. Separated surviving regions
stop as comparison ties. A single surviving root whose coefficient enclosure
misses the requested tolerance stops numerically.

The likelihood bounds establish global coverage; the score-root enclosure
supplies coefficient accuracy. Both tails, all remaining intervals and all
compatible face comparisons must pass before a finite estimate is returned.

**Floating-point scope and work.** Certification follows the package standard:
certified in exact arithmetic conditional on the computed comparisons, with
stated practical floating-point margins. The implementation uses binary64
interval endpoints with 32-epsilon operation padding; Mills/score evaluation
uses 64-epsilon operand allowances, including log-to-exp amplification.
Location signs use 128 epsilons and consecutive positive-tail Mills
convergents. Likelihood bounds use 256 epsilons on separate normalization and
log-density operands and compensated reductions.

The natural-coordinate kernel additionally propagates standardized-input
uncertainty. If $E_z$ bounds the error in $bx-u$ or $d/k$, the score enclosure
bounds its likelihood effect by $(|q|+E_q)E_z$, since
$d\log f/dz=-q$ and $|dq/dz|<1$. This prevents cancellation at a large location
origin from shrinking the reported likelihood error. The independent
[80-digit reference](2026-11-07-emg-cert-data/input-margin.json) and C++
regression cover `b=.7` (its exact binary64 value), `u=7e9`, and observations
near `1e10`. Unrepresentable witness bounds, including overflow of a finite
likelihood's error endpoints, stop numerically. These are practical enclosures;
outward-rounded interval arithmetic is not claimed.

The global cap is `128 * min(iterlim, 100000)` certificate evaluations.
Profile points, segment bounds/refinements, proposer attempts, tail attempts
and the signed polynomial work are metered. Cached profile points are reused.
Nested solves obey `min(iterlim, 100000)` solver iterations as specified in the
[work-budget design](../../design/work-budgets.md). Exhaustion returns budget
with finite/face witnesses and conservative bounds, preserving an unresolved
parent interval when necessary. No capped estimate is returned.

**Proved non-existence and non-uniqueness scope.** If held mu is at or above
all observations, every delayed Gaussian kernel is smaller than its
zero-delay value. The compatible normal face dominates globally, also with
free sigma. Constant data with held sigma and free location/rate use the
normal peak bound. Constant data with held rate and free scale use the
exponential peak bound, including half the rate when location is held exactly
at the observation. Constant-data unboundedness uses exact data equality;
underflowed or rounded sample variance cannot manufacture that status.

A global uniqueness theorem for the free shape cannot be inferred from the
inner theorem. With held `mu=0, sigma=1` and observations `{-10,9}`, the finite
rate-one likelihood exceeds the normal face. Analytically,

$$
L(1)-N>\log(11/122)+32+\tfrac12\log(2\pi)-\log2>28,
$$

using the Mills lower bound at 11 and $\Phi(8)>1/2$. Yet as rate goes to
infinity, $L-N=(-10+9)/\lambda+O(\lambda^{-2})<0$. Thus the profile has a
finite peak and a normal-boundary local maximum, with a dip between them.
An outer unimodality assumption would be false.

There is also an exact finite/normal tie in the compatible closure. Replace 9
by $v\in[-10,9]$. At $v=-10$, pointwise domination gives strict normal victory
over every finite rate. Uniformly on this v interval, the large-rate leading
term is $(-10+v)/\lambda\leq-1/\lambda$; the remainder is uniformly quadratic
in inverse rate. This follows by twice differentiating
$E\exp(z\delta Y-\delta^2Y^2/2)$ at $\delta=0$, with $Y$ unit exponential
and bounded z. The small-rate bound $L\leq2\log\lambda$ is also uniform.
Consequently all finite competitors capable of reaching N lie in one compact rate interval.
Its continuous maximum relative to N is negative at $v=-10$ and positive at
$v=9$, so an intermediate v has a finite maximizer tied globally with the
normal face.

An independent 70-digit stationary equality occurs near
`v=2.746841848406631`, `lambda=1.070870015754127`, with negative curvature;
[the numerical record](2026-11-07-emg-cert-data/face-tie.json) identifies its
scope. The continuity proof establishes existence of a global closure tie.
It does not locate that global transition rigorously at the rounded decimal
coordinates. The counterexample concerns the closure; general finite-interior
uniqueness remains open. The executor requires data-specific uniqueness of a
surviving region before returning its estimate.

**Golden accuracy and numerical evidence.** All eight golden masks return Ok
under both default controls and `tol=1e-10, maxit=500`. The maximum coefficient
drift from frozen golden values is 3.21571e-08 absolute and 9.86074e-08 relative;
the maximum likelihood drift is 1.02318e-12. This is within the existing parameter
and likelihood tolerances. The new all-free strict-control estimate is
`mu=0.17576155852910086, sigma=1.0518147256348569, lambda=1.2091859030311058`, with
`logLik=-846.75971897546663`. Goldens were not edited.

[Independent 70-digit checks](2026-11-07-emg-cert-data/outer-oracle.py) solve the
physical scores using erfc and check every returned free coefficient against
the validated enclosure. All seven golden subsets pass in native and
no-long-double modes. Maximum normalized coefficient errors are 4.38e-11
and 4.38e-11, respectively, below `1e-10`. The companion
[oracle](2026-11-07-emg-cert-data/oracle.py) checks nine signed location brackets
and 24 conditional fixed-shape likelihood/coordinate enclosures in each mode.
The numerical kernel was also checked on simbox in both arithmetic modes.
Global optimality is supplied by the analytic bounds and executed certificate;
these independent roots validate numerical coordinates.

**Paired timings.** The following are medians of three alternating before/after
runs on the workstation, GCC 15.2.0, `-O2`, one reserved CPU slot, `nice -n 10`.
Both binaries use the same frozen n=500 sample and holds from its golden
coefficients. The baseline is the original c118a003 EMG implementation.
Timings exclude builds and slot waits. Shared-machine load makes them practical
comparisons rather than fixed runtime guarantees.

Default controls, `tol=1e-8, maxit=500`:

| Free coefficients | Before (s) | Certified (s) | Ratio |
|---|---:|---:|---:|
| mu | 0.000627 | 0.002263 | 3.61x |
| sigma | 0.015142 | 1.034282 | 68.30x |
| lambda | 0.012193 | 0.074272 | 6.09x |
| mu, sigma | 0.149754 | 5.424618 | 36.22x |
| mu, lambda | 0.119854 | 0.295957 | 2.47x |
| sigma, lambda | 0.224629 | 0.395283 | 1.76x |
| mu, sigma, lambda | 0.343653 | 0.768714 | 2.24x |
| none (evaluation) | 0.000072 | 0.000053 | 0.73x |

Strict controls, `tol=1e-10, maxit=500`:

| Free coefficients | Before (s) | Certified (s) | Ratio |
|---|---:|---:|---:|
| mu | 0.000583 | 0.002721 | 4.67x |
| sigma | 0.019060 | 1.114617 | 58.48x |
| lambda | 0.015299 | 0.083570 | 5.46x |
| mu, sigma | 0.186593 | 5.654944 | 30.31x |
| mu, lambda | 0.149065 | 0.305574 | 2.05x |
| sigma, lambda | 0.275178 | 0.391140 | 1.42x |
| mu, sigma, lambda | 0.378807 | 0.789886 | 2.09x |
| none (evaluation) | 0.000064 | 0.000053 | 0.83x |

The default all-free fit is 2.24x slower. Held-rate certification is much more
expensive: free sigma is 68.30x slower and free mu/sigma is 36.22x slower under
default controls. The fixed-rate curvature bound is conservative and requires
many more conditional profiles. The proof remains enabled on these paths.
Raw samples and paired summaries are
[retained with the evidence](2026-11-07-emg-cert-data/timing-summary-default.json).

**Final verification.** Every command below completed green on the final
fitter. Simbox standalone C++ uses GCC 13.3.0; its R package uses GCC 15.2.0.
The workstation C++ run uses GCC 15.2.0. All long runs used the requested
reservation/mirroring helpers, with BLAS/OpenMP threads pinned and low priority.

| Check | Machine | Final result |
|---|---|---|
| `just cpp-test` | simbox | 106/106 dev shards |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 129/129 full shards |
| `just cpp-test-no-long-double` | simbox | 129/129 |
| `just cpp-test-no-long-double` | workstation | 129/129 |
| Targeted EMG + shared inner test, both arithmetic modes | simbox | 31 cases, 4,305 assertions per mode |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21,465 |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44,283 |
| `just js-build` / `just js-test` | workstation | build succeeded; 485/485 tests |
| `just site-check` | workstation | 99 family pages with MathML; 99 C++ snippets compile/link |
| `just quality emg` | workstation | pass |
| Final paired benchmark and independent enclosures | workstation | all masks Ok; native/double oracle checks pass |

The R full tier includes the independent optimizer comparison for all eight
held masks. C++ regressions cover budget witnesses, unresolved normal faces,
unrepresentable coefficient tolerances, overflowing face-witness endpoints,
large-origin likelihood cancellation, and finite witnesses above an
exponential face. R/JS regressions check the common structured stop contract.
The site-generated unrelated screenshot was restored to its original bytes.

Durable summaries are
[workstation runs](2026-11-07-emg-cert-data/validation-runs.txt),
[simbox runs](2026-11-07-emg-cert-data/machines/simbox/validation-runs.txt), and
[source hashes / numerical checks](2026-11-07-emg-cert-data/validation.json).
The reproduction driver is
[production-evidence.sh](2026-11-07-emg-cert-data/production-evidence.sh);
invoke it under one CPU reservation, or through the supplied simbox wrapper.
Its baseline header snapshot is retained under `original-emg/` and can also be
recovered read-only from the stated base commit.

**Open mathematics and performance.** Generic signed normal-tail domination
with arbitrary observations and free nuisance coordinates remains unavailable
when no finite witness beats the normal face. Those inputs stop unresolved
with their witnesses. General finite-interior uniqueness is unproved;
separated surviving regions stop as comparison ties. Flat or ill-conditioned
root/curvature enclosures and unrepresentable arithmetic stop numerically.
Tighter held-rate interval bounds could substantially reduce the measured
certification overhead. These limitations leave no public route that returns
an unchecked finite estimate.

**Open artifact display.** The report and evidence are written at the requested
paths. The artifact viewer returned HTTP 403 because this worktree is absent
from its readable roots. Pane display requires external viewer-root
registration (`CODEX_MATH_WEB_VIEW_ROOTS` or a registered thread in this
worktree). This does not affect the completed numerical validation.
