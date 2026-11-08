# SGED free-shape certificates — TASK-116.44

O1 is implemented: asymmetric held skew with shape, mean and sd free now uses a global weighted shape certificate. O4 with free moments is implemented: every fixed positive shape with skew, mean and sd free is now covered, combining the new smooth-mode certificate with the previous normal and subunit certificates. Both are wired into the public C++ fitter and therefore the common R/JS binding. Neither helper is an unused experimental implementation.

The earlier exact symmetric delegation, balanced two-point theorem and uniform-in-skew large-shape bound are retained. The family flag remains `partial`: held-moment asymmetric free-shape fits, nonnormal free-skew fits with held moments, and generic joint estimation of both shapes remain open. Their precise missing bounds are recorded below. The default all-free fit and its frozen golden bytes are unchanged.

Worktree: sged-free; branch: codex/sged-free; base: `cfb7c4b37d13d1f23ca8ceb546333eb87290b1f6`. No commits, board edits, installs or changes to another worktree were made.

## Delivered scopes and routing

| Free coefficients | Held values | Certificate |
|---|---|---|
| nu, with either/both/neither moment free | xi exactly 1 | Existing GED certificate, transported in identical public coordinates |
| mean, sd, nu | asymmetric positive xi | New weighted shape certificate, O1 closed |
| mean, sd, xi | every fixed nu > 1 other than 2 | New exhaustive mode/skew certificate, O4 free moments closed |
| mean, sd, xi | fixed 0 < nu <= 1 | Existing knot certificate |
| xi, with either/both/neither moment free | nu = 2 | Existing skew-normal delegation |
| mean, sd, nu, with xi held or free | balanced two-point data | Analytic uniform supremum |
| Other asymmetric free-nu or nonnormal held-moment free-xi subsets | as detailed below | Retained uncertified numerical searches |

The [public fitter](../../../univariateML-cpp/include/cppunivariate/distributions/sged/mle.hpp) dispatches to [weighted_shape_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/sged/weighted_shape_certificate.hpp) and [mode_skew_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/sged/mode_skew_certificate.hpp). They use SGED-local [profile_bounds.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/sged/profile_bounds.hpp). The earlier routes live in [free_shape_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/sged/free_shape_certificate.hpp). The [common binding](../../../bindings/common/distributions/sged/fit.hpp) propagates structured numerical, budget, tie and limit stops. The [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) names these scopes in `fit.note` and retains `fit.certified: partial`.

## O1: weighted shape proof and implementation

Write $p=\nu$, $a=h/\xi$, $b=h\xi$, $C=\xi+\xi^{-1}$ and

$$
r_i(c)=\begin{cases}\xi(c-x_i),&x_i<c,\\(x_i-c)/\xi,&x_i\ge c.\end{cases}
\qquad T=\sum_i r_i^p.
$$

For $p>1$ the mode minimizes a strictly convex weighted power sum. Its score is monotone in $c$. On every shape interval the implementation encloses that score, brackets all possible modes, and contracts using a lower mode-Hessian bound when available. It does not assume that the optimizing mode is monotone in shape. Residuals and side weights are rescaled before taking powers, including point scores at large shape; this prevents a numerically zero score from residual-power underflow. At $p=1$ it retains the entire weighted median interval, with quantile $1/(1+\xi^2)$.

Profile scale by $h^p=pT/n$. Put $U=\sum r_i^p\log r_i$, $V=\sum r_i^p\log^2r_i$. With side weight $w_i=\xi$ on the left and $\xi^{-1}$ on the right, define

$$
G=\sum_i\operatorname{sign}(c-x_i)w_i r_i^{p-1}\log r_i,
\qquad H=\sum_iw_i^2r_i^{p-2}.
$$

The profiled shape score and its derivative are

$$
A(p)=\frac{p^2}{n}\ell'(p)
=\psi(1+1/p)+\log p-pU/T+\log(T/n),
$$

$$
A'(p)=\frac1p-\frac{\psi_1(1+1/p)}{p^2}
-p\left(\frac VT-\frac{U^2}{T^2}\right)
+\frac{p^2G^2}{(p-1)TH}.
$$

These follow by implicit differentiation of the monotone mode equation. The final positive term is essential; dropping it would certify the wrong curvature. SGED-local interval bounds cover the weighted moments, side changes, and singular Hessians at knots. Power-log extrema inside residual intervals are included. Negative **finite** upper curvature enables exhaustive root contraction; otherwise intervals are subdivided or excluded by score/likelihood bounds. Infinite arithmetic never authorizes contraction.

The minimized weighted $L_p$ norm is nondecreasing in $p$. Also
$h_0(p)=-\log\Gamma(1+1/p)-(1+\log p)/p$ increases: its derivative has the sign of $\psi(1+1/p)+\log p>0$, obtained from Jensen's inequality applied to the reciprocal of a gamma variable. Combining the lower-end norm with the upper-end $h_0$ bounds the likelihood throughout each shape interval. Thus pruning does not depend on sampled score signs.

### Uniform large-shape tail

Let $R=x_{\max}-x_{\min}>0$. For every mode and skew at least one extreme residual is at least $R/C$, so $T\ge(R/C)^p$. Since $\log\Gamma(1+t)\ge-\gamma t$,

$$
\frac\ell n=-\log\Gamma(1+1/p)-\log C-
\frac{\log p+\log(T/n)+1}{p}
\le-\log R-\frac{\log p+1-\gamma-\log n}{p}.
$$

The skew factor cancels, even along paths with skew changing with shape. For $p\ge\max(2,n)$ the correction is strictly negative. The reachable uniform law has height $-n\log R$. The certificate therefore covers $[1,\max(2,n)]$ exhaustively and excludes the entire remaining tail analytically; the upper endpoint is not an arbitrary shape cap.

All retained stationary regions, the full shape-one face and the uniform face are compared globally. Regions touching shape one remain competitors. A winning nontrivial weighted median interval returns a tie with its endpoints; it is never silently replaced by a representative.

### Public coordinates and nonuniqueness

With $k=\Gamma(2/p)/\Gamma(1/p)$ and $v=\Gamma(3/p)/\Gamma(1/p)$,

$$
\mu=c+k(b-a),\qquad
\sigma^2=(v-k^2)(b-a)^2+vab,\qquad
\xi=\sqrt{b/a}.
$$

The gamma ratios are **not monotone** in shape. Bounds use a digamma derivative enclosure in $t=1/p$ instead of endpoint-only ratio evaluations. Every candidate region is propagated through this full map. Original-coordinate mean widths must be at most $2\times10^{-7}(1+|\mu|)$; log-sd and log-skew widths at most $2\times10^{-7}$; shape widths at most $2\times10^{-7}(1+p)$. Stored representatives are refined further and their union with the full public enclosure must still satisfy these widths. This last check prevents accepting a narrow interval around one root while returning a stale representative from another.

Recovery checks normalization collisions, cancellation in the original mean, round-trip coordinates and the likelihood evaluated by the actual public SGED objective. Held skew is restored exactly. The enclosure thresholds are the existing GED golden-accuracy acceptance scales; they are not a universal $10^{-10}$ error theorem. The independently measured coefficient errors below are substantially smaller.

Uniqueness fails: at held $\xi=2$, data consisting of $-100$, three copies of $-.01$, fifteen copies of $.01$, and $100$ have an exactly flat weighted Laplace median interval $[-.01,.01]$. Its weighted slope is zero. The global comparison certificate selects that shape-one face and returns its distinct public-coordinate endpoints as a tie. This is covered in C++, R and JS.

## O4 with free moments: exhaustive mode/skew certificate

Fix $p>1$, $p\ne2$. For a mode $c$ put

$$
A=\sum_{x_i<c}(c-x_i)^p,\quad B=\sum_{x_i>c}(x_i-c)^p,
\quad r=\frac1{p+1},\quad S=A^r+B^r.
$$

Profiling the two side scales gives

$$
a=tA^r,\quad b=tB^r,\quad t=(pS/n)^{1/p},
$$

$$
\frac\ell n=\log p-\log\Gamma(1/p)
-\frac{\log(p/n)+1}{p}-(1+1/p)\log S.
$$

Every optimizing mode lies in the sample range: moving a mode outside the range inward reduces all residuals at fixed side scales. Endpoints represent the two translated half-GED faces. For interior modes define $C_L=\sum_{x<c}(c-x)^{p-1}$, $C_R=\sum_{x>c}(x-c)^{p-1}$, and corresponding Hessian sums $H_L,H_R$ with exponent $p-2$. With $f=p/(p+1)$,

$$
F=\frac{C_L}{A^f}-\frac{C_R}{B^f},\qquad S'=rpF,
$$

$$
F'=(p-1)\left(\frac{H_L}{A^f}+\frac{H_R}{B^f}\right)
-fp\left(\frac{C_L^2}{A^{f+1}}+\frac{C_R^2}{B^{f+1}}\right).
$$

Monotone side sums enclose $F$ and $F'$ on every mode interval. Positive finite lower curvature contracts all possible minima; other intervals are subdivided or excluded by score and likelihood bounds. For $c\in[l,h]$,
$S(c)\ge A(l)^r+B(h)^r$, supplying the likelihood upper bound. Every distinct observation knot is checked; a definite nonzero derivative excludes an interior knot as a maximum, while stationary or unresolved knots remain candidates. No uniqueness assumption replaces exhaustive comparison.

The apparent endpoint $0/0$ is resolved analytically. Before the first distinct knot, a left endpoint of multiplicity $m$ contributes exactly $m^r(c-x_{\min})^{-r}$ to $F$; reflection closes the right endpoint. Consequently both skew tails are covered without a finite skew cap. Both half-GED witnesses verify support and their actual likelihood in the stored original coordinates.

Positive side sums use relative roundoff margins plus a subnormal allowance. A positive side whose powers underflow to zero triggers numerical failure rather than being relabeled a half-GED face. Nonfinite face arithmetic is checked before knot enumeration. Candidate mode regions are merged only when the public box of their **union** is narrow; selecting a midpoint between distinct stationary roots cannot pass this check. Final coefficients obey the same original-coordinate recovery and enclosure checks as O1.

A counterexample to uniqueness is $x=(-1,1)$: $S=(c+1)^{p/(p+1)}+(1-c)^{p/(p+1)}$ is strictly concave on $[-1,1]$, so its two endpoint minima tie. The certificate returns the two reflected half-GED witnesses, including for the requested smooth shapes. This equality is exact; numerical comparisons use the practical margins below.

## Arithmetic, budgets and failure handling

The certificates are certified in exact arithmetic conditional on the computed comparisons, with practical floating-point allowances; they do not claim outward-rounded arithmetic. Compensated sums are used. The main allowance is $4096\epsilon(1+\text{magnitude})(1+n\epsilon)$, enlarged by $1+p_{\max}$ where shape conditioning enters. Likelihood transfers include original normalization/Jacobian magnitudes. Positive powers in the mode/skew profile instead receive relative allowances plus $n$ times the smallest subnormal.

Certificate work is metered by $128\min(\mathrm{iterlim},100000)$; inner solver iterations by $\min(\mathrm{iterlim},100000)$. The initial evaluation is charged. An unfundable number of distinct knots is rejected before quadratic enumeration, with an evaluated linear-cost face witness. Tests cover small budgets, 100,000 distinct observations, singular curvature, high shapes, power underflow, invalid face recovery and stale representatives. An unresolved comparison, failed coefficient enclosure, arithmetic failure or exhausted budget returns a structured stop, never an unchecked `Ok` or a capped shape/skew estimate. Budget/numerical stops preserve witnesses and an unresolved global upper bound.

### Exact symmetric transport

At held $\xi=1$, the Fernandez–Steel moment shift is zero and its
variance factor is one for every $\nu$. The public mean and sd therefore
coincide with GED's, including when either is held. The likelihoods and
their constrained parameter spaces are identical, with the same free-shape
restriction $\nu\ge1$. Uniform endpoints and likelihood bounds require
no Jacobian or coordinate conversion.

The new route calls the public GED certificate with the caller's controls.
It accepts a finite answer only when the delegate reports both success
and certification. A selected uniform face also requires the delegate's
certification flag. Otherwise it retains a numerical, comparison, or
budget stop; it never falls back to the SGED finite scan. Exact constant-data
collapse with free sd is mapped to unbounded likelihood only after checking
that every observation equals the free or held location.

Consequently the proofs, coefficient enclosures, comparison allowances,
and work caps of the
[GED certificate](2026-11-07-ged-free-report.md) transfer unchanged.
In particular its finite root boxes have the stated $2\times10^{-7}$
coefficient-width acceptance scales, with representatives refined further.
This report does not replace those enclosures by a universal
$10^{-10}$ guarantee. The independent representative errors measured below
are numerical evidence about the tested samples.

There is no general uniqueness theorem. For $x=(-1,1)$, held sd $0.1$,
and held skew one, GED's small-scale theorem makes the entire Laplace
median interval globally maximizing. The SGED route now returns a tie
with both endpoints, each carrying mean, sd, nu, and xi. On the small timing
sample, the formerly selected single Laplace representative likewise becomes
a tie in the two free-location rows.

The binary-rounded observations $(-\sqrt3,\sqrt3)$ with sd one held do
not establish an exact uniform endpoint contact. Delegation correctly
retains GED's numerical stop instead of selecting the formerly penalized
uniform face. Three obsolete expectations in the SGED family C++ test
were corrected; the new C++ cases remain appended. The corresponding R
error expectations were corrected while preserving the finite-shape
penalty check. The original prefix of the shared JS test file is unchanged.

### Balanced two-point uniform supremum

Let the sample contain the same positive number of copies of $l<h$.
Every finite SGED in the free-shape fitting domain is positive and
log-concave. Concavity of $\log f$ gives

$$
1>\int_l^h f(x)\,dx
\ge (h-l)\int_0^1 f(l)^{1-t}f(h)^t\,dt
\ge (h-l)\sqrt{f(l)f(h)}.
$$

The first inequality is strict because a finite SGED has positive density
outside the interval. The last follows from convexity of the exponential
and averaging the linear interpolation of the endpoint log densities.
Equal counts therefore imply

$$
\ell<-n\log(h-l).
$$

This bound is reachable with every fixed $\xi>0$. Choose
$c\in(l,h)$ with $(h-c)/(c-l)=\xi^2$ and the kernel side scales

$$
a=(c-l)(1+\delta),\qquad b=(h-c)(1+\delta),\qquad
\delta=\frac{\log\nu}{\nu}.
$$

Then $b/a=\xi^2$, both endpoint penalties are $(1+\delta)^{-\nu}\to0$,
and the normalizer
$\nu/[\Gamma(1/\nu)(a+b)]$ tends to $1/(h-l)$.
Both public moments are free, so this path obeys all holds.
Free skew may choose $\xi=1$. Thus there is no finite MLE and the
uniform law supplies the global supremum. Different latent joins describe
the same limiting law, not different winning distributions.

The implementation checks the two distinct original observations and their
counts without normalization or sorting. It computes the log width through
the half-width to avoid overflow and uses the practical likelihood margin
$4096\epsilon(1+n+|\ell|)$. A subnormal half-width or nonfinite
likelihood produces a numerical stop with an infinite unresolved upper
bound. Tests cover repeated endpoints, highly asymmetric skew, an
overflowing full width, and subnormal width. Reductions cost $O(n)$;
storage beyond the fixed diagnostic payload is constant.

This is an exact-arithmetic certificate conditional on the computed
comparisons, with the stated practical floating-point margin. It is not
an outward-rounded interval claim. The symmetric route inherits the same
conditional-comparison standard and margins from GED. Its solver and
certificate caps remain respectively $\min(\mathrm{iterlim},100000)$
and $128\min(\mathrm{iterlim},100000)$; transport adds no search loop.

## Held-moment reductions and remaining open entries

These entries identify the remaining work; the implemented O1 and O4 free-moment routes above are closed.

Write $s=a+b$, $z=(b-a)/s$, $D=3v-4k^2$. The actual moment constraints are

$$
\mu=c+kzs,\qquad \sigma^2=\frac{s^2}{4}(v+Dz^2),
\qquad \xi=\sqrt{\frac{1+z}{1-z}}.
$$

Thus held sd fixes $s=2\sigma/\sqrt{v+Dz^2}$, and held mean fixes $c=\mu-kzs$. A held mean cannot be treated as a held mode; a held sd cannot be treated as the unconstrained profiled scale.

**O2 — Free nu, asymmetric held xi, with mean and/or sd held (three masks).** The pointwise held-shape certificates remain available, but the outer shape search still needs interval score/curvature and public recovery bounds for these constrained profiles. With sd held and mean free, the kernel scale depends on gamma ratios, so the O1 scale equation is invalid; with mean held, mode and scale are coupled. Missing also are uniform shape-tail comparisons that retain the held moments, including infeasible, supported and exact-contact endpoints. O1's free-moment tail bound does not justify replacing these constrained faces by its reachable uniform height. Exact xi=1 is already closed by delegation.

**O4 held moments — Fixed nu != 2, free xi, with mean and/or sd held (three masks, including 0 < nu <= 1).** The unconstrained two-side-scale minimizer cannot be used. Global constrained skew-profile bounds in $z$, reachable endpoint penalties at both skew faces, and scale-tail bounds where sd is free remain missing. The shape-two delegation covers all moment masks; the subunit knot certificate and the new smooth certificate cover both moments free. No other held-moment scope was promoted to certified.

**O3 — Nu and xi jointly free, all four moment masks.** With free moments, the uniform-in-skew tail proof closes the large-shape direction. The new fixed-shape mode certificate is pointwise in shape; it does not provide an interval two-dimensional critical-point cover. A finite-shape/mode global cover with coefficient enclosures and a global half-GED shape profiler are still missing. With held moments, constrained half-GED profilers and joint shape/skew corner envelopes must also obey the displayed moment equations. The following endpoint calculation shows why separate face scans cannot replace those envelopes. The unchanged default fit is explicitly among these uncertified searches; balanced two-point free-moment data are the proved exception.

### Held moments: joint endpoint penalties depend on the path

Take exact held mean zero and sd one, and observations at the exact
limiting uniform endpoints $\pm\sqrt3$. Put
$z=1-\lambda/\nu$ for fixed $\lambda>0$.
From the convergent log-gamma Taylor series,

$$
\begin{aligned}
k&=\tfrac12[1-\gamma/\nu+
(\gamma^2/2+3\zeta(2)/2)/\nu^2+O(\nu^{-3})],\\
v&=\tfrac13[1-2\gamma/\nu+
(2\gamma^2+4\zeta(2))/\nu^2+O(\nu^{-3})],\\
D&=\zeta(2)/\nu^2+O(\nu^{-3}).
\end{aligned}
$$

Using the exact held-moment $s$ and $c$ above, and
$a=s(1-z)/2$, $b=s(1+z)/2$, yields

$$
\nu\log\frac{c+\sqrt3}{a}\longrightarrow
-\gamma+\frac{2\zeta(2)}{\lambda},\qquad
\nu\log\frac{\sqrt3-c}{b}\longrightarrow-\gamma.
$$

These are logarithms of the kernel penalties. The left penalty itself
tends to $\exp[-\gamma+2\zeta(2)/\lambda]$, rather than
$e^{-\gamma}$. This path has
$\xi\sim\sqrt{2\nu/\lambda}$. Taking skew to infinity first at
fixed large shape instead puts the left contact outside the half-GED
support: its endpoint is
$c=-k/\sqrt{v-k^2}=-\sqrt3+2\sqrt3\zeta(2)/\nu^2+O(\nu^{-3})$.
The separate one-parameter limits therefore cannot substitute for a
uniform constrained-corner argument.

The checker uses exact high-precision $\sqrt3$ for this mathematical
example. It does not treat the binary-rounded C++ fixture as exact contact.
It verifies the predicted limits at shapes $10^3,10^5,10^7$ and
$\lambda=1,10,100$.

## Independent evidence and timings

The new [profile oracle](2026-11-08-sged-free-data/profile-oracle.py), [C++ probe](2026-11-08-sged-free-data/profile-oracle.cpp) and [runner](2026-11-08-sged-free-data/run-profile-oracle.sh) use 70-digit arithmetic, independent monotone mode solves, raw moment/score derivatives and the exact binary input sample. There were **zero violations in 495 interval checks**, including nested score, curvature and coefficient boxes. Independently solved golden roots give:

| Certificate | Held value | Max absolute coefficient error |
|---|---:|---:|
| O1, xi | 0.3 | 1.862e-13 |
| O1, xi | 0.7 | 1.625e-15 |
| O1, xi | 1.4 | 1.813e-11 |
| O1, xi | 3 | 2.946e-15 |
| O4, nu | 1.15 | 4.066e-14 |
| O4, nu | 1.7 | 5.959e-11 |
| O4, nu | 2.5 | 1.156e-10 |
| O4, nu | 4 | 1.182e-10 |
| O4, nu | 8 | 5.272e-11 |

The largest new error is $1.182\times10^{-10}$; likelihood discrepancies are below $6\times10^{-13}$. All nine new oracle fits returned certified finite maxima. In particular the held-xi 0.3 solution has shape about 53.6594, exercising a root far beyond the ordinary fitted shape. [Full roots and errors](2026-11-08-sged-free-data/profile-oracle.json) and [interval inputs](2026-11-08-sged-free-data/profile-boxes.txt) are retained. These comparisons are numerical evidence, separate from the global exclusion proofs.

The earlier [independent checker](2026-11-08-sged-free-data/check_math.py) also passed: four symmetric root comparisons (largest coefficient error $3.602\times10^{-10}$), 120 moment configurations/720 densities, 28 uniform tail checks including xi equal to shape and shape squared, and nine joint-corner checks using exact high-precision $\sqrt3$. [Results](2026-11-08-sged-free-data/math-checks.json).

Times below are workstation GCC 15.2 medians of three sequential fits at C++17/O2, unchanged controls gradtol $1.220703\times10^{-4}$ and iterlim 200. Held mean/sd are 0.2/1.4. Small data are $(-3,-1,-.5,0,.1,.2,.4,1,2,4)$; golden data are the frozen 500 observations. Before means the initial base, before this job's symmetric delegation as well as the new profiles. Both stages ran under shared load, so differences on unchanged routes are not causal speedup estimates. Each cell is before → after milliseconds.

| nu | xi | Held moments | n=10 ms | n=500 ms |
|---|---|---|---:|---:|
| free | 1 | none | 2.185 → 3.513 | 20.982 → 439.578 |
| free | 1 | mean | 1.269 → 0.308 | 10.748 → 65.095 |
| free | 1 | sd | 0.962 → 4.229 | 12.115 → 462.997 |
| free | 1 | mean, sd | 0.284 → 0.978 | 3.000 → 59.582 |
| free | 1.4 | none | 4.991 → 4.417 | 36.544 → 1175.510 |
| free | 1.4 | mean | 6.152 → 3.174 | 10.516 → 9.933 |
| free | 1.4 | sd | 4.078 → 2.094 | 12.283 → 11.602 |
| free | 1.4 | mean, sd | 0.330 → 0.147 | 2.991 → 2.847 |
| free | free | none | 67.692 → 41.731 | 1179.028 → 1178.751 |
| free | free | mean | 36.164 → 11.597 | 396.105 → 385.848 |
| free | free | sd | 6.420 → 3.326 | 42.270 → 41.551 |
| free | free | mean, sd | 3.911 → 2.009 | 17.123 → 16.965 |
| 0.7 | free | none | 0.062 → 0.031 | 59.643 → 58.478 |
| 0.7 | free | mean | 123.460 → 61.692 | 757.868 → 745.386 |
| 0.7 | free | sd | 1.401 → 0.709 | 20.313 → 20.410 |
| 0.7 | free | mean, sd | 0.232 → 0.119 | 2.622 → 2.635 |
| 1 | free | none | 0.030 → 0.019 | 24.501 → 25.000 |
| 1 | free | mean | 40.549 → 21.885 | 587.932 → 582.765 |
| 1 | free | sd | 1.201 → 0.647 | 21.994 → 21.372 |
| 1 | free | mean, sd | 0.173 → 0.095 | 2.600 → 2.522 |
| 1.7 | free | none | 1.013 → 0.261 | 14.665 → 140.119 |
| 1.7 | free | mean | 24.271 → 12.288 | 409.358 → 395.728 |
| 1.7 | free | sd | 0.515 → 0.260 | 8.997 → 9.007 |
| 1.7 | free | mean, sd | 0.233 → 0.116 | 2.606 → 2.618 |
| 2 | free | none | 0.131 → 0.092 | 0.882 → 0.877 |
| 2 | free | mean | 11.385 → 10.263 | 489.777 → 486.831 |
| 2 | free | sd | 1.519 → 1.249 | 56.349 → 56.694 |
| 2 | free | mean, sd | 0.505 → 0.434 | 21.926 → 21.992 |
| 4 | free | none | 4.593 → 0.207 | 12.527 → 146.340 |
| 4 | free | mean | 30.277 → 15.430 | 379.416 → 382.437 |
| 4 | free | sd | 0.546 → 0.283 | 36.769 → 36.985 |
| 4 | free | mean, sd | 0.259 → 0.133 | 5.333 → 5.350 |

Balanced data $(-2,-2,3,3)$, both moments free:

| Held xi | Before ms | After ms |
|---|---:|---:|
| 1 | 2.950569 | 0.077129 |
| 1.4 | 3.940641 | 0.001048 |
| free | 25.536593 | 0.000720 |

[Benchmark source](2026-11-08-sged-free-data/benchmark.cpp), [runner](2026-11-08-sged-free-data/final-benchmark.sh) and [all repetitions](2026-11-08-sged-free-data/timings-raw.json) retain statuses, certification flags, coefficients and likelihoods. New routes now produce certified fits or correct structured stops; timings alone do not imply finite success. For example the small fixed-nu 4 free-moment fit selects a certified half-GED face. The collector verifies every unchanged route bit-for-bit against before, ignoring time, on all three samples. The ordinary default golden row and frozen `golden.json` bytes are unchanged. No golden or tolerance was changed.

## Final validation

All final commands finished on the frozen production source. [Validation JSON](2026-11-08-sged-free-data/validation.json) and [text](2026-11-08-sged-free-data/validation.txt) retain source hashes and machine results. The collector verifies that the current source is precisely what both final runs tested.

| Check | Machine | Result |
|---|---|---|
| C++ dev | simbox, GCC 13.3 | 106/106, 111.51 s |
| C++ full | simbox, GCC 13.3 | 129/129, 184.60 s |
| No-long-double full | simbox, GCC 13.3 | 129/129, 172.23 s |
| No-long-double full | workstation, GCC 15.2 | 129/129, 256.39 s |
| R dev | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21902; 82.2 s |
| R full | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44731; 306.1 s |
| JS build | workstation | Passed |
| JS full | workstation | 539/540; only the maintainer-identified stale binomial duplicate fails |
| Four appended SGED JS cases | workstation | 4/4 passed inside the full run |
| Site | workstation | Passed: 99 pages with MathML, 99 C++17 snippets, browser checks and links |

The full [simbox runner](2026-11-08-sged-free-data/final-simbox.sh) executes both C++ tiers, no-long-double and both R tiers. The [workstation runner](2026-11-08-sged-free-data/final-local.sh) executes no-long-double, JS build, JS tests and site. No simbox fallback was needed. SGED C++ and R regression cases cover the two new routes, comparisons/ties, budget stops and original-coordinate recovery; full R additionally verifies independent oracle likelihoods for structured tie witnesses.

The JS exception is exactly “binomial relatives retain unresolved finite and Poisson witnesses”. Per the maintainer's review it is a stale duplicate already removed on master and will disappear at merge. Its existing source is untouched; shared JS edits are append-only. [Final failure excerpt](2026-11-08-sged-free-data/js-upstream-duplicate.txt) is retained. This is an accepted upstream exception, not an outstanding approval request. The record accurately says `full_js_green: false` and all other checks green. No SGED test failed.

The site checker rewrote an unrelated PNG; its original bytes were restored. No GED, d/p/q/r, special-function, truth, code-generator, shared core, NEWS, status, known-issues or roadmap source changed. Existing family test expectations were corrected where symmetric delegation exposes exact ties or an unresolved rounded endpoint; new family cases are appended. No shared header was edited.

## Artifact delivery

The [local link checker](2026-11-08-sged-free-data/check-links.py) checks this report and the dossier; [result](2026-11-08-sged-free-data/links.txt) is retained. The artifact viewer was attempted; this worktree is outside its registered readable roots and the viewer returns HTTP 403. No viewer configuration or other checkout was changed. The Markdown report is available at the requested path.
