# Huber certificates — TASK-116.29

Completed on 2026-11-07 in `codex/huber-cert`, based on
`7c52f905f21bee7d997d59211d4093e2593ec727`. No commits were made.
All final builds and tests finished successfully before this report was written.
The tested, uncommitted `huber/mle.hpp` SHA-256 is
`d094916edf885084799cfe3a62d9ca654f0231551ed521a8b382b9c7f055174f`.
The [source manifest](2026-11-07-huber-cert-data/source-manifest.txt) identifies
all changed sources, the unchanged golden, and the validation drivers.

## Result and scope

Every supported held/free subset now returns a globally bounded finite fit or
stops with the appropriate statistical or computational diagnostic. The dossier
has `fit.certified: 'yes'` and a note describing that contract. Certification
means an exact-arithmetic likelihood enclosure conditional on the computed
comparisons, with practical floating-point padding. It does not mean an
outward-rounded interval or a theorem about uniform coefficient error.

The implementation retains the existing fitter as a proposal generator and
adds verification in [mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp).
The [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/huber/README.md)
contains the certificate, face conditions, tolerances and failure behavior.
Routing is in the binding-neutral
[Huber fit layer](../../../bindings/common/distributions/huber/fit.hpp): R and
JavaScript receive the same limit, tie, budget and numerical classifications.
Both family test files were extended only at their ends. No shared header,
inference implementation, d/p/q/r implementation, truth fixture, generator,
NEWS, status file or roadmap was changed.

| Held parameters | Free parameters | Certificate and eligible faces |
|---|---|---|
| epsilon | mu, sigma | Concave nuisance bound; detect constant-data scale collapse. |
| epsilon, mu | sigma | Concave scale-ray bound; collapse only if every observation equals held mu. |
| epsilon, sigma | mu | Concave location bound; maximizing intervals allowed. |
| epsilon, mu, sigma | none | Evaluate the specified point. |
| none | mu, sigma, epsilon | Profile subdivision; constrained normal and Laplace faces. |
| mu | sigma, epsilon | Same subdivision with held location respected on both faces. |
| sigma | mu, epsilon | Profile subdivision; normal face and a uniform small-shape bound. |
| mu, sigma | epsilon | Scalar subdivision; held normal face and the same small-shape bound. |

The default remains held `epsilon = 1.345`. Its coefficient and likelihood
golden checks pass at the existing tolerances; `huber/golden.json` is
byte-identical to the base revision. Free-shape fits can change when the
retained proposal fails a score check; the measured large-sample example below
documents one such change.

## Mathematical justification

Write the Huber loss as rho_c and its derivative as psi_c. For fixed c,

\[
 \ell(\eta,h;c)=n\log\eta-n\log Z(c)
                  -\sum_i\rho_c(\eta x_i-h),\qquad
 \eta=1/\sigma,\quad h=\mu/\sigma.
\]

This is concave in the nuisance coordinates. Held location restricts it to
h = mu eta, a ray; held scale restricts eta. A free maximizing location can be
restricted to the sample hull: moving an exterior location toward that hull
cannot increase any residual loss.

At a candidate let z_i = (x_i-mu)/sigma, G = sum z_i psi_c(z_i),
H = sum psi_c(z_i), and

\[
 M=\max_{\mu'\in[\min x,\max x]}H(\mu'-\mu)/\sigma,
\]

or M = 0 for held location. Applying a supporting plane to every convex loss
bounds the improvement at a new inverse-scale ratio t by
G + n log t - t(G-M). If scale is free and K = G-M > 0, maximizing this
expression over all positive t gives

\[
 \ell^*-\ell\le M+n\{\delta-\log(1+\delta)\},\qquad
 \delta=(K-n)/n.
\]

With held scale the bound is M. This is a global bound, rather than an
optimizer convergence assertion. Failed inner solves or unavailable or loose
bounds stop. Fixed-shape finite acceptance requires the gap to be at most
`1e-8*n + 1024*eps*(1+abs(logLik))`.

For free shape, rho_c(z) is concave in c. Therefore, for any fixed nuisance
point, the negative loss lies below its endpoint chord on [l,u]. Taking a
supremum over nuisance parameters preserves the endpoint upper bounds.
With q = 2 exp(-c²/2)/(c Z(c)),

\[
 (\log Z)''=q+(2q-q^2)/c^2\le1+2/l^2,
 \qquad 0<q<1.
\]

Thus an entire profiled segment has upper bound

\[
 \max(U_l,U_u)+\frac{n}{8}(1+2/l^2)(u-l)^2.
\]

The largest active bound is bisected geometrically. Inner endpoint solves use
`min(gradtol, 1e-12)` and receive their own nuisance bound. Segments with upper
bound at most incumbent minus `1e-8*n` can be discarded. A finite return needs
a global likelihood gap of at most `1e-8*n`, a lead over both face upper bounds
larger than the arithmetic margin, and the score gates. Retaining an original
proposal within `64*eps*(1+abs(incumbent))` adds at most that rounding allowance.
No unimodality assumption about the profiled shape is used.

A finite free-shape result must also satisfy the historical solver gates:
mean location score at most `max(gradtol, 1e-7)` when location is free, mean
scale score at most `gradtol` when scale is free, and absolute total log-shape
score at most `1e-3`. Small value uncertainty does not waive these checks.

### Closing the noncompact ends

At or above the largest residual of the exact constrained Gaussian fit,
that fit lies in the quadratic core and maximizes the concave inner problem.
Only Z(c) changes; Z decreases toward sqrt(2 pi), so this entire upper tail
is below the Gaussian face. Held location and scale remain held.

For small shape with scale free, rho_c(z) >= c|z|-c²/2 and c Z(c) >= 2 give
the uniform upper bound `Laplace optimum + n*c²/2`, after writing b = sigma/c.
With scale held, the uniform bound is
`n*(log(c)-log(2*sigma)+c²/2)`, which tends to minus infinity even when location
is free. These envelopes close the tails for finite winners and Gaussian
winners when separated; the first envelope alone cannot certify Laplace.

A strict median cluster supplies the additional Laplace proof. Let k
observations equal median m, D be right count minus left count, and
b = mean absolute deviation from m. If no other point enters the core, the
inner solution is sigma = cb, mu = m+c²bD/k, and

\[
 \ell(c)-\ell_L=\tfrac12 Bc^2-n\log F(c),\quad
 B=n-k+D^2/k,\quad F(c)=cZ(c)/2.
\]

For held location use its zero-residual count and D = 0. These coordinates
satisfy the concave nuisance scores. The implementation checks the cluster
geometry for the entire excluded interval. To prove the needed bound,
F(0)=1, F'(c)=integral from 0 to c of exp(-t²/2) dt, and F''(c)=exp(-c²/2).
Consequently F <= 1+c²/2 and F' >= c-c³/6. For 0<c<1,
F'/F >= c-2c³/3, hence

\[
 \log F(c)\ge c^2/2-c^4/6.
\]

The entire lower tail is strictly below Laplace if B<n and
`c0² < min(1, 3*(n-B)/n)`. A face is selected only after the compact upper
bounds are below it by the arithmetic margin and the relevant tail comparison
also closes. Otherwise the algorithm stops with its competing witnesses.

### Arithmetic and work limits

Point likelihoods are reevaluated at the represented public coefficients in
original coordinates. The supporting-plane padding is

\[
 256\,\mathrm{eps}\,
 (1+|\ell|+\sum|z_i\psi_i|+|M|+Q\sum|\psi_i|)(1+n\,\mathrm{eps}),
\]

where Q is the maximum standardized distance to a hull endpoint, or zero for
held location. Free-shape face comparisons use
`1024*eps*(1+abs(incumbent)+n)`; if this exceeds `1e-8*n/8`, verification stops.

Gaussian face mean scores must be within `1e-7` for location and `1e-10` for
scale, when free. The Laplace scale ratio must be within `1e-10` of one.
These imply ideal face improvements below `6e-15*n` and `6e-21*n`, respectively;
each face upper bound adds `128*eps*(1+n+abs(face logLik))`.
The Gaussian core endpoint is inflated to `(max_abs_z+1e-6)*(1+1e-6)`, covering
the optimizer displacement allowed by these gates. Median-cluster geometry
uses an additional factor `1+1e-8` for its scale arithmetic.

These margins are practical, conditional floating-point safeguards. They are
not a proof about every floating-point operation on every sample. Face score
failures, underflow, overflow and unrepresentable objectives produce numerical
stops. Malformed face rows are removed; proper finite points remain available
as lower witnesses, with an unknown global upper bound. A nonrepresentable
objective can legitimately leave no finite lower witness. Laplace evaluations
avoid an unscaled absolute sum and the potentially overflowing product 2b.

Inner iteration limits are `min(iterlim,100000)` and adaptive certificate
allowance is `128*min(iterlim,100000)` evaluations, following
[work budgets](../../design/work-budgets.md). The proposal grid is also bounded
by that allowance; finite ordered grid bounds are required before conversion
or indexing. Exhaustion produces a budget stop advising increased `iterlim`.
An unresolved comparison produces a tie stop, retaining the unresolved
segment endpoints, the finite proposal, and both eligible face witnesses.

## Counterexamples and reproducible checks

**Nonuniqueness is proved:** with x = (-10,10), held sigma = 1 and epsilon = 1,
every mu in [-9,9] has the same linear-tail loss and globally maximizes the
concave likelihood. The returned midpoint remains zero. Both C++ and R tests
check this example. No general free-shape uniqueness theorem is asserted.

**An unresolved finite–normal comparison is numerical evidence:** set
`d(c)=2*exp(-c²/2)/(c²*Z(c))` and numerically solve
`d(c)²/2-log(Z(c)/sqrt(2*pi))=0`. The driver obtains
c = 0.76842842824488278 and a = c+d(c) = 1.5242981269692526.
For x = (a,a), held mu = 0 and sigma = 1, a finite stationary witness and
the normal face agree to arithmetic precision. The retained proposer chooses
NormalLimit; the certificate correctly stops as a tie with bounds
[-4.1613618462913173, -4.1613618462900259]. This is not an analytic proof of two
exact global maximizers. It demonstrates why overlapping computed comparisons
must not be converted into a certified face declaration.

**A genuine work-budget stop:** the search seeded by 11629 reaches sample
1372, n = 116, with held mu = 0, sigma = 1. At `iterlim=1`, the common binding
reports `budget`, five candidates and bounds
[-126.76206444392845, -126.76206147245944]. Raising `iterlim` to 200 gives Ok,
logLik = -126.76206443944403. The
[frozen sample](2026-11-07-huber-cert-data/budget-sample.json) avoids dependence
on a particular standard library's random-distribution implementation.

**Arithmetic regressions:** in double-width mode, x = (+/-1e-300) and
(+/-1e300) produce typed numerical stops with three proper witnesses and an
unknown upper bound. The earlier malformed Gaussian row suppressed all
binding evidence; [the captured failure](2026-11-07-huber-cert-data/scale-probe-before.txt)
records that issue. A 5001-point sample at 0 and +/-1e308 retains a proper
Laplace witness and exercises the accelerated-grid guard. An extremely small
held scale with x = (-100,100) produces an explicit numerical stop with an
unavailable lower bound. Additional independent likelihood checks use adjacent
large-offset observations and subnormal mean absolute deviations: inaccurate
represented face coordinates cannot pass as certified optima.

The appended C++ tests also compare nuisance bounds against competitors,
compare chord bounds against refitted intermediate profiles, check both face
witnesses and the all-zero held-scale normal face, and independently
differentiate the settled density to check the total shape-score gate. Existing
Huber covariance and subset-projection tests remain included and pass; no new
inference claim is made.

## Timings and coefficient behavior

The [timing driver](2026-11-07-huber-cert-data/check.cpp) compares the retained
original proposal routine with the certificate wrapper on simbox, GCC 13.3.0,
`-O2 -std=c++17`, one thread, nice 10. Ordinary benchmark proposal paths are
unchanged; this is a paired routine comparison, not a separately frozen old
binary. Small samples have 20 repetitions; the large sample has one repetition
and excludes quantile generation. These shared-machine wall times are
descriptive, not a timing-uncertainty study.

| Workload | Before | After | Ratio |
|---|---:|---:|---:|
| Six points, epsilon held 1.345 | 0.32525 microseconds | 0.39560 microseconds | 1.22 |
| Same six points, epsilon free | 103.5385 microseconds | 179.50445 microseconds | 1.73 |
| 100000 deterministic Huber quantiles, epsilon free | 0.147309 seconds | 1.192527 seconds | 8.10 |

The small sample is (-7,-1,-1,0,2,12). The large sample is
`qhuber((i+.5)/100000,2,3,.8)`. Its coefficients change from
(1.9999999999999998, 3.0000616192695388, 0.80002562451792358) to
(2.0000000000000004, 3.000057581814727, 0.80002434343572137).
This is a free-shape change, not a default or golden change.
The [independent point check](2026-11-07-huber-cert-data/point-check.txt)
finds the old total shape score -0.005076294067, outside the 0.001 gate;
the new score is 0.000701068784, with mean scale score 1.15e-15.
The old stored logLik is -305874.52068983053, but evaluation at its coefficients
is -305874.52068983798. The new stored and independently evaluated value is
-305874.52068983449. Preserving the failed proposal would defeat the score
requirement even though its value was already close.

The certificate adds full data reductions at every endpoint. The proposal's
prefix-statistic acceleration therefore does not remove verification's
linear dependence on n. Active segment storage is linear in the evaluation
allowance; selecting the largest bound by a vector scan can be quadratic in
that allowance in the worst case. These are accepted costs of the current
implementation; no fast uniform certificate-complexity theorem is claimed.

## Final validation

All commands ran in the foreground and finished. Workstation computing used
`/tmp/umvl-cpu/run` or `run4`, nice 10 and one BLAS/OpenMP thread per worker.
Long suites ran through `/tmp/umvl-cpu/simbox-run`; no fallback was needed.
C++ CMake builds used GCC 15.2.0 on the workstation and GCC 13.3.0 on simbox.
Both use R 4.6.1; simbox's R package compiler is `ccache g++-15`.
R tests use four workers; the launcher configures six-way C++ suites on simbox.
Workstation temporary files used the requested `build/tmp` variables.

| Machine | Command | Final result |
|---|---|---|
| Workstation | `just cpp-test` | 106/106 dev shards pass. |
| Workstation | `just cpp-test-no-long-double` | 129/129 pass. |
| Simbox | `UMVL_TEST_TIER=full just cpp-test` | 129/129 pass. |
| Simbox | `just cpp-test-no-long-double` | 129/129 pass. |
| Simbox | `just test` | FAIL 0, WARN 0, SKIP 154, PASS 21268. |
| Simbox | `just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 44086. |
| Workstation | `just js-build` and `just js-test` | Build succeeds; 469/469 pass, no skips or cancellations. |
| Workstation | `just site-check` | Green: 99 family pages with offline MathML; 99 generated C++17 snippets compile/link; links, graph and coverage checks pass. |
| Both | Focused Huber binary, native and no-long-double | Each mode: 32 cases pass, including two shared loader cases; native 4435 and double 4459 assertions, no skips. |
| Simbox | Certificate, budget, scale and independent point drivers | All exit 0; outputs retained. |

The 154 dev-tier skips were followed by the full R run with zero skips. There
were no permitted test failures. Existing compiler warnings in unrelated
headers remain visible in build output; the R test warning counts are zero.
The initial standalone focused compile omitted the installed BH include path;
corrected commands using the existing BH headers passed on both machines.
Evidence extraction was corrected for the differing CTest summary formats;
this did not rerun or alter any solver result.

The remote final
[validation harness](2026-11-07-huber-cert-data/run-checks.sh) records both the
production and assembled R-package header hashes; both match the hash above.
The [combined simbox record](2026-11-07-huber-cert-data/cpp-simbox.txt),
[workstation dev record](2026-11-07-huber-cert-data/cpp-workstation-dev.txt),
[workstation double record](2026-11-07-huber-cert-data/cpp-workstation-no-long-double.txt),
[JavaScript results](2026-11-07-huber-cert-data/js-tests.txt),
[site results](2026-11-07-huber-cert-data/site-check.txt),
[driver outputs](2026-11-07-huber-cert-data/drivers.txt) and
[timing JSON](2026-11-07-huber-cert-data/timings.json) preserve the evidence.
The unrelated screenshot regenerated by site validation was restored from the
base revision. The final tracked diff contains only the Huber dossier, fitter,
common binding and appended family tests, plus this report and its evidence.

## Remaining limits

There is no implementation blocker for the eight supported subsets. A sample
can still stop when bounds overlap, the budget is exhausted, face arithmetic
is unavailable, or the small-shape envelope cannot separate a proposed
Laplace winner without the strict-cluster proof. Such cases retain evidence
and do not return an unchecked maximizer or an unchecked face.

Open theoretical questions are global uniqueness of the fully free fit and
parameter-distance guarantees from the likelihood enclosure. Settling the
former needs a profile uniqueness/separation argument; the latter needs local
curvature and separation assumptions and cannot hold uniformly because of
the proved flat-location example. Neither is used by the implemented
certificate. No inference, bootstrap calibration or universal floating-point
error theorem is claimed.

The requested artifact-pane `show` command was attempted after verification.
The viewer returned HTTP 403 because this worktree is outside its configured
readable roots. The report remains at the requested worktree path; no viewer
configuration or other checkout was changed.
