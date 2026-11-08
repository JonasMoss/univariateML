# Bhattacharjee certificates — TASK-116.43

The six nonempty free subsets with at least one coefficient held now use
certificates: a returned finite maximum has a global acceptance argument and
a quantitative coefficient enclosure; otherwise fitting stops with a proved
limit or a numerical, budget, unresolved-comparison or indistinguishable-winner
diagnostic. The all-free route remains **open**, for the frozen-golden conflict
documented below. The dossier therefore says `fit.certified: partial`, never
`yes`.

Work was confined to the `codex/bhatt-cert` worktree, based on
`7d877b7005048ccd22106f15e5fd8af5757f5ea2`. There were no commits, pushes,
board edits, installations or sub-agents. All required final suites finished
successfully before this report was written.

## Delivered behavior

| Free coefficients | Holds | Acceptance argument |
|---|---|---|
| `mu` | `sigma, a` | Strictly concave endpoint likelihood; enclosed score root. |
| `sigma` | `mu, a` | Positive held-width concavity; exact uniform covering test first. |
| `a` | `mu, sigma` | Global width certificate, including both tails and the attained normal member. |
| `mu, sigma` | `a` | Positive held-width concavity; optimized endpoint penalty on the uniform face. |
| `mu, a` | `sigma` | Concave inner location profile, global width bounds and isolated finite winners. |
| `sigma, a` | `mu` | Concave inner scale profile, global width bounds and a bound for the entire small-sigma neighborhood. |
| none | all | Direct likelihood evaluation at the supplied coefficients. |
| `mu, sigma, a` | none | Existing numerical route retained; not certified. |

Holding `a = 0` uses the compatible normal problem. An attained normal winner
returns `Ok` with `a = 0`, rather than an escape instruction. Positive held
`sigma` removes the unattained uniform face. Failure to separate the normal
member from positive widths produces a witnessed stop; it does not claim
that the normal member wins.

The new headers are
[certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/bhatt/certificate.hpp),
[certificate_math.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/bhatt/certificate_math.hpp)
and [winner_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/bhatt/winner_certificate.hpp).
The old search is a family-local proposal function. Held-subset acceptance
never trusts its likelihood or convergence status. Binding-neutral
[routing](../../../bindings/common/distributions/bhatt/fit.hpp) supplies the
same structured outcomes to R and JS. The
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bhatt/README.md)
records the scope, limits and uniqueness qualifications.

The EMG root/inclusion structure was adapted locally. No shared header,
including `emg/logconcave_profile.hpp`, changed. C++ cases and shared JS cases
were appended. The family R oracle test also checks the likelihood witnesses
of structured stops; successful fits retain its independent multistart checks.
Goldens, d/p/q/r, special functions, truth tables and code generation sources
are unchanged. Default control values and the frozen all-free golden output
are unchanged. Added guards reject invalid all-free outputs, including false
collapse caused by underflow on distinct subnormal observations.

## Exact-arithmetic arguments

### Positive physical width held

Write `u = mu/sigma`, `b = 1/sigma`, and

\[
 L(u,b)=-n\log(2a)+\sum_i\log\{\Phi(b(x_i+a)-u)-\Phi(b(x_i-a)-u)\}.
\]

The normal interval log probability is strictly concave in its endpoints.
For endpoints `l < h`, let `D = Phi(h)-Phi(l)`, `A = phi(l)/D`,
`B = phi(h)/D`. Its Hessian has diagonal entries `l*A-A*A` and
`-h*B-B*B`, and off-diagonal entry `A*B`. The first diagonal is negative:
`A > l` for positive `l`, and the other signs are immediate. Its determinant is

\[
 AB\{hA-lB-lh\}
 =AB\{1+E[(Z-l)(h-Z)\mid l<Z<h]\}>0.
\]

For positive held `a`, the endpoint map in `(u,b)` is injective, with
determinant of magnitude `2a`. Holding `mu` imposes the affine constraint
`u = b*mu`; holding `sigma` fixes `b`. These restrictions preserve strict
concavity. A validated stationary root is consequently the unique global
maximum, not merely a successful local optimization.

If location is free and the range exceeds `2a`, scale collapse loses all
likelihood; diverging scale or location also loses likelihood. A finite
maximum therefore exists. Range below `2a` instead has unattained supremum
`(2a)^(-n)`. At equality, endpoint multiplicities `j,l` give the additional
factor

\[
 (j/(j+l))^j(l/(j+l))^l.
\]

For any finite scale, the two endpoint probabilities are bounded by
complementary normal probabilities. Maximizing their powers gives this
factor; choosing a location displacement proportional to sigma attains it
in the limit. With location held, a covering interval has the same peak
bound, with an additional factor `1/2` for each observation exactly at an
endpoint. An exterior observation instead forces the small-scale likelihood
to zero. These are proved face decisions, not numerical comparisons.

Covering and endpoint equality use error-free difference comparisons in the
original input coordinates. They do not decide equality from rounded
standardized distances or a rounded midrange.

### Width free

Put `k = a/sigma`. For fixed `k`, the nuisance coordinates have a concave
likelihood, including the affine restriction from held physical `mu`.
Joint log concavity of interval mass, together with the `n*log(b)` term
when scale is free, implies that

\[
 F(k)=P(k)+n\log k
\]

is concave after nuisance profiling. Enclosed nuisance roots and Hessian
bounds give supporting tangents of `F`. On a width interval, the minimum of
the endpoint tangents bounds `F` above. Subtracting `n*log(k)` makes each
linear piece convex, so an upper bound is obtained at the interval endpoints
and the support-line intersection. The implementation encloses that
intersection and keeps a conservative fallback.

Endpoint interpolation with only a bound `P'' <= n/k^2` is not a valid upper
bound. A regression uses `x = {10}`, held `mu = 0, sigma = 0.1`, and
`k in [90,110]`: the likelihood at `a = 10.25` is about `-3.026653912`,
above the endpoint-chord construction. The implemented tangent bound is
about `-2.947291952` and covers the unseen peak.

Negative definite full-Hessian boxes isolate finite stationary winners.
A uniform nuisance Krawczyk inclusion puts the unique inner optimum inside
the box for every width in its accepted interval. Thus its profile is
strictly concave there. Other width intervals must be excluded by upper
bounds or covered by such winner regions before a finite fit is accepted.
General width-profile unimodality is neither assumed nor claimed.

For small `k`, Hoeffding's inequality gives the normal comparison bound.
With sigma held, the addition is at most
`k^2 * sum(((x-mu_normal)/sigma)^2)/2`; with sigma free it is
`-n*log(1-k^2)/2`, for `k < 1`, after optimizing the normal scale.
The normal profile upper also includes a bound on likelihood lost through
rounding its mean. If this tail cannot be excluded, the result is unresolved.
There is a separate exact normal-winner shortcut: held-location distances
at most sigma, or free-location range at most sigma. The inequality
`exp(-t^2/2)*cosh(z*t) <= 1` for `|z| <= 1` proves pointwise domination;
the free optimum can be restricted to the data hull. These conditions are
checked in original coordinates before standardizing.

For large `k` with sigma held, `f <= 1/(2*k*sigma)` closes the tail.
With mu held and sigma free, put `r = max(abs(x-mu))`. For `a >= r`,
the density peak bound gives the compatible uniform-face upper. For
`a < r`, retain the most distant observation and put `t = r/a > 1`:

\[
 \prod_i f(x_i)\le (2r)^{-n}t^n Q(k(t-1)).
\]

The derivative of the log of the last factor is
`n/t - k*R(k*(t-1))`, where the normal inverse Mills ratio satisfies
`R(v) >= sqrt(2/pi)` for `v >= 0`. It is negative for every `k >= 2n`.
The factor is therefore at most `1/2`. Together with the peak bound, this
controls the **whole** `k >= 2n` neighborhood, not a finite grid near
sigma zero. It supplies the quantitative small-sigma bound missing from
the triage. The same inequality can use the sample half-range when location
is free, since some extreme is at least that far from any centre; this does
not itself certify the disabled all-free public route.

## Numerical contract and stops

The proofs above are exact-arithmetic arguments. Numerical acceptance follows
the package standard: conditional on the computed comparisons, with practical
floating-point margins, not outward-rounded interval arithmetic.

* Interval operations add `32*eps` relative padding and a subnormal floor.
  Normal interval log-mass bounds include `256*eps` scaling and a cancellation
  term `64*eps*(1+abs(L)+abs(R))/(-expm1(R-L))` for the two log tails `L,R`.
  Score and Hessian comparisons propagate these enclosures. Unusable
  intervals stop the search.
* Compensated likelihood sums use a base margin of
  `1024*eps*(n + sum(abs(log_density)))`, with the interval-mass conditioning
  error included in certificate evaluations. Supporting-line and tail
  comparisons add their stated `128` or `256*eps` margins.
* A scaled Krawczyk contraction below `1/2`, strict inclusion and a negative
  definite Hessian enclose the stationary root. Enclosures must meet the
  requested relative coefficient tolerance in original units, using sigma
  as the location floor. Rounded physical coefficients are evaluated again;
  inconsistent normalized and physical likelihoods produce a numerical stop.
* Conditional normal residual RMS is scaled before squaring. Actual equality
  of observations, not a rounded zero sum of squares, decides collapse.
  Unrepresentable roots or precision beyond the practical margin stop.
* Finite stopped witnesses are reevaluated after physical recovery. Stops
  retain compatible normal, uniform and finite candidates when available.
  Their global upper bound remains infinity while an unexcluded neighborhood
  remains; a last-segment upper is separate. A `Tie` diagnostic means surviving
  isolated candidates could not be distinguished, not a proof of exact equality.
* Solver and backtracking loops use `min(iterlim,100000)`; certificate work
  uses `128*min(iterlim,100000)`. There is no capped estimate returned as a
  certified fit.

The tests cover tiny RMS, subnormal false collapse, endpoint subtraction
residuals, narrow-mass cancellation, an unrepresentable shifted location,
physical witness likelihoods, and a finite likelihood near `-1e308` whose
naive interval-midpoint computation would overflow.

## Uniqueness qualifications and counterexamples

Positive held width gives a unique **attained** finite maximum. Its winning
uniform limit need not have a unique centre: with `x = {-1,0,1}`, `a = 2`,
every centre in `(-1,1)` gives supremum `-3*log(4)` as sigma vanishes.

The unrestricted uniform face is a local maximum of the likelihood closure.
For sample half-range `r`, the peak bound handles `a >= r`; the two-extreme
probability product at most `1/4` handles
`r*4^(-1/n) <= a < r`. It need not be global. Twenty zeros and the two
observations `-10,10` have normal variance `100/11`; the normal-member
likelihood exceeds the uniform-face likelihood. Their excess kurtosis is
eight, and the normal-profile expansion is
`L(k) = L(0) - (44/45)*k^4 + O(k^6)`. Thus even the unrestricted closure
need not have one local maximum. This supplies a counterexample to global
unimodality, not an assertion of two finite global maximizers. General
joint finite uniqueness remains unproved.

## Open: all-free acceptance and the frozen golden

The frozen sample has 500 observations. Its existing fit is

```
mu     -0.0038000682197418395
sigma   1.057897350270679
a       1.7336058787117143
logLik -896.31045982723822
```

The independent [70-digit analytic-score calculation](2026-11-08-bhatt-cert-data/golden-score.py)
uses the binary64 observations exactly. Its
[recorded result](2026-11-08-bhatt-cert-data/golden-score.json) is

```
mu     -0.00380006892159174971312956250794
sigma   1.05789733329049518933051976791
a       1.73360592386775098991319797969
```

The frozen sigma and width scores are approximately `1.468586125e-6` and
`1.349170173e-6`. Relative coefficient changes to this stationary point are
`1.84694029e-7`, `-1.60508804e-8`, `2.60474640e-8`. Both scale changes exceed
the default `1e-8` coefficient tolerance and the ordinary native golden
regression tolerance `1e-12`. The new C++ root test rejects quantitative
acceptance of the frozen proposal, then encloses a local root at `1e-10`
which agrees with the independent calculation. This is local stationary-root
evidence; it does not certify the root as the all-free global winner.

Replacing the default output would violate this job's frozen-fit/golden
constraint. Returning that frozen output as newly certified would make a
false claim. The public all-free numerical search is therefore retained and
explicitly excluded from certification. **What is missing:** authorization to
change the frozen fit/reference, or a decision to change its acceptance to a
structured stop; then enable and validate the unrestricted global outer route,
including its competitor comparisons. No all-free global certificate is
claimed by this delivery.

An additional shared issue was found but left outside this job: the
conditional normal delegate squares residuals without scaling. In double
mode, `x = {1e-200,2e-200}`, held mean zero and free sd, can underflow to a
false unbounded result. Bhatt now handles that normal member locally, with
`sigma/1e-200 = sqrt(2.5)`. The shared normal fitter needs a separate repair;
no shared source was changed here.

## Timings

Workstation GCC 15.2, `-O2`, one reserved CPU, `nice` through the slot wrapper;
500 frozen observations, tolerance `1e-8`, iteration limit 500. Holds use the
frozen coefficient values. Each entry is one sequential wall-clock call,
including acceptance. These are workload measurements, not stable speedup
estimates. In particular, the retained all-free algorithm did not acquire a
speedup merely because its two samples differ.

| Free coefficients | Before, seconds | After, seconds |
|---|---:|---:|
| all | 0.919274 | 0.571724 |
| `sigma, a` | 0.367235 | 2.491129 |
| `mu, a` | 0.247546 | 0.285174 |
| `a` | 0.038272 | 0.096226 |
| `mu, sigma` | 0.334149 | 0.024069 |
| `sigma` | 0.049625 | 0.017836 |
| `mu` | 0.001361 | 0.009972 |
| none | 0.000241 | 0.000154 |

All eight final timing calls returned `Ok`. The unrestricted row preserves
the frozen coefficients and likelihood exactly. Raw rows:
[before](2026-11-08-bhatt-cert-data/before.csv),
[after](2026-11-08-bhatt-cert-data/after.csv). The mask uses bits
`mu=1, sigma=2, a=4` for holds; columns are mask, status, mu, sigma, a,
log likelihood, seconds. Reproduce with
[benchmark.sh](2026-11-08-bhatt-cert-data/benchmark.sh), optionally passing
`timings.cpp baseline` to call the retained legacy proposer. A separate
[check.cpp](2026-11-08-bhatt-cert-data/check.cpp) checks every held mask at
tolerance `1e-10`, the tangent peak, tiny RMS and witnessed unresolved stop.

## Final validation

The final source was frozen before these runs. No remote/local compute
overlapped in these final batches; total task compute stayed within four cores. Workstation commands
used the slot locks; simbox commands used `simbox-run`, with one-thread BLAS
and four build/test workers. Simbox stayed available; no fallback was needed.

| Machine | Command | Result |
|---|---|---|
| Workstation, GCC 15.2 | `just cpp-test-no-long-double` | 129/129 |
| Workstation, Emscripten/Node | `just js-build` | success |
| Workstation, Node | `just js-test` | 508/508 |
| Workstation | `just site-check` | green; 99 family pages, MathML, 99 compiled C++ snippets, browser checks |
| Simbox, GCC 13.3 | `UMVL_TEST_TIER=dev just cpp-test` | 106/106 |
| Simbox, GCC 13.3 | `UMVL_TEST_TIER=full just cpp-test` | 129/129 |
| Simbox, GCC 13.3 | `just cpp-test-no-long-double` | 129/129 |
| Simbox, R 4.6.1 | `just test` | FAIL 0, WARN 0, SKIP 154, PASS 21613 |
| Simbox, R 4.6.1 | `just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 44448 |

The targeted final workstation double-precision Bhatt run also passed all
33 cases and 4,391 assertions. The full C++ suites include EMG, preserving
the worked example's behavior. Final machine summaries are kept as
[workstation output](2026-11-08-bhatt-cert-data/validation-workstation.txt) and
[simbox output](2026-11-08-bhatt-cert-data/validation-simbox.txt).
[validation.json](2026-11-08-bhatt-cert-data/validation.json) records commands,
counts, changed-source hashes and unchanged protected-file hashes. The site
check's regenerated historical screenshot was restored to its original
bytes, keeping the change set within this job's scope.

The supplementary repository-wide `check_links.py` returns four pre-existing
diagnostics, all in files byte-identical to HEAD: the absent
`dev/research/boundary-points.pdf`, a `1+m/r` formula parsed as a destination
in the nbinom dossier, and two `/inference/` site-route destinations in the
inference note. A scoped check of changed and new Markdown has no broken
destinations; see [link validation](2026-11-08-bhatt-cert-data/validation-links.txt).
These unrelated link-check diagnostics remain open outside this task's edit
scope. None is a failed numerical or binding test.
