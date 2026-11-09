# GB2 and Feller–Pareto power subsets — TASK-116.52

Completed validation: 2026-11-09. Base: `ddb39917`; no commits, merges or rebases.
Public GB2 order is `(q, a, p, b)`, with power `a`. Feller–Pareto statements below require held `min` and use positive excesses `x-min`.

This change certifies both held-scale mixed-power subsets, `{p,a}` and `{q,a}`, and the held-beta-shape power/scale subsets `{a}`, `{b}`, `{a,b}`. It wires the proofs into both fitters and the common binding routes, including finite constant-data regimes and explicit numerical, budget, tie and limit stops. Five larger subsets remain open, with the precise missing arguments below. Both dossiers therefore retain `fit.certified: partial`; their `fit.note` now describes this coverage. Whole-family certification is not claimed.

## Delivered coverage

| Free coefficients | Held coefficients | Result |
|---|---|---|
| `{a,b}` | `p,q` | Strict-concavity coefficient enclosure; constant-data unbounded case retained |
| `{a}` | `p,q,b` | Restriction of the same concave likelihood; appropriate constant-data cases included |
| `{b}` | `p,q,a` | Restriction of the same concave likelihood, with floating-point spacing check |
| `{p,a}` | `q,b` | Global profile certificate, including power-face comparisons and finite constant-data fits |
| `{q,a}` | `p,b` | Reciprocal global profile certificate, including Pareto-face comparisons |

Held unit beta shapes retain the existing Burr/inverse-Burr certified routes. Held-power conditional shape/scale subsets retain `power_certificate.hpp` and its beta-prime interface. No beta-prime files were changed. The five open generic subsets below exclude these already certified special slices.

Implementation lives in [concave_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/concave_certificate.hpp), [endpoint_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/endpoint_certificate.hpp), [shape_power_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/shape_power_certificate.hpp), the two `mle.hpp` files and `bindings/common/distributions/{gb2,fpareto}/fit.hpp`. All new helpers are reached by fitter routes. No shared production header needed editing.

## Why the new certificates are global

### Held beta shapes

For normalized log data `z=(log x-m)/s`, set `A=a*s` and `c=a*(log b-m)`. The negative Hessian is

\[
\operatorname{diag}(n/A^2,0)+(p+q)\sum_i w(Az_i-c)(z_i,-1)(z_i,-1)^T,
\qquad w(t)=e^{-|t|}/(1+e^{-|t|})^2.
\]

It is positive definite. Holding scale restricts the likelihood to an affine line; holding power fixes `A`. The certificate bounds weights throughout a ball around a Newton candidate and lower-bounds its least curvature by determinant/trace. If the score norm, including arithmetic allowance, is less than half the curvature times radius, every outward boundary derivative is negative. A stationary point therefore exists inside; strict concavity makes it the unique global maximizer. Radius and coordinate transport bound relative power and scale error. A supporting tangent bounds the maximum likelihood from above. Newton convergence alone is never the acceptance condition.

Log-coordinate uncertainty enters the score, likelihood and curvature bounds. Free-scale representable spacing must be below a quarter of the target relative accuracy. Thus huge powers cannot amplify lost log differences into a false coefficient certificate. Distinct observations whose logarithms collapse stop numerically. Constant observations with exactly one free power/scale coefficient use the same argument with unit normalization; jointly free power/scale retain their analytic unbounded stop.

### One free beta shape, free power, held scale

In the direct orientation put `w_i=log(x_i/b)` and `S_minus(a)=sum softplus(-a*w_i)`. At each power the inner shape root is unique:

\[
\psi(p+q)-\psi(p)=S_-(a)/n.
\]

The left side strictly decreases from infinity to zero. Signed scores enclose the root, using lower derivative bounds in log shape. Ending an inner refinement leaves an enclosure, never an unchecked estimate. The reciprocal route reverses log ratios and exchanges beta shapes, preserving the original data Jacobian without computing reciprocal observations.

For exterior control define `H(p)=log(p*B(p,q))`. For `q<=1`, `H>=0`. For `q>1`, convexity gives `H>=-C*p`, where `C=psi(q)-psi(1)>0`. Write `L=sum(-w_i)_+`, `R=sum(w_i)_+`, and let `Z` count endpoint observations.

* On a lower-side sample, the limiting power likelihood has rate `n/L` and penalty `Z*q*log(2)`. For `q>1`, the sign of `n*C-Z*log(2)` distinguishes endpoint domination from a finite witness above the face. In the latter case the excess upper bound decays as `-n*log(1-(n*C-Z*log(2))/(a*L))`. Subunit shapes have direct pointwise domination. Ambiguous computed signs stop numerically.
* On a crossing sample, the large-power upper bound is `n*log(2*n/L)-n-q*a*R-sum(log x)` once `a>=2*n*max(C,0)/L`.
* On a nonconstant upper-side sample, integrating `psi(t)<log(t)` gives `B(p,q)>=Gamma(q)/(p+q)^q`. Maximizing this bound gives `n*log(a)-q*a*D+K`, with `D=sum(w_i-min w)>0`, hence decay to minus infinity. Constant upper-side data instead concentrate without bound.
* For sufficiently small power, `S_minus/n>=log(2)/2`. Maximizing the strictly concave shape function against this lower bound yields `n*log(a)+n*M-sum(log x)`, tending to minus infinity.

These inequalities enclose every possible finite global maximum in a compact power interval. Inside, convexity bounds `S_minus` and all corresponding inner roots. Negative diagonal Hessian entries and the decreasing cross entry give rectangle likelihood bounds; signed corner scores exclude stationary-point-free cells. Dominated cells are discarded. Interval Newton contracts a cell only after a strictly negative profile Schur complement is verified throughout it; other cells undergo general subdivision. No assumption of profile concavity or unimodality is used.

Success requires all surviving global candidates to lie in a coefficient envelope with relative spans below `min(tol,1e-9)/8` in both free coefficients. Separated unresolved candidates stop as a tie. Budget exhaustion and unresolved precision/range stop with structured details and available witnesses. Full derivations and arithmetic allowances are in the [GB2 dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md).

### Arithmetic and work contract

These are certificates in exact arithmetic conditional on the computed comparisons, with practical floating-point margins, not outward-rounded interval proofs. The new paths use `8*epsilon` log-input allowances, `16*epsilon` transform/reduction allowances, and `128*epsilon` special-function, likelihood and transport allowances, including subnormal floors. Curvature is discounted for input and weight uncertainty. Unsupported precision requests or unresolved comparisons stop numerically.

Scalar iteration allowances are `min(iterlim,100000)`; the mixed certificate shares `128*min(iterlim,100000)` evaluations. Caps do not silently turn into estimates. The held-shape solver reports numerical failure with its witness if it cannot establish its ball within its allowance. The global mixed route distinguishes certificate budget, tie and numerical stops.

## Observable corrections and transport

For five observations equal to `0.5`, held `q=2,b=1`, free `p,a`, the previous constant-data classification was incorrect. The new route certifies a finite maximum near

\[
a=4.92939151464796897516,\qquad p=0.367911763774812962487,
\]

with log likelihood `1.3409811768553846`, exceeding the power supremum `0.2983005057080482`. The independent high-precision stationary calculation supports coefficient accuracy; the enclosure/exterior argument supplies globality.

Conversely, five observations equal to `2`, held power `1e-300`, held `q=10`, `p=nextafter(10,+infinity)`, previously returned `Ok` at scale `2`. Independent arithmetic gives `log(b)≈-1.77635683940025e284`. The new route stops numerically because that coefficient cannot be represented and certified.

Power witnesses use public `(alpha=b,beta=rate)` order; Pareto witnesses use `(a=rate,b=b)`. Tied endpoint observations retain the pointwise GB2 limit penalty. A refitted weak-limit density has the unpenalized endpoint value; these are different likelihood conventions, explicitly documented. Common binding advice preserves the held endpoint, using `mlpower(x,alpha=scale)` or `mlpareto(x,b=scale)` and corresponding excess data for Feller–Pareto.

Held-min Feller–Pareto transports the same proof with unit Jacobian. Finite witnesses append the held minimum; named face witnesses remain in excess coordinates. Nonfinite subtraction, distinct excesses that merge, or inexact subtraction that lands on held scale stop numerically rather than changing the mathematical case. Free-min structural restrictions remain unchanged. R and JS obtain these routes from the binding-neutral fit headers.

## Uniqueness: what is and is not established

Held-beta-shape strict concavity proves uniqueness. The mixed profile can have positive curvature: the reproducible search found three examples, independently recomputed at 80 digits. One has held `q=30.76588164180298`, power `7.6960258267838375`, inner shape approximately `0.180114033781085`, and profile curvature `0.149037523005252`. This disproves profile concavity; it does not prove multiple maxima in that subset. The global algorithm does not need such a claim.

Uniqueness for the family already fails in a held-power conditional subset. The [earlier transported counterexample](2026-11-08-gb2-cert-report.md#nonuniqueness), with held `p=0.22690393135372058,a=2`, switches between two separated winning modes as its data parameter moves from `5.6463` to `5.6466`. The parent comparison certificate and continuity give an intervening exact dataset with two global maxima. The rounded crossing near `5.646438395028704` produces a tie stop; exact equality at the rounded dataset is not asserted.

## Independent accuracy and frozen fixtures

[high_precision.py](2026-11-08-gb2-power-data/high_precision.py) evaluates independent 80-digit scores. [accuracy.json](2026-11-08-gb2-power-data/accuracy.json) compares both families' new finite fits against them. The largest measured relative coefficient error is `2.43754e-11`; every checked coefficient is below `5e-10`.

| Sample route | Independent power | Independent free beta shape or scale |
|---|---:|---:|
| Held `p=3,q=2`, free power/scale | 0.501294051918804210390 | scale 0.289996241879792981725 |
| Free `{p,a}`, held `q=2,b=1` | 0.578343632137284815316 | p 1.794475219041363995775 |
| Free `{q,a}`, held `p=3,b=1` | 0.431551183840810323577 | q 3.308542732930816108603 |

For the existing Feller–Pareto held-shape golden (`q=.3,p=.4,min=0`, 500 observations), the independent reference is `a=1.980293253079855976543`, `b=1.008784292117851040993`. Its relative differences from the stored coefficients are `5.75074e-12` and `2.10108e-12`, within tolerance. This is oracle-versus-fixture drift, not a claim that stored decimals are exact. Existing golden regressions pass; no golden was edited. Public controls/defaults and the larger numerical-search bodies were retained, with the new prevalidation and certified routing described above.

## Paired timings

Milliseconds per fit on the workstation, GCC 15.2, C++17 `-O2`, one CPU slot, `tol=1e-7,iterlim=300`. Before is reconstructed read-only from `ddb39917`; after uses the final source. Fixed eight-point data are in [check.cpp](2026-11-08-gb2-power-data/check.cpp); Feller–Pareto shifts them by two. Held-shape timings average 50 calls; mixed and four-point endpoint timings average three. Shared workstation load makes these measurements noisy, especially across families; they are observed costs, not worst-case bounds. Open rows time the unchanged numerical searches, not certified solutions.

| Route | GB2 before | GB2 after | FP before | FP after |
|---|---:|---:|---:|---:|
| `{a,b}`, p,q held | 0.007552 | 0.012936 | 0.005818 | 0.011107 |
| `{a}`, p,q,b held | 0.006719 | 0.012692 | 0.006785 | 0.013191 |
| `{b}`, p,q,a held | 0.005449 | 0.009851 | 0.005507 | 0.010293 |
| `{p,a}` | 0.123643 | 1.851609 | 0.122557 | 0.492188 |
| `{q,a}` | 0.129172 | 1.195003 | 0.126948 | 0.327769 |
| `{p,q,b}` — open | 0.976930 | 2.174025 | 0.944891 | 0.962906 |
| `{p,q,a}` — open | 1.037904 | 1.757394 | 1.025030 | 1.061161 |
| `{p,a,b}` — open | 103.084191 | 379.450593 | 158.668294 | 153.605486 |
| `{q,a,b}` — open | 115.383596 | 431.717777 | 117.976900 | 98.906388 |
| `{p,q,a,b}` — open | 778.777567 | 1231.168141 | 966.944112 | 915.376011 |
| Power endpoint stop | 0.180280 | 0.003033 | 0.173092 | 0.001784 |
| Pareto endpoint stop | 0.174348 | 0.000984 | 0.167086 | 0.001047 |

The endpoint routes become direct comparisons. The mixed certificates spend additional work on global coverage and coefficient enclosures. On the eight-point example, the direct certificate uses 102 evaluations and the reciprocal one 62. Numerical open routes retain the same outputs/statuses as the base on these data; their timing differences are not evidence of changed algorithms.

## Remaining mathematical work: five precise open entries

The derivations, including additional exterior progress, are retained in [open-analysis.md](2026-11-08-gb2-power-data/open-analysis.md). These entries identify missing certificates, not structural exceptions or proved nonexistence.

1. **Free `{p,q,b}`, held power.** This needs a compatible joint beta-prime global and coefficient certificate, then scale transport with enclosure error controlled under `b=s^(1/a)`. The frozen branch does not provide that joint certificate; the separate beta-prime job/interface was not modified or merged. At fixed power, diverging beta shapes collapse log variance rather than yielding a nondegenerate lognormal. This corrects the old gb2-cert report's lognormal-corner suggestion for the fixed-power case.
2. **Free `{p,q,a}`, held scale.** Varying power does admit a genuine lognormal corner. With target log location `m`, standard deviation `s`, and `K=2/(a*a*s*s)`, take `p=K*exp(a*(m-log b)/2)`, `q=K*exp(-a*(m-log b)/2)` as `a` tends to zero. Log mean and variance tend to `m,s*s`. Missing are uniform finite-sample likelihood remainders and a global outer search covering this corner and its other seams. Large-power paths can also move the effective endpoint despite held scale: for `0<theta<b`, take `p=r/a`, `q=(b/theta)^a*exp(-(log a)^2)`. The fixed-data density tends to the power density with endpoint `theta`, including its unpenalized endpoint value; reciprocation gives Pareto endpoints above `b`. The derivation is explicit in open-analysis. These paths establish genuine escapes, not a uniform comparison certificate. The existing numerical endpoint-`b` comparison does not exhaust them.
3. **Free `{p,a,b}`, held `q`.** Uniform small-power exclusion and eventual large-power domination by the freely fitted power face are now proved for every fixed `q>0`, uniformly in shape and scale. The missing exterior is the inverse-transformed-gamma face as `p` diverges and `b` tends to zero over a likelihood-relevant bounded-power region. At fitted face scale, `T_i=p*(b/x_i)^a`, `sum T_i=n*q`, the formal first correction is `(sum T_i^2-n*q*(q+1))/(2*p)`. A signed uniform remainder, including vanishing leading coefficients and profile displacement, is missing. A certified finite two-coordinate search is also missing. Therefore the whole subset is not wired as certified.
4. **Free `{q,a,b}`, held `p`.** The reciprocal problem has the same missing uniform signed remainder and finite-region search, with the transformed-gamma face replacing inverse transformed gamma.
5. **All four free.** Complete compactness/face coverage must combine lognormal, transformed and reciprocal gamma, power/Pareto and moving-endpoint corners, with uniform comparisons at their intersections and certified finite-region exclusion. The fixed-shape exterior thresholds are not uniform in a varying exponential beta shape. No complete joint certificate is claimed.

The new free-scale large-power result in entries 3–4 retains endpoint observations rather than discarding their softplus terms. For log data with maximum `M`, write `S0=n*M-sum y_i>0`, let `k` be its multiplicity and `C=max(0,psi(q)-psi(1))`. Above the maximum, with `u=a*(log b-M)` and `B=n/(a*S0)`, the likelihood excess above the free power face is at most

\[
-n\log(1+B(u-C))-qk\log(1+e^{-u}).
\]

This is strictly negative for all `u>=0` once `a` exceeds the explicit threshold in open-analysis. Just below the maximum, retaining `k*softplus(a*(M-log b))` gives a uniform negative bound. Farther below it, two gamma-ratio bounds decrease as `n*log(a)-c*a+K`. Together they prove eventual domination uniformly in free scale and shape. The limiting face is attainable through `p=(n/S0)/a`, `b=exp(M+(log a)^2/a)`. This is a proved exterior result, not an unused production helper or a completed larger fitter. A separate 80-digit sanity check at 2,160 parameter points passed; sampling itself does not prove the uniform assertion.

At the gamma-face zero-first-correction seam, even a second fixed-face coefficient is insufficient: profiling adds `-0.5*grad(A)'*H^(-1)*grad(A)` to the second coefficient when the face Hessian is nonsingular. The expansion and this term are recorded as asymptotic analysis, not certified uniform bounds. This is why a sampled boundary or an iteration cap was not substituted for the missing comparison.

## Final validation

All commands below completed in the foreground through the specified helpers; each final suite was a separate command. Local commands used `run4`, research used one-slot `run`, and simbox commands used `simbox-run` with bounded parallelism. Shared CPU-slot waits were allowed to finish. No started command remains running.

| Check | Machine | Final result |
|---|---|---|
| `just cpp-test` | Workstation, GCC 15.2 | 106/106 |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 22177 |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 129/129 |
| `just cpp-test-no-long-double` | Workstation, GCC 15.2 | 129/129 |
| `just cpp-test-no-long-double` | simbox, GCC 13 | 129/129 |
| `just test-full` | Workstation fallback | FAIL 0, WARN 0, SKIP 0, PASS 45009 |
| `just js-build` | Workstation | Exit 0 |
| `just js-test` | Workstation | 559/559, no failures or skips |
| `just site-check` | Workstation | Exit 0 |
| `sh .../timings.sh` | Workstation, one slot | Exit 0; accuracy and exterior sanity assertions passed |

The initial simbox full R run lost its connection and exited 75 after 33,957 passes; it is superseded by the completed local full run. The JS build emits the existing binom self-comparison warning; no task-specific build failure remains. Site validation regenerated an unrelated tracked screenshot, which was restored byte-for-byte from HEAD.

Appended C++/R/JS tests cover independent coefficient accuracy, reciprocal symmetry, likelihood bounds, finite constants, budget/precision stops and witnesses, power/Pareto advice, tied endpoint penalties, log collapse, subnormal spacing, huge-power input uncertainty and Feller–Pareto subtraction/translation. Tests and dossier claims agree with the limited certified scope.

Evidence:

* [validation.json](2026-11-08-gb2-power-data/validation.json) records machine, command, exit and result; [validation-logs.txt](2026-11-08-gb2-power-data/validation-logs.txt) retains compact output excerpts.
* [timings.sh](2026-11-08-gb2-power-data/timings.sh), [before.txt](2026-11-08-gb2-power-data/before.txt), [after.txt](2026-11-08-gb2-power-data/after.txt) and [environment-local.txt](2026-11-08-gb2-power-data/environment-local.txt) reproduce the paired measurements and environment.
* [high-precision.json](2026-11-08-gb2-power-data/high-precision.json), [curvature.json](2026-11-08-gb2-power-data/curvature.json) and [free-scale-bound.json](2026-11-08-gb2-power-data/free-scale-bound.json) distinguish independent numerical checks from the proof arguments.
* [source-sha256.json](2026-11-08-gb2-power-data/source-sha256.json) identifies the 14 tested source/doc/test files. [settled-files.json](2026-11-08-gb2-power-data/settled-files.json) verifies both families' goldens, d/p/q/r and truth files unchanged against the base. All five modified test files are append-only. [evidence-sha256.json](2026-11-08-gb2-power-data/evidence-sha256.json) identifies the retained research evidence.

No shared core, special-function seam, beta-prime implementation, generated d/p/q/r, code generator, roadmap, status, NEWS or board files were changed. No package installs, commits or other worktree edits were made.

Artifact display was attempted after writing the report. The viewer returned HTTP 403 because this worktree is outside its configured readable roots; the report remains available at its repository path. No viewer configuration or other checkout was changed.
