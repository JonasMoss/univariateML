# TASK-116.22: fatigue, shifted Gompertz and generalized beta certificates

The requested subsets are implemented and the final required suites passed. Fatigue and shifted Gompertz now have `fit.certified: 'yes'`; generalized beta remains `partial`, with the newly certified power-only subset identified explicitly. No golden files or tolerances changed. The three default golden fits retain their coefficients and log likelihoods exactly.

Source: base `017b9f8ec3bed7610ccd41c62738bce1ea987316` plus the uncommitted changes in `codex/scalar-trio`. [The source manifest](2026-11-07-scalar-trio-data/source-manifest.json) records SHA-256 hashes of every changed implementation, dossier and test file. No commit, push, package installation, board edit or other-worktree edit was made.

## Scope and return contract

| Family | Newly certified work | Remaining scope |
|---|---|---|
| fatigue | Held-alpha scale fit; profiled-alpha scale fit; analytic held-beta fit, with original-data evaluation and reconstruction checks | Estimating the threshold remains structurally excluded. |
| sgomp | Held-eta rate root; complete compact joint profile subdivision | Joint uniqueness is not asserted. Held-rate shape fitting and the exponential face remain supported. |
| genbeta | Free power with endpoint and both beta shapes held, for every positive held shape | Other free-power nuisance profiles remain numerical; see the open entry below. |

Certificates use the package convention: exact-arithmetic arguments conditional on the computed bounds and comparisons, with explicit practical floating-point margins. They are not outward-rounded interval proofs. Failure to complete the required comparisons returns a stop rather than a capped estimate. Numerical, comparison-tie and certificate-budget routing lives in the three binding-neutral `fit.hpp` files, so R and JS share the classification. Existing nonexistence and limit-face routing is retained.

New C++ cases and shared JS cases are appended. No shared implementation header, d/p/q/r source, `special/`, truth table, code generator, NEWS, status file or roadmap was changed. Family dossiers contain the formulas, margins and search contracts; this report records the validation and decisions.

## Fatigue: complete rational-score enumeration

For shifted positive observations $t_i$, write $A=\overline t$, $H=1/\overline{1/t}$, and $s_1(b)=\sum_i(t_i+b)^{-1}$. With alpha held, the scale score has the sign of

\[
S(b)=2\alpha^2b^2s_1(b)-n\alpha^2b+nA-(n/H)b^2.
\]

For one observation, the log-scale score is
$\tfrac12\tanh(u/2)-\sinh(u)/\alpha^2$, where $u=\log(b/t_i)$. Consequently every maximum lies in
$[\min(t)r_-,\max(t)r_+]$, with
$r_+=\{\alpha/2+\sqrt{\alpha^2/4-1}\}^2$, $r_-=1/r_+$ for alpha above two, and both equal to one otherwise. All individual scores point inward outside this interval; the likelihood tends to minus infinity as the scale approaches zero or infinity.

Profiling alpha gives $\hat\alpha^2=A/b+b/H-2$. The remaining scale score has the sign of

\[
F(b)=b^2-2Hb+HA+(H-b)\frac{n}{s_1(b)}.
\]

It is positive below $H$. Since $n/s_1(b)\ge b+H$, it is negative above $A$. Thus all profiled maxima lie in $[H,A]$. These are finite search domains, not chosen log-coordinate caps.

The new family-private `certificate.hpp` bounds the rational score and its derivative on each segment. A score interval excluding zero removes the segment. A derivative interval with strict sign proves monotonicity; increasing roots are minima, and decreasing signed brackets are bisected. Ambiguous segments are subdivided. A root that cannot be resolved stops numerically or on budget. This establishes completeness without assuming that sampled sign changes find every root, and without expanding a potentially ill-conditioned degree-$n+2$ polynomial.

The interval operations have a practical allowance of $16\epsilon$ times the endpoint magnitude. Root brackets shrink to relative width $2\cdot10^{-13}$. Distinct maxima within $10^{-10}n$ log likelihood stop as a comparison tie. Each enumeration permits $128\min(\mathrm{iterlim},100000)$ segment evaluations; each root solve permits $\min(\mathrm{iterlim},100000)$ iterations.

**The held-alpha likelihood is not generally unimodal.** Two exact counterexamples invalidate a monotone-root interpretation of the earlier analysis:

* For $t=1,\alpha=3$, the score numerator, after multiplying by the positive denominator, is proportional to $-(b-1)(b^2-7b+1)$. The two global maxima are $(7\pm3\sqrt5)/2$; $b=1$ is a minimum.
* For $t=(1/2,2),\alpha=3$, it is proportional to $-(b^2-1)(10b^2-47b+10)$. The two positive maxima are $(47\pm3\sqrt{201})/20$, separated by the minimum at one.

In each example reciprocal symmetry of the likelihood makes the two maxima exactly equal. Their gains over $b=1$ are approximately 0.1276873303 and 0.09500362925, respectively. [The 60-digit reproducer and results](2026-11-07-scalar-trio-data/counterexamples.py) corroborate these algebraic counterexamples; numerical equality alone is not the proof. The historical analysis is retained as historical evidence; the current dossier supersedes its monotone-solve interpretation.

With beta held, alpha is evaluated as the RMS of original-data standardized differences. This avoids both cancelling moments and loss of small differences during normalization. Apparent collapses created by rounded threshold subtraction stop numerically, rather than falsely asserting nonexistence. An unresolved near-coincident (A,H) bracket also stops, because a narrow beta interval does not establish alpha accuracy. A reconstructed free beta must retain relative accuracy $2\cdot10^{-13}$, including at subnormal scales.

The historical joint Newton result is retained only after the complete enumeration finds the winning root, both coordinates agree relatively within $10^{-8}$, and likelihoods agree within $10^{-10}n$. A failed historical proposal is not accepted through this path. This preserves default golden rounding while making the independent certificate responsible for acceptance.

## Shifted Gompertz: scalar concavity and compact joint bounds

Put $v_i=e^{-bx_i}$, $a_i=1-v_i$. Holding eta gives

\[
\ell_{bb}=-n/b^2-\eta\sum_i x_i^2v_i
-\eta(1+\eta)\sum_i\frac{x_i^2v_i}{(1+\eta a_i)^2}<0.
\]

For positive sample sum the likelihood tends to minus infinity at both rate ends. A signed expanding bracket followed by bisection therefore isolates the unique global rate maximum, to relative width $2\cdot10^{-13}$. Eta zero uses the exact exponential estimate. All-zero data with free rate stop for unbounded likelihood.

For the joint fit, the existing mesh and local refinements only propose a candidate. For $m=\min x_i$, the global envelopes

\[
\ell\le n\log b+n\log n-b(\sum x_i-nm),\qquad
\ell\le n\log b+n\quad(b\max x_i\le1)
\]

exclude both rate tails for nonconstant samples at the candidate's likelihood level. The upper-tail coefficient is strictly positive. At bounded rates, increasing eta eventually reduces the likelihood without bound. Constant positive samples with both coefficients free have an unbounded concentration path and stop instead.

The conditional eta likelihood is strictly concave. Its maximizing eta is nondecreasing in rate because

\[
C=\ell_{b\eta}=\sum_i x_iv_i\{1+(1+\eta a_i)^{-2}\}>0,
\qquad D=-\ell_{\eta\eta}=\sum_i\frac{a_i^2}{(1+\eta a_i)^2}>0.
\]

Endpoint eta solutions therefore enclose the entire profile on a rate segment. Bounding these expressions and the negative terms in $\ell_{bb}$ gives an upper bound $M$ for the profile curvature $\ell_{bb}+C^2/D$. On the eta-zero face the curvature is $\ell_{bb}$, so the same upper bound remains valid. The profile has a continuous first derivative at the face transition.

A Taylor bound about the segment midpoint uses $\max(M,0)$, an absolute slope bound, and the residual conditional-root tangent error. Separate uniform score bounds exclude segments without stationary points. Nonfinite curvature or a nonpositive computed denominator stops numerically; a NaN cannot silently become zero curvature through `max`.

Every remaining small possible-winning region is retained until the final lower witness is known. Thus improving the proposal cannot invalidate earlier retained coordinate boxes. Success requires **all** surviving regions to fit within relative rate error $10^{-7}$ and eta error $10^{-7}\max(1,\eta)$ of the returned coefficients. Separated contenders stop as a comparison tie. This replaces a preliminary local negative-curvature shortcut: acceptance now propagates the conditional-root uncertainty into the global coordinate enclosure, without presuming joint uniqueness.

The conditional certificate tolerance is $\min(\mathrm{reltol},10^{-13})$; log-eta brackets are inflated by twice this plus $10^{-12}$. The practical likelihood margin is $512\epsilon(n+|\ell|)$. Retained regions have relative rate width at most $10^{-9}$, eta width at most $10^{-8}\max(1,\eta)$, and upper-bound excess over the center at most twice that margin. The final global likelihood upper bound must be within eight margins of the returned value. Endpoint shape solves are reused. The nomination mesh and subdivision share the certificate budget $128\min(\mathrm{iterlim},100000)$; scalar loops use the solver budget. An unfinished outer bracket is a numerical stop, which can precede certificate-budget exhaustion.

## Generalized beta: the held-shapes power root

With endpoint $s$ held, set $y_i=\log(s/x_i)>0$, beta shapes $p,q>0$, and power $\tau>0$. Apart from constant terms,

\[
\ell(\tau)=n\log\tau-p\tau\sum_i y_i
+(q-1)\sum_i\log(1-e^{-\tau y_i}).
\]

Its second derivative is

\[
\ell''(\tau)=-n/\tau^2-(q-1)\sum_i
\frac{y_i^2e^{\tau y_i}}{(e^{\tau y_i}-1)^2}.
\]

For $q\ge1$ this is negative. For $0<q<1$, $[z/(2\sinh(z/2))]^2<1$ implies $\ell''<-nq/\tau^2<0$. The likelihood tends to minus infinity at both power ends, proving a unique finite global maximum for every positive pair of held shapes. Signed bracket expansion and bisection replace the generic mesh for this subset. The root has relative bracket width $2\cdot10^{-13}$; exhausting either solver loop stops numerically, with a finite witness when available.

For small $q$, the dimensionless score uses a series for $z/\operatorname{expm1}(z)-1$. When $p=1/2$, the linear term is combined analytically, preserving the quadratic balance even for $q=10^{-300}$. Large $q$ weights the exponential tail before underflow; the test with $q=e^{705}$ has its root beyond $z=700$. Omitting the denominator correction only beyond $z=50$ has relative effect below $2\cdot10^{-22}$. The attained $q=1$ product ridge keeps its existing handling. Neither the default unrestricted fit nor its golden changed.

## Validation and timings

[The validation record](2026-11-07-scalar-trio-data/validation.json) and compressed [simbox](2026-11-07-scalar-trio-data/simbox-final.log.gz) and [workstation](2026-11-07-scalar-trio-data/workstation-final.log.gz) logs preserve the final commands and outcomes.

| Final command | Machine | Result |
|---|---|---|
| `just cpp-test` | simbox | 106/106 |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 129/129 |
| `just cpp-test-no-long-double` | simbox | 129/129 |
| `just test` | simbox | 21,026 passes; 154 tier skips; 0 failures, 0 warnings |
| `just test-full` | simbox | 43,840 passes; 0 skips, failures or warnings |
| `just js-build && just js-test` | workstation | 446/446 |
| `just site-check` | workstation | Green; 99 family pages and 99 compiled C++ snippets |

Simbox used WSL2 Ubuntu, standalone GCC 13.3, R 4.6.1 and GCC 15.2 for the R package. Workstation timings used GCC 15.2 on Linux; site checks used Python 3.14.5 and SymPy 1.14.0. Long suites ran through `simbox-run` under nice priority, with four build/test workers and one BLAS/OpenMP thread per worker. Workstation computations used the shared slot runners; JS/site ran sequentially under `run4`. No final run needed the simbox-unreachable fallback. Existing compiler warnings and the recipes' Boost-header fallback are visible in the logs; they are not failed test assertions.

The independent reproducer is `run-checks.sh` in the evidence directory, invoked from the worktree root through `/tmp/umvl-cpu/run`. It uses seed 11622, 24 samples of size eight with three lognormal dispersion levels, and 18 generalized-beta held-shape combinations including the extreme cases. [Independent results](2026-11-07-scalar-trio-data/independent.json) use 60 decimal digits, increasing to 400 for extreme beta shapes. Maximum held-alpha fatigue score error was $9.35\cdot10^{-12}$; profiled scale score error was $1.30\cdot10^{-15}$. Gompertz rate/shape score errors were $4.17\cdot10^{-8}$ and $4.89\cdot10^{-11}$; its largest likelihood discrepancy was $3.16\cdot10^{-15}$. Ordinary genbeta score error was $4.70\cdot10^{-12}$; the tiny-$q$ relative score error was $5.47\cdot10^{-13}$. The huge-$q$ relative score error was $3.19\cdot10^{-11}$. All likelihood comparisons and asserted tolerances passed. Rounded-translation cases distinguish numerical failure from genuine nonexistence.

[Separate 60-digit envelope checks](2026-11-07-scalar-trio-data/bounds.json) checked eta monotonic bounds, curvature bounds and Taylor likelihood bounds on 16 boxes at 64 points. All passed. These numerical checks support implementation agreement; the algebra above supplies the global arguments, not the finite point checks.

Timing reproducer: `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-scalar-trio-data/timings.py`. It builds frozen base and working fitters with C++17 `-O2`, uses identical 500-observation golden samples (seeds fatigue 13086, sgomp 12895, genbeta 13071), and averages 20 fits per workload. [Raw timings and coefficients](2026-11-07-scalar-trio-data/timings.json) record source hashes. Milliseconds per fit:

| Workload | Before | After |
|---|---:|---:|
| fatigue default | 0.0468 | 0.7271 |
| fatigue alpha held at 3 | 13.2036 | 0.5721 |
| fatigue beta held at 2 | 0.1066 | 0.0189 |
| sgomp default | 24.9085 | 61.4194 |
| sgomp eta held at 2 | 14.3601 | 0.4894 |
| sgomp rate held at 1 | 0.1868 | 0.1812 |
| genbeta default | 3.6098 | 3.5944 |
| genbeta beta shapes held at 2, 0.7 | 2.5787 | 0.1638 |
| genbeta beta shapes held at 2, 2 | 2.6137 | 0.1965 |

These are workstation measurements under shared load, not runtime guarantees. Joint certification costs additional time; the new scalar routes are faster. Default coefficient and likelihood differences were exactly zero for all three families. Changed scalar estimates differ from the previous optimizer by at most $5.54\cdot10^{-8}$ in these workloads, with likelihood differences at most $1.14\cdot10^{-12}$.

Earlier red runs are retained separately: [golden drift](2026-11-07-scalar-trio-data/preliminary-golden-failures.log.gz), [YAML metadata typing](2026-11-07-scalar-trio-data/preliminary-metadata-failure.log.gz), and [an incorrect new stop-phase expectation](2026-11-07-scalar-trio-data/preliminary-test-expectation-failure.log.gz). Golden drift was corrected by retaining independently validated historical proposals, not by editing references or weakening tests. Quoting the YAML `yes` avoids its conversion to a boolean by the R parser. The low-iteration test now expects the numerical outer-bracket stop it actually exercises. No failures are waived in the final results. The 154 dev skips are tier selections; full has none.

## Open entries

1. **Other genbeta free-power profiles.** When a beta nuisance shape is fitted along with power, the old mesh is not a global certificate. Strict concavity with both shapes held does not prove concavity after profiling them. Certification needs uniform small-power bounds against the gamma face, large-power bounds against the moving-endpoint face where applicable, and complete interior contender isolation. These bounds are still missing; the existing face comparisons and finite mesh are insufficient. Those routes and their default output were preserved, and the family flag remains `partial`. No broader global or uniqueness claim is made.

2. **Settled fatigue density helper, outside the authorized edit scope.** [A standalone reproducer](2026-11-07-scalar-trio-data/dpqr-gap.cpp) takes beta $10^{300}$, data `beta` and `nextafter(beta, +Inf)`, and fits alpha with beta held. The fitter returns alpha $1.051479739723696\cdot10^{-16}$ and log likelihood $-1310.8066067830262$. Summing the existing `dfatigue` gives $-1309.8066067830262$, exactly 1 too high; [the recorded output](2026-11-07-scalar-trio-data/dpqr-gap.txt) confirms this. `fatigue_w` subtracts two rounded logarithms and loses the adjacent-data difference. The original-difference fitter calculation retains the quadratic contribution. The needed follow-up is a stable relative-difference calculation in `fatigue_w`, with density/CDF and inversion validation under TASK-78 ownership. `dpqr.hpp` and generated R d/p/q/r were deliberately left untouched as instructed. This is a concrete density-evaluation gap, not evidence against the scalar likelihood argument or permission to return an unchecked fit.

