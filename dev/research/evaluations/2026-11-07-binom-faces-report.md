# TASK-116.35: signed face tails for the binomial relatives

Worktree `codex/binom-faces`, base `96e493ccafd8f9b3c5c495a394b521523a503aba`.
Final validation ran on 8 November 2026. Production-source manifest SHA256:
`c2bf735fab76b7e90f6ebada58d50ec9dcda1f3ca9c51922a4d1d2530fb9096a`.

Signed tail certificates now resolve additional positive-Poisson, truncated-Poisson and ZIP escapes. They are uniform over the probability being profiled; the ZIP arguments retain the inflation constraint. Every remaining integer size is covered before a face is declared the winner. Exact finite-window arguments also settle zero first-order gaps, including the mixed-moment example below. All four `fit.certified` flags remain `partial`, and their notes describe the actual sufficient certificates. Unsettled comparisons retain witnesses and stop unresolved, on budget, or numerically.

The final C++ development, full and double-only suites passed, including double-only runs on both machines. The focused R tests and site check passed. The full R and JavaScript runs retain one failing shared test each because existing assertions expect the former unresolved outcome. The brief forbids changing those assertions. The exact proposed test-only patch is supplied under Open; the runs are **not all green**.

## Implementation and scope

The reusable signed bound and complete prefix checker are in [ztbinom/face_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/ztbinom/face_certificate.hpp). [tbinom/face_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/face_certificate.hpp) supplies rational moment tests and the second-order bound. [zib/face_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/zib/face_certificate.hpp) supplies the inflation-uniform and localized ZIP bounds.

- **ztbinom:** a Poisson proposal must pass the signed tail and complete integer prefix. A discovered finite improvement goes through the existing retained-support finite-winner certificate.
- **zmbinom:** its positive conditional likelihood is exactly ztbinom's. The zero-indicator likelihood is independent of size and probability, including when its parameter is held. Its candidate values and bounds are transported with that additive likelihood term.
- **tbinom:** free bounds are profiled to the observed extremes. Held bounds retain their integer window. Finite and infinite upper windows use the signed argument; finite windows additionally admit exact empirical-Poisson, factorial-moment and second-order certificates. Infinite truncation endpoints remain representable in stop witnesses.
- **zib:** held inflation uses the certified ZIP face profiler and the uniform or localized bounds below. Free inflation uses the zero-modified relaxation only where its face is admissible, or a separate inactive-inflation argument at the Poisson boundary. Held zero inflation and samples without zeros reduce to the ordinary binomial, with the appropriate constant likelihood term.

Routing is in the binding-neutral common fit headers. `ZibStatus::FaceTie` is appended, preserving the earlier enum values, and maps to a structured `tie` in both R and JavaScript. It means that the winning face's rate modes cannot be ordered at the stated comparison margin; it does not assert exact equality. Finite-versus-face uncertainty remains `unresolved` with an infinite upper bound. Outer work exhaustion is distinguished from numerical failure.

Reusable helpers were added in family headers; existing common binding edits are confined to the tbinom and zib family routes. New C++ test cases are at the ends of their family files; the shared JavaScript file has 121 added lines and no deleted lines. No settled d/p/q/r, special-function, truth-table, code-generation, status or roadmap source was changed. No commits were made.

## Conditional-Poisson tail proof

Let $P$ be the empirical law on an integer conditioning window and $Q$ its Poisson law with the same mean. Write

$$c_N(k)=\sum_{j=0}^{k-1}\log(1-j/N),\qquad
 g=E_P(X)_2-E_Q(X)_2,\qquad M=E_Q(X)_3/3+E_Q(X)_2/2.$$

For a finite window ending at $U$, Jensen's inequality applied to the binomial normalizer cancels the free natural parameter because the two means agree. Uniformly over probability,

$$\frac{\ell_N-\ell_\infty}{n}\le E_Pc_N-E_Qc_N
 \le -\frac{g}{2N}+\frac{M}{N^2},\qquad N\ge2U.$$

Thus a strictly positive, padded $g$ gives a tail cutoff $N\ge\max(2U,8M/g)$. This is an eventual bound, not a statement that the integer profile is monotone or unimodal.

For an infinite upper window, applying this Jensen argument to the unmodified $Q$ would encounter its mass above $N$. Instead set $K=\lfloor N/2\rfloor$. Collapse the mass above $K$ to $K$, then move mass $d=E_Q(X-K)_+/(K-L)$ from $L$ to $K$. The resulting law $Q_K$ has the same mean and is smaller in convex order. Its second and third factorial moments are no larger than those of $Q$. Provided $d<q_L$, the uniform bound becomes

$$\frac{\ell_N-\ell_\infty}{n}\le
 D(Q_K\Vert Q)+E_Pc_N-E_{Q_K}c_N.$$

With $r=\lambda/(K+1)$ and
$H=r/(1-r)+r/((K-L)(1-r)^2)$, the geometric Poisson-tail bounds give

$$d\le\frac{q_Kr}{(K-L)(1-r)^2},\qquad
 D(Q_K\Vert Q)\le q_K(1+H)\log(1+H).$$

For $K\ge2\lambda+2$, the bound on $d$ and the entropy bound multiplied by $2K+1$ decrease with $K$. The cutoff checks these envelopes in logarithms, preserving very small tails. Together with the remainder condition, they give a likelihood gap at most $-g/(4N)$ for every larger size.

Below either cutoff, every integer from the observed maximum is evaluated or excluded by the existing convex-correction block bound. An individual comparison within the likelihood margin remains unresolved. A finite improvement must subsequently pass the existing global finite-winner certificate. No grid depth or largest searched size is accepted as a maximizer.

## Exact and second-order finite-window arguments

If the empirical proportions are themselves Poisson on a finite window, they attain the multinomial entropy bound. Reduced integer ratios $(k+1)n_{k+1}/n_k$ recognize this identity without a tolerance-based equality. On at least three adjacent support points, a finite binomial's corresponding ratios vary strictly with $k$, so it cannot attain that bound.

For a window $0,\ldots,U$, the identity
$E_Q(X)_2=\lambda(m-U)+Um$ supplies the rational candidate
$\lambda=(Um-E_P(X)_2)/(U-m)$ at zero first-order gap. Checked unsigned-integer arithmetic builds its rational Poisson weights and verifies the mean equation exactly. Arithmetic overflow refuses the optional criterion. If every factorial moment of $P$ is at least the corresponding moment of $Q$, the Newton expansion of $c_N$ proves the face bound: $\Delta^r c_N(0)<0$ for $r\ge2$. This settles counts $(1,9,6,10)$ on $0,\ldots,3$ with rate three, even though the first gap is zero and the empirical law is not Poisson.

Mixed higher moments need a stronger comparison. Define

$$a(k)=-k(k-1)/2,\quad b(k)=-k(k-1)(2k-1)/12,\quad
 h=a-E_Qa-\beta(X-m),\quad
 \beta=\frac{\operatorname{Cov}_Q(a,X)}{\operatorname{Var}_Q(X)}.$$

Then $E_Qh=E_QXh=0$ and $E_Qha=E_Qh^2$. The law $Q_N=Q(1+h/N)$ has unit mass and preserves the mean. For $N\ge\max(2U,2\max|h|)$, Jensen with this law, its relative entropy, and Taylor remainders gives

$$\frac{\ell_N-\ell_\infty}{n}\le B/N^2+R/N^3,$$
$$B=E_Pb-E_Qb-\tfrac12E_Qh^2,\qquad
 R=\tfrac23E_Q|h|^3+E_Q|hb|+E_Qa^2.$$

Indeed $c_N=a/N+b/N^2+r_N$, with $0\ge r_N\ge-(2/3)a^2/N^3$. The entropy expansion contributes $E_Qh^2/(2N^2)$ and a remainder bounded by $(2/3)E_Q|h|^3/N^3$. The change in the $b$ expectation contributes at most $E_Q|hb|/N^3$; the remaining log-product term contributes at most $E_Qa^2/N^3$.

A negative padded $B$ therefore gives a tail cutoff. The implementation uses the conservative cutoff $\lceil\max(2U,4\max|h|,4R/(-B))\rceil$, then covers the entire finite prefix. Pairwise positive sums stabilize the variance and covariance; moment and projection errors receive explicit practical padding.

For counts $(50,41,33,3,3)$, the matching rate-one Poisson proportions are $(48,48,24,8,2)/130$. Here $B=-20333/128830$. The independent rational bound gives cutoff 52; all 48 smaller admissible sizes are below the face, with maximum excess about $-0.00822180625$. The fitter now returns a proved rate-one limit. Counts $(104,0,0,608,112)$ instead have $B=20224/331351>0$ at rate four; the sufficient criterion correctly refuses them.

## ZIP tail proof and branch constraints

Let $S=\sum x_i$, $k$ be the number of positive observations, $n_0=n-k$, and $F_2=\sum x_i(x_i-1)$. At a large-size probability maximum, $u=Np$ lies in $[S/n,S/k]$. For $v=S/k$ and $N\ge2v$, the normalized binomial pmf gives, uniformly over inflation,

$$\ell_N(u,\pi)\le\ell_{ZIP}(u,\pi)+A(u,\pi)/N+C/N^2,$$
$$A=-F_2/2+uS-(k+n_0w)u^2/2,\quad
 w=\frac{(1-\pi)e^{-u}}{\pi+(1-\pi)e^{-u}},\quad
 C=Sv^2+n_0v^4/32.$$

The zero-mixture term follows by monotonicity in its log base mass and the log-sum-exp curvature bound $1/4$. In particular, $A\le-G/2$ for $G=F_2-S^2/k$. Positive padded $G$ supplies the cutoff $\max(\max(x)+1,2v,4C/G)$.

When this bound is too loose for held inflation, rate boxes either have a strictly negative upper bound on $A$, or are separated from the certified ZIP supremum by a strict likelihood gap. A tangent bounds the concave positive-count term; a chord bounds the convex zero term. Both bounds hold over the entire box. The face profiler retains every competing rate branch.

For free inflation, the zero-modified upper bound is used only when its matching face has admissible positive inflation. At a noninflated face, set $z=n_0/n$ and $t=-\log z$. In a rate box ending at $b<t$,
$q_N(u)\ge\exp(-b-b^2/N)>z$ for sufficiently large $N$, forcing the optimal inflation to zero. The ordinary binomial escape certificate then applies. Other boxes are excluded using a tangent to the positive conditional-Poisson likelihood and the Bernoulli zero-indicator upper bound. This also settles the noninflated equality-of-mean-and-variance example $(0,1,1,1,2,3,4,4)$.

For 198 zeros followed by 1 and 19, with held inflation `0.7254971052291191`, the ZIP face has two competing rate modes near `0.52230412466` and `9.98272792837`. The signed tail and prefix prove that no finite size wins, while the mode ordering remains within the numerical comparison margin: the stop is `tie` with both candidates and a finite upper bound. This binary inflation is not claimed to give exact equal maxima. The control sample of 99 zeros followed by 10 has the same face-profile shape, but its finite fit $N=10,p=1$ wins, with log likelihood `-33.0617149459`, about 2.0768 above the face. It remains a certified finite fit.

## Numerical safeguards and nonuniqueness

The certificate standard is exact arithmetic conditional on the computed comparisons, with stated practical floating-point margins, not outward-rounded interval arithmetic. Signed moment comparisons use padding based on $8192\epsilon$. Prefix comparisons use $10^{-9}n+4096\epsilon(1+|\ell_\infty|)$; face upper bounds retain their explicit rounding allowance. Close comparisons do not become exact ties or limits.

Held-inflation finite-prefix profiling required a ZIB numerical repair. The fitter now evaluates zero mixtures and scores in logarithms, partitions at the analytic turning points, and accepts actual score roots or physical boundaries. Necessary mean-interval endpoints do not compete merely because likelihoods round alike. For $N=1075,p=1/2,\pi=2^{-1074}$ and observations $(0,500)$, the former fitter log likelihood differed from the density by about 0.405465. For 1,000 observations equal to 535 and one zero, the former probability `0.4971795822385483` missed the high-precision root beyond the accuracy contract; the repaired value `0.49717957987576133` agrees with the 70-digit root. Tiny held probabilities on all-zero data now retain the exact inflation-one optimum when appropriate. Valid interior likelihoods outside floating-point range stop numerically. The [subnormal reference](2026-11-07-binom-faces-data/subnormal-reference.json) and before/after probes distinguish these checks from the tail proof.

Uniqueness is false even in simple supported subsets:

- ztbinom data consisting only of ones: $N=1$ and either $p=0.2$ or $0.8$ give the same point mass.
- zmbinom all-zero data: zero mass one makes the base parameters arbitrary.
- zib data $(0,1,1)$ at $N=1$: $(p,\pi)=(2/3,0)$ and $(1,1/3)$ give the same saturated law.
- tbinom data $(2,2,3,3)$ on the window $\{2,3\}$: every integer $N\ge3$, with $p=3/(N+1)$, attains log likelihood $-4\log2$. The implementation returns the finite member $N=3,p=3/4$, rather than declaring a limit. Real truncation endpoints also have intervals of equivalent values.

Certificate evaluation and enumeration caps use the existing work-budget multipliers and bounded iteration control. Tbinom retains its documented control-free exception: effective budget 512, hence 65,536 certificate evaluations and 51,200,000 enumeration cells; see [work-budgets.md](../../design/work-budgets.md). An exhausted or unrepresentable comparison never returns a capped estimate as `Ok`.

## Final validation and timings

| Check | Machine | Final result |
|---|---|---|
| `just cpp-test` | simbox, GCC 13 | 106/106 passed; 74.25 s |
| `UMVL_TEST_TIER=full just cpp-test` | simbox, GCC 13 | 129/129 passed; 192.39 s |
| `just cpp-test-no-long-double` | simbox, GCC 13 | 129/129 passed; 170.67 s |
| `just cpp-test-no-long-double` | workstation, GCC 15.2 | 129/129 passed; 239.74 s |
| Four-family focused R dev | simbox | 672 passed, 0 failed/warnings, 10 skipped; 2.3 s |
| `just test` | simbox | 21,475 passed, 1 protected assertion failed, 0 warnings, 154 skipped; 79.5 s |
| `just test-full` | simbox | 44,293 passed, 1 protected assertion failed, 0 warnings/skips; 336.1 s |
| `just js-build` | workstation | passed |
| `just js-test` | workstation | 491 passed, 1 protected shared test failed; 155.21 s |
| `just site-check` | workstation | passed: 99 pages, MathML, browsers, links and 99 C++ snippets |
| `python3 dev/tools/check_links.py` | workstation | 4 pre-existing destinations in unchanged files; details below |

The [70-digit checker](2026-11-07-binom-faces-data/check.py) passed 48 conditioned-likelihood inequalities, 180 monotone-envelope checks, 315 uniform ZIP checks, 336 held-inflation box checks, 112 free-inflation box checks, 189 inactive-inflation checks and five exact Newton checks. The [second-order checker](2026-11-07-binom-faces-data/second-order-face.py) independently checked exact rational coefficients, 54 sampled uniform likelihood bounds and the mixed fixture's 48 integer profiles. These numerical checks support the derivations; sampled checks alone are not uniform proofs.

All four golden files are byte-identical to the base. Their C++ coefficient and likelihood comparisons at relative tolerance $10^{-10}$ passed. See [golden-integrity.json](2026-11-07-binom-faces-data/golden-integrity.json).

Fresh paired workstation timings use GCC 15.2, C++17 `-O2`, one executing thread, three calls per measurement, excluding compilation and slot waits. The before executable uses the base revision; the after executable uses the final manifest. The shared workstation and small number of calls do not support claims about small timing differences.

| Fixture | Before, ms | After, ms |
|---|---:|---:|
| ztbinom face | 2.471 | 2.564 |
| zmbinom face | 2.422 | 2.751 |
| zib face | 2.533 | 2.021 |
| tbinom face | 1.011 | 3.485 |
| ztbinom finite | 0.332 | 0.324 |
| tbinom finite | 1.985 | 1.534 |

The tbinom face fixture pays for complete exclusion instead of an unresolved stop. Exact fixture definitions and raw measurements are in [timing.cpp](2026-11-07-binom-faces-data/timing.cpp) and [timings.json](2026-11-07-binom-faces-data/timings.json).

Final logs: [simbox](2026-11-07-binom-faces-data/final-simbox.log.gz), [workstation C++/JS](2026-11-07-binom-faces-data/final-workstation.log.gz), [site/timings/link scan](2026-11-07-binom-faces-data/final-site-timing.log.gz). The [source manifest](2026-11-07-binom-faces-data/source-manifest.json) identifies the tested production files. Workstation computation used the shared slot wrappers; the simbox suite explicitly limited its build/test concurrency to four. The historical screenshot regenerated by site-check was restored to its base bytes.

## Open

1. **Protected shared expectations prevent all-green R/JS runs.** `bindings/r/distributions/binom/test.R:235` expects `univariateML_unresolved`; `bindings/js/test.mjs:620` expects kind `unresolved`, followed by an infinite upper bound. The new proofs require `limit` and a finite upper bound. The R assertion is outside the four-family edit scope, and editing either existing JS assertion violates the shared-test append-only rule. The review applied the [three-assertion patch](2026-11-07-binom-faces-data/shared-expectations.patch) at merge. Both R tiers and the complete JS file were run to completion despite this known conflict. No production compatibility behavior was added to satisfy an incorrect expectation.

2. **Near-critical held-inflation ZIP comparisons.** For $(0,0,0,0,0,1,1,2,2,4)$, the real inflation $3e^{-1.6}/(1+3e^{-1.6})$ has a unique face rate 1.6 and zero first coefficient, with negative profiled second coefficient about $-2.72280701754$. The adjacent binary64 inflations `0.3772146069463776` and `0.37721460694637765` are different inputs: the 70-digit calculation gives first coefficients about $-2.36833\,10^{-16}$ and $+1.36263\,10^{-16}$ respectively. Both safely remain unresolved at the implemented margin. For the positive neighbor, the formal two-term size scale is about $3.99639\,10^{16}$, beyond consecutive-double integer resolution; this is not an MLE estimate. A decisive implementation needs a finer certified sign, a uniform remainder over the profiled rate, global size coverage and coefficient accuracy. Merely treating a small first coefficient as zero is wrong. See [higher-order.json](2026-11-07-binom-faces-data/higher-order.json).

3. **The sufficient finite-window criteria do not classify every higher-order boundary.** Checked rational arithmetic can refuse on overflow; general nonpositive first gaps require the existing finite certificate or an unresolved/numerical stop. There are exact boundaries beyond the negative-second-order theorem: at rate four, counts $(242187,60672,121344,1488980,406274)$ have first and second coefficients exactly zero and formal third coefficient $443772828460/24004266924673>0$. The implemented helper was tested directly on this 2,319,457-observation sample and refused the cutoff with neither budget exhaustion nor numerical failure. This diagnostic is not a full-fit result or a global finite-MLE certificate. A higher-order remainder and global finite-size comparison are missing. See [zero-second-order.py](2026-11-07-binom-faces-data/zero-second-order.py), [its exact output](2026-11-07-binom-faces-data/zero-second-order.json) and [native helper output](2026-11-07-binom-faces-data/zero-second-order.txt).

4. **Settled quantile nontermination outside the permitted edit scope.** The public all-held R call `mlzib(0,size=1e308,prob=.9,pi=0)` bypasses the native fitter through the fixed-model constructor, then requests a median. In [zib/dpqr.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/zib/dpqr.hpp), `qzib` uses `floor((lo+hi)/2)` without midpoint-overflow, stagnation or NaN-CDF guards. The targeted test remained busy; only its verified worktree-owned R worker was stopped. The native adapter now gives the appropriate numerical stop for the unrepresentable likelihood, and that native route has R/JS regressions. The public constructor path still needs the settled DPQR repair, and possibly constructor failure propagation, in a separately authorized change. See [r-quantile-nontermination.json](2026-11-07-binom-faces-data/r-quantile-nontermination.json).

The repository-wide Markdown scan additionally reports `../boundary-points.pdf` in the untouched 2026-11-03 literature report, `1+m/r` in the untouched nbinom dossier, and two `/inference/` destinations in the untouched C++ inference note. These four baseline scan findings are recorded in [link-scan.json](2026-11-07-binom-faces-data/link-scan.json); they are outside this job's edit scope.
