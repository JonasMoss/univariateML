# snorm held-parameter certificates — TASK-116.24

All seven nonempty hold subsets now use algebraic global comparisons or exact conditional solves, with terminal limit, tie, budget or numerical stops when a finite maximizer cannot be certified. The all-free degree-seven mode comparison is unchanged; it was already algebraic, as noted in the [J3b triage](2026-11-07-partial-triage-report.md). The dossier now declares `fit.certified: 'yes'` and describes the actual coverage. This means certification in exact arithmetic, conditional on the computed comparisons, with the stated practical floating-point margins. It does not assert uniqueness or outward-rounded interval arithmetic.

The source base is `7c52f905f21bee7d997d59211d4093e2593ec727`, with uncommitted changes in this worktree. The [source manifest](2026-11-07-snorm-held-data/source-sha256.txt) identifies the tested implementation, bindings, tests and checkers. The certificate header SHA-256 is `87c830fbd7331bc37ba6296d6c55573d6fe663df9ad05573ffa138b3bc50d753`; both simbox suite invocations recorded that hash. No commit, push, board edit, package install, golden update, settled d/p/q/r change or shared core-header change was made.

The implementation is in [snorm/certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/certificate.hpp), routed from `snorm_mle`. The [canonical dossier](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/README.md#details) contains the derivation, endpoint factors, numerical margins and failure contract. Routing and stop classification live in the binding-neutral snorm fitter. R and JavaScript both expose the appended `iterlim` control, defaulting to 200. Existing all-free messages are preserved; held-moment boundary messages correctly describe an endpoint constrained by those moments, rather than recommending an unconstrained half-normal refit.

| Held coefficients | Conditional calculation | Global comparison |
|---|---|---|
| None | Existing analytic side-scale profile | Existing degree-seven mode equations and both faces; unchanged |
| `mean` | Positive inverse-scale quadratic | Bernstein elimination polynomial of represented degree 54 and both constrained faces |
| `sd` | Weighted optimal mode on each observation cell | Polynomial of represented degree 27 and both constrained faces |
| `mean`, `sd` | Direct moment-constrained coordinates | Polynomial of represented degree 15 and both constrained faces |
| `xi` | Weighted mode and profiled scale | Consistent-cell exact solves; strict concavity |
| `mean`, `xi` | Positive inverse-scale quadratic | Consistent-cell exact solves; strict concavity |
| `sd`, `xi` | Weighted mode | Consistent-cell exact solves; strict concavity |
| All three | Direct evaluation with recovery checks | No free coefficient |

For the mathematical argument, write the side scales as $a,b>0$, the mode as $c$, and $m=\sqrt{2/\pi}$, $h^2=1-m^2$. The total log likelihood is

$$
\ell=n\log\frac{m}{a+b}
 -\frac12\sum_i\frac{(x_i-c)^2}{a_i^2},\qquad
a_i=a\ \text{if }x_i<c,\quad a_i=b\ \text{otherwise}.
$$

With skewness held, $a=A\sigma,b=B\sigma,c=\mu-m\sigma(B-A)$, where $A,B$ are fixed positive constants. In inverse scale and location divided by scale, the objective is strictly concave: the logarithm supplies strict curvature in inverse scale, and the piecewise quadratic loss supplies strict curvature in location. Squared residuals have matching first derivatives at the join. The linear location score and positive quadratic scale score therefore give the unique finite conditional maximum whenever scale collapse does not occur. Weighted means use anchored compensated sums; the profiled residual sum uses nonnegative within-group and between-group terms. This avoids the cancellation in a weighted raw-moment variance.

For free skewness, the rational variance-ellipse parametrization in the dossier covers every positive side-scale ratio exactly once. Free location is eliminated by its weighted score; free inverse scale is eliminated by its unique positive quadratic root. Every finite conditional maximum is consequently among the roots of the resulting observation-cell polynomials. The likelihood is continuously differentiable at moving joins, so joins do not introduce unenumerated constrained maxima. Extraneous resultant roots only add feasible comparison points. Bernstein convex-hull sign exclusion and subdivision retain all roots in exact arithmetic, including multiple roots and roots on subdivision seams. Any unresolved interval must be excluded by a likelihood upper bound or cause a numerical stop.

The two half-normal limits complete this comparison. The variance identity gives $\max(a,b)\le\sigma/h$ and $a+b\ge\sigma/h$. Thus scale tending to infinity forces the likelihood to zero uniformly in skewness. Scale tending to zero cannot maximize a nonconstant sample, or constant data away from a held mean. With sd held, escaping location also forces the likelihood to zero. The remaining maximizing escapes are exactly the two half-normal faces. Their scale scores and support constraints are solved analytically. With both moments held, a contact observation has the shrinking-side penalty $m^2/(8h^4)$, as in the [skew endpoint analysis](2026-11-07-skew-theory-report.md). When floating-point support comparisons cannot distinguish contact, strict support and infeasibility, the unpenalized nearby face remains an upper bound; a competitive unresolved face causes a stop.

Only proved denominator factors are removed. The independent [symbolic checker](2026-11-07-snorm-held-data/endpoint-factors.py) verifies the following orders at a shrinking side; reflection gives the other endpoint. Extra special-case factors are not removed by tolerance.

| Free skewness, held moments | General nonempty side | Constant nonzero side | Zero side | Empty side |
|---|---:|---:|---:|---:|
| `sd` only | 1 | 4 | 4 | 8 |
| `mean` only | 0 | 3 | 6 | 12 |
| Both moments | 0 | 0 | 0 | 3 |

A zero side has normalized observations equal to zero; with mean held, these observations equal that held mean. With mean free, the constant-side order is independent of its value. [Saved symbolic results](2026-11-07-snorm-held-data/endpoint-factors.txt) verify the structural orders. Bernstein endpoint division also checks its computed remainder before proceeding.

Uniqueness fails in both finite and boundary comparisons. Constant data with sd held and mean free have the sharp per-observation density bound $mh/\sigma$, approached on both opposite half-normal faces. The implementation reports the two distinct closure maximizers as a tie. Constant data $x\ne\mu$, with mean held and sd free, instead have the sharp bound $m^2/|x-\mu|$: the selected limit ends at $x$, has half-scale $|x-\mu|/m$, and is reflected when $x>\mu$. This case is solved before root isolation, even at `iterlim=1`; it is distinct from the unbounded scale collapse when the held mean equals the data.

A finite counterexample is $x=(-1,0,1)$, held mean zero and held sd $151/200$. Both half-normal endpoints lie strictly inside the sample extremes and are infeasible. The [exact rational check](2026-11-07-snorm-held-data/nonunique.py), using bounded Machin, square-root and logarithm calculations, proves

$$
0.07685<\ell(6)-\ell(1)<0.07686.
$$

Reflection gives $\ell(\xi)=\ell(1/\xi)$, and both tails tend to minus infinity. A finite global maximum exists, cannot occur at one, and hence has a distinct reciprocal partner. This proves at least two finite global maxima without relying on the fitter or a grid. The C++, R and JavaScript tests retain both finite witnesses. [Saved proof output](2026-11-07-snorm-held-data/nonunique.txt) records the strict inequalities.

The numerical checks are deliberately separate from those exact-arithmetic arguments. Root boxes have rational-parameter width at most $2\times10^{-14}$; public-coordinate variation must be at most $10^{-7}(1+|\theta|)$, and enclosed likelihood width at most $10^{-8}(n+|\ell|)$. Computed endpoint-factor remainders use $8192\epsilon_{double}(d_p+1)\|p\|_\infty$. Near a possible rounded turning point, the Bernstein guard uses $8\epsilon_w(d_p+1)\|p\|_\infty$. Its unresolved boxes can be discarded only using the unrestricted-location-and-scale profile upper bound derived in the dossier. This is important in double arithmetic: demanding unnecessary resolution of a dominated turning point otherwise prevents a valid golden fit.

Every recovered finite point is checked against the actual public density arithmetic. The standardized-residual error allowance is $16\epsilon_{double}(1+|L|+|M|)$, with the shorter side used if the possible error crosses the join. Its propagated likelihood error must be at most $10^{-10}(n+|\ell|)$, and natural/public likelihood disagreement at most $10^{-10}(1+|\ell|)$. Fixed-skewness profile peaks and reconstructed half-normal faces have additional likelihood agreement checks. Finite comparison ties use $10^{-9}(n+|\ell|)$; the historical face comparison allowance remains $10^{-8}n$. These are practical margins, not outward rounding.

No capped skewness or scale is returned. Near-endpoint evaluations used for extreme held moments are diagnostic lower witnesses only. Faces are evaluated before interior isolation, so stops retain available witnesses. Unclosed searches retain an infinite global upper bound; unresolved interior segment bounds are identified separately. Enumeration and certificate caps use the shared `100000 * min(iterlim,100000)` and `128 * min(iterlim,100000)` budgets; solver depth uses `min(iterlim,100000)`. Exhausted enumeration budgets are classified separately from arithmetic/root-enclosure failures. No optimizer fallback is used. The old `snorm_detail` search helpers remain solely because existing `sstd` and `sged` code consumes them; removing them broke the initial integration build and was reverted within this uncommitted work.

Two numerical decisions have retained evidence. A root width of $2\times10^{-12}$ passed a parameter-only check but failed the held-mean golden likelihood enclosure; the [initial conformance failure](2026-11-07-snorm-held-data/initial-conformance-failure.txt) motivated the tighter width. Initial no-long-double checks also exposed lost subdivision-seam roots and lost extreme finite/face witnesses; [failure evidence](2026-11-07-snorm-held-data/initial-no-long-double-failure.txt) is retained. One initial even-root test itself formed a polynomial through rounded thirds, which did not preserve its intended exact roots. The final tests use exact dyadic coefficients and separately require a non-dyadic rounded even-root case to remain unresolved rather than certify an empty root set. Distinct observations lost in normalization, weighted-variance cancellation, uncertain endpoint contact and face-coordinate recovery now have explicit guards.

All final commands finished before this report was written. Long R/C++ suites ran on simbox through the supplied helper; the required GCC 15 no-long-double run, WASM and site checks ran on the workstation through the slot wrapper. BLAS/OpenMP were pinned to one thread and runs used `nice -n 10`. Compiler and runtime versions are saved for the [workstation](2026-11-07-snorm-held-data/environment.txt) and [simbox](2026-11-07-snorm-held-data/environment-simbox.txt).

| Final run | Machine | Result |
|---|---|---|
| Targeted snorm C++ | Workstation | 27 cases, 12,333 assertions passed; 2 other cases outside the filter |
| `just cpp-test` | Simbox, GCC 13 | 106/106 dev shards |
| `UMVL_TEST_TIER=full just cpp-test` | Simbox, GCC 13 | 129/129 full shards |
| `just cpp-test-no-long-double` | Simbox, GCC 13 | 129/129 shards |
| `just cpp-test-no-long-double` | Workstation, GCC 15 | 129/129 shards; 237.26 s test wall time |
| `just test` | Simbox, R 4.6.1 / g++-15 | FAIL 0, WARN 0, SKIP 154, PASS 21,257 |
| `just test-full` | Simbox, R 4.6.1 / g++-15 | FAIL 0, WARN 0, SKIP 0, PASS 44,072 |
| `just js-build && just js-test` | Workstation, WASM | 472 passed; zero failures or skips |
| `just site-check` | Workstation | Passed, including 99 family pages/snippets and MathML checks |

The R dev skips are tier exclusions, not permitted failures. The full tier has no skips. Build output contains nonfatal compiler style diagnostics and shell startup notices; the zero-warning counts above are testthat's counts. Compact final logs preserve the [simbox runs](2026-11-07-snorm-held-data/final-simbox-tests.txt), [workstation no-long-double run](2026-11-07-snorm-held-data/final-workstation-no-long-double.txt), [native target](2026-11-07-snorm-held-data/final-native-target.txt), [JavaScript run](2026-11-07-snorm-held-data/final-js-tests.txt) and [site check](2026-11-07-snorm-held-data/final-site-check.txt). The site check regenerated an unrelated tracked screenshot; its original HEAD bytes were restored afterwards.

The independent deterministic grid check compares two held-moment fits against 4,001 skewness values each. Fixed-skewness tests span data units from $10^{-30}$ to $10^{30}$ and compare extreme two-point profiles against an analytic optimum, accepting a numerical stop when public coordinates cannot be verified. These checks supplement the algebraic argument; a grid does not prove globality.

The [strict golden checker](2026-11-07-snorm-held-data/golden-check.cpp) fits all eight hold masks on the canonical 500-observation fixture, holding coefficients at their golden values. It requires status `Ok`, scaled coefficient error at most $10^{-10}$, and likelihood error at most $10^{-10}(1+|\ell_{golden}|)$. All masks pass in [native](2026-11-07-snorm-held-data/golden-check-final-native.txt) and [double](2026-11-07-snorm-held-data/golden-check-final-double.txt) arithmetic. Maximum scaled coefficient errors are $3.53\times10^{-14}$ and $1.19\times10^{-13}$, respectively; absolute likelihood errors are below $2\times10^{-12}$. Canonical goldens and the all-free fitting algorithm are unchanged.

The before/after timing workload uses 100 standard-normal draws from `std::mt19937(11624)`, held values mean 0.2, sd 1.4 and xi 1.3, and three calls per mask. Both versions compile with GCC 15, `-O2 -std=c++17`; the baseline header is extracted read-only from `7c52f905`. The [runner](2026-11-07-snorm-held-data/run-timings.py) and [workload](2026-11-07-snorm-held-data/timing.cpp) reproduce the measurements. These are small shared-workstation measurements without confidence intervals; all timed fits returned `Ok`.

| Held coefficients | Before, ms/fit | After, ms/fit |
|---|---:|---:|
| None | 0.3681 | 0.3683 |
| `mean` | 299.5 | 121.1 |
| `sd` | 121.1 | 16.45 |
| `mean`, `sd` | 1.331 | 5.211 |
| `xi` | 18.04 | 0.01416 |
| `mean`, `xi` | 0.3279 | 0.01241 |
| `sd`, `xi` | 0.3073 | 0.01116 |
| All three | 0.006230 | 0.009149 |

The both-moment comparison costs more because it covers the entire shape interval and verifies recovered likelihoods. Exact held-skewness solves remove the old nested search cost. Raw [before](2026-11-07-snorm-held-data/timing-before.txt), [after](2026-11-07-snorm-held-data/timing-after.txt) and [after-double](2026-11-07-snorm-held-data/timing-after-double.txt) measurements are retained.

Reproduction uses the repository's usual temp-directory environment (`TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` set to `$PWD/build/tmp`). The saved final logs give the exact suite invocations. The additional checks can be run under one reserved slot as follows:

```sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-snorm-held-data/nonunique.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-snorm-held-data/endpoint-factors.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-snorm-held-data/run-timings.py
```

Compile `golden-check.cpp` with `-O2 -std=c++17 -IunivariateML-cpp/include -IunivariateML-cpp/tests`, once normally and once with `-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`, and run from the repository root. The checker returns nonzero on any failed mask. `sha256sum -c dev/research/evaluations/2026-11-07-snorm-held-data/source-sha256.txt` verifies the tested source manifest. Diff whitespace and append-only test checks passed.

Open numerical reach, outside the settled-files scope: the public normalizer in [special/skewed_densities.hpp](../../../univariateML-cpp/include/cppunivariate/special/skewed_densities.hpp) squares xi, so extreme finite values such as $10^{200}$ or $10^{-200}$ overflow or underflow that intermediate. Moment-to-mode cancellation can make a much less extreme optimum unverifiable in public double coordinates. The new fitter detects these failures and stops numerically; it does not return the otherwise plausible natural-coordinate optimum. Extending this reach requires a separate stable public-evaluation/coordinate investigation, with corresponding d/p/q/r checks. Those settled files were not changed. There is no unimplemented hold subset remaining in this job.
