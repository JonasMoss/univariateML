# Beta-prime all-free global certificate — TASK-116.46

The all-free fitter now closes the joint exterior and the remaining compact shape cover before returning a finite estimate or selecting a face. Finite success requires enclosures in both shapes and scale. Unresolved arithmetic, comparisons and budgets stop with witnesses. The family is marked `fit.certified: "yes"`; its note describes this scope. All required final suites passed on the frozen source. Final validation completed on 2026-11-09 in Europe/Oslo; this report retains the requested filename.

The baseline is `9fee8c34b838db7b11b7518a071f61495d23f282`. No commit was made and no golden was edited.

**Certificate and proof.** The implementation is in [joint_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/joint_certificate.hpp), using [joint_bounds.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/joint_bounds.hpp). The family-specific proofs and practical floating-point margins are recorded in the [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md).

The scale-uniform harmonic-shape bounds exclude both small harmonic shape and simultaneous large shapes, uniformly over scale. With harmonic shape retained, either large individual shape has its other shape bounded. Uniform gamma and inverse-gamma remainders then give finite exterior cutoffs. There is no missing nondegenerate lognormal corner: the variance of log X is trigamma(p) + trigamma(q), independent of scale, and tends to zero when both shapes diverge. The stronger likelihood bound excludes this coupled escape for every nonconstant sample. Constant positive samples instead have unbounded likelihood and produce a structured `nomle` stop.

For the compact cover, the implementation retains cells in both p and q rather than assuming a single-valued conditional maximizer p(q). This encloses every possible conditional branch on each q interval. The unchanged q-held certificate supplies a global conditional anchor at a proposed q; its upper bound includes all branches and its inverse-gamma face. An enclosing q-score integrates that anchor across cells containing it. The remaining cover is independently exhaustive, including cells without an anchor.

At fixed shapes the log-scale score is strictly decreasing. Its root decreases in p and increases in q, so two corner roots enclose every maximizing scale in a shape cell. Digamma-increment and scale enclosures bound both shape scores throughout that cell. The cover discards a cell only through a proved exclusion: a score cannot vanish; a concave-normalizer supporting plane puts its value below an incumbent; a signed tail excludes it; an interval-Newton image excludes or contracts its stationary set; or an enclosed positive curvature direction excludes a local maximum. Every competitive finite maximum is interior to the exterior cutoffs and therefore stationary.

The scale-profile Hessian is the Schur complement, with Q the mean of u(1-u):

\[
 H_{pp}=\psi_1(p+q)-\psi_1(p)+\frac{q^2}{(p+q)^3Q},\quad
 H_{qq}=\psi_1(p+q)-\psi_1(q)+\frac{p^2}{(p+q)^3Q},\quad
 H_{pq}=\psi_1(p+q)-\frac{pq}{(p+q)^3Q}.
\]

Corner scale bounds enclose Q; monotonicity encloses the trigamma differences. Around a proposed maximum, a symmetric negative-definite approximate inverse C and the checked bound `||I-CH||_infinity < 1/2` prove contraction and negative definiteness. An explicitly checked componentwise self-map enclosure proves existence and uniqueness of that stationary point. Monotone corner scale roots enclose its scale. A full cell is assigned to that maximum only after the inclusion checks succeed. Other maxima remain separate candidates and are compared globally.

Finite acceptance requires relative coefficient errors at most `min(tol, 1e-6)` in p, q and b, including scale reconstruction. Failure of the accuracy inclusion is a numerical stop, even if a proposal has an excellent score. The old multistart is now only a metered witness generator; its shape guard does not cap the certified search. There is no successful fallback to its unchecked output.

A sharper signed tail closes difficult gamma-face covers. Let G be the gamma profile, a* its enclosed optimum, h the retained upper p bound, u = max(h, a*_upper), and kappa = 1/(2u²). Trigamma(p) > 1/p + 1/(2p²) gives

\[
 G(p)\le G(a_*)-\kappa(p-a_*)^2/2.
\]

For normalized second moment V, the leading tail coefficient is A(p) = p{p(V-1)-1}/2. An upper bound A0 over the optimum enclosure and B = 1/2 + u(V-1) give A(p) <= A0 + B|p-a*|. Completing the square yields

\[
 F(p,q)\le G(a_*)+A_0/q+\{C(h)+B^2u^2\}/q^2.
\]

When A0 < 0, a strict cutoff above both 2hM and {C(h)+B²u²}/(-A0) excludes the whole strip below the gamma supremum, including intervals on which A(p) changes sign. Narrower cells also use their gamma likelihood loss and the uniform signed remainder. Reciprocation supplies the inverse-gamma argument. A face is selected only after the finite cover closes and its likelihood bounds separate it from every competitor. A tail sign alone cannot select it.

These are exact-arithmetic certificates conditional on the computed comparisons, with the practical floating-point margins stated in the dossier. They are not outward-rounded interval arithmetic. Independent high-precision checks below are numerical evidence, separate from the proof.

**Nonuniqueness and stops.** No theorem of arbitrary-sample joint uniqueness is assumed or claimed. The existing [held-shape counterexample and continuity argument](2026-11-07-betapr-prof-report.md) remain applicable: conditional profiles can have separated maxima, including a two-maxima tie. The cover retains branches rather than choosing one conditional root. Closed-cover maxima whose likelihood enclosures overlap produce a comparison-tie stop with witnesses.

For symmetric face comparisons such as `{0.5,1,2}`, an early unresolved comparison retains both proper face witnesses and a finite witness, with infinite global upper bound. It does not assert that either face wins or that all maxima were enumerated. This follows the package's [diagnostic tie contract](../../design/structured-stop-details.md): a tie includes numerically unresolved comparisons and does not assert an exact mathematical tie. Budget and numerical stops likewise retain witnesses and never select an unproved face.

All supported subsets are covered: fully held likelihood evaluation; either single free shape by its decreasing score; free scale by its unique log-scale root; both shapes with scale held by beta concavity; the two one-held-shape profiles by the existing conditional certificate and unit-shape delegates; and all three free by the new cover. There is no remaining unimplemented all-free certification stage. Degenerate inclusions, unresolved exterior signs or insufficient floating-point separation can correctly stop numerically; exhausted work stops on budget.

**Accuracy and compatibility.** An independent 80-digit solve of the three original stationary equations gives the golden maximizer

| coefficient | independent value |
|---|---:|
| p | 2.014271443297261273 |
| q | 3.260009867187485947 |
| b | 1.059410449292310428 |

Its log likelihood is approximately -439.615859932363092128. The oracle lies inside the C++ enclosures:

| coefficient | lower | upper |
|---|---:|---:|
| p | 2.014271443152606 | 2.014271443452208 |
| q | 3.260009866722711 | 3.260009867624023 |
| b | 1.059410449014113 | 1.059410449552813 |

The independent value's largest relative difference from a stored golden coefficient is 7.74e-10, below the 1e-6 golden tolerance. The golden remains unchanged. The [oracle script and result](2026-11-08-betapr-joint-data/coefficient-oracle.json) record the independent value and enclosure check; [coefficient-oracle.py](2026-11-08-betapr-joint-data/coefficient-oracle.py) reproduces it. Globality comes from the cover, not from the independent root solve. The [80-digit exterior checks](2026-11-08-betapr-joint-data/independent-check.json) also passed: 240 sampled inequalities, 24 profiled unsigned-tail comparisons and 13 signed-tail comparisons.

`betapr/certificate.hpp`, the `betapr_mle_holds` interface, and `gb2/power_certificate.hpp` are unchanged. GB2's held-power transport continues to use that conditional interface. Existing conditional and GB2 tests passed in the final suites. The new all-free routing is binding-neutral in the [common beta-prime binding](../../../bindings/common/distributions/betapr/fit.hpp); R and JS use the same core. Fixed-scale fits also explicitly require the delegated beta solver's successful status. No failed optimizer is rescued by a score check.

**Budget and performance.** A shared ledger covers proposal objective/gradient calls, conditional-certificate work, exterior work, new profile points and cell/Hessian evaluations. Its cap is `128 * min(iterlim,100000)`. Nested solver/refinement loops have their own `min(iterlim,100000)` caps. Cached points are reused.

The all-free C++ default and R/common-binding default `iterlim` are now 2000, following the work-budget policy for ordinary seeded samples. The current R seed-12, n=20 sample needs 139352 evaluations on simbox to certify its gamma limit, above the former 128000 allowance. It is now an appended C++ regression. The conditional C++ interface keeps its existing default. Low-budget stops retain finite witnesses; the strict `tol=1e-14` regression stops numerically rather than accepting a wider enclosure.

The certified path is substantially slower than the old multistart. These are medians of three fit-only timings, with sampling and compilation outside the timer, using `c++ -std=c++17 -O2`. The baseline fitter is taken from the stated base commit.

| machine / sample | old multistart, ms | certified, ms | slowdown |
|---|---:|---:|---:|
| simbox, golden n=500 | 7.183 | 100.379 | 13.97x |
| simbox, seeded n=10000 | 159.481 | 2080.836 | 13.05x |
| workstation, golden n=500 | 22.718 | 168.126 | 7.40x |
| workstation, 20 repeats of golden, n=10000 | 216.655 | 3875.476 | 17.89x |

The independent n=10000 sample is generated as Gamma(2)/Gamma(3), using libstdc++ `mt19937_64` seed 11646. It certifies p approximately 1.976058079228, q approximately 2.903869524099 and b approximately 0.977305422298, using 6520 evaluations, 1282 cells and 1740 profile points. The golden uses 5907 evaluations, 1246 cells and 1686 profile points. Workstation runs were contended: the repeated n=10000 certified timings ranged from 3.39 to 7.08 seconds. These wall times describe these runs, not a portable speed guarantee. The slowdown is material on both machines.

[Raw and summarized timings](2026-11-08-betapr-joint-data/final-timings.json), [seeded timing driver](2026-11-08-betapr-joint-data/random-timing.cpp), [simbox evidence runner](2026-11-08-betapr-joint-data/run-evidence.sh), and [workstation timing runner](2026-11-08-betapr-joint-data/joint-timings.sh) retain the measurements and reproduction commands. The [current R sampler diagnostics](2026-11-08-betapr-joint-data/joint-r-diagnostic.txt) cover all 18 exported datasets. Earlier prototype artifacts in the evidence directory are historical and do not supply acceptance evidence.

**Final validation.** The [collected results](2026-11-08-betapr-joint-data/validation.json) verify the [frozen source hashes](2026-11-08-betapr-joint-data/source-sha256.txt).

| required run | machine | final result |
|---|---|---|
| C++ dev | simbox | 106/106 shards |
| C++ full | simbox | 129/129 shards |
| C++ no-long-double | simbox | 129/129 shards |
| C++ no-long-double | workstation | 129/129 shards |
| R dev | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21950 |
| R full | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44773 |
| JS build and tests | workstation | build passed; 541/541, no skips |
| site-check | workstation | passed; 99 generated C++ snippets compile and link |

Targeted checks also passed: 43 C++ cases / 4301 assertions in each native and no-long-double mode, and the full beta-prime R file with 335 passes, no failures, warnings or skips. Final remote commands all used simbox without an exit-75 fallback. Workstation lock waits were long and were allowed to finish; every started command completed. The [suite](2026-11-08-betapr-joint-data/run-suites.sh) and [local](2026-11-08-betapr-joint-data/run-local.sh) drivers and their TSV results retain machine and exit-status records.

Changes are confined to beta-prime, its bindings, the appended shared JS test, and this report/evidence directory. C++ and R family tests were appended. No shared core header, d/p/q/r, special function, truth fixture, golden, code generator, NEWS, status, roadmap, board, main checkout or other worktree was changed. Historical images regenerated by site-check were restored. The final whitespace check passed.

Open display item: the artifact viewer rejected this report with HTTP 403 because its readable-root configuration excludes this worktree. The Markdown file is present at the requested path. Displaying it in that pane requires the viewer to allow this worktree; no global configuration or other checkout was edited.
