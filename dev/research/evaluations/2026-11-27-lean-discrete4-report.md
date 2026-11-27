# TASK-420 · Discrete-family Lean proofs

2026-11-27 · Worktree `lean-discrete4`

Lean checked 64 public theorems in 12 new modules. The atlas fragment records all declarations and leaves coverage empty for independent review. [Atlas fragment](../existence-atlas/lean-map.d/lean-discrete4.csv)

The target ledger contains 111 main questions and 48 path and comparison keys. It records 58 targets with checked statements across their listed branches, 23 with a checked branch or a remaining join, and 78 open. These labels describe this job’s proof scope.

## Verification

Each new module passed a focused `lake env lean` check on the workstation through the single-slot runner. The final workstation build ran with temporary root imports for all twelve modules, under the same slot runner and pinned to one CPU. The root import file was restored and its Git diff was empty.

`Build completed successfully (8002 jobs).`

The build reused the existing Mathlib artifacts. Lake emitted 42 lint warnings in the new modules.

## Source and scope findings

- The CMB mathematical report documents a discrepancy in the fixed-odd-design central singleton example. At size three and probability three quarters, the upper central count is 2 and its central-face mass is three quarters. The example reports that the fitter passed the neighboring pair 2 and 3, giving one quarter for that count. This job checks the capped Bernoulli and constant branches. The odd-center classification remains an open target here. [CMB proof](2026-11-25-cmb-math-report.md#held-size)
- The negative-binomial shorthand “no escape” is an attainment statement on the degenerate flat fiber. At held probability one, size can grow without bound at the same maximal height on zeros. [Negative-binomial dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md#existence.reason)
- The previous exact beta-negative-binomial tie proof uses the sample `(0, 200)`. The atlas target uses the original 500-count golden sample, so the previous theorem leaves that target open. [Previous Lean proof](../../lean/UnivariateML/Family/TiesBNBinom.lean)
- The discretised normal CDF and normalized working likelihoods are proved about separately. The public equation for the interval mass exposes an opaque primitive. Bridging the CDF and interval representations needs a public lemma about that primitive. [Working-model join](../../lean/UnivariateML/Family/JoinsDNorm.lean)

## Target ledger

### hyper

Reused the urn mass and symmetry, then checked Vandermonde, exact finite feasibility, normalized falling factors, both compatible floor paths, and the observation-wise Poisson depletion bound. The second-order residue proof and joint uniform cutoff were analyzed at the formula level.

| Hold | Target | Status | Proof work or exact remaining step |
|---|---|---|---|
| `N` | `criterion` | Checked | Explicit support test and finite compatible grid. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md#existence.reason) |
| `all free` | `criterion` | Checked | Likelihood one at the matching deterministic urn. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md#existence.reason) |
| `k` | `criterion` | Checked | Falling-factorial limits, a uniform correction, explicit feasible integer paths to every probability, and finite attainment above the face. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md#existence.reason) |
| `m` | `criterion` | Checked | Falling-factorial limits, a uniform correction, explicit feasible integer paths to every probability, and finite attainment above the face. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md#existence.reason) |
| `N` | `failure` | Checked | Explicit support test and finite compatible grid. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md#existence.reason) |
| `all free` | `failure` | Open | The cellwise Poisson envelope and fixed-design correction are proved. Still needed are sample aggregation, optimization over the Poisson mean, and one population cutoff uniform in both free design counts. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md#existence.reason) |
| `k` | `failure` | Checked | Falling-factorial limits, a uniform correction, explicit feasible integer paths to every probability, and finite attainment above the face. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md#existence.reason) |
| `m` | `failure` | Checked | Falling-factorial limits, a uniform correction, explicit feasible integer paths to every probability, and finite attainment above the face. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md#existence.reason) |
| `all free` | `uniqueness` | Open | The entire constant-sample set is proved. The nonconstant strict-witness branch needs the uniform all-free cutoff and a rational enumeration of the retained box. [Proof](2026-11-27-uniq-atlas-math-report.md#hyper-held-all-free) |
| `k` | `uniqueness` | Open | The strict-witness cutoff is proved. Still needed are the second-order neighboring-height expansion, rational residue bracketing, and the eventual signs of cleared-denominator polynomials, including identically equal rays. [Proof](2026-11-27-uniq-atlas-math-report.md#hyper-held-k) |
| `m` | `uniqueness` | Open | The strict-witness cutoff is proved. Still needed are the second-order neighboring-height expansion, rational residue bracketing, and the eventual signs of cleared-denominator polynomials, including identically equal rays. [Proof](2026-11-27-uniq-atlas-math-report.md#hyper-held-m) |

### nhyper

Reconciled total draws and reduced black draws. Formalized the saturated support, fixed grids, factorial recurrences, strict integer crossings and tail cutoff. The positive fixed-stop residue argument was analyzed against the source conditions.

| Hold | Target | Status | Proof work or exact remaining step |
|---|---|---|---|
| `N` | `criterion` | Checked | Compatible finite grids at held population, with the extra holds enforced by the filter. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md#existence.reason) |
| `N+m` | `criterion` | Checked | Compatible finite grids at held population, with the extra holds enforced by the filter. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md#existence.reason) |
| `N+r` | `criterion` | Checked | Compatible finite grids at held population, with the extra holds enforced by the filter. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md#existence.reason) |
| `all free` | `criterion` | Checked | Vandermonde gives the saturation bound. Compatibility separately rules out mixed zeros and positives. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md#existence.reason) |
| `m` | `criterion` | Checked | Finite positive-count cutoff uniform over the stopping counts, and the attained zero atom. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md#existence.reason) |
| `m+r` | `criterion` | Checked | Positive stopping-count crossing and vanishing ray limit, together with explicit compatibility and flatness at zero stopping count. [Proof](2026-11-25-nhyper-math-report.md#white-count-held) |
| `r` | `criterion` | Open | The zero-stopping flatness statement is available. Positive fixed stopping count requires the residue-class polynomial cutoff against the optimized shifted NB face, including the proof that no comparison polynomial is identically zero and the attained equality branch. [Proof](2026-11-25-nhyper-math-report.md#an-exact-finite-test-including-equality-and-escape) |
| `N` | `failure` | Checked | Compatible finite grids at held population, with the extra holds enforced by the filter. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md#existence.reason) |
| `N+m` | `failure` | Checked | Compatible finite grids at held population, with the extra holds enforced by the filter. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md#existence.reason) |
| `N+r` | `failure` | Checked | Compatible finite grids at held population, with the extra holds enforced by the filter. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md#existence.reason) |
| `all free` | `failure` | Open | The mixed-zero obstruction is proved. Positive nonconstant data still need exhaustive maximizing escapes across the finite stopping-count slices, with the shifted NB limit and finite/face equality accounted for. [Proof](2026-11-27-uniq-atlas-math-report.md#nhyper-held-all-free) |
| `m` | `failure` | Checked | Finite positive-count cutoff uniform over the stopping counts, and the attained zero atom. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md#existence.reason) |
| `m+r` | `failure` | Checked | Positive stopping-count crossing and vanishing ray limit, together with explicit compatibility and flatness at zero stopping count. [Proof](2026-11-25-nhyper-math-report.md#white-count-held) |
| `r` | `failure` | Open | The zero-stopping flatness statement is available. Positive fixed stopping count requires the residue-class polynomial cutoff against the optimized shifted NB face, including the proof that no comparison polynomial is identically zero and the attained equality branch. [Proof](2026-11-25-nhyper-math-report.md#an-exact-finite-test-including-equality-and-escape) |
| `N` | `uniqueness` | Open | The exact one-or-two-composition crossing is proved at each positive stopping count. Still needed is its assembly into the full set across stopping slices, including the flat zero-stopping slice. [Proof](2026-11-27-uniq-atlas-math-report.md#nhyper-held-n) |
| `N+m` | `uniqueness` | Open join or branch | The finite positive-sample first-crossing algorithms include terminal candidates and exact ties. The separate zero-stopping results supply the zero branches. A single theorem joining the branches per mask remains open. [Proof](2026-11-27-uniq-atlas-math-report.md#nhyper-held-nm) |
| `N+r` | `uniqueness` | Open join or branch | The finite positive-sample first-crossing algorithms include terminal candidates and exact ties. The separate zero-stopping results supply the zero branches. A single theorem joining the branches per mask remains open. [Proof](2026-11-27-uniq-atlas-math-report.md#nhyper-held-nr) |
| `all free` | `uniqueness` | Open | The complete saturated set on constants is proved. The nonconstant set needs the terminating fixed-stopping-count residue tests and comparison of both attained and escaped heights across slices. [Proof](2026-11-27-uniq-atlas-math-report.md#nhyper-held-all-free) |
| `m` | `uniqueness` | Open | The fixed-stop crossing and a uniform positive-data cutoff are proved. The remaining step assembles precisely the per-stop one-or-two maximizers with restored mass constants and all cross-stop ties. [Proof](2026-11-27-uniq-atlas-math-report.md#nhyper-held-m) |
| `m+r` | `uniqueness` | Checked | Positive stopping-count crossing and vanishing ray limit, together with explicit compatibility and flatness at zero stopping count. [Proof](2026-11-25-nhyper-math-report.md#white-count-held) |
| `r` | `uniqueness` | Open | The zero-stopping flatness statement is available. Positive fixed stopping count requires the residue-class polynomial cutoff against the optimized shifted NB face, including the proof that no comparison polynomial is identically zero and the attained equality branch. [Proof](2026-11-25-nhyper-math-report.md#an-exact-finite-test-including-equality-and-escape) |

### nbinom

Reused the two open-fiber score theorems and zero-data results, then checked both joins on the actual mass scale.

| Hold | Target | Status | Proof work or exact remaining step |
|---|---|---|---|
| `prob` | `criterion` | Checked | Interior score optima transfer to the full likelihood, and the closed zero-atom cases are explicit. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md#existence.reason) |
| `size` | `criterion` | Checked | Interior score optima transfer to the full likelihood, and the closed zero-atom cases are explicit. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md#existence.reason) |
| `prob` | `failure` | Open join or branch | Attainment on every compatible sample rules out a strictly superior excluded value. Strict tail exclusion as a standalone theorem is open. Flat held probability one admits maximizing sequences with size diverging. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md#existence.reason) |
| `size` | `failure` | Open join or branch | Attainment on every compatible sample rules out a strictly superior excluded value. Strict tail exclusion as a standalone theorem is open. Flat held probability one admits maximizing sequences with size diverging. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md#existence.reason) |
| `prob` | `uniqueness` | Checked | Interior score optima transfer to the full likelihood, and the closed zero-atom cases are explicit. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md#existence.reason) |
| `size` | `uniqueness` | Checked | Interior score optima transfer to the full likelihood, and the closed zero-atom cases are explicit. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md#existence.reason) |

### cmb

Checked logistic transfer of the held-dispersion profile, exact capped size selection, endpoint constants, and the mixed-binary maximizer set. Read the larger fixed-probability and free-size comparisons against the existing exposed-ray API.

| Hold | Target | Status | Proof work or exact remaining step |
|---|---|---|---|
| `all free` | `criterion` | Open | Constants and the mixed binary branch are checked. Needed is the classification of every other occupancy pattern via the fixed-size moment hull and its capped aggregation. [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `nu` | `criterion` | Checked | Every closed probability slice attains. A finite grid gives the capped fit, and at dispersion zero positive data force the smallest size. [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `nu+prob` | `criterion` | Checked | Finite size-grid formulation for both scalar coefficients held, and endpoint-inclusive fixed-size profiling for dispersion and size held. [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `nu+size` | `criterion` | Checked | Finite size-grid formulation for both scalar coefficients held, and endpoint-inclusive fixed-size profiling for dispersion and size held. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `prob` | `criterion` | Open | Still needed are the full scalar-dispersion classification at each trial size and the capped comparison of all attained and excluded profile heights, including equality and flat fibers. [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `prob+size` | `criterion` | Open | The prior scalar endpoint and center supremum lemmas are available. The missing formalization identifies log-choose extrema for arbitrary designs, obtains the unique intermediate score root, and joins the special designs and endpoint probabilities. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `all free` | `failure` | Open | The size-one winner on mixed zero/one data is checked, and the prior modules prove fixed-size adjacent and endpoint profiles. A capped theorem comparing both exceptional support patterns against every larger size and the tying binary face is still needed. [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `nu` | `failure` | Checked | Every closed probability slice attains. A finite grid gives the capped fit, and at dispersion zero positive data force the smallest size. [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `nu+prob` | `failure` | Checked | Finite size-grid formulation for both scalar coefficients held, and endpoint-inclusive fixed-size profiling for dispersion and size held. [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `nu+size` | `failure` | Checked | Finite size-grid formulation for both scalar coefficients held, and endpoint-inclusive fixed-size profiling for dispersion and size held. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `prob` | `failure` | Open | Still needed are the full scalar-dispersion classification at each trial size and the capped comparison of all attained and excluded profile heights, including equality and flat fibers. [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `prob+size` | `failure` | Open | The prior scalar endpoint and center supremum lemmas are available. The missing formalization identifies log-choose extrema for arbitrary designs, obtains the unique intermediate score root, and joins the special designs and endpoint probabilities. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `size` | `failure` | Open | The prior exposed-ray and moment-hull theorems provide the pieces. The missing branch argument identifies every occupied-cell pattern with its precise edge or vertex and joins strict interior uniqueness to the exact flat cases. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `all free` | `uniqueness` | Checked | Explicit saturated sets on constants and complete capped size-one Bernoulli set on mixed binary data. [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `nu` | `uniqueness` | Checked | Every closed probability slice attains. A finite grid gives the capped fit, and at dispersion zero positive data force the smallest size. [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `nu+prob` | `uniqueness` | Checked | Finite size-grid formulation for both scalar coefficients held, and endpoint-inclusive fixed-size profiling for dispersion and size held. [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `nu+size` | `uniqueness` | Checked | Finite size-grid formulation for both scalar coefficients held, and endpoint-inclusive fixed-size profiling for dispersion and size held. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `prob` | `uniqueness` | Open | Still needed are the full scalar-dispersion classification at each trial size and the capped comparison of all attained and excluded profile heights, including equality and flat fibers. [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `prob+size` | `uniqueness` | Open | The prior scalar endpoint and center supremum lemmas are available. The missing formalization identifies log-choose extrema for arbitrary designs, obtains the unique intermediate score root, and joins the special designs and endpoint probabilities. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `size` | `uniqueness` | Open | The prior exposed-ray and moment-hull theorems provide the pieces. The missing branch argument identifies every occupied-cell pattern with its precise edge or vertex and joins strict interior uniqueness to the exact flat cases. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `all free` | `path:3` | Open | Existing exposed-ray results work at a single design. A joined all-free statement must select the compatible design and compare its height with every other size on the cap. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `all free` | `path:4` | Open | Existing exposed-ray results work at a single design. A joined all-free statement must select the compatible design and compare its height with every other size on the cap. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `nu` | `path:1` | Open | The fixed-dispersion optimizer is proved. Still needed is the pointwise probability-endpoint path over all cells, with the size-zero convention. [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `prob` | `path:2` | Open | Still needed are the full scalar-dispersion classification at each trial size and the capped comparison of all attained and excluded profile heights, including equality and flat fibers. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `prob` | `path:3` | Open | Still needed are the full scalar-dispersion classification at each trial size and the capped comparison of all attained and excluded profile heights, including equality and flat fibers. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `prob` | `path:4` | Open | Still needed are the full scalar-dispersion classification at each trial size and the capped comparison of all attained and excluded profile heights, including equality and flat fibers. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `prob+size` | `path:2` | Open | The prior scalar endpoint and center supremum lemmas are available. The missing formalization identifies log-choose extrema for arbitrary designs, obtains the unique intermediate score root, and joins the special designs and endpoint probabilities. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `prob+size` | `path:3` | Open | The prior scalar endpoint and center supremum lemmas are available. The missing formalization identifies log-choose extrema for arbitrary designs, obtains the unique intermediate score root, and joins the special designs and endpoint probabilities. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `prob+size` | `path:4` | Open | The prior scalar endpoint and center supremum lemmas are available. The missing formalization identifies log-choose extrema for arbitrary designs, obtains the unique intermediate score root, and joins the special designs and endpoint probabilities. [Proof](2026-11-25-cmb-math-report.md#held-size) |
| `all free` | `limit:3` | Open | Existing exposed-ray results work at a single design. A joined all-free statement must select the compatible design and compare its height with every other size on the cap. [Proof](2026-11-25-cmb-math-report.md#held-size) [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `all free` | `limit:4` | Open | Existing exposed-ray results work at a single design. A joined all-free statement must select the compatible design and compare its height with every other size on the cap. [Proof](2026-11-25-cmb-math-report.md#held-size) [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `prob` | `limit:2` | Open | Still needed are the full scalar-dispersion classification at each trial size and the capped comparison of all attained and excluded profile heights, including equality and flat fibers. [Proof](2026-11-25-cmb-math-report.md#held-size) [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `prob` | `limit:3` | Open | Still needed are the full scalar-dispersion classification at each trial size and the capped comparison of all attained and excluded profile heights, including equality and flat fibers. [Proof](2026-11-25-cmb-math-report.md#held-size) [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `prob` | `limit:4` | Open | Still needed are the full scalar-dispersion classification at each trial size and the capped comparison of all attained and excluded profile heights, including equality and flat fibers. [Proof](2026-11-25-cmb-math-report.md#held-size) [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `prob+size` | `limit:2` | Open | The prior scalar endpoint and center supremum lemmas are available. The missing formalization identifies log-choose extrema for arbitrary designs, obtains the unique intermediate score root, and joins the special designs and endpoint probabilities. [Proof](2026-11-25-cmb-math-report.md#held-size) [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `prob+size` | `limit:3` | Open | The prior scalar endpoint and center supremum lemmas are available. The missing formalization identifies log-choose extrema for arbitrary designs, obtains the unique intermediate score root, and joins the special designs and endpoint probabilities. [Proof](2026-11-25-cmb-math-report.md#held-size) [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `prob+size` | `limit:4` | Open | The prior scalar endpoint and center supremum lemmas are available. The missing formalization identifies log-choose extrema for arbitrary designs, obtains the unique intermediate score root, and joins the special designs and endpoint probabilities. [Proof](2026-11-25-cmb-math-report.md#held-size) [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `size` | `limit:2` | Open | The prior exposed-ray and moment-hull theorems provide the pieces. The missing branch argument identifies every occupied-cell pattern with its precise edge or vertex and joins strict interior uniqueness to the exact flat cases. [Proof](2026-11-25-cmb-math-report.md#held-size) [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `size` | `limit:3` | Open | The prior exposed-ray and moment-hull theorems provide the pieces. The missing branch argument identifies every occupied-cell pattern with its precise edge or vertex and joins strict interior uniqueness to the exact flat cases. [Proof](2026-11-25-cmb-math-report.md#held-size) [Proof](2026-11-25-cmb-math-report.md#free-size) |
| `size` | `limit:4` | Open | The prior exposed-ray and moment-hull theorems provide the pieces. The missing branch argument identifies every occupied-cell pattern with its precise edge or vertex and joins strict interior uniqueness to the exact flat cases. [Proof](2026-11-25-cmb-math-report.md#held-size) [Proof](2026-11-25-cmb-math-report.md#free-size) |

### cmp

Joined the held-dispersion branches. Constructed the adjacent and singleton supporting lines, then checked summable domination and pointwise convergence of normalized masses.

| Hold | Target | Status | Proof work or exact remaining step |
|---|---|---|---|
| `all free` | `criterion` | Open | Zero attainment and adjacent-supported nonattainment are proved. Attainment on samples with a gap requires a compactness argument at the geometric seam and large dispersion. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md#existence.reason) |
| `nu` | `criterion` | Checked | The geometric and positive-dispersion fibers are joined into one theorem, including rate zero. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md#existence.reason) |
| `all free` | `failure` | Open join or branch | The degenerate branches and both pointwise paths are proved. An exact height and global comparison theorem for all occupied adjacent counts, including the geometric boundary, is still needed. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md#existence.reason) |
| `nu` | `failure` | Open join or branch | Each admissible fiber attains. A separate likelihood-tail theorem for positive dispersion on the unbounded rate domain is open. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md#existence.reason) |
| `all free` | `uniqueness` | Open | The full zero-rate ray is proved on all zeros. The positive branch needs strict convexity of the countable partition on the joint natural domain, including the geometric seam. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md#existence.reason) |
| `nu` | `uniqueness` | Checked | The geometric and positive-dispersion fibers are joined into one theorem, including rate zero. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md#existence.reason) |
| `all free` | `path:1` | Checked | The supporting-line rays and summable domination give pointwise limits on every count. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md#existence.reason) |
| `all free` | `path:2` | Checked | The supporting-line rays and summable domination give pointwise limits on every count. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md#existence.reason) |
| `all free` | `limit:1` | Open join or branch | There is strict nonattainment on adjacent-supported positive data and there are pointwise recovery paths. A single sharp comparison identifying the saturated heights on the full parameter domain remains open. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md#existence.reason) |
| `all free` | `limit:2` | Open join or branch | There is strict nonattainment on adjacent-supported positive data and there are pointwise recovery paths. A single sharp comparison identifying the saturated heights on the full parameter domain remains open. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md#existence.reason) |

### skellam

Transferred the signed-Poisson support and checked constant uniqueness via optimal Poisson atom heights and the convolution series. Reused the one-magnitude theorem.

| Hold | Target | Status | Proof work or exact remaining step |
|---|---|---|---|
| `mu1` | `criterion` | Checked | Positive held-rate optima are joined to exact signed-Poisson support and optimization at held zero rates. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/README.md#existence.reason) |
| `mu2` | `criterion` | Checked | Positive held-rate optima are joined to exact signed-Poisson support and optimization at held zero rates. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/README.md#existence.reason) |
| `mu1` | `failure` | Checked | Positive held-rate optima are joined to exact signed-Poisson support and optimization at held zero rates. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/README.md#existence.reason) |
| `mu2` | `failure` | Checked | Positive held-rate optima are joined to exact signed-Poisson support and optimization at held zero rates. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/README.md#existence.reason) |
| `all free` | `uniqueness` | Checked | Poisson atom-height ordering and a strict convolution bound handle constants. The existing single-magnitude result supplies the other stated scope. [Proof](2026-11-25-skellam2-math-report.md#reflection-and-a-uniqueness-theorem) |
| `mu1` | `uniqueness` | Checked | Positive held-rate optima are joined to exact signed-Poisson support and optimization at held zero rates. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/README.md#existence.reason) |
| `mu2` | `uniqueness` | Checked | Positive held-rate optima are joined to exact signed-Poisson support and optimization at held zero rates. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/README.md#existence.reason) |

### bbinom

Checked the rising-factorial mass, compact held-shape attainment, reflection, and both kinds of small-shape limits. Inspected the Laplace tilt argument and free-size tail bounds.

| Hold | Target | Status | Proof work or exact remaining step |
|---|---|---|---|
| `all free` | `criterion` | Open | The fixed-size endpoint behavior is checked. The requested free-size cases still need the optimized NB/Poisson face bounds, a uniform size comparison, and exact tied global height verification on the reported examples. [Proof](2026-11-25-bbinomfree-math-report.md#fixed-size-ordering-and-the-active-face) |
| `alpha` | `criterion` | Open | The fixed-size profiles attain away from endpoint-constant data. A free-size result needs the single-crossing profile score, fixed-alpha NB exhaustion and cutoff, and the exact face equality and binary branches. [Proof](2026-11-27-uniq-atlas-math-report.md#bbinom-held-alpha) |
| `alpha+size` | `criterion` | Checked | Continuous rational likelihood, positive witness and compact restriction between the two endpoint limits. Shape reflection gives the dual branch. [Proof](2026-11-25-bbinom-math-report.md#held-shapes) |
| `beta` | `criterion` | Open | The fixed-size positive-shape endpoint alternatives are checked. Still needed are the uniform growing-size bound and exact profile cutoff for nonconstant data, plus the all-zero scale condition and full co-winner comparison. [Proof](2026-11-27-uniq-atlas-math-report.md#bbinom-held-beta) |
| `beta+size` | `criterion` | Checked | Continuous rational likelihood, positive witness and compact restriction between the two endpoint limits. Shape reflection gives the dual branch. [Proof](2026-11-25-bbinom-math-report.md#held-shapes) |
| `size` | `criterion` | Open | The prior overdispersion theorem covers data with an interior cell. The two-endpoint path is proved here. Still needed is the strict saturated comparison on endpoint mixtures and the combined classification including sizes zero and one. [Proof](2026-11-25-bbinom-math-report.md#global-necessity-proof) |
| `all free` | `failure` | Open | The fixed-size endpoint behavior is checked. The requested free-size cases still need the optimized NB/Poisson face bounds, a uniform size comparison, and exact tied global height verification on the reported examples. [Proof](2026-11-25-bbinomfree-math-report.md#fixed-size-ordering-and-the-active-face) |
| `alpha` | `failure` | Open | The fixed-size profiles attain away from endpoint-constant data. A free-size result needs the single-crossing profile score, fixed-alpha NB exhaustion and cutoff, and the exact face equality and binary branches. [Proof](2026-11-27-uniq-atlas-math-report.md#bbinom-held-alpha) |
| `alpha+size` | `failure` | Checked | Continuous rational likelihood, positive witness and compact restriction between the two endpoint limits. Shape reflection gives the dual branch. [Proof](2026-11-25-bbinom-math-report.md#held-shapes) |
| `beta` | `failure` | Open | The fixed-size positive-shape endpoint alternatives are checked. Still needed are the uniform growing-size bound and exact profile cutoff for nonconstant data, plus the all-zero scale condition and full co-winner comparison. [Proof](2026-11-27-uniq-atlas-math-report.md#bbinom-held-beta) |
| `beta+size` | `failure` | Checked | Continuous rational likelihood, positive witness and compact restriction between the two endpoint limits. Shape reflection gives the dual branch. [Proof](2026-11-25-bbinom-math-report.md#held-shapes) |
| `size` | `failure` | Open | The prior overdispersion theorem covers data with an interior cell. The two-endpoint path is proved here. Still needed is the strict saturated comparison on endpoint mixtures and the combined classification including sizes zero and one. [Proof](2026-11-25-bbinom-math-report.md#global-necessity-proof) |
| `all free` | `uniqueness` | Open | The fixed-size endpoint behavior is checked. The requested free-size cases still need the optimized NB/Poisson face bounds, a uniform size comparison, and exact tied global height verification on the reported examples. [Proof](2026-11-25-bbinomfree-math-report.md#nonunimodality-and-exact-ties) |
| `alpha` | `uniqueness` | Open | The fixed-size profiles attain away from endpoint-constant data. A free-size result needs the single-crossing profile score, fixed-alpha NB exhaustion and cutoff, and the exact face equality and binary branches. [Proof](2026-11-27-uniq-atlas-math-report.md#bbinom-held-alpha) |
| `alpha+size` | `uniqueness` | Open | Attainment and the flat size-zero branch are checked. The interior branch requires the Laplace representation of the score kernel, strict prefix-ratio monotonicity, and the one-sign-change tilt argument. [Proof](2026-11-27-uniq-atlas-math-report.md#bbinom-held-alphasize) |
| `beta` | `uniqueness` | Open | The fixed-size positive-shape endpoint alternatives are checked. Still needed are the uniform growing-size bound and exact profile cutoff for nonconstant data, plus the all-zero scale condition and full co-winner comparison. [Proof](2026-11-27-uniq-atlas-math-report.md#bbinom-held-beta) |
| `beta+size` | `uniqueness` | Open | Attainment and the flat size-zero branch are checked. The interior branch requires the Laplace representation of the score kernel, strict prefix-ratio monotonicity, and the one-sign-change tilt argument. [Proof](2026-11-27-uniq-atlas-math-report.md#bbinom-held-betasize) |
| `size` | `uniqueness` | Open | The special size-two and size-three curvature proof and injective coefficient reconstruction are not formalized. [Proof](2026-11-25-bbinom2-math-report.md#universal-uniqueness-at-size-three) |
| `size` | `path:2` | Checked | After one factor is cancelled, the rising-factorial expression extends continuously at zero concentration. [Proof](2026-11-25-bbinom-math-report.md#global-necessity-proof) |
| `size` | `path:3` | Open join or branch | Both atoms are reached on fixed-shape paths. The arbitrary mixing-mean endpoint path still needs a bound uniform in total concentration. [Proof](2026-11-25-bbinom-math-report.md#global-necessity-proof) |
| `size` | `limit:2` | Open | The pointwise two-atom limit is proved. A strict saturated bound for every finite shape pair on mixed endpoint data is still missing. [Proof](2026-11-25-bbinom-math-report.md#global-necessity-proof) |

### bnbinom

Joined positive held-alpha attainment to the strict zero limit and checked the three sample-likelihood paths. Compared the atlas golden-sample tie with the proof available for the different synthetic sample.

| Hold | Target | Status | Proof work or exact remaining step |
|---|---|---|---|
| `all free` | `criterion` | Open | Still needed is a concrete positive sample and a formal strict comparison of a finite candidate with all optimized faces. [Proof](2026-11-25-bnbinom-math-report.md#faces-and-the-finite-versus-face-test) |
| `alpha` | `criterion` | Checked | Positive-count compact attainment is joined to strict nonattainment on zeros. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#existence.reason) |
| `beta` | `criterion` | Open | The required Gamma-ratio tail limit, compact stationary-root comparison, exact equality branch, and strict underdispersion analysis remain. The beta/size symmetry and zero-atom sample limit are available. [Proof](2026-11-27-uniq-atlas-math-report.md#bnbinom-held-beta) |
| `size` | `criterion` | Open | The required Gamma-ratio tail limit, compact stationary-root comparison, exact equality branch, and strict underdispersion analysis remain. The beta/size symmetry and zero-atom sample limit are available. [Proof](2026-11-27-uniq-atlas-math-report.md#bnbinom-held-size) |
| `all free` | `failure` | Open join or branch | The zero-data sample likelihood strictly approaches one along all three coordinate paths. Pointwise probability convergence outside zero and the NB and Poisson comparisons on positive data are separate. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#existence.reason) |
| `beta` | `failure` | Open | The required Gamma-ratio tail limit, compact stationary-root comparison, exact equality branch, and strict underdispersion analysis remain. The beta/size symmetry and zero-atom sample limit are available. [Proof](2026-11-27-uniq-atlas-math-report.md#bnbinom-held-beta) |
| `size` | `failure` | Open | The required Gamma-ratio tail limit, compact stationary-root comparison, exact equality branch, and strict underdispersion analysis remain. The beta/size symmetry and zero-atom sample limit are available. [Proof](2026-11-27-uniq-atlas-math-report.md#bnbinom-held-size) |
| `alpha` | `uniqueness` | Open | The checked prior exact tie uses a different sample. The atlas asks for the original 500-count golden sample and needs its diagonal exclusion and resulting global tie. [Proof](2026-11-27-uniq-atlas-math-report.md#bnbinom-held-alpha) |
| `beta` | `uniqueness` | Open | The required Gamma-ratio tail limit, compact stationary-root comparison, exact equality branch, and strict underdispersion analysis remain. The beta/size symmetry and zero-atom sample limit are available. [Proof](2026-11-27-uniq-atlas-math-report.md#bnbinom-held-beta) |
| `size` | `uniqueness` | Open | The required Gamma-ratio tail limit, compact stationary-root comparison, exact equality branch, and strict underdispersion analysis remain. The beta/size symmetry and zero-atom sample limit are available. [Proof](2026-11-27-uniq-atlas-math-report.md#bnbinom-held-size) |
| `all free` | `path:1` | Open | The two Gamma-ratio asymptotic regimes and their optimized height comparisons remain to be formalized. [Proof](2026-11-25-bnbinom-math-report.md#faces-and-the-finite-versus-face-test) |
| `all free` | `path:2` | Open | The two Gamma-ratio asymptotic regimes and their optimized height comparisons remain to be formalized. [Proof](2026-11-25-bnbinom-math-report.md#faces-and-the-finite-versus-face-test) |
| `all free` | `path:3` | Open join or branch | The zero-data sample likelihood strictly approaches one along all three coordinate paths. Pointwise probability convergence outside zero and the NB and Poisson comparisons on positive data are separate. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#existence.reason) |
| `beta` | `path:1` | Open | The required Gamma-ratio tail limit, compact stationary-root comparison, exact equality branch, and strict underdispersion analysis remain. The beta/size symmetry and zero-atom sample limit are available. [Proof](2026-11-25-bnbinom-math-report.md#faces-and-the-finite-versus-face-test) |
| `beta` | `path:3` | Open join or branch | The zero-data sample likelihood limit is proved. The exact pointwise distribution path and the separately stated optimized face comparison remain. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#existence.reason) |
| `size` | `path:1` | Open | The required Gamma-ratio tail limit, compact stationary-root comparison, exact equality branch, and strict underdispersion analysis remain. The beta/size symmetry and zero-atom sample limit are available. [Proof](2026-11-25-bnbinom-math-report.md#faces-and-the-finite-versus-face-test) |
| `size` | `path:3` | Open join or branch | The zero-data sample likelihood limit is proved. The exact pointwise distribution path and the separately stated optimized face comparison remain. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#existence.reason) |
| `all free` | `limit:1` | Open | The two Gamma-ratio asymptotic regimes and their optimized height comparisons remain to be formalized. [Proof](2026-11-25-bnbinom-math-report.md#faces-and-the-finite-versus-face-test) |
| `all free` | `limit:2` | Open | The two Gamma-ratio asymptotic regimes and their optimized height comparisons remain to be formalized. [Proof](2026-11-25-bnbinom-math-report.md#faces-and-the-finite-versus-face-test) |
| `all free` | `limit:3` | Open join or branch | The zero-data sample likelihood strictly approaches one along all three coordinate paths. Pointwise probability convergence outside zero and the NB and Poisson comparisons on positive data are separate. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#existence.reason) |
| `beta` | `limit:1` | Open | The required Gamma-ratio tail limit, compact stationary-root comparison, exact equality branch, and strict underdispersion analysis remain. The beta/size symmetry and zero-atom sample limit are available. [Proof](2026-11-25-bnbinom-math-report.md#faces-and-the-finite-versus-face-test) |
| `beta` | `limit:3` | Open join or branch | The zero-data sample likelihood limit is proved. The exact pointwise distribution path and the separately stated optimized face comparison remain. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#existence.reason) |
| `size` | `limit:1` | Open | The required Gamma-ratio tail limit, compact stationary-root comparison, exact equality branch, and strict underdispersion analysis remain. The beta/size symmetry and zero-atom sample limit are available. [Proof](2026-11-25-bnbinom-math-report.md#faces-and-the-finite-versus-face-test) |
| `size` | `limit:3` | Open join or branch | The zero-data sample likelihood limit is proved. The exact pointwise distribution path and the separately stated optimized face comparison remain. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#existence.reason) |

### pig

Checked the shape chart bijection, score identity, endpoint extension, and held-shape joins. Inspected the public chart and Bessel interfaces for the physical likelihood path.

| Hold | Target | Status | Proof work or exact remaining step |
|---|---|---|---|
| `mean` | `criterion` | Open join or branch | The variance-excess identity and unique fit or strict Poisson dominance are checked on the compact chart, with the actual nonnegative shape bijection and zero-shape optimum. A public identity transferring the full physical-coordinate likelihood to the normalized chart remains. [Proof](2026-11-25-pig-math-report.md#existence-and-uniqueness-by-held-mask) |
| `shape` | `failure` | Open join or branch | All requested positive-shape cases are joined. The held zero-shape compatible mean ray is available through the existing zero-data theorem but is not joined in the new held-shape statement. [Proof](2026-11-25-pig-math-report.md#existence-and-uniqueness-by-held-mask) |
| `mean` | `uniqueness` | Open join or branch | The variance-excess identity and unique fit or strict Poisson dominance are checked on the compact chart, with the actual nonnegative shape bijection and zero-shape optimum. A public identity transferring the full physical-coordinate likelihood to the normalized chart remains. [Proof](2026-11-25-pig-math-report.md#existence-and-uniqueness-by-held-mask) |
| `shape` | `uniqueness` | Open join or branch | All requested positive-shape cases are joined. The held zero-shape compatible mean ray is available through the existing zero-data theorem but is not joined in the new held-shape statement. [Proof](2026-11-25-pig-math-report.md#existence-and-uniqueness-by-held-mask) |
| `all free` | `path:1` | Open | The earlier positive-count theorem describes the maximum in chart variables. The Poisson path and strict released-mean comparison require the physical likelihood bridge and its limiting normalization. [Proof](2026-11-25-pig-math-report.md#joint-paths-to-the-boundary) |
| `mean` | `path:1` | Open | The strict one-sided Poisson comparison is checked in the compact chart. Physical-parameter convergence still needs the Bessel-polynomial constant-term evaluation and the convergence of the physical Bessel argument factors. [Proof](2026-11-25-pig-math-report.md#joint-paths-to-the-boundary) |
| `all free` | `limit:1` | Open | The earlier positive-count theorem describes the maximum in chart variables. The Poisson path and strict released-mean comparison require the physical likelihood bridge and its limiting normalization. [Proof](2026-11-25-pig-math-report.md#existence-and-uniqueness-by-held-mask) |
| `mean` | `limit:1` | Open join or branch | The variance-excess identity and unique fit or strict Poisson dominance are checked on the compact chart, with the actual nonnegative shape bijection and zero-shape optimum. A public identity transferring the full physical-coordinate likelihood to the normalized chart remains. [Proof](2026-11-25-pig-math-report.md#existence-and-uniqueness-by-held-mask) |

### dgamma

Joined the checked saturated suprema. Inspected the source trace criterion and identified the additional moment and special-function lemmas it needs.

| Hold | Target | Status | Proof work or exact remaining step |
|---|---|---|---|
| `all free` | `failure` | Checked | The constant and adjacent saturated suprema are joined on the same positive shape/rate domain. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/README.md#existence.reason) |
| `all free` | `uniqueness` | Open | The sample-only stationary curvature proof needs conditional moment identities, the zero-cell variance bound from the inverse hazard, the trigamma lower bound, and the covariance trace and Schur-complement argument. [Proof](2026-11-25-dgamma2-math-report.md#a-sample-only-curvature-check) |

### dnorm

Checked direct CDF paths and held-scale tails, plus uniqueness of attained working-model fits. Checked the exposed interval-mass interface for a route to unify the two likelihood representations.

| Hold | Target | Status | Proof work or exact remaining step |
|---|---|---|---|
| `all free` | `failure` | Checked | The full finite likelihood strictly misses the saturated heights. The CDF proofs give pointwise limits on all cells for both collapse paths. [Proof](2026-11-25-dnorm-math-report.md#existence-and-limiting-laws) |
| `sd` | `failure` | Checked | At held positive scale the likelihood vanishes at both location tails. [Proof](2026-11-25-dnorm-math-report.md#existence-and-limiting-laws) |
| `mean` | `uniqueness` | Open join or branch | Uniqueness conditional on attainment is checked for the normalized working product. An explicit public identity joining the opaque Gaussian interval integral to the CDF objective used for attainment remains. [Proof](2026-11-25-dnorm-math-report.md#existence-and-limiting-laws) |
| `sd` | `uniqueness` | Open join or branch | Uniqueness on the normalized working fiber and fixed-scale attainment for the CDF likelihood are separate. The public interval-mass identity would join them. [Proof](2026-11-25-dnorm-math-report.md#existence-and-limiting-laws) |
| `all free` | `path:1` | Checked | The full finite likelihood strictly misses the saturated heights. The CDF proofs give pointwise limits on all cells for both collapse paths. [Proof](2026-11-25-dnorm-math-report.md#existence-and-limiting-laws) |
| `all free` | `path:2` | Checked | The full finite likelihood strictly misses the saturated heights. The CDF proofs give pointwise limits on all cells for both collapse paths. [Proof](2026-11-25-dnorm-math-report.md#existence-and-limiting-laws) |

### dweibull

Checked strict finite cell comparisons, products and both held limits, plus the free point-mass paths in log cumulative-hazard coordinates.

| Hold | Target | Status | Proof work or exact remaining step |
|---|---|---|---|
| `shape1` | `failure` | Checked | Exact product limit and strict finite loss on the two held-mask branches. [Proof](2026-11-25-dweibull-math-report.md#attainment-and-maximizer-sets) |
| `shape2` | `failure` | Checked | Exact product limit and strict finite loss on the two held-mask branches. [Proof](2026-11-25-dweibull-math-report.md#attainment-and-maximizer-sets) |
| `all free` | `path:1` | Checked | Pointwise collapse between adjacent integer boundaries, plus the zero-probability path for the zero count. [Proof](2026-11-25-dweibull-math-report.md#attainment-and-maximizer-sets) |

## Checked theorem statements

### `UnivariateML.Discrete4Hyper.constant_attains_saturated`

A constant sample attains the saturated likelihood, including at count zero. [Lean source](../../lean/UnivariateML/Family/Discrete4Hyper.lean#L58)

```lean
theorem constant_attains_saturated {h : ℕ} (c : ℕ) :
    likelihood c c c (fun _ : Fin h ↦ c) = 1 ∧
      ∀ K N k, likelihood K N k (fun _ : Fin h ↦ c) ≤
        likelihood c c c (fun _ : Fin h ↦ c)
```

### `UnivariateML.Discrete4Hyper.constant_saturated_iff`

Every saturated urn for a nonempty constant sample. A zero sample permits either design count to vanish. At a positive count one design margin occupies the whole population and the other equals the observed count. [Lean source](../../lean/UnivariateML/Family/Discrete4Hyper.lean#L103)

Constant branch of the all-free maximizer-set claim.

```lean
theorem constant_saturated_iff {h K N k c : ℕ} (hh : 0 < h) :
    likelihood K N k (fun _ : Fin h ↦ c) = 1 ↔
      K ≤ N ∧ k ≤ N ∧
        ((c = 0 ∧ (K = 0 ∨ k = 0)) ∨ (K = N ∧ k = c) ∨ (k = N ∧ K = c))
```

### `UnivariateML.Discrete4Hyper.fixed_population_feasible_iff`

A fixed population admits the observations exactly when twice every observed count is at most the population plus every observed count. [Lean source](../../lean/UnivariateML/Family/Discrete4Hyper.lean#L149)

```lean
theorem fixed_population_feasible_iff {h N : ℕ} (x : Fin h → ℕ) :
    (∃ K k, K ≤ N ∧ k ≤ N ∧ ∀ i, Compatible K N k (x i)) ↔
      ∀ i j, 2 * x i ≤ N + x j
```

### `UnivariateML.Discrete4Hyper.fixed_population_attainment`

At fixed population the compatible design grid is finite, so every nonempty grid attains its maximum. The inequality includes every tied design. [Lean source](../../lean/UnivariateML/Family/Discrete4Hyper.lean#L179)

```lean
theorem fixed_population_attainment {h N : ℕ} (x : Fin h → ℕ) :
    (({p : ℕ × ℕ | p.1 ≤ N ∧ p.2 ≤ N ∧
      ∀ i, Compatible p.1 N p.2 (x i)} : Set (ℕ × ℕ))).Finite ∧
      ((∃ K k, K ≤ N ∧ k ≤ N ∧ (∀ i, Compatible K N k (x i)) ∧
        ∀ J j, J ≤ N → j ≤ N → (∀ i, Compatible J N j (x i)) →
          likelihood J N j x ≤ likelihood K N k x) ↔
        ∀ i j, 2 * x i ≤ N + x j)
```

### `UnivariateML.Discrete4Hyper.held_draw_binomial_limit`

With fixed draws and a convergent marked fraction, the sample likelihood converges to the corresponding binomial face, including both point masses. [Lean source](../../lean/UnivariateML/Family/Discrete4Hyper.lean#L289)

```lean
theorem held_draw_binomial_limit {h : ℕ} (x : Fin h → ℕ) (k : ℕ)
    {A N : ℕ → ℕ} {p : ℝ} (hp : p ∈ Icc 0 1) (hN : Tendsto N atTop atTop)
    (hA : ∀ᶠ t in atTop, A t ≤ N t)
    (hAN : Tendsto (fun t ↦ (A t : ℝ) / N t) atTop (𝓝 p)) :
    Tendsto (fun t ↦ likelihood (A t) (N t) k x) atTop
      (𝓝 (∏ i, binomialFace k (x i) p))
```

### `UnivariateML.Discrete4Hyper.held_mark_binomial_limit`

Swapping the two design counts gives the fixed-composition binomial limit. [Lean source](../../lean/UnivariateML/Family/Discrete4Hyper.lean#L298)

```lean
theorem held_mark_binomial_limit {h : ℕ} (x : Fin h → ℕ) (K : ℕ)
    {A N : ℕ → ℕ} {p : ℝ} (hp : p ∈ Icc 0 1) (hN : Tendsto N atTop atTop)
    (hA : ∀ᶠ t in atTop, A t ≤ N t)
    (hAN : Tendsto (fun t ↦ (A t : ℝ) / N t) atTop (𝓝 p)) :
    Tendsto (fun t ↦ likelihood K (N t) (A t) x) atTop
      (𝓝 (∏ i, binomialFace K (x i) p))
```

### `UnivariateML.Discrete4Hyper.held_draw_strict_witness`

With draws held, a finite compatible urn strictly above every compatible binomial height implies finite attainment. [Lean source](../../lean/UnivariateML/Family/Discrete4Hyper.lean#L372)

```lean
theorem held_draw_strict_witness {h K₀ N₀ : ℕ} (x : Fin h → ℕ) (k : ℕ)
    (hK₀ : K₀ ≤ N₀) (hk₀ : k ≤ N₀) (hx : ∀ i, Compatible K₀ N₀ k (x i))
    (hC : ∀ p ∈ Icc (0 : ℝ) 1,
      (∏ i, binomialFace k (x i) p) < likelihood K₀ N₀ k x) :
    ∃ K N, K ≤ N ∧ k ≤ N ∧ (∀ i, Compatible K N k (x i)) ∧
      ∀ J M, J ≤ M → k ≤ M → (∀ i, Compatible J M k (x i)) →
        likelihood J M k x ≤ likelihood K N k x
```

### `UnivariateML.Discrete4Hyper.held_mark_strict_witness`

Fixed composition inherits the finite attainment criterion by symmetry. [Lean source](../../lean/UnivariateML/Family/Discrete4Hyper.lean#L398)

```lean
theorem held_mark_strict_witness {h k₀ N₀ : ℕ} (x : Fin h → ℕ) (K : ℕ)
    (hK₀ : K ≤ N₀) (hk₀ : k₀ ≤ N₀) (hx : ∀ i, Compatible K N₀ k₀ (x i))
    (hC : ∀ p ∈ Icc (0 : ℝ) 1,
      (∏ i, binomialFace K (x i) p) < likelihood K N₀ k₀ x) :
    ∃ k N, k ≤ N ∧ K ≤ N ∧ (∀ i, Compatible K N k (x i)) ∧
      ∀ j M, j ≤ M → K ≤ M → (∀ i, Compatible K M j (x i)) →
        likelihood K M j x ≤ likelihood K N k x
```

### `UnivariateML.Discrete4Hyper.held_draw_reaches_binomial_face`

Each binomial probability in the closed unit interval is reached by a sequence of compatible urns, including both degenerate probabilities. [Lean source](../../lean/UnivariateML/Family/Discrete4Hyper.lean#L416)

```lean
theorem held_draw_reaches_binomial_face {h : ℕ} (x : Fin h → ℕ) (k : ℕ)
    (hx : ∀ i, x i ≤ k) {p : ℝ} (hp : p ∈ Icc 0 1) :
    ∃ A N : ℕ → ℕ, Tendsto N atTop atTop ∧
      (∀ t i, Compatible (A t) (N t) k (x i)) ∧
      Tendsto (fun t ↦ (A t : ℝ)/N t) atTop (𝓝 p) ∧
      Tendsto (fun t ↦ likelihood (A t) (N t) k x) atTop
        (𝓝 (∏ i, binomialFace k (x i) p))
```

### `UnivariateML.Discrete4Hyper.held_mark_reaches_binomial_face`

The marked-count hold has the same compatible closed binomial paths by exchanging the design counts. [Lean source](../../lean/UnivariateML/Family/Discrete4Hyper.lean#L457)

```lean
theorem held_mark_reaches_binomial_face {h : ℕ} (x : Fin h → ℕ) (K : ℕ)
    (hx : ∀ i, x i ≤ K) {p : ℝ} (hp : p ∈ Icc 0 1) :
    ∃ k N : ℕ → ℕ, Tendsto N atTop atTop ∧
      (∀ t i, Compatible K (N t) (k t) (x i)) ∧
      Tendsto (fun t ↦ (k t : ℝ)/N t) atTop (𝓝 p) ∧
      Tendsto (fun t ↦ likelihood K (N t) (k t) x) atTop
        (𝓝 (∏ i, binomialFace K (x i) p))
```

### `UnivariateML.Discrete4Hyper.poisson_envelope_mass`

The observation-wise Poisson envelope used in the all-free tail argument. The expression keeps the exact two design depletions. [Lean source](../../lean/UnivariateML/Family/Discrete4Hyper.lean#L532)

Observation-wise bound. The optimized uniform all-free cutoff is separate.

```lean
theorem poisson_envelope_mass {K N k c : ℕ} (hc : Compatible K N k c) (hN : c < N) :
    let lam := (K : ℝ)*k/N
    mass K N k c ≤
      lam^c / ((c.factorial : ℝ)*(1-(c : ℝ)/N)^c) *
        exp (-(((K : ℝ)-c)*((k : ℝ)-c)/(N : ℝ)))
```

### `UnivariateML.Discrete4NHyper.constant_attains_saturated`

A constant sample attains likelihood one at the all-white urn with population and stopping count equal to the common observation. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L105)

```lean
theorem constant_attains_saturated {h : ℕ} (c : ℕ) :
    totalLikelihood (fun _ : Fin h ↦ c) c c c = 1 ∧
      ∀ N m r, totalLikelihood (fun _ : Fin h ↦ c) N m r ≤ 1
```

### `UnivariateML.Discrete4NHyper.mixed_zero_infeasible`

A sample containing both zero and a positive number of draws is infeasible. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L126)

Mixed zero and positive branch only.

```lean
theorem mixed_zero_infeasible {h : ℕ} (x : Fin h → ℕ) {i j : Fin h}
    (hi : x i = 0) (hj : 0 < x j) :
    ¬ ∃ N m r, ∀ k, Compatible N m r (x k)
```

### `UnivariateML.Discrete4NHyper.constant_saturated_iff`

The saturated set on constants consists precisely of all zero-stopping urns on zeros, and all-white urns with stopping count equal to the common positive observation otherwise. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L201)

Constant branch of the all-free maximizer-set claim.

```lean
theorem constant_saturated_iff {h : ℕ} (hh : 0 < h) (N m r c : ℕ) :
    totalLikelihood (fun _ : Fin h ↦ c) N m r = 1 ↔
      m ≤ N ∧ r ≤ m ∧ r = c ∧ (c = 0 ∨ m = N)
```

### `UnivariateML.Discrete4NHyper.fixed_population_feasible_iff`

A fixed population admits precisely all-zero samples and samples whose observations are all positive and at most the population. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L238)

```lean
theorem fixed_population_feasible_iff {h N : ℕ} (x : Fin h → ℕ) :
    (∃ m r, m ≤ N ∧ r ≤ m ∧ ∀ i, Compatible N m r (x i)) ↔
      (∀ i, x i = 0) ∨ (∀ i, 0 < x i ∧ x i ≤ N)
```

### `UnivariateML.Discrete4NHyper.fixed_population_attainment`

At known population the feasible composition and stopping grid is finite. Attainment is equivalent to the explicit data compatibility test. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L261)

```lean
theorem fixed_population_attainment {h N : ℕ} (x : Fin h → ℕ) :
    let D : Set (ℕ × ℕ) := {p | p.1 ≤ N ∧ p.2 ≤ p.1 ∧
      ∀ i, Compatible N p.1 p.2 (x i)}
    D.Finite ∧ (((∀ i, x i = 0) ∨ (∀ i, 0 < x i ∧ x i ≤ N)) ↔
      ∃ p ∈ D, IsMaxOn (fun q ↦ totalLikelihood x N q.1 q.2) D p)
```

### `UnivariateML.Discrete4NHyper.fixed_population_mark_attainment`

At known population and composition the finite stopping-count set attains exactly when it is nonempty. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L280)

```lean
theorem fixed_population_mark_attainment {h N m : ℕ} (x : Fin h → ℕ) :
    let D : Set (ℕ × ℕ) := {p | p.1 ≤ N ∧ p.2 ≤ p.1 ∧ p.1 = m ∧
      ∀ i, Compatible N p.1 p.2 (x i)}
    D.Finite ∧ (D.Nonempty ↔ ∃ p ∈ D,
      IsMaxOn (fun q ↦ totalLikelihood x N q.1 q.2) D p)
```

### `UnivariateML.Discrete4NHyper.fixed_population_stop_attainment`

At known population and stopping count the finite composition set attains exactly when it is nonempty. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L289)

```lean
theorem fixed_population_stop_attainment {h N r : ℕ} (x : Fin h → ℕ) :
    let D : Set (ℕ × ℕ) := {p | p.1 ≤ N ∧ p.2 ≤ p.1 ∧ p.2 = r ∧
      ∀ i, Compatible N p.1 p.2 (x i)}
    D.Finite ∧ (D.Nonempty ↔ ∃ p ∈ D,
      IsMaxOn (fun q ↦ totalLikelihood x N q.1 q.2) D p)
```

### `UnivariateML.Discrete4NHyper.held_mark_stop_positive`

With positive stopping count and held composition, the black count can be optimized by the first crossing. The successor is retained exactly when the adjacent likelihoods tie. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L299)

Positive stopping count. See the compatible-zero and flatness results for zero stopping count.

```lean
theorem held_mark_stop_positive {h m r : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hr : 0 < r) (hm : r ≤ m) (hx : ∀ i, r ≤ x i) :
    let y := fun i ↦ x i-r
    let D := Ici (maxBlackDraws y)
    ∃ b ∈ D, whiteRatio r m (m+b+1) y ≤ 1 ∧
      (∀ j ∈ D, j < b → 1 < whiteRatio r m (m+j+1) y) ∧
      ∀ j, (IsMaxOn (fun n ↦ totalLikelihood x (m+n) m r) D j ∧ j ∈ D) ↔
        j = b ∨ (j = b+1 ∧ whiteRatio r m (m+b+1) y = 1)
```

### `UnivariateML.Discrete4NHyper.held_mark_stop_zero`

When the held stopping count is zero, compatibility requires all observations to be zero and every population above the held composition ties. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L337)

```lean
theorem held_mark_stop_zero {h m : ℕ} (x : Fin h → ℕ) :
    (∃ N ≥ m, ∀ i, Compatible N m 0 (x i)) ↔ ∀ i, x i = 0
```

### `UnivariateML.Discrete4NHyper.held_zero_stop_flat`

On compatible zeros with the stopping count held at zero, the likelihood equals one throughout the population ray. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L347)

```lean
theorem held_zero_stop_flat {h N m : ℕ} (x : Fin h → ℕ) (hm : m ≤ N)
    (hx : ∀ i, x i = 0) : totalLikelihood x N m 0 = 1
```

### `UnivariateML.Discrete4NHyper.held_mark_stop_limit`

With positive stopping count and composition held, the likelihood vanishes along the population ray. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L412)

```lean
theorem held_mark_stop_limit {h m r : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hr : 0 < r) (hm : r ≤ m) :
    Tendsto (fun b : ℕ ↦ totalLikelihood x (m+b) m r) atTop (𝓝 0)
```

### `UnivariateML.Discrete4NHyper.held_mark_positive_cutoff`

With composition held, positive data have a finite population cutoff against any positive likelihood witness, uniformly over feasible stopping counts. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L443)

```lean
theorem held_mark_positive_cutoff {h m : ℕ} (x : Fin h → ℕ)
    (hx : ∃ i, 0 < x i) {C : ℝ} (hC : 0 < C) :
    ∃ B : ℕ, ∀ N ≥ B, ∀ r, (∀ i, Compatible N m r (x i)) →
      totalLikelihood x N m r < C
```

### `UnivariateML.Discrete4NHyper.held_mark_attainment`

With composition held, every feasible sample attains a maximum. The zero branch retains every compatible urn with stopping count zero. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L472)

```lean
theorem held_mark_attainment {h m : ℕ} (x : Fin h → ℕ)
    (hfeas : ∃ N r, m ≤ N ∧ r ≤ m ∧ ∀ i, Compatible N m r (x i)) :
    ∃ N r, m ≤ N ∧ r ≤ m ∧ (∀ i, Compatible N m r (x i)) ∧
      ∀ M s, m ≤ M → s ≤ m → (∀ i, Compatible M m s (x i)) →
        totalLikelihood x M m s ≤ totalLikelihood x N m r
```

### `UnivariateML.Discrete4NHyper.fixed_population_stop_crossing`

At fixed population and positive stopping count the complete set of maximizers is the first crossing and its successor precisely on equality. If there is no crossing, the last feasible composition is the sole fit. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L629)

Positive branch. The separate zero-stopping flatness theorem applies on zeros.

```lean
theorem fixed_population_stop_crossing {h N r : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hr : 0 < r) (hx : ∀ i, r ≤ x i) (hfeas : ∃ m, ∀ i, Compatible N m r (x i)) :
    let u := N - maxBlackDraws (fun i ↦ x i-r)
    let D := Icc r u
    ∃ b ∈ D, (b < u → compositionRatio x N r b ≤ 1) ∧
      (∀ j ∈ Ico r u, j < b → 1 < compositionRatio x N r j) ∧
      ∀ j ∈ D, IsMaxOn (fun m ↦ totalLikelihood x N m r) D j ↔
        j = b ∨ (j = b+1 ∧ b < u ∧ compositionRatio x N r b = 1)
```

### `UnivariateML.Discrete4NHyper.fixed_population_mark_crossing`

At held population and composition on positive data the first nonpositive ratio crossing identifies every winning stopping count, including an adjacent tie and the terminal-size case. [Lean source](../../lean/UnivariateML/Family/Discrete4NHyper.lean#L758)

Positive branch. The zero branch forces stopping count zero by compatibility.

```lean
theorem fixed_population_mark_crossing {h N m : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hx : ∀ i, 0 < x i) (hfeas : ∃ r, ∀ i, Compatible N m r (x i)) :
    let l := max 1 (Finset.univ.sup x-(N-m))
    let u := min m (Finset.univ.inf' ⟨⟨0,hh⟩,Finset.mem_univ _⟩ x)
    let D := Icc l u
    ∃ b ∈ D, (b < u → stoppingRatio x N m b ≤ 1) ∧
      (∀ j ∈ Ico l u, j < b → 1 < stoppingRatio x N m j) ∧
      ∀ j ∈ D, IsMaxOn (fun r ↦ totalLikelihood x N m r) D j ↔
        j = b ∨ (j = b+1 ∧ b < u ∧ stoppingRatio x N m b = 1)
```

### `UnivariateML.Discrete4NBinom.held_probability`

A held interior probability has exactly one maximizer over nonnegative size, with optimum zero on all-zero data. Probability one is compatible only with zeros, for which the entire size ray maximizes. [Lean source](../../lean/UnivariateML/Family/Discrete4NBinom.lean#L44)

```lean
theorem held_probability {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {p : ℝ} (hp : p ∈ Ioc 0 1) :
    ((∀ i, x i = 0) →
      ∀ r ∈ Ici (0 : ℝ), IsMaxOn (fun s ↦ likelihood x s p) (Ici 0) r ↔ r = 0 ∨ p = 1) ∧
    ((¬ ∀ i, x i = 0) →
      (p < 1 → ∃! r, r ∈ Ici (0 : ℝ) ∧ IsMaxOn (fun s ↦ likelihood x s p) (Ici 0) r) ∧
      (p = 1 → ∀ r, likelihood x r p = 0))
```

### `UnivariateML.Discrete4NBinom.held_size`

At held positive size the fitted probability is unique, including probability one on all-zero data. At size zero only zero data are compatible and all admissible probabilities maximize. [Lean source](../../lean/UnivariateML/Family/Discrete4NBinom.lean#L86)

```lean
theorem held_size {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r : ℝ} (hr : 0 ≤ r) :
    ((∀ i, x i = 0) →
      ∀ p ∈ Ioc (0 : ℝ) 1, IsMaxOn (likelihood x r) (Ioc 0 1) p ↔ r = 0 ∨ p = 1) ∧
    ((¬ ∀ i, x i = 0) →
      (0 < r → prob x r ∈ Ioc (0 : ℝ) 1 ∧
        ∀ p ∈ Ioc (0 : ℝ) 1, IsMaxOn (likelihood x r) (Ioc 0 1) p ↔ p = prob x r) ∧
      (r = 0 → ∀ p, likelihood x r p = 0))
```

### `UnivariateML.Discrete4CMB.constant_saturated_iff`

At any feasible trial size, the likelihood is bounded by one. On a nonempty constant sample all saturated parameters are exactly the size-zero fiber or the matching endpoint atom at positive size. [Lean source](../../lean/UnivariateML/Family/Discrete4CMB.lean#L61)

Constant branch of the all-free maximizer-set claim.

```lean
theorem constant_saturated_iff {N n : ℕ} (hn : 0 < n) (c : Fin (N+1))
    {p : ℝ} (_hp : p ∈ Icc 0 1) (ν : ℝ) :
    closedLikelihood (fun _ : Fin n ↦ c) p ν ≤ 1 ∧
      (closedLikelihood (fun _ : Fin n ↦ c) p ν = 1 ↔
        N = 0 ∨ (p = 0 ∧ c.val = 0) ∨ (p = 1 ∧ c.val = N))
```

### `UnivariateML.Discrete4CMB.held_probability_dispersion_capped`

Holding probability and dispersion reduces the fit to exact comparison of a finite compatible grid. Every size tied for the best height maximizes. [Lean source](../../lean/UnivariateML/Family/Discrete4CMB.lean#L124)

```lean
theorem held_probability_dispersion_capped {n B : ℕ} (x : Fin n → ℕ) (p ν : ℝ) :
    let D : Set ℕ := {N | N ≤ B ∧ Compatible x N p}
    D.Finite ∧ (D.Nonempty ↔ ∃ N ∈ D, IsMaxOn (fun J ↦ sizeLikelihood x J p ν) D N)
```

### `UnivariateML.Discrete4CMB.held_probability_zero_dispersion`

At zero dispersion and held interior probability, the smallest compatible size is the sole maximizer over the capped trial-size domain. [Lean source](../../lean/UnivariateML/Family/Discrete4CMB.lean#L141)

```lean
theorem held_probability_zero_dispersion {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {p : ℝ} (hp : p ∈ Ioo 0 1) (hB : ∀ i, x i ≤ B) :
    let D : Set ℕ := {N | N ≤ B ∧ Compatible x N p}
    ∀ N ∈ D, IsMaxOn (fun J ↦ sizeLikelihood x J p 0) D N ↔ N = Finset.univ.sup x
```

### `UnivariateML.Discrete4CMB.held_dispersion_size`

A positive fixed size has exactly one fitted probability at any held finite dispersion. The endpoint fits are included. Size zero is flat. [Lean source](../../lean/UnivariateML/Family/Discrete4CMB.lean#L222)

```lean
theorem held_dispersion_size {N n : ℕ} (hn : 0 < n) (x : Fin n → Fin (N+1)) (ν : ℝ) :
    (N = 0 → ∀ p ∈ Icc (0 : ℝ) 1, closedLikelihood x p ν = 1) ∧
    (0 < N → ∃! p, p ∈ Icc (0 : ℝ) 1 ∧
      IsMaxOn (fun q ↦ closedLikelihood x q ν) (Icc 0 1) p)
```

### `UnivariateML.Discrete4CMB.held_dispersion_capped`

At held dispersion, the closed probability profiles each attain and a finite comparison over the capped trial designs gives a global fit. [Lean source](../../lean/UnivariateML/Family/Discrete4CMB.lean#L331)

```lean
theorem held_dispersion_capped {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ) (ν : ℝ)
    (hx : ∀ i, x i ≤ B) :
    let D : Set (ℕ × ℝ) := {q | q.1 ≤ B ∧ (∀ i, x i ≤ q.1) ∧ q.2 ∈ Icc 0 1}
    ∃ q ∈ D, IsMaxOn (fun q ↦ sizeLikelihood x q.1 q.2 ν) D q
```

### `UnivariateML.Discrete4CMB.held_zero_dispersion_size_unique`

Zero dispersion and at least one positive count force the smallest compatible size, even when probability is optimized. [Lean source](../../lean/UnivariateML/Family/Discrete4CMB.lean#L365)

```lean
theorem held_zero_dispersion_size_unique {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 0 < x i) :
    let D : Set (ℕ × ℝ) := {q | q.1 ≤ B ∧ (∀ i, x i ≤ q.1) ∧ q.2 ∈ Icc 0 1}
    ∀ q ∈ D, IsMaxOn (fun t ↦ sizeLikelihood x t.1 t.2 0) D q →
      q.1 = Finset.univ.sup x
```

### `UnivariateML.Discrete4CMB.mixed_binary_maximizers`

Mixed binary observations have exactly the capped size-one Bernoulli maximizers, with dispersion arbitrary. [Lean source](../../lean/UnivariateML/Family/Discrete4CMB.lean#L417)

Includes the whole dispersion fiber at size one.

```lean
theorem mixed_binary_maximizers {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ 1) (hz : ∃ i, x i = 0) (ho : ∃ i, x i = 1) (hB : 1 ≤ B) :
    let z := (Finset.univ.filter fun i ↦ x i = 0).card
    let o := (Finset.univ.filter fun i ↦ x i = 1).card
    let D : Set (ℕ × ℝ × ℝ) := {q | q.1 ≤ B ∧ q.2.1 ∈ Icc 0 1}
    ∀ q ∈ D, IsMaxOn (fun q ↦ sizeLikelihood x q.1 q.2.1 q.2.2) D q ↔
      q.1 = 1 ∧ q.2.1 = (o : ℝ)/(o+z)
```

### `UnivariateML.Discrete4CMP.held_dispersion`

Every admissible held dispersion, including zero on the geometric chart, has a unique maximizing rate. [Lean source](../../lean/UnivariateML/Family/Discrete4CMP.lean#L29)

```lean
theorem held_dispersion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {ν : ℝ} (hν : 0 ≤ ν) :
    let D := {r : ℝ | (r,ν) ∈ CMP.rateDomain}
    ∃! r, r ∈ D ∧ IsMaxOn (fun q ↦ CMP.likelihood x q ν) D r
```

### `UnivariateML.Discrete4CMP.all_free_degenerate`

All zeros attain exactly on the zero-rate fiber. Any sample supported on adjacent cells and containing a positive count has an excluded supremum. [Lean source](../../lean/UnivariateML/Family/Discrete4CMP.lean#L78)

Covers all zeros and adjacent-supported nonzero data. The remaining attainment branch is separate.

```lean
theorem all_free_degenerate {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    ((∀ i, x i = 0) → ∀ p ∈ CMP.rateDomain,
      IsMaxOn (fun q : ℝ × ℝ ↦ CMP.likelihood x q.1 q.2) CMP.rateDomain p ↔ p.1 = 0) ∧
    ((∃ i, x i ≠ 0) → (∃ c, ∀ i, x i = c ∨ x i = c+1) →
      ¬ ∃ p ∈ CMP.rateDomain,
        IsMaxOn (fun q : ℝ × ℝ ↦ CMP.likelihood x q.1 q.2) CMP.rateDomain p)
```

### `UnivariateML.Discrete4CMP.adjacent_path`

The exposed adjacent-cell ray has a two-point limiting distribution. Its two positive masses are proportional to the base weights on those cells. [Lean source](../../lean/UnivariateML/Family/Discrete4CMP.lean#L134)

Pointwise path for the adjacent law, with its two masses expressed as normalized base weights.

```lean
theorem adjacent_path (c : ℕ) (η : ℝ) (k : ℕ) :
    let s := log ((c : ℝ)+1)
    Tendsto (fun t : ℝ ↦ CMP.likelihood (fun _ : Fin 1 ↦ k) (exp (η+t*s)) (1+t))
      atTop (𝓝 ((if k ∈ ({c,c+1} : Finset ℕ) then CMP.weight η 1 k else 0) /
        (CMP.weight η 1 c + CMP.weight η 1 (c+1))))
```

### `UnivariateML.Discrete4CMP.positive_point_path`

A ray with slope strictly between the two adjacent supporting slopes concentrates at the positive count between them. [Lean source](../../lean/UnivariateML/Family/Discrete4CMP.lean#L158)

```lean
theorem positive_point_path (c : ℕ) (hc : 0 < c) (k : ℕ) :
    let s := (log (c : ℝ)+log ((c : ℝ)+1))/2
    Tendsto (fun t : ℝ ↦ CMP.likelihood (fun _ : Fin 1 ↦ k) (exp (t*s)) (1+t))
      atTop (𝓝 (if k = c then 1 else 0))
```

### `UnivariateML.Discrete4Skellam.held_second`

With the second rate held, positive values admit a unique first rate. At zero, precisely the nonnegative signed samples admit a unique fit. Any negative observation forces likelihood zero throughout that fiber. [Lean source](../../lean/UnivariateML/Family/Discrete4Skellam.lean#L103)

```lean
theorem held_second {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) {r : ℝ} (hr : 0 ≤ r) :
    ((0 < r ∨ ∀ i, 0 ≤ x i) →
      ∃! v, v ∈ Ici 0 ∧ IsMaxOn (fun w ↦ likelihood x (w,r)) (Ici 0) v) ∧
    (r = 0 → (∃ i, x i < 0) → ∀ v, likelihood x (v,r) = 0)
```

### `UnivariateML.Discrete4Skellam.held_first`

Reflection transfers the held-rate result to the first rate. At zero the admissible signed counts are nonpositive. [Lean source](../../lean/UnivariateML/Family/Discrete4Skellam.lean#L119)

```lean
theorem held_first {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) {r : ℝ} (hr : 0 ≤ r) :
    ((0 < r ∨ ∀ i, x i ≤ 0) →
      ∃! v, v ∈ Ici 0 ∧ IsMaxOn (fun w ↦ likelihood x (r,w)) (Ici 0) v) ∧
    (r = 0 → (∃ i, 0 < x i) → ∀ v, likelihood x (r,v) = 0)
```

### `UnivariateML.Discrete4Skellam.constant_unique`

Every nonempty constant Skellam sample has a unique maximizing pair. The compatible Poisson axis supplies the fit, including the origin for zero. [Lean source](../../lean/UnivariateML/Family/Discrete4Skellam.lean#L203)

Also joined with the one-magnitude branch in joint_unique_at_stated_scope.

```lean
theorem constant_unique {n : ℕ} (hn : 0 < n) (c : ℤ) :
    ∃! p, p ∈ domain ∧ IsMaxOn (likelihood (fun _ : Fin n ↦ c)) domain p
```

### `UnivariateML.Discrete4Skellam.joint_unique_at_stated_scope`

The proved all-free uniqueness scopes are constants and mean-zero samples supported on both signs of one positive magnitude. [Lean source](../../lean/UnivariateML/Family/Discrete4Skellam.lean#L252)

```lean
theorem joint_unique_at_stated_scope {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    (hx : (∃ c, ∀ i, x i = c) ∨
      (∃ k : ℕ, 0 < k ∧ (∀ i, x i = (k : ℤ) ∨ x i = -(k : ℤ)) ∧
        (∑ i, x i = 0) ∧ (∃ i, 0 < x i) ∧ ∃ i, x i < 0)) :
    ∃! p, p ∈ domain ∧ IsMaxOn (likelihood x) domain p
```

### `UnivariateML.Discrete4BBinom.fixed_alpha_paths`

With alpha held, beta tending to zero gives the upper atom, and beta tending to infinity gives the lower atom. [Lean source](../../lean/UnivariateML/Family/Discrete4BBinom.lean#L134)

```lean
theorem fixed_alpha_paths {m n : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m)
    {a : ℝ} (ha : 0 < a) :
    Tendsto (likelihood m x a) (𝓝[>] 0) (𝓝 (if ∀ i, x i = m then 1 else 0)) ∧
    Tendsto (likelihood m x a) atTop (𝓝 (if ∀ i, x i = 0 then 1 else 0))
```

### `UnivariateML.Discrete4BBinom.fixed_alpha_attainment`

At held positive alpha and size, exactly the two endpoint-constant samples fail to attain. Each such sample has supremum one and strict inequality at finite beta. At size zero beta is arbitrary. [Lean source](../../lean/UnivariateML/Family/Discrete4BBinom.lean#L155)

The attained positive-size maximizer is not shown unique here.

```lean
theorem fixed_alpha_attainment {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ m) {a : ℝ} (ha : 0 < a) :
    (m = 0 → ∀ b > 0, likelihood m x a b = 1) ∧
    (0 < m →
      ((∃ b ∈ Ioi (0 : ℝ), IsMaxOn (likelihood m x a) (Ioi 0) b) ↔
        (¬ ∀ i, x i = 0) ∧ (¬ ∀ i, x i = m)) ∧
      (((∀ i, x i = 0) ∨ (∀ i, x i = m)) →
        IsLUB (likelihood m x a '' Ioi 0) 1 ∧ ∀ b > 0, likelihood m x a b < 1))
```

### `UnivariateML.Discrete4BBinom.fixed_beta_attainment`

Holding beta at known size gives the reflected criterion, with the same size-zero flat branch and excluded endpoint-constant suprema. [Lean source](../../lean/UnivariateML/Family/Discrete4BBinom.lean#L260)

The attained positive-size maximizer is not shown unique here.

```lean
theorem fixed_beta_attainment {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ m) {b : ℝ} (hb : 0 < b) :
    (m = 0 → ∀ a > 0, likelihood m x a b = 1) ∧
    (0 < m →
      ((∃ a ∈ Ioi (0 : ℝ), IsMaxOn (fun t ↦ likelihood m x t b) (Ioi 0) a) ↔
        (¬ ∀ i, x i = 0) ∧ (¬ ∀ i, x i = m)) ∧
      (((∀ i, x i = 0) ∨ (∀ i, x i = m)) →
        IsLUB ((fun t ↦ likelihood m x t b) '' Ioi 0) 1 ∧
          ∀ a > 0, likelihood m x a b < 1))
```

### `UnivariateML.Discrete4BBinom.two_point_path`

At positive held trial size, sending both shapes to zero at fixed interior mixing mean leaves only the two endpoint atoms. [Lean source](../../lean/UnivariateML/Family/Discrete4BBinom.lean#L308)

```lean
theorem two_point_path {m k : ℕ} (hm : 0 < m) (hk : k ≤ m) {p : ℝ}
    (hp : p ∈ Ioo 0 1) :
    Tendsto (fun s : ℝ ↦ mass m k (p*s) ((1-p)*s)) (𝓝[>] 0)
      (𝓝 (if k = 0 then 1-p else if k = m then p else 0))
```

### `UnivariateML.Discrete4BNBinom.zero_atom_likelihood_paths`

On all-zero data the likelihood is strictly below one at every positive triple. Each of the three free-coordinate routes reaches one. [Lean source](../../lean/UnivariateML/Family/Discrete4BNBinom.lean#L28)

Sample likelihood convergence on zero data. Pointwise mass convergence on positive observations is separate.

```lean
theorem zero_atom_likelihood_paths {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0) {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    exp (logLikelihood x r a b) < 1 ∧
      Tendsto (fun t ↦ exp (logLikelihood x t a b)) (𝓝[>] 0) (𝓝 1) ∧
      Tendsto (fun t ↦ exp (logLikelihood x r a t)) (𝓝[>] 0) (𝓝 1) ∧
      Tendsto (fun t ↦ exp (logLikelihood x r t b)) atTop (𝓝 1)
```

### `UnivariateML.Discrete4BNBinom.held_alpha_attainment_iff`

At any positive held alpha, a finite joint size and beta fit exists exactly when a positive count occurs. [Lean source](../../lean/UnivariateML/Family/Discrete4BNBinom.lean#L51)

```lean
theorem held_alpha_attainment_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {a : ℝ} (ha : 0 < a) :
    (∃ p ∈ Ioi (0 : ℝ) ×ˢ Ioi 0,
      IsMaxOn (fun p : ℝ × ℝ ↦ logLikelihood x p.1 a p.2) (Ioi 0 ×ˢ Ioi 0) p) ↔
      ∃ i, 0 < x i
```

### `UnivariateML.Discrete4BNBinom.all_free_zero_supremum`

All-zero data have a strictly unattained supremum over every positive triple, with a feasible path shrinking size alone. [Lean source](../../lean/UnivariateML/Family/Discrete4BNBinom.lean#L64)

Zero-data branch of the all-free failure claim.

```lean
theorem all_free_zero_supremum {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0 ×ˢ Ioi 0
    let L := fun p : ℝ × ℝ × ℝ ↦ exp (logLikelihood x p.1 p.2.1 p.2.2)
    IsLUB (L '' D) 1 ∧ ∀ p ∈ D, L p < 1
```

### `UnivariateML.Discrete4PIG.shape_bijOn`

The compact chart covers exactly all nonnegative physical shapes. [Lean source](../../lean/UnivariateML/Family/Discrete4PIG.lean#L51)

```lean
theorem shape_bijOn {μ : ℝ} (hμ : 0 < μ) : BijOn (shape μ) (Ioc 0 1) (Ici 0)
```

### `UnivariateML.Discrete4PIG.held_mean_positive`

On positive data, the held-mean fit exists exactly at positive variance excess. Whenever it exists it is unique. Otherwise the normalized Poisson height strictly exceeds all finite profiles. [Lean source](../../lean/UnivariateML/Family/Discrete4PIG.lean#L127)

Positive sample branch in the compact shape chart. The physical shape chart bijection is separate.

```lean
theorem held_mean_positive {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 0 < x i) {μ : ℝ} (hμ : 0 < μ) :
    let m := (∑ i, (x i : ℝ))/(n : ℝ)
    let v := (∑ i, ((x i : ℝ)-m)^2)/(n : ℝ)
    if m < v+(m-μ)^2 then
      ∃! w, w ∈ Ioc (0 : ℝ) 1 ∧ IsMaxOn (normalizedProfile x μ) (Ioc 0 1) w
    else
      IsLUB (normalizedProfile x μ '' Ioc 0 1) 1 ∧
        ∀ w ∈ Ioc (0 : ℝ) 1, normalizedProfile x μ w < 1
```

### `UnivariateML.Discrete4PIG.held_mean_all_zero`

With positive mean held on zero data, the shape-zero member is the unique fit on the nonnegative shape domain. [Lean source](../../lean/UnivariateML/Family/Discrete4PIG.lean#L206)

```lean
theorem held_mean_all_zero {n : ℕ} (hn : 0 < n) {μ : ℝ} (hμ : 0 < μ)
    {lam : ℝ} (hlam : 0 ≤ lam) :
    IsMaxOn (HeldSweepPIG.zeroLikelihood n μ) (Ici 0) lam ↔ lam = 0
```

### `UnivariateML.Discrete4PIG.held_shape`

Positive held shape has a unique mean on positive data. All-zero data approach the excluded zero-mean atom. [Lean source](../../lean/UnivariateML/Family/Discrete4PIG.lean#L228)

Positive held shape only. The zero-shape likelihood is flat in mean on compatible zeros.

```lean
theorem held_shape {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {lam : ℝ} (hlam : 0 < lam) :
    let f := JoinsPIG.heldShapeLikelihood x lam
    ((∃ i, 0 < x i) → ∃! μ, μ ∈ Ioi (0 : ℝ) ∧ IsMaxOn f (Ioi 0) μ) ∧
      ((∀ i, x i = 0) → Tendsto f (𝓝[>] 0) (𝓝 1) ∧
        IsLUB (f '' Ioi 0) 1 ∧ ∀ μ > 0, f μ < 1)
```

### `UnivariateML.Discrete4DGamma.saturated_suprema`

A constant sample or a sample occupying two adjacent cells has an unattained saturated supremum. The two-cell height uses its empirical split. [Lean source](../../lean/UnivariateML/Family/Discrete4DGamma.lean#L27)

```lean
theorem saturated_suprema {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    let L := {v | ∃ a > 0, ∃ r > 0, likelihood x a r = v}
    ((∃ j, ∀ i, x i = j) → IsLUB L 1 ∧ ∀ a > 0, ∀ r > 0, likelihood x a r < 1) ∧
    (∀ j, (∀ i, x i = j ∨ x i = j+1) → (∃ i, x i = j) → (∃ i, x i = j+1) →
      let m := (Finset.univ.filter fun i ↦ x i = j).card
      let k := (Finset.univ.filter fun i ↦ x i ≠ j).card
      let w := (m : ℝ)/(m+k)
      IsLUB L (w^m*(1-w)^k) ∧ ∀ a > 0, ∀ r > 0, likelihood x a r < w^m*(1-w)^k)
```

### `UnivariateML.Discrete4DNorm.point_path`

At fixed location strictly inside cell `j`, shrinking scale concentrates at `j`, with every other cell probability tending to zero. [Lean source](../../lean/UnivariateML/Family/Discrete4DNorm.lean#L32)

```lean
theorem point_path (j k : ℤ) {m : ℝ} (h₁ : (j : ℝ) < m) (h₂ : m < (j : ℝ)+1) :
    Tendsto (cell k m) atTop (𝓝 (if k = j then 1 else 0))
```

### `UnivariateML.Discrete4DNorm.adjacent_path`

Sending inverse scale to infinity and setting the mean at the shared boundary minus `t/B` gives masses `Φ t` and `1 - Φ t` on the adjacent cells. [Lean source](../../lean/UnivariateML/Family/Discrete4DNorm.lean#L50)

```lean
theorem adjacent_path (j k : ℤ) (t : ℝ) :
    Tendsto (fun B : ℝ ↦ cell k ((j : ℝ)+1-t/B) B) atTop
      (𝓝 (if k = j then Φ t else if k = j+1 then 1-Φ t else 0))
```

### `UnivariateML.Discrete4DNorm.held_scale_tails`

With positive scale held, either location escape makes the entire sample likelihood tend to zero. [Lean source](../../lean/UnivariateML/Family/Discrete4DNorm.lean#L81)

```lean
theorem held_scale_tails {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) {B : ℝ} (hB : 0 < B) :
    Tendsto (fun m ↦ heldLocationLikelihood x m B) atTop (𝓝 0) ∧
    Tendsto (fun m ↦ heldLocationLikelihood x m B) atBot (𝓝 0)
```

### `UnivariateML.Discrete4DNorm.held_mean_atMostOne`

Any attained held-location maximum of the normalized working likelihood is unique. [Lean source](../../lean/UnivariateML/Family/Discrete4DNorm.lean#L103)

Uniqueness for an attained fit in the normalized working model.

```lean
theorem held_mean_atMostOne {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) (m : ℝ)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (hma : IsMaxOn (fun B ↦ workingLikelihood x (m*B,B)) (Ioi 0) a)
    (hmb : IsMaxOn (fun B ↦ workingLikelihood x (m*B,B)) (Ioi 0) b) : a = b
```

### `UnivariateML.Discrete4DNorm.held_scale_atMostOne_working`

Any attained held-positive-scale maximum of the normalized working likelihood is unique. [Lean source](../../lean/UnivariateML/Family/Discrete4DNorm.lean#L124)

Uniqueness for an attained fit in the normalized working model.

```lean
theorem held_scale_atMostOne_working {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {B : ℝ} (hB : 0 < B) {a b : ℝ}
    (ha : IsMaxOn (fun A ↦ workingLikelihood x (A,B)) univ a)
    (hb : IsMaxOn (fun A ↦ workingLikelihood x (A,B)) univ b) : a = b
```

### `UnivariateML.Discrete4DNorm.saturated_suprema`

The all-free constant or adjacent two-cell data have saturated heights that every finite normal fit strictly misses. [Lean source](../../lean/UnivariateML/Family/Discrete4DNorm.lean#L144)

```lean
theorem saturated_suprema {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) :
    let Y := workingLikelihood x '' workingDomain
    ((∃ j, ∀ i, x i = j) → IsLUB Y 1 ∧
      ∀ p ∈ workingDomain, workingLikelihood x p < 1) ∧
    (∀ j, (∀ i, x i = j ∨ x i = j+1) →
      0 < cellCount x j → 0 < cellCount x (j+1) →
      let C := adjacentHeight (cellCount x j) (cellCount x (j+1))
      IsLUB Y C ∧ ∀ p ∈ workingDomain, workingLikelihood x p < C)
```

### `UnivariateML.Discrete4DWeibull.held_probability_binary_supremum`

Holding probability fixed on a binary sample with a positive count gives a strict excluded Bernoulli supremum at infinite shape. [Lean source](../../lean/UnivariateML/Family/Discrete4DWeibull.lean#L48)

```lean
theorem held_probability_binary_supremum {n : ℕ} (x : Fin n → ℕ) (a : ℝ)
    (hx : ∀ i, x i ≤ 1) (hp : ∃ i, x i = 1) :
    let L := fun β ↦ likelihood x (a,β)
    let U := ∏ i, if x i = 0 then 1-exp (-exp a) else exp (-exp a)
    Tendsto L atTop (𝓝 U) ∧ IsLUB (L '' Ioi 0) U ∧ ∀ β > 0, L β < U
```

### `UnivariateML.Discrete4DWeibull.held_shape_all_zero_supremum`

Holding shape fixed on an all-zero sample gives the strict excluded zero atom as the survival probability tends to zero. [Lean source](../../lean/UnivariateML/Family/Discrete4DWeibull.lean#L76)

```lean
theorem held_shape_all_zero_supremum {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0) (β : ℝ) (_hβ : 0 < β) :
    let L := fun a ↦ likelihood x (a,β)
    Tendsto L atTop (𝓝 1) ∧ IsLUB (range L) 1 ∧ ∀ a, L a < 1
```

### `UnivariateML.Discrete4DWeibull.free_point_paths`

With free probability and shape, holding the collapse location inside a positive cell gives its point mass. At a zero common count the fixed-shape zero-probability path supplies the atom. [Lean source](../../lean/UnivariateML/Family/Discrete4DWeibull.lean#L124)

The chart uses a = log(-log q). The zero branch uses a fixed positive shape.

```lean
theorem free_point_paths (j k : ℕ) {c : ℝ} (h₁ : (j : ℝ) < c) (h₂ : c < (j : ℝ)+1) :
    Tendsto (fun β : ℝ ↦ mass k (-β*log c,β)) atTop (𝓝 (if k = j then 1 else 0)) ∧
    (j = 0 → ∀ β > 0, Tendsto (fun a : ℝ ↦ mass k (a,β)) atTop
      (𝓝 (if k = 0 then 1 else 0)))
```
