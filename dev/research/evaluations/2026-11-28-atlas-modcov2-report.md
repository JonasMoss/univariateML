# TASK-482 · Independent review of the second and third modifier Lean maps

I reviewed all 292 mappings against the Lean declaration at the recorded line and the written claim for the exact family, hold, modifier and question. The second fragment has 2 full rows, 112 partial rows and 32 blank rows. The third has 122 partial rows and 24 blank rows. The two full rows use the same theorem. They promote the geometric zero-truncation criterion and add a second theorem to its already Lean-backed uniqueness claim. A full grade requires one declaration to cover the entire written claim. Separate partial declarations can establish complementary pieces of a claim. [Second fragment](../existence-atlas/modifier-lean-map.d/lean-trunc2.csv) · [Third fragment](../existence-atlas/modifier-lean-map.d/lean-trunc3.csv) · [Written claims](../existence-atlas/modifier-proofs.csv) · [Precedent](2026-11-28-atlas-modlean-report.md)

## Atlas counts

Each cell shows full Lean claims / partial-only claims before and after. A claim can have several Lean declarations. The 2,340 eligible regimes and the complete and one-sided proof counts were identical in both runs. [Modifier overview](../existence-atlas/modifier-overview.md)

| Modifier | Criterion | Failure | Uniqueness |
| --- | ---: | ---: | ---: |
| `truncate:known` | 0 / 4 → 0 / 11 | 0 / 4 → 0 / 9 | 0 / 4 → 0 / 11 |
| `truncate:lower-estimated` | 0 / 0 → 0 / 4 | 0 / 0 → 0 / 4 | 0 / 0 → 0 / 4 |
| `truncate:upper-estimated` | 0 / 0 → 0 / 4 | 0 / 0 → 0 / 4 | 0 / 0 → 0 / 4 |
| `truncate:both-estimated` | 0 / 0 → 0 / 4 | 0 / 0 → 0 / 4 | 0 / 0 → 0 / 4 |
| `zero:truncated` | 1 / 3 → 2 / 8 | 3 / 1 → 3 / 5 | 3 / 1 → 3 / 1 |
| `zero:modified` | 0 / 4 → 0 / 34 | 0 / 0 → 0 / 30 | 0 / 4 → 0 / 4 |
| `zero:inflated` | 0 / 8 → 0 / 38 | 2 / 4 → 2 / 4 | 0 / 2 → 0 / 2 |
| **Total** | **1 / 19 → 2 / 103** | **5 / 9 → 5 / 60** | **3 / 11 → 3 / 30** |

## Second fragment

The known-window claims include admitted endpoint laws and singleton fits. The estimated-cut claims also describe the nominal bound fibers and the exact positive alias. The table gives the review by declaration. Row numbers below count the CSV header as line 1. [Lean count statements](../../lean/UnivariateML/Family/Trunc2Known.lean) · [Tight-window profiling](../../lean/UnivariateML/Generic/TruncEstimated.lean) · [Written estimated-cut argument](2026-11-27-trunc-est-math-report.md)

| Declaration or group | Grade and scope |
| --- | --- |
| `regular_known`, `regular_known_failure` | Partial for the mapped known-cut questions. They work on effective integer intervals or rays in the ordinary parent chart. In particular an all-ones sample on exact nominal `[1, Inf]` needs the separately tagged zero-parent alias, which is outside the ordinary chart. NB also requires positive held size. The failure conclusion is a nonattainment equivalence expressed by `escapes`. Convergence to the named laws is separate. |
| `regular_window_fit` | Partial for criterion, failure and uniqueness. It gives the exact attainable-or-alias test and all optimal pairs in the chart, with binomial clipping and the zero-removed-mass plateau. The lower cut is real in both the fixed and optimized windows. A real cut at or below zero can encode the same retained cells as an unbounded lower side, but the theorem does not quantify over the negative-infinite nominal bound or identify that bound as a member of a maximizing fiber. Positive held size is required for NB. `escapes` identifies the tests and the theorem does not state convergence to the named laws. |
| `regular_finite_faces`, `regular_ray_face` | Partial where the relevant failure can occur. The finite theorem gives pointwise endpoint laws and the upper carrier monotonicity test for negative tilts. It assumes an effective nonsingleton with a valid carrier. The ray theorem gives pointwise convergence at the low tilt without testing whether the limit wins or is already admitted. |
| `nbinom_zero_window_fit` | Partial for all three questions in all four designs. It treats size zero and the exact alias on real nominal lower cuts. It proves the two-way attainment test, so its negation gives the incompatible branch. It specifies the maximizing set in its tagged chart, without the matched-probability equation in the statement. The positive-size branches need their own statements. |
| `nbinom_zero_alias`, `nbinom_zero_alias_atom`, `nbinom_zero_alias_match`, `nbinom_zero_ordinary` | Partial for the mapped questions. The first gives existence and uniqueness in the log-series chart given a compatible mean. The atom theorem identifies unit mean. The matching theorem assumes an interior maximizing tilt and gives the log-series mean equation. The ordinary theorem is a candidate-membership equivalence for the zero-atom branch, with no joint maximizer assertion. The fixed or estimated nominal design enters through `nbinom_zero_window_fit`. |
| `geom_zt_reciprocal` | Full for criterion and uniqueness at lines 60–61. The iff identifies the exact public probability on the whole compatible positive-mean domain, including probability one at mean one. |

### Blank mappings

- `regular_coefficient_chart` at lines 2, 7, 15, 20, 27, 35, 46, 54, 70, 84, 101, 115, 121, 127, 136 and 142 is a chart bijection. It does not constrain the number of maximizers.
- `geom_carrier_midpoint` at lines 25–26, 33–34, 44–45 and 52–53 computes the threshold. Attainment and nonattainment at that threshold require separate results.
- `regular_ray_face` at lines 29, 56, 72, 117, 123 and 144 concerns rays. Tightening an estimated upper cut to the observed maximum gives a finite window. A looser admitted alias is a separate finite-coefficient branch.
- `regular_finite_faces` at lines 3 and 122 gives Poisson or binomial endpoint limits in a general finite window. With both cuts estimated, each nonsingleton tight window has a strictly interior mean and each constant sample has an attained singleton. Those endpoint limits do not establish absence of an escape in that design.

## Third fragment

### CMB, lines 2–38

- `canonical_fit` and `positive_fit` give partial criterion coverage for finite canonical coefficients. Their existential conclusions give no cardinality result. They exclude the deterministic endpoint tags. `singleton` gives a flat canonical objective, and `endpoint_admission` tests retained atom mass. The latter alone does not compare likelihoods on a sample. `endpoint_strictly_wins` only compares a specified admitted atom, on its endpoint-only data at a nonsingleton, against one finite canonical coefficient at a time.
- `pair_fibre` assumes an existing maximizer. `pair_empirical_odds` identifies the full finite-chart line for an empirical mixture of two different retained cells. `identified` assumes a nonzero determinant and two maximizers. `three_cell_unique` removes the determinant hypothesis for three ordered cells, with both maximizers still supplied. These give partial known-cut cardinality coverage at fixed size.
- `held_dispersion_fit` covers the nonsingleton scalar fit with admitted count endpoint tags. `held_odds_fit` assumes distinct attained extreme heights with all retained heights between them, in the finite log-odds chart. `held_odds_flat` covers a flat slice at fixed finite log odds. `held_dispersion_limits` gives partial failure coverage for the scalar fixed-size tilt on valid retained counts. It gives two pointwise limits and does not classify which endpoint is already admitted.
- `exposed_window_supremum` gives partial failure coverage when all observations lie on a supplied proper exposed face. Its LUB ranges over one ray with prescribed base vector. It does not optimize the base vector on the face or compare rays and design slices.
- `size_slice_winners` gives partial criterion coverage for free size and partial known-cut uniqueness coverage. It assumes exact slice suprema and characterizes attainment and ties once those heights have been supplied. It supplies no reachable limit law for either failure mapping. The zero-truncation uniqueness claim at free size specifically asks for the `(1,2)` example, size two and the exact odds fiber. None of these conclusions identifies all three.
- The fixed-size zero-truncation cardinality questions tagged at lines 3–4, 6, 8, 12, 16, 18, 24, 30 and 32 have no written uniqueness row at those masks. The canonical uniqueness tag at line 2 also exceeds its existential conclusion. Leave lines 23–28 blank because `adjacent_strict`, `adjacent_exposure` and `lower_chord` describe geometry without cardinality or a reaching likelihood path. The `exposed_window_mass` results at lines 33–34 are limits for a supplied cell on a supplied face. Their mapped criterion requires attainment. [CMB statements](../../lean/UnivariateML/Family/Trunc3CMB.lean) · [Known-cut claims](2026-11-27-trunc-discrete-math-report.md#cmb-all-free-truncateknown) · [Positive-cut claims](2026-11-27-zero-mod-math-report.md#cmb-all-free-truncated)

### CMP, lines 39–62

- The zero-rate compatibility and singleton results give partial criterion coverage for the atom and flat branches. At held dispersion, the positive-dispersion finite fit gives the count-mean test with a tagged zero atom. The zero-dispersion finite fit requires a strictly interior empirical count mean and an open geometric chart. Its failure theorem treats means at or above the midpoint. The finite atom limits give reachable pointwise laws and leave the endpoint admissions and global branch comparison separate.
- At held positive rate, the interior theorem takes a strictly interior empirical factorial statistic and a nonconstant statistic window. The minimum failure theorem also assumes a nonconstant factorial statistic. The geometric theorem tests winners only at or above the carrier threshold. Its separate limit theorem gives the geometric carrier and its minimum-limit counterpart gives masses on the exposed set. The binary-flat theorem supplies the `{0,1}` slice. Each supports a proper part of its mapped claim.
- On the positive ray, `held_dispersion_positive` and `held_zero_dispersion_positive` give distinct scalar attainment branches. `held_rate_positive_fit` is an iff for existence of a unique minimizer of the centered normalizer. It does not state the matching equation in the positive-dispersion interior. `held_rate_geometric_edge` states the edge score only for rate below one. The atom statements give the reachable cell masses and do not assert a winning escape or exhaust the escaping sequences.
- Line 47 maps a uniform limit to criterion. Line 53 maps the identity of the minimum factorial cells to failure. Neither conclusion settles the mapped question, and the reaching path for line 53 has its own theorem. Lines 59–60 concern zero cellwise mass at an inadmissible geometric edge and convergence to an admitted geometric law, respectively. Neither law is an excluded positive-likelihood winner of the cited positive-ray claim. Lines 55, 58 and 61 have no written uniqueness row at the mapped hold. [CMP finite and positive windows](../../lean/UnivariateML/Family/Trunc3CMP.lean) · [Held-rate ray](../../lean/UnivariateML/Family/Trunc3CMPRay.lean) · [Written positive-ray tests](2026-11-27-zero-mod-math-report.md#cmp-all-free-truncated)

### Zero-modifier transfers, lines 63–140

- `modified_held` assumes a positive fixed Bernoulli factor. `modified_free` assumes a positive likelihood witness and covers free weight. `modified_zero` takes an all-zero sample and a supplied parent domain. Each covers a branch of the written criterion. `modified_face_limit` transfers the height of a supplied convergent path. It supplies neither a reaching path for a particular family face nor exhaustion or preservation of the full set of maximizing escape ties.
- `inflated_clip` optimizes the weight with the parent fixed and positive conditional likelihood. `inflated_transfer`, `inflated_saturated_max_iff` and `inflated_free_feasible` presuppose an attained positive-part maximum and realizability of the empirical effective zero weight. They cover the saturated branch, including its exact maximizing set where stated. They leave the unsaturated branch, held-weight score and family-specific edge comparisons open.
- `inflated_zero` gives the exact height-one test on all zeros. `inflated_zero_held` orders parent zero masses at held inflation below one and leaves their maximization and attainment to the parent. `inflated_no_zeros` and `inflated_no_zeros_held` transfer parent ranking for zero-free samples under their stated compatibility conditions. These are criterion subcases. The factorization at lines 123–128 is an identity with no comparison or attainment conclusion.
- I specialized the statements at the written parent masks for each of the six mapped families. A theorem for one branch of the weight choice can partially cover the written criterion at a parent mask because that criterion discusses both free and held weight. A separate mask containing `p0` or `pi` needs its own written claim. There are no such written keys in these six mappings. [Transfer statements](../../lean/UnivariateML/Family/Trunc3Zero.lean) · [Written rules](2026-11-27-zero-mod-math-report.md#likelihood-and-comparison-rules)

### Signed zeta, lines 141–147

Line 141 is an objective identity. `signed_fit` gives an iff for an attained singleton maximizing set on `[1,N]` with `N>1`. It leaves the `N=1` criterion and cardinality branch separate. Its left-hand side combines existence and pairwise uniqueness. By itself, failure of that conjunction is weaker than nonexistence. `uniform_fit` and `singleton` cover their respective criterion subcases. The atom declarations give pointwise limits. `endpoint_supremum` gives the strict supremum on samples entirely at one specified endpoint. It does not say that these exhaust the nonattainment cases on compatible samples. All six graded rows are partial. [Signed zeta statements](../../lean/UnivariateML/Family/Trunc3Zeta.lean) · [Written criterion](2026-11-27-trunc-expfam-math-report.md#zeta-free-truncateknown)

## Open formalization

| Regime | What is missing for the written scope |
| --- | --- |
| Regular count windows | Formalize the extension from the real lower-cut chart to the negative-infinite nominal lower bound, including the maximizing fibers. Join the public coefficient chart, ordinary and alias branches, and the failure tests with the stated laws. At held NB size zero, join the compatibility decision and exact bounds with the matched probability. |
| CMB windows | Join the canonical and scalar conditions to the admitted atoms, singleton and short-window fibers. Derive all relevant face heights at fixed size and at held odds, carry the attainment bits into the size comparison, and retain ties. The all-free positive `(1,2)` claim still needs the global size-two selection and public odds ridge. |
| CMP windows | Join the zero-rate, flat, scalar and boundary cases in the same chart, including lower-extreme nonattainment and the excluded geometric threshold. On the held-rate positive ray, state the interior matching equation and divergence of the expected log factorial at an excluded geometric edge. Prove exhaustion and winning-height comparisons for the all-free point and adjacent faces. |
| Six zero-modifier families | Instantiate the factored likelihoods on the family hold domains, including degenerate parent atoms, and join the sample branches. For failure, prove the hold-compatible family face list, transport attaining paths and compare limiting heights with ties. For inflation, the transfer hypotheses leave the unsaturated and held-weight critical comparisons to be formalized. |
| Signed zeta | State a single sample-level result splitting the singleton, interior log-mean and two endpoint-only cases, with attainment, cardinality and limiting law in their respective branches. |

## Written claims remaining by family

C, F and U denote criterion, failure and uniqueness. The first table lists every main question with some reviewed Lean support after regeneration, including earlier work. Every other written main question at the exact family, modifier and held mask in the proof ledger has no reviewed Lean support. Partial-only claims still need full formalization. Every written named case has no reviewed Lean grade. The second table counts the unsupported claims and the partial-only main questions by family. [Proof ledger](../existence-atlas/modifier-proofs.csv) · [Generated atlas](../existence-atlas/modifier-atlas.csv)

| Family | Modifier | Holds | Full | Partial only |
| --- | --- | --- | --- | --- |
| `bbinom` | `zero:modified` | `all free`, `alpha`, `alpha+size`, `beta`, `beta+size`, `size` | — | CF |
| `bbinom` | `zero:inflated` | `all free`, `alpha`, `alpha+size`, `beta`, `beta+size`, `size` | — | C |
| `binom` | `truncate:known`, `truncate:lower-estimated`, `truncate:upper-estimated`, `truncate:both-estimated`, `zero:truncated` | `size` | — | CFU |
| `binom` | `zero:modified` | `size` | — | CU |
| `binom` | `zero:inflated` | `size` | F | CU |
| `binom` | `zero:inflated` | `pi+size` | F | C |
| `bnbinom` | `zero:modified` | `all free`, `alpha`, `alpha+beta`, `alpha+beta+size`, `alpha+size`, `beta`, `beta+size`, `size` | — | CF |
| `bnbinom` | `zero:inflated` | `all free`, `alpha`, `alpha+beta`, `alpha+beta+size`, `alpha+size`, `beta`, `beta+size`, `size` | — | C |
| `cmb` | `truncate:known` | `all free`, `prob+size` | — | CU |
| `cmb` | `zero:truncated` | `all free`, `prob+size` | — | C |
| `cmb` | `zero:inflated` | `nu+prob` | — | C |
| `cmb` | `truncate:known` | `nu+size`, `size` | — | CFU |
| `cmb` | `zero:truncated` | `nu+size`, `size` | — | CF |
| `cmp` | `zero:modified` | `all free`, `lambda`, `lambda+nu`, `nu` | — | CF |
| `cmp` | `zero:inflated` | `all free`, `lambda`, `lambda+nu`, `nu` | — | C |
| `cmp` | `truncate:known` | `lambda`, `nu` | — | CFU |
| `cmp` | `zero:truncated` | `lambda`, `nu` | — | CF |
| `dgamma` | `zero:modified` | `all free`, `rate`, `rate+shape`, `shape` | — | CF |
| `dgamma` | `zero:inflated` | `all free`, `rate`, `rate+shape`, `shape` | — | C |
| `dweibull` | `zero:modified` | `all free`, `shape1`, `shape1+shape2`, `shape2` | — | CF |
| `dweibull` | `zero:inflated` | `all free`, `shape1`, `shape1+shape2`, `shape2` | — | C |
| `geom` | `truncate:known`, `truncate:lower-estimated`, `truncate:upper-estimated`, `truncate:both-estimated`, `zero:inflated` | `all free` | — | CFU |
| `geom` | `zero:truncated` | `all free` | CFU | — |
| `geom` | `zero:modified` | `all free` | — | CU |
| `nbinom` | `truncate:known`, `truncate:lower-estimated`, `truncate:upper-estimated`, `truncate:both-estimated` | `size` | — | CFU |
| `nbinom` | `zero:truncated` | `size` | FU | C |
| `nbinom` | `zero:modified` | `size` | — | CU |
| `nbinom` | `zero:inflated` | `size`, `pi+size` | — | CF |
| `pig` | `zero:modified` | `all free`, `mean`, `mean+shape`, `shape` | — | CF |
| `pig` | `zero:inflated` | `all free`, `mean`, `mean+shape`, `shape` | — | C |
| `pois` | `truncate:known`, `truncate:lower-estimated`, `truncate:upper-estimated`, `truncate:both-estimated` | `all free` | — | CFU |
| `pois` | `zero:truncated` | `all free` | CFU | — |
| `pois` | `zero:modified` | `all free` | — | CU |
| `pois` | `zero:inflated` | `all free` | — | CF |
| `pois` | `zero:inflated` | `pi` | — | C |
| `zeta` | `truncate:known` | `all free` | — | CFU |

| Family | Criterion | Failure | Uniqueness | Named cases | Partial only |
| --- | ---: | ---: | ---: | ---: | ---: |
| `bbinom` | 30 | 36 | 13 | 23 | 18 |
| `beta` | 12 | 12 | 12 | 0 | 0 |
| `betapr` | 32 | 32 | 16 | 48 | 0 |
| `bhatt` | 16 | 16 | 4 | 55 | 0 |
| `binom` | 17 | 18 | 18 | 0 | 20 |
| `bnbinom` | 40 | 48 | 5 | 65 | 24 |
| `burr` | 32 | 32 | 8 | 48 | 0 |
| `cauchy` | 16 | 16 | 4 | 10 | 0 |
| `cmb` | 47 | 52 | 29 | 1 | 17 |
| `cmp` | 16 | 20 | 15 | 0 | 22 |
| `dblpareto` | 32 | 32 | 32 | 27 | 0 |
| `dgamma` | 20 | 24 | 4 | 25 | 12 |
| `dlaplace` | 16 | 16 | 16 | 0 | 0 |
| `dnorm` | 16 | 16 | 4 | 21 | 0 |
| `dunif` | 16 | 16 | 16 | 0 | 0 |
| `dweibull` | 20 | 24 | 8 | 14 | 12 |
| `emg` | 16 | 16 | 4 | 55 | 0 |
| `exp` | 4 | 4 | 4 | 0 | 0 |
| `expweibull` | 12 | 12 | 4 | 40 | 0 |
| `fatigue` | 12 | 12 | 4 | 48 | 0 |
| `foldnorm` | 16 | 16 | 4 | 28 | 0 |
| `fpareto` | 88 | 88 | 60 | 134 | 0 |
| `frechet` | 32 | 32 | 8 | 56 | 0 |
| `gamma` | 12 | 12 | 12 | 0 | 0 |
| `gb2` | 28 | 28 | 12 | 42 | 0 |
| `ged` | 20 | 20 | 8 | 52 | 0 |
| `genbeta` | 60 | 60 | 32 | 94 | 0 |
| `gengamma` | 28 | 28 | 4 | 42 | 0 |
| `geom` | 2 | 3 | 2 | 0 | 17 |
| `gev` | 0 | 0 | 0 | 42 | 0 |
| `gompertz` | 16 | 16 | 10 | 17 | 0 |
| `gpd` | 16 | 16 | 8 | 44 | 0 |
| `gumbel` | 16 | 16 | 8 | 6 | 0 |
| `hcauchy` | 16 | 16 | 4 | 15 | 0 |
| `hnorm` | 16 | 16 | 16 | 4 | 0 |
| `ht` | 32 | 32 | 4 | 47 | 0 |
| `huber` | 28 | 28 | 0 | 66 | 0 |
| `hyper` | 28 | 28 | 12 | 32 | 0 |
| `invburr` | 32 | 32 | 8 | 48 | 0 |
| `invexp` | 8 | 8 | 8 | 6 | 0 |
| `invgamma` | 4 | 4 | 4 | 0 | 0 |
| `invgauss` | 12 | 12 | 12 | 0 | 0 |
| `invparalogis` | 16 | 16 | 4 | 24 | 0 |
| `invpareto` | 16 | 16 | 8 | 24 | 0 |
| `invtrgamma` | 32 | 32 | 16 | 48 | 0 |
| `invweibull` | 4 | 4 | 4 | 0 | 0 |
| `kumar` | 12 | 12 | 8 | 12 | 0 |
| `laplace` | 16 | 16 | 12 | 13 | 0 |
| `lgamma` | 16 | 16 | 16 | 16 | 0 |
| `lgser` | 8 | 8 | 8 | 0 | 0 |
| `llogis` | 16 | 16 | 4 | 21 | 0 |
| `lnorm` | 12 | 12 | 12 | 0 | 0 |
| `logis` | 16 | 16 | 8 | 16 | 0 |
| `logitnorm` | 16 | 16 | 16 | 16 | 0 |
| `lomax` | 16 | 16 | 8 | 18 | 0 |
| `lst` | 28 | 28 | 4 | 43 | 0 |
| `naka` | 16 | 16 | 16 | 24 | 0 |
| `nbinom` | 9 | 10 | 10 | 0 | 19 |
| `nhyper` | 28 | 28 | 12 | 32 | 0 |
| `norm` | 12 | 12 | 12 | 0 | 0 |
| `nsbeta` | 56 | 56 | 44 | 144 | 0 |
| `paralogis` | 16 | 16 | 4 | 24 | 0 |
| `pareto` | 12 | 12 | 12 | 0 | 0 |
| `pareto2` | 32 | 32 | 16 | 39 | 0 |
| `pareto3` | 20 | 20 | 12 | 44 | 0 |
| `pareto4` | 32 | 32 | 24 | 82 | 0 |
| `pgw` | 16 | 16 | 4 | 49 | 0 |
| `pig` | 20 | 24 | 4 | 27 | 12 |
| `pois` | 1 | 3 | 3 | 0 | 17 |
| `power` | 16 | 16 | 16 | 12 | 0 |
| `rayleigh` | 4 | 4 | 4 | 0 | 0 |
| `sged` | 24 | 24 | 4 | 101 | 0 |
| `sgomp` | 16 | 16 | 4 | 24 | 0 |
| `skellam` | 16 | 16 | 4 | 21 | 0 |
| `slash` | 12 | 12 | 0 | 22 | 0 |
| `sn` | 20 | 20 | 4 | 55 | 0 |
| `snorm` | 20 | 20 | 4 | 55 | 0 |
| `sstd` | 20 | 20 | 4 | 101 | 0 |
| `st` | 8 | 8 | 0 | 12 | 0 |
| `std` | 16 | 16 | 4 | 47 | 0 |
| `tlambda` | 16 | 16 | 4 | 41 | 0 |
| `trgamma` | 32 | 32 | 16 | 48 | 0 |
| `triang` | 32 | 4 | 32 | 84 | 0 |
| `unif` | 16 | 16 | 16 | 0 | 0 |
| `weibull` | 16 | 16 | 8 | 18 | 0 |
| `yulesimon` | 8 | 8 | 8 | 0 | 0 |
| `zeta` | 3 | 3 | 3 | 0 | 3 |
| **Total** | 1721 | 1733 | 892 | 2612 | 193 |

## Verification

On the workstation at base `42448496`, both foreground `python3 dev/tools/existence_atlas.py --overview` runs exited successfully and produced identical SHA-256 hashes for all four generated outputs. Byte comparisons passed for the base `atlas.csv` and `overview.md` against their pre-review copies. The foreground generator `--check`, the coverage-column and status-transition audit, and `git diff --check` passed. `/tmp/umvl-cpu/run4 just test-changed --no-long-double` exited successfully after selecting zero families. The login shell reported two missing Rust environment files before the runner printed its selection. The job used no builds.
