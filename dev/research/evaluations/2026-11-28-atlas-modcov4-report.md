# TASK-492 · Independent review of the fourth modifier Lean coverage batch

I checked 214 mappings to 43 declarations against the exact family, held mask, modifier and question in the proof ledger. All recorded source lines point to the named declarations, and the recorded signatures match. In `lean-zero6`, 18 rows are full and 130 are partial. In `lean-trunc5`, 8 rows are fully graded, 56 have only partial grades, one mixes full and partial grades, and one is blank. The grades record individual declaration scope. Complementary partial results do not promote a claim. [Zero6 map](../existence-atlas/modifier-lean-map.d/lean-zero6.csv) · [Trunc5 map](../existence-atlas/modifier-lean-map.d/lean-trunc5.csv) · [Proof ledger](../existence-atlas/modifier-proofs.csv) · [Review precedents](2026-11-28-atlas-modcov2-report.md), [count-window review](2026-11-28-atlas-modcov3-report.md)

## Atlas counts

Each cell gives full / partial-only Lean claims, before → after where the counts changed. There are still 2,340 eligible regimes. The complete and one-sided proof totals stayed the same. [Modifier overview](../existence-atlas/modifier-overview.md)

| Modifier | Criterion | Failure | Uniqueness |
| --- | ---: | ---: | ---: |
| `truncate:known` | 5 / 6 → 5 / 10 | 1 / 8 → 1 / 10 | 5 / 6 → 5 / 10 |
| `truncate:lower-estimated` | 4 / 0 → 6 / 10 | 0 / 4 | 4 / 0 → 6 / 10 |
| `truncate:upper-estimated` | 4 / 0 → 6 / 10 | 0 / 4 | 4 / 0 → 6 / 10 |
| `truncate:both-estimated` | 4 / 0 → 6 / 10 | 2 / 2 | 4 / 0 → 6 / 10 |
| `zero:truncated` | 2 / 8 → 2 / 12 | 3 / 5 → 3 / 7 | 3 / 1 → 5 / 1 |
| `zero:modified` | 0 / 34 → 6 / 32 | 0 / 30 | 0 / 4 → 5 / 3 |
| `zero:inflated` | 0 / 38 → 3 / 36 | 2 / 4 → 3 / 5 | 0 / 2 → 1 / 5 |
| **Total** | 19 / 86 → 34 / 120 | 8 / 57 → 9 / 62 | 20 / 13 → 34 / 49 |

## Zero6 review

Numbers in this section are CSV lines, with the header at line 1. For the scalar count charts I checked the positive-mean reductions and the tagged conditional endpoints. The Bernoulli domain is `[0,1]` or its intersection with a supplied held weight. The selected weight is the empirical fraction or the held value, respectively. [Modification statements](../../lean/UnivariateML/Family/Zero6.lean) · [Inflation statements](../../lean/UnivariateML/Family/Zero6Inflated.lean) · [Written zero-modifier rules](2026-11-27-zero-mod-math-report.md#likelihood-and-comparison-rules)

| Lines | Scope and grade |
| --- | --- |
| 2–9 | Full for Poisson and geometric modification, including the held-weight routes. The iff gives the compatible Bernoulli factor, the matched positive mean or reciprocal mean including the unit-count endpoint, and the flat entire parent slice on compatible all-zero data. |
| 10–13 | Full for the generic transfer criterion and maximizing fiber at held size. The chosen point comes from the unique positive NB fit, including the logarithmic carrier at size zero. Partial for the more explicit held-`p0` claims. The statement selects a witness by `Classical.choose` and leaves its public probability, mean equation and unit-count endpoint implicit. |
| 14–17 | Partial for the generic binomial transfer because the theorem assumes positive size and omits the size-zero all-zero slice. Full for the held-`p0` criterion, whose scope explicitly requires size at least one. Size one is flat in the tagged probability chart. Partial for the held-`p0` cardinality claim, which calls for the identified mean root and endpoint branches at larger size. The chosen maximizer alone leaves those branches implicit. |
| 18–21, 139 | `pois_inflated` gives the entire attainment criterion by specializing the compact inflation domain to the full interval or a singleton. The held all-zero result is a subcase of criterion and gives a strict likelihood bound and the rate-down-to-zero path. Its failure statement leaves exhaustion over maximizing sequences and the limiting pmf implicit. The positive held score adds exact comparison of score roots, with ties, for its sample branch. |
| 22–27 | `binom_inflated` proves attainment on both compact slices, giving full failure coverage for both masks. Criterion is partial because the positive branch lacks the written two-tilt rule or the held-`pi` compatibility and scalar rules. Cardinality is partial. The theorem gives the all-zero set and the free-inflation size-one product fiber. It leaves size-at-least-two uniqueness or held score contenders and the clipped held size-one probability open. |
| 28–37 | The positive NB declaration supplies an interior winner for positive size and a compatible inflation domain. It lacks the full boundary comparison. The all-zero declaration gives the height-one set, with the flat size-zero slice, but supplies no positive-data cardinality. Both contribute partial coverage for their respective mapped branches. |
| 38–40 | Full geometric free-inflation criterion, failure and cardinality. With continuation probability `q=1-prob` the effective zero mass is `v=1-q+q*pi`. For `q>0` this is a bijection over the stated interval, and `q=0` maps the whole raw inflation fiber to `v=1`. Positive likelihood is zero at `q=0` and positive at interior witnesses. Pulling back the all-zero optimizer `v=1` gives exactly `q=0` or `pi=1`. An explicit Lean corollary in the public coordinates would state that pullback directly. |
| 41–43, 137–138 | `geom_held` gives the all-zero fiber and positive attainment or incompatibility, with no positive-root conclusion or excluded-end path. The score theorem gives the unique interior root and its maximizing iff for compatible positives. These are complementary partial statements for the mapped questions. |
| 44–133 | All six `*_sample` declarations give partial modification criterion and failure and partial inflation criterion. The optional holds specialize to the mapped slices. `SampleClaim` assumes `zeroMass`, `positiveMass` and `conditionalWitness`. In addition to the modification maximizing iff and flat unused component, it multiplies a supplied conditional likelihood limit by the selected Bernoulli factor. It leaves reachability, law-level exhaustion and preservation of all escape ties to further statements. For inflation it states the factorization and all-zero height-one set, with neither the saturated or unsaturated branch optimization nor the held-inflation critical comparison. Even on the all-held parent slices it leaves the positive-data clipping rule implicit. |
| 134–136 | The two Weibull declarations resolve the all-ones positive-component subcase at held `shape1`. The strict bounds and converging likelihoods certify nonattainment on the respective scalar slices. The modification theorem takes a supplied compatible weight, and the inflation theorem takes a supplied held inflation weight. Neither asserts a distributional limit or covers arbitrary positive histograms. |
| 140–149 | The NB-beta and discretised-gamma held-inflation declarations supply a strict upper bound and sharp paths on all zeros with held inflation below one. For NB-beta at least one parent coefficient must be free, and the slice must be nonempty. The gamma result gives both one-coordinate paths from every positive parameter pair. These are partial criterion grades for that branch of the written parent-mask claims. |

The main statement-scope cautions are the exact-fit descriptions using chosen probability witnesses, the `*_sample` docstrings that advertise failure and inflation criterion transfers, and the Weibull docstrings that name the limiting law. The latter two path statements give a likelihood limit, while the law-level identification requires a separate conclusion. `Zero6Faces.compare` is a conditional comparison with supplied reachability, critical-set and strict-gap hypotheses. It has no mapping in these fragments and supplies no independent family closure classification. [Sample transfers and Weibull paths](../../lean/UnivariateML/Family/Zero6Families.lean) · [Held scores](../../lean/UnivariateML/Family/Zero6Scores.lean) · [All-zero paths](../../lean/UnivariateML/Family/Zero6ZeroAtoms.lean) · [Generic comparison](../../lean/UnivariateML/Family/Zero6Faces.lean)

## Trunc5 review

| Lines | Scope and grade |
| --- | --- |
| 2 | Full for the CMB zero-truncation nonuniqueness claim, which is specifically about `(1,2)`. The public-coordinate iff retains precisely size two and the complete odds ridge `2^ν` for every cap admitting size two. On a known finite window larger sizes can have their extra cells clipped. |
| 3–10 | `finite_size_comparison` is partial across the eight mapped designs and questions. It takes the allowed sizes, effective clipped cell sets, hold domains, slice suprema and attaining coefficient sets as data. It compares them exactly, including ties and whether any winning slice attains. It leaves deriving the slice data open. |
| 11–16 | The fixed-size CMB chart is partial for each mapped question. `fixed_size_maxima` gives the hull criterion and endpoint tags, leaving the explicit coefficient fibers in a canonical maximizer set. `fixed_size_face_height` requires a proper exposed face, exposing direction and maximizing face tilt. Its conclusion gives the sharp height and atom tag comparison, without a law-level reaching statement. `fixed_size_mixed_pair` treats only the two-cell branch with both counts observed and the empirical mixture supplied. |
| 17–20 | The held interior odds theorem needs finite log odds and supplied statistic extrema. It gives a unique interior winner, flat fits and sharp extreme heights, with no normalized-law convergence statement or held probability endpoint branches. At held dispersion, the extreme theorem gives the sharp constant-sample height and its endpoint admission, with only the extreme cell's mass limit stated. It omits interior mean matching and the singleton branch. These grades are partial. |
| 21–22 | The held-rate CMP finite theorem has flat and threshold tests and pointwise laws. The zero-rate compatibility conjunct is separate from the positive-rate optimizer statement. It lacks the joint public atom fiber, the interior moment equation and the ray scope. The all-free finite comparison takes the geometric count match as a hypothesis and only covers at least three cells. It gives the inward score test and the sharp partition bound, leaving the positive-dispersion fit, smaller windows, atom and adjacent branches open. All mapped questions are partial. |
| 23–28 | The sharp adjacent supremum and point path each earn partial failure coverage. Range separation gives partial criterion coverage through compact superlevels and attainment. `positive_window_unique` and `positive_window_attainment` each cover the complete cardinality claim, including the admissible geometric segment. The latter gives the exact range test for attainment and the sharp excluded height, so criterion and failure are partial. The criterion asks for the joint moment or geometric KKT solution. `positive_adjacent_winning_limit` assumes a pointwise limit and a maximizing likelihood path, and identifies the maximizing split implicitly. It leaves extraction of a limiting law along arbitrary maximizing paths and the explicit empirical split open. |
| 29–31 | The interior matching implication is partial. The joined held-rate positive theorem adds existence, uniqueness, the geometric score and the all-ones statistic case. It leaves interior root sufficiency and the sample-level translation of the excluded atom into the sharp winning likelihood and law to further statements. Its criterion and failure are partial. |
| 32–67 | The estimated-cut statements give the tight-window criterion, every zero-padding tie and two distinct nominal cuts whenever a fit attains. Where coefficients are free, the fixed-window problem is left as a maximizer set over the supplied hold domain, so both questions are partial. On the six fully held mappings at 41–43 and 62–64, specializing `D` to the held singleton gives the entire compatibility and cardinality claims. Positivity at every observed cell ensures positive retained mass because the tight window contains the sample. |

Two tags received no grade. At line 25 the failure tag on `positive_nonadjacent_attainment` concerns winning escape laws on nonattainment, while its hypotheses place the sample entirely in the attainment branch. At line 30 the divergent factorial mean is a boundary lemma. It does not assert moment matching, solve the inward score or establish attainment. All 214 mappings have corresponding ledger keys. The finite-size and estimated-cut docstrings describe exact comparisons at their stated reduction level. A family-level reading that includes the computation of the input heights or the full fixed-window coefficient solution would exceed the conclusions. Similarly, the positive CMP range theorem decides existence and cardinality, while the mapped criterion names a particular characterization of the coefficients. [CMB design comparison](../../lean/UnivariateML/Family/Trunc5CMB.lean) · [CMB fixed-size chart](../../lean/UnivariateML/Family/Trunc5CMBChart.lean) · [CMB scalar charts](../../lean/UnivariateML/Family/Trunc5CMBOdds.lean) · [CMP finite windows](../../lean/UnivariateML/Family/Trunc5CMP.lean) · [CMP positive faces](../../lean/UnivariateML/Family/Trunc5CMPFaces.lean) · [CMP held-rate ray](../../lean/UnivariateML/Family/Trunc5CMPRay.lean) · [Estimated cuts](../../lean/UnivariateML/Family/Trunc5Estimated.lean)

## Verification

All runs used the workstation at `7f070c2d`. I ran the generator once without options and twice with `--overview`. The two overview runs produced matching SHA-256 hashes for all four outputs. The base atlas and overview match their starting copies byte for byte. The generator's `--check`, the coverage-only fragment comparison, the generated-row comparison and `git diff --check` passed. All 130 changed modifier regimes have a reviewed grade in this batch, and each changed field records Lean evidence or its status.

The foreground `/tmp/umvl-cpu/run4 nice -n 10 just test-changed --no-long-double` passed with zero families selected. Shell startup printed two missing Cargo environment-file messages before selection. This was a statement and source review, with no Lean or package builds.

## Open Lean coverage by modifier and family group

The first three numeric columns count main questions per eligible held regime. “Partial only” still needs a full statement. “Written or cited” counts claims with written or cited evidence and no reviewed Lean grade. “Open in ledger” counts questions lacking a written or cited result and a reviewed Lean grade. Named cases are counted separately. The groups are the dossier's primary catalog groups. [Generated modifier atlas](../existence-atlas/modifier-atlas.csv)

| Modifier | Family group | Partial only | Written or cited | Open in ledger | Named cases without full Lean |
| --- | --- | ---: | ---: | ---: | ---: |
| `truncate:known` | Pareto and Lomax | 0 | 167 | 55 | 8 |
| `truncate:known` | Poisson, binomial and negative binomial | 30 | 66 | 34 | 20 |
| `truncate:known` | beta and generalised beta | 0 | 194 | 46 | 13 |
| `truncate:known` | bounded continuous | 0 | 53 | 7 | 10 |
| `truncate:known` | discrete heavy-tailed | 0 | 12 | 3 | 0 |
| `truncate:known` | extreme value | 0 | 38 | 31 | 11 |
| `truncate:known` | finite sampling | 0 | 46 | 8 | 11 |
| `truncate:known` | gamma and Weibull | 0 | 151 | 74 | 6 |
| `truncate:known` | normal and Student | 0 | 89 | 37 | 11 |
| `truncate:known` | normal convolutions | 0 | 18 | 30 | 8 |
| `truncate:known` | other positive | 0 | 36 | 21 | 3 |
| `truncate:known` | skewed and robust location-scale | 0 | 142 | 161 | 41 |
| `truncate:lower-estimated` | Pareto and Lomax | 0 | 167 | 55 | 123 |
| `truncate:lower-estimated` | Poisson, binomial and negative binomial | 24 | 68 | 34 | 35 |
| `truncate:lower-estimated` | beta and generalised beta | 0 | 194 | 46 | 164 |
| `truncate:lower-estimated` | bounded continuous | 0 | 53 | 7 | 34 |
| `truncate:lower-estimated` | discrete heavy-tailed | 0 | 15 | 3 | 0 |
| `truncate:lower-estimated` | extreme value | 0 | 38 | 31 | 45 |
| `truncate:lower-estimated` | finite sampling | 0 | 46 | 8 | 19 |
| `truncate:lower-estimated` | gamma and Weibull | 0 | 151 | 74 | 107 |
| `truncate:lower-estimated` | normal and Student | 0 | 89 | 37 | 58 |
| `truncate:lower-estimated` | normal convolutions | 0 | 18 | 30 | 34 |
| `truncate:lower-estimated` | other positive | 0 | 36 | 21 | 28 |
| `truncate:lower-estimated` | skewed and robust location-scale | 0 | 142 | 161 | 171 |
| `truncate:upper-estimated` | Pareto and Lomax | 0 | 167 | 55 | 119 |
| `truncate:upper-estimated` | Poisson, binomial and negative binomial | 24 | 68 | 34 | 34 |
| `truncate:upper-estimated` | beta and generalised beta | 0 | 194 | 46 | 164 |
| `truncate:upper-estimated` | bounded continuous | 0 | 53 | 7 | 34 |
| `truncate:upper-estimated` | discrete heavy-tailed | 0 | 15 | 3 | 0 |
| `truncate:upper-estimated` | extreme value | 0 | 38 | 31 | 47 |
| `truncate:upper-estimated` | finite sampling | 0 | 46 | 8 | 17 |
| `truncate:upper-estimated` | gamma and Weibull | 0 | 151 | 74 | 107 |
| `truncate:upper-estimated` | normal and Student | 0 | 89 | 37 | 63 |
| `truncate:upper-estimated` | normal convolutions | 0 | 18 | 30 | 34 |
| `truncate:upper-estimated` | other positive | 0 | 35 | 22 | 29 |
| `truncate:upper-estimated` | skewed and robust location-scale | 0 | 142 | 161 | 176 |
| `truncate:both-estimated` | Pareto and Lomax | 0 | 167 | 55 | 118 |
| `truncate:both-estimated` | Poisson, binomial and negative binomial | 22 | 68 | 34 | 34 |
| `truncate:both-estimated` | beta and generalised beta | 0 | 194 | 46 | 164 |
| `truncate:both-estimated` | bounded continuous | 0 | 53 | 7 | 34 |
| `truncate:both-estimated` | discrete heavy-tailed | 0 | 15 | 3 | 0 |
| `truncate:both-estimated` | extreme value | 0 | 38 | 31 | 45 |
| `truncate:both-estimated` | finite sampling | 0 | 46 | 8 | 17 |
| `truncate:both-estimated` | gamma and Weibull | 0 | 151 | 74 | 110 |
| `truncate:both-estimated` | normal and Student | 0 | 89 | 37 | 58 |
| `truncate:both-estimated` | normal convolutions | 0 | 18 | 30 | 34 |
| `truncate:both-estimated` | other positive | 0 | 35 | 22 | 29 |
| `truncate:both-estimated` | skewed and robust location-scale | 0 | 142 | 161 | 171 |
| `zero:truncated` | Poisson, binomial and negative binomial | 20 | 58 | 38 | 14 |
| `zero:truncated` | discrete heavy-tailed | 0 | 0 | 6 | 0 |
| `zero:truncated` | gamma and Weibull | 0 | 16 | 8 | 0 |
| `zero:modified` | Poisson, binomial and negative binomial | 49 | 48 | 144 | 0 |
| `zero:modified` | discrete heavy-tailed | 0 | 0 | 12 | 0 |
| `zero:modified` | gamma and Weibull | 16 | 0 | 32 | 0 |
| `zero:inflated` | Poisson, binomial and negative binomial | 38 | 50 | 157 | 0 |
| `zero:inflated` | discrete heavy-tailed | 0 | 0 | 12 | 0 |
| `zero:inflated` | gamma and Weibull | 8 | 8 | 32 | 0 |
| **Total** | | **231** | **4,241** | **2,471** | **2,612** |
