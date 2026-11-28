# TASK-486 · Independent review of lean-trunc4 modifier coverage

The 17 declarations promote 37 written claims to Lean and add 14 partial failure mappings. Every recorded line points to the named theorem, and every mapping addresses a written claim at its exact family, hold mask and modifier. The review applies the scope standard of the two previous audits. [Reviewed map](../existence-atlas/modifier-lean-map.d/lean-trunc4.csv) · [Proof ledger](../existence-atlas/modifier-proofs.csv) · [Previous audit](2026-11-28-atlas-modcov2-report.md) · [First audit](2026-11-28-atlas-modlean-report.md)

## Review by declaration

All 17 declarations have full criterion and uniqueness coverage. The table records the failure grade and the design-specific content checked in each statement. [Count declarations](../../lean/UnivariateML/Family/Trunc4Count.lean) · [Signed-zeta declaration](../../lean/UnivariateML/Family/Trunc4Zeta.lean) · [Known-cut proofs](2026-11-27-trunc-expfam-math-report.md) · [Estimated-cut proofs](2026-11-27-trunc-est-math-report.md)

| Declaration | Line | Failure | Scope checked |
| --- | ---: | --- | --- |
| `pois_known` | 766 | Partial | Singleton, interior rate or admitted lower atom. The excluded lower and finite upper atoms have rate limits zero and infinity. |
| `pois_lower_estimated` | 777 | Partial | Tight lower cut at the minimum. Nonconstant data fit, and a nonsingleton constant has an excluded lower atom unless zero or the exact alias admits it. |
| `pois_upper_estimated` | 788 | Partial | Tight upper cut at the maximum. A nonsingleton constant escapes to the top atom, and the looser alias can add a zero-rate pair to the all-ones singleton fit. |
| `pois_both_estimated` | 799 | Full | The extrema give the tight window. The explicit nonempty maximizing set proves absence of escape, including on constant data. |
| `geom_known` | 814 | Partial | Lower atom and alias admission, the finite midpoint and the unbounded upper threshold on a ray. The excluded laws are the lower atom and the finite uniform. |
| `geom_lower_estimated` | 825 | Partial | The minimum is the retained bottom. The lower atom can be excluded, and the finite midpoint is the threshold for the excluded uniform. |
| `geom_upper_estimated` | 836 | Partial | The maximum is the retained top. A nonsingleton can escape to the uniform at or above the midpoint. |
| `geom_both_estimated` | 847 | Partial | The explicit attainment test is a singleton or a mean below the midpoint of the sample extrema. The nonsingleton failure law is the finite uniform. |
| `binom_known` | 870 | Partial | Held size clips the upper count. Compatible endpoint atoms and the alias enter the maximizing set, and the remaining endpoint atoms are approached at probabilities zero and one. |
| `binom_lower_estimated` | 882 | Partial | The lower cut is the minimum. The nonsingleton excluded atom is at the bottom, subject to the ordinary zero and exact alias admissions. |
| `binom_upper_estimated` | 894 | Partial | The upper cut is the maximum. The top atom is admitted on a nonsingleton at the held size and excluded below that size. |
| `binom_both_estimated` | 906 | Full | The explicit nonempty maximizing set proves absence of escape on every sample compatible with the held size. |
| `nbinom_known` | 922 | Partial | Positive size uses the lower atom and finite rising-factorial thresholds. Compatible size-zero samples attain through the zero atom or exact positive alias. |
| `nbinom_lower_estimated` | 931 | Partial | Positive size uses the sample minimum and the carrier threshold. At size zero the positive alias can attain even when the sample minimum exceeds one. |
| `nbinom_upper_estimated` | 940 | Partial | Positive size uses the sample maximum and the finite carrier threshold. At size zero the maximizing alias can require a looser infinite upper cut. |
| `nbinom_both_estimated` | 949 | Partial | Positive size uses both extrema and the finite carrier threshold. At size zero all-zero samples and positive alias samples attain, and mixed zero and positive samples have no positive-likelihood candidate. |
| `signed_zeta_known` | 20 | Full | On finite signed `[1,N]`, the singleton is flat, interior empirical log means give one maximizer, and both endpoint-only branches have an empty maximizing set and likelihood supremum one reached in the limit. |

The compatibility witnesses encode a nonempty sample in an admissible window. The extrema and mean hypotheses name statistics every such sample supplies. The binomial bound `hi ≤ N` is compatibility with held size. The NB statements require a compatible-window witness only at positive size, leaving size-zero incompatibility within the conclusion.

Unfolding `RegularClaim` gives the public coefficient bijection, the attainment equivalence, every maximizing coefficient-window pair, the flat singleton and the nonsingleton coefficient cardinality. The zero-removed-weight condition gives the ordinary nominal plateaus, including a negative-infinite lower cut when admissible. The exact nominal `[1, Inf]` condition controls the added atom. In `ZeroNBClaim` the zero branch gives the full probability-by-window fiber, the positive branch gives the unique alias pair and its mean equation, and the incompatible branch has an empty positive-likelihood candidate set.

## Statement limits and open formalization

| Regime | Remaining statement or scope |
| --- | --- |
| Regular count windows | The earlier nominal-cut, public-chart, endpoint-admission, alias and size-zero NB obligations are covered. The 14 partial failure grades are on the count declarations whose docstrings advertise the failure-law claim. The statements classify nonattainment and give the limiting public coefficient and pointwise law on the retained counts. For the finite uniform and rising-factorial limits they also give strict increase of the tight-window scalar log likelihood up to zero tilt. To state the full winning-limit comparison directly, show that the sample likelihood on each specified path converges to the supremum over the joint coefficient-window candidate set. The estimated-cut proof also states total variation convergence, which needs a statement with normalized conditional laws and convergence in total variation. These gaps concern the reach and mode of the formal statements. The excluded branch tests themselves are present. |
| Signed zeta | The previous open item is closed for the written known-cut claim on the finite signed window. The declaration gives the sample likelihood limit and its least upper bound in each excluded branch. Its name is scoped by the signed window, and it supplies no grade for the parent-only domain or the three estimated-cut designs. |

There were no helper-only or unrelated mappings and no incorrect line values among the 17 rows. The other open entries in the previous audit concern regimes outside this review. [Previous open formalization table](2026-11-28-atlas-modcov2-report.md#open-formalization)

## Atlas counts

Cells show full Lean claims / partial-only claims before and after. The atlas still has 2,340 eligible regimes. Complete and one-sided proof counts are unchanged. [Modifier overview](../existence-atlas/modifier-overview.md)

| Modifier | Criterion | Failure | Uniqueness |
| --- | ---: | ---: | ---: |
| `truncate:known` | 0 / 11 → 5 / 6 | 0 / 9 → 1 / 8 | 0 / 11 → 5 / 6 |
| `truncate:lower-estimated` | 0 / 4 → 4 / 0 | 0 / 4 → 0 / 4 | 0 / 4 → 4 / 0 |
| `truncate:upper-estimated` | 0 / 4 → 4 / 0 | 0 / 4 → 0 / 4 | 0 / 4 → 4 / 0 |
| `truncate:both-estimated` | 0 / 4 → 4 / 0 | 0 / 4 → 2 / 2 | 0 / 4 → 4 / 0 |
| `zero:truncated` | 2 / 8 | 3 / 5 | 3 / 1 |
| `zero:modified` | 0 / 34 | 0 / 30 | 0 / 4 |
| `zero:inflated` | 0 / 38 | 2 / 4 | 0 / 2 |
| **Total** | **2 / 103 → 19 / 86** | **5 / 60 → 8 / 57** | **3 / 30 → 20 / 13** |

## Verification

On the workstation at base `81aed46b`, two foreground runs of `python3 dev/tools/existence_atlas.py --overview` produced identical SHA-256 hashes for all four generated files. The base `atlas.csv` and `overview.md` matched their pre-review copies byte for byte. The generator's `--check`, the coverage-only input audit, the generated-row audit and `git diff --check` passed. The generated-row audit confirmed that the 17 reviewed regimes account for all atlas row changes.

The workstation's foreground `/tmp/umvl-cpu/run4 nice -n 10 just test-changed --no-long-double` passed with zero families selected. The login shell printed two missing Cargo environment-file messages before selection. There were no builds.
