# TASK-391 · Existence atlas Lean coverage

The atlas derives Lean status from scoped theorem coverage in the main map and any fragments. Full coverage records the theorem names in the claim’s evidence. Restricted results appear as partial evidence alongside the existing status. Written and cited references remain in the claim inventory. The map uses dossier IDs and exact hold masks, with a reserved generic inventory for reusable declarations and zero-truncated Poisson, which has no dossier.

## Before and after

Both snapshots cover 87 families and 520 regimes. The criterion-proved middle group includes every exact criterion with at least one other question open. Lean-backed counts use the status of each displayed claim, including one-sided claims. Partial evidence does not contribute to those counts.

| Measure | Before | After | Change |
| --- | ---: | ---: | ---: |
| All three questions proved | 346 | 346 | +0 |
| Criterion proved with another question open | 73 | 73 | +0 |
| Criterion open | 101 | 101 | +0 |
| Lean-backed criterion | 77 | 149 | +72 |
| Lean-backed failure | 95 | 149 | +54 |
| Lean-backed cardinality | 96 | 144 | +48 |
| Path records with lean status | 76 | 78 | +2 |
| Path records with written status | 203 | 201 | -2 |
| Path records with cited status | 0 | 0 | +0 |
| Path records with unproved status | 111 | 111 | +0 |
| Detector records with lean status | 60 | 67 | +7 |
| Detector records with written status | 123 | 117 | -6 |
| Detector records with cited status | 1 | 0 | -1 |
| Detector records with unproved status | 206 | 206 | +0 |

The review found 34 paths and detectors that had lost their proved status in the initial derivation. The corrected atlas restores Lean status for 29 and uses existing written proofs for the other five. Every status below its pre-derivation rank appears with its reason in the table below and in the [exceptions list](../existence-atlas/proof-status-exceptions.csv). The checker uses a [fixed pre-derivation baseline](../existence-atlas/proof-status-baseline.csv) for both the saved atlas and a fresh derivation.

## Scope review

- The endpoint results establish the Pareto, power and discrete uniform fits on their compatible hold masks. The uniform results also cover positive width, zero-width collapse and incompatibility.
- Family/Transfers3.lean covers Pareto III with threshold held, nonstandard beta with both endpoints held, and beta prime with scale held. The corresponding one-free-parameter subregimes also transfer.
- Family/DiscreteWeibull.lean covers exact range-two joint attainment, the joint excluded suprema, and the held-fiber attainment and maximizing sets. The held-fiber boundary claims retain their written proofs for the precise limiting laws.
- Family/TDist.lean covers the exact fixed-location, fixed-shape scale fit for lst and std. The fixed-shape joint collapse theorem covers the negative-exponent branch. Positive exponents and equality in the joint regimes still require a joint argument, and the all-free regimes require control of simultaneous changes in shape and scale.
- Family/BinomialSize.lean was reviewed from master at eb1f3060. The worktree predates that file. Its positive-mean attainment and Poisson supremum theorems, together with the all-zero maximizer theorem, cover the criterion and failure claims. The cardinality evidence is partial because the finite-set and profile-graph statements do not establish positive-mean multiplicity.
- The CMP results prove nonattainment on nonzero adjacent support and identify the all-zero maximizers. The finite-support exponential-family theorem and the truncated CMP boundary result leave the untruncated existence and uniqueness argument for nonadjacent data open in Lean, including comparison at the geometric boundary.
- The GEV results cover the held-shape criterion, failure branches and uniqueness. The fixed-shape collapse supplies partial evidence for the all-free GEV boundary path, which also varies shape.
- The held-threshold Pareto II theorem covers strictly positive excesses. The threshold-contact criterion remains written. The negative hypergeometric held-white-count theorem covers positive stopping count, so its full cardinality claim retains the written proof for the zero-stopping branch.

## Regimes with changed statuses

79 regimes changed a top-level question status. The table also includes changes in the structured path, detector and special-case statuses.

| Family | Held | Questions | Paths, detectors and cases | Reason for downgrade |
| --- | --- | --- | --- | --- |
| bbinom | size | — | path:3 lean → written | path:3. The endpoint theorems give the strict supremum but do not state a feasible path for the endpoint proportion. |
| betapr | scale | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| betapr | scale+shape1 | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| betapr | scale+shape2 | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| binom | all free | criterion cited → lean, failure cited → lean | limit:1 cited → lean, path:1 written → lean | — |
| binom | prob | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| burr | scale+shape1 | — | path:1 lean → written | path:1. The first-shape theorem varies scale and the fixed-scale theorem varies the first shape. The written proof fixes both and gives collapse only at scale contact. |
| burr | scale+shape2 | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| burr | shape1+shape2 | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| burr | shape2 | criterion written → lean, failure written → lean | — | — |
| cauchy | location | failure written → lean | — | — |
| cmp | all free | — | path:1 lean → written | path:1. The adjacent-support theorem proves nonattainment without giving a rate-dispersion path to the two-point law. |
| cmp | lambda | — | path:1 lean → written | path:1. The adjacent-support theorem compares free rates and dispersions. The written held-rate specialization takes dispersion to infinity at constant rate. |
| dblpareto | left_rate | — | limit:2 lean → written, path:2 lean → written | limit:2. The Lean scan optimizes both rates. The written held-left-rate crossing gives the relevant face comparison. path:2. The Lean power path fixes the surviving rate at its face optimum. The written limit allows an arbitrary positive held left rate. |
| dblpareto | right_rate | — | limit:1 lean → written, path:1 lean → written | limit:1. The Lean scan optimizes both rates. The reflected written crossing gives the held-right-rate face comparison. path:1. The Lean Pareto path fixes the surviving rate at its face optimum. The written limit allows an arbitrary positive held right rate. |
| dunif | all free | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| dunif | max | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| dunif | min | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| dweibull | all free | criterion written → lean, failure written → lean, uniqueness written → lean | limit:1 written → lean, limit:3 written → lean, limit:4 written → lean, path:3 written → lean, path:4 written → lean | — |
| dweibull | shape1 | criterion written → lean, uniqueness written → lean | limit:5 written → lean | — |
| dweibull | shape2 | criterion written → lean, uniqueness written → lean | limit:2 written → lean | — |
| foldnorm | all free | failure written → lean | — | — |
| foldnorm | mu | criterion written → lean | — | — |
| frechet | lambda | criterion written → lean | — | — |
| frechet | lambda+mu | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| gamma | all free | criterion written → lean | — | — |
| ged | nu | criterion written → lean | — | — |
| gev | all free | — | path:2 lean → written | path:2. The positive-collapse theorem fixes shape while the recorded all-free path varies shape toward one. |
| gev | xi | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| gompertz | all free | criterion written → lean, uniqueness written → lean | — | — |
| gompertz | a | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| gompertz | b | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| gumbel | all free | criterion written → lean, failure written → lean | — | — |
| hcauchy | all free | uniqueness written → lean | — | — |
| hcauchy | mu | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| invgamma | all free | criterion written → lean, uniqueness written → lean | — | — |
| invgamma | alpha+beta | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| invparalogis | scale | criterion written → lean, failure written → lean | — | — |
| invparalogis | shape | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| invtrgamma | scale+shape2 | criterion written → lean, failure written → lean | — | — |
| invtrgamma | shape1 | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| invtrgamma | shape1+shape2 | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| invtrgamma | shape2 | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| invweibull | all free | criterion written → lean | — | — |
| invweibull | scale | criterion written → lean | — | — |
| invweibull | shape | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| kumar | all free | criterion written → lean | — | — |
| laplace | mu | criterion written → lean | — | — |
| llogis | all free | criterion written → lean | — | — |
| llogis | scale | criterion written → lean | — | — |
| logis | all free | criterion written → lean, failure written → lean | — | — |
| lst | all free | failure written → lean | path:1 written → lean, path:4 written → lean | — |
| lst | df+mu | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| naka | all free | criterion written → lean | — | — |
| naka | scale | criterion written → lean | — | — |
| nbinom | all free | criterion written → lean, uniqueness written → lean | — | — |
| nhyper | m+r | uniqueness lean → written | — | uniqueness. The Lean crossing assumes positive stopping count. The written cardinality claim includes the flat feasible set at zero. |
| nsbeta | max+min | criterion written → lean | — | — |
| nsbeta | max+min+shape1 | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| nsbeta | max+min+shape2 | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| pareto | all free | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| pareto | a | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| pareto | a+b | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| pareto | b | criterion written → lean, failure written → lean, uniqueness written → lean | limit:1 written → lean, limit:2 written → lean, path:1 written → lean, path:2 written → lean | — |
| pareto2 | min | criterion lean → written, failure written → lean | — | criterion. The Lean transfer assumes positive excesses. The written criterion also refers to the separate threshold-contact rule. |
| pareto2 | min+scale | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| pareto2 | min+shape | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| pareto3 | min | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| pareto3 | min+scale | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| pareto3 | min+shape | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| power | all free | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| power | alpha | criterion written → lean, failure written → lean, uniqueness written → lean | limit:1 written → lean, path:1 written → lean | — |
| power | alpha+beta | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| power | beta | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| std | all free | — | path:5 written → lean | — |
| std | mean+nu | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| trgamma | scale+shape2 | criterion written → lean, failure written → lean | — | — |
| trgamma | shape1 | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| trgamma | shape1+shape2 | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| trgamma | shape2 | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| unif | all free | criterion written → lean | — | — |
| unif | max | criterion written → lean | — | — |
| unif | max+min | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |
| unif | min | criterion written → lean | — | — |
| weibull | all free | criterion written → lean | — | — |
| weibull | scale | criterion written → lean | — | — |
| weibull | shape | criterion written → lean, failure written → lean, uniqueness written → lean | — | — |

## Checks

All checks ran on the workstation through the single-slot lock. Two foreground --overview runs produced byte-identical atlas.csv and overview.md. The --check run returned zero with no diagnostics. No live fragments were present in this worktree at verification. A scratch fixture exercised full and partial coverage from a fragment, proof-marker filtering, invalid family and hold masks, a saved Lean claim with an unmapped theorem, legacy aliases, and scoped generic coverage. It also verified that the checker accepts an explicit exception and rejects unapproved regressions in both the saved atlas and the fresh derivation.

The link checker reported 47 inherited destinations and the declared false positive at 2026-11-25-betapr-math-report.md line 461 (P+Q). It exits 1 for that false positive. No other new broken destination was reported.
