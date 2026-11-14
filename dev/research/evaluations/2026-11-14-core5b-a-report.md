# Consolidation step 5b: core5b-a

Completed for all 25 assigned families on branch codex/core5b-a, based on da2174a11af7d31c47434a54a65d4108c6b00388. All required final checks passed on the workstation. No simbox command, commit, board edit, package installation or publication was performed.

The strict registry loses **120 assigned entries**: 18 under TASK-214.15, 48 under TASK-214.16 and 54 under TASK-214.17. No known entry was added. The 12 entries under TASK-214.5 and TASK-214.13 remain. TASK-214.18 behavior is covered by the core/adapter changes, outcome inventory and regression cases below; it has no additional assigned registry entries to prune.

Both final snapshots contain the same 4,081 assigned cases as their baselines. There are **608 native and 607 double-precision changed cases**, all classified. **No existing successful fit changed coefficients or became a stop.** See the [verification summary](2026-11-14-core5b-a-data/verification-summary.json), [source hashes](2026-11-14-core5b-a-data/source-manifest.json) and [strict-entry inventory](2026-11-14-core5b-a-data/strict-entry-inventory.json).

## Family decisions and mappings

“Nonexistence” below means the shared nonexistence outcome with the stated reason. Binding checks for supplied arguments remain; family adapters no longer inspect samples to reclassify a core result or suppress its structured stop. Numerical decisions, comparison likelihoods and the corresponding diagnostics belong to the core.

| Family | Core decision and old → new outcome or payload | Strict fixed / retained |
|---|---|---:|
| bbinom | The core records the unsupported all-free size/shape configuration: binding invalid-argument refusal → Unsupported. Unidentified maxima now carry a finite representative and the complete positive-shape freedom or shape ray: unresolved rendering → certified non-unique estimate. Fixed-size comparison points are marked accurate only after refinement and point-likelihood evaluation. | 0 / 0 |
| bhatt | The public core wrapper owns comparison likelihoods, accurate candidate marking and physical uniform payloads. Uniform limits retain observations after outward rounding and report the density sum at those endpoints. The boundary-layer supremum remains separate. A selected normal member is represented as the attained a=0 member when a is free, inheriting its certificate. | 12 / 4 |
| cauchy | Counts every atom before deciding collapse. Strict majority → unbounded; exactly half at one atom with more than two support points: unbounded → finite unattained. Balanced two-point maxima carry their full curve or both held-scale roots: unique/unresolved rendering → certified non-unique estimate. Point log likelihood is recomputed at unchanged coefficients; an unevaluable point is an evaluation failure. | 6 / 4 |
| dgamma | Audited; its existing core statuses already determine the assigned decisions. No implementation change. | 0 / 2 |
| dunif | Audited; no residual adapter decision or assigned strict entry required a change. | 0 / 0 |
| expweibull | InvalidFixed: invalid-input failure → invalid-argument failure, because it denotes an invalid held parameter. | 0 / 0 |
| fpareto | Own family statuses carry concentration, unsupported minimum estimation, arithmetic failure and refuted faces. Valid constant samples previously treated as invalid/failures now yield proved unbounded nonexistence in the applicable hold routes. Selected lognormal/double-Pareto boundaries previously rendered as failures become evaluated, uncertified limits. A feasible finite witness that beats a proposed lognormal face rejects that face as non-convergence; the witness is retained as a lower bound, not returned as an MLE. Excess overflow is an evaluation failure. | 10 / 0 |
| ged | Core validation and the free-scale constant-sample collapse replace adapter classification: invalid/failure → unbounded nonexistence. Uniform payload endpoints retain the sample and their likelihood is evaluated in physical units. | 8 / 0 |
| gev | Constant samples and admissible minimum-tie collapse are decided before the free search, after validating controls and honoring the held dispatcher. Invalid-data/adapter collapse → the appropriate nonexistence. A separate finite-collapse flag distinguishes finite unattainment from divergence despite padded likelihood bounds. Valid two-point data use the existing analytic certificate instead of a blanket invalid-input refusal. | 6 / 0 |
| gumbel | Removes the adapter's constant-data classification and structured-stop suppression; existing core collapse status is authoritative. Point log likelihood is the physical density sum at unchanged coefficients. Failed evaluation uses NumericalFailure/Evaluation. | 4 / 0 |
| ht | Removes the adapter's sample/payload classifier and outcome suppression. InvalidParameter: invalid input → invalid argument. The half-normal escape retains its status and now carries the physical half-normal likelihood. | 2 / 0 |
| invburr | The core recognizes a valid positive singleton under the free fit as concentration: InvalidInit/invalid input → Unbounded/nonexistence. Compatible held cases remain with their existing solvers. | 4 / 0 |
| invgauss | Removes the adapter classifier. A zero computed residual proves unboundedness only when the observations actually equal the fitted/held mean; a zero caused by arithmetic loss on nonconstant data is an evaluation failure. | 0 / 0 |
| invtrgamma | Core constant-sample concentration and the reciprocal delegate's proved +Inf concentration become Unbounded/nonexistence, rather than invalid data or adapter inference. InvalidFixed becomes invalid argument. Lognormal and Pareto payloads use their physical densities after reciprocal transport. | 8 / 0 |
| lgamma | Removes the adapter's sample/payload classifier; the existing core status and stop detail determine the outcome. No numerical mapping change was needed. | 0 / 0 |
| logis | Validation and constant-sample collapse are decided in the core, including held location. Removes the pre-core statistical refusal and post-core outcome overrides. Point likelihood is summed at unchanged coefficients; failed physical evaluation has its own failure status. | 8 / 2 |
| lst | The private search's “NoMLE” was absence of a proposal, not a nonexistence proof: renamed NoProposal, invalid-input failure → non-convergence. NormalBoundary explicitly names the normal model and carries its physical point likelihood. | 4 / 0 |
| nsbeta | The core carries unsupported bound estimation and data outside valid held support. Invalid-held-argument rendering → invalid-input failure for the latter. Removes adapter scans and reclassification. | 8 / 0 |
| pareto2 | Own statuses decide unsupported threshold estimation, closed-endpoint samples, unbounded/finite-unattained collapse and unrepresentable limit rates. Endpoint data previously refused as invalid are handled analytically: proved nonexistence or a unique conditional fit. An infinite-rate exponential payload becomes an evaluation failure. Positive-excess default estimates retain their coefficients. | 10 / 0 |
| pig | Audited; its existing core result already owns the assigned decisions and there are no assigned strict entries. No implementation change. | 0 / 0 |
| sged | Core validation and constant-sample collapse replace the adapter's invalid-data/constant-data inference. Valid collapse → unbounded nonexistence. Uniform payloads retain sample support and use physical point likelihoods. | 16 / 0 |
| slash | Removes the sample/payload classifier; the core's existing outcomes and diagnostics are rendered directly. | 0 / 0 |
| sstd | Status meaning takes precedence over retained model diagnostics. Named skew-normal, half-normal, half-t and df=2 faces: evaluation-failure rendering → evaluated limits with the existing/inherited certificate flag. The held search carries the selected face's actual coordinates, including exact sample endpoints and a finite base scale for the df=2 face. Unit-skew delegation preserves the parent's failure/accuracy flags and certificate; finite candidates are lifted to four coefficients. An unproved scale equality changes NoConvergence → Unresolved, not proved nonexistence. Nonfinite attempted faces remain failures with Evaluation. | 4 / 0 |
| tlambda | The core owns singleton/constant collapse and finite-data transform failures: invalid/failure → unbounded nonexistence or Evaluation, respectively. Two-point data enter the profile/certificate rather than being declared invalid; an unresolved search without an accepted proposal remains a reported comparison failure. Removes adapter classification. | 10 / 0 |
| unif | Removes the adapter classifier and suppression; the existing core collapse and validation statuses already decide the outcome. No numerical mapping change. | 0 / 0 |

Unclosed global comparisons retain an available accepted search result uncertified. A failed certificate alone does not discard such a result. The new Feller–Pareto refusal requires a strictly better feasible witness. The seven SSTD face-evaluation changes start from already failed searches; the six unproved scale-face changes only replace an inappropriate failure reason. No old estimate is lost in either snapshot mode.

## Mathematical evidence

These are different kinds of evidence, rather than interchangeable claims of certification.

- **Proof, conditional on computed comparisons.** Cauchy's two-point product bound gives the complete semicircle maximum set. Pairing half the sample at one atom with the remaining observations proves finite nonattainment when those observations have distinct support points; a majority gives divergence. The [Cauchy dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cauchy/README.md) records the maximum sets. The held-scale symmetric pair has two algebraic location maxima.
- **Proof of the endpoint classifications.** For Pareto II with k observations at the endpoint and held shape a, the scale-collapse exponent is a(n−k)−k. Negative, zero and positive signs give divergence, finite unattainment and a strictly monotone scale score with one root. The held-scale shape formula is explicit. The [Pareto II dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/README.md) gives the derivation and practical 128-epsilon sign margin.
- **Proof of GEV finite nonattainment in the stated fixed-positive-shape route.** At the critical multiplicity, both terms of the endpoint-distance profile decrease strictly. The [GEV dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md) gives the profile, supremum and held-location variant. A fused residual resolves rounded equality in the multiplicity comparison; the reported supremum uses a practical 512-epsilon margin. The existing two-point certificate is used within the family's admitted parameter domain.
- **Proof of concentration.** Constant-data Feller–Pareto concentration uses the beta-log-odds density: free power and scale can increase the density indefinitely even when both beta shapes are held. The beta shapes are shape1 and shape3; shape2 is the power. Inverse transformed gamma also retains its concentration proof and the reciprocal delegate's concentration result. See the [Feller–Pareto](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md) and [inverse transformed gamma](../../../univariateML-cpp/include/cppunivariate/distributions/invtrgamma/README.md) dossiers.
- **Analytic face profile; numerical global selection.** The double-Pareto face is asymmetric Laplace in log excess. Its fixed-kink rate optimum is explicit, and the kink comparison includes the one-sided power/Pareto endpoints through the parent search. This does not certify the full Feller–Pareto search; the selected face remains uncertified.
- **Independent high-precision comparisons.** [reference-checks.py](2026-11-14-core5b-a-data/reference-checks.py) checks 64 of the 66 original density entries against independent symbolic mpmath formulas; all pass. The other two formerly infinite-rate Pareto II payloads have independently computed rates above binary64's maximum and are precisely retired by [prune-unrepresentable.py](2026-11-14-core5b-a-data/prune-unrepresentable.py).
- **Independent checks of named faces and new conditional fits.** [check-named-faces.py](2026-11-14-core5b-a-data/check-named-faces.py) passes 86 comparisons at 100 and 160 digits, including faces unavailable as standalone C++ laws. [check-boundaries.py](2026-11-14-core5b-a-data/check-boundaries.py) passes 14 100-digit checks across both modes, including the new Pareto II endpoint roots and GEV critical suprema. These check numerical values; the monotonicity/nonexistence claims come from the proofs above.
- **Numerical refutation, not a replacement MLE.** [check-refutations.py](2026-11-14-core5b-a-data/check-refutations.py) passes 14 independent feasible-witness comparisons against rejected Feller–Pareto lognormal faces. The core uses a practical 4096-epsilon separation margin. [check-sstd-refutation.py](2026-11-14-core5b-a-data/check-sstd-refutation.py) verifies at 100/160 digits that the feasible symmetric Student member for the documented fixed-sd, fixed-df sample beats the two proposed half-faces by about 4.783 and 24.781 log-likelihood units. An unfinished interior restart cannot replace that failed search with a refuted face.
- **Numerical regression.** The snapshot and binding suites verify coefficients, payload transport, flags and rendered outcome agreement over their recorded cases. They do not prove global optimality for an uncertified search.

The Bhattacharyya boundary-layer supremum was deliberately preserved: for the documented three-point example it differs from the likelihood of the returned uniform law. The public limit payload reports the latter. Tests retain the former supremum check and separately check the uniform density.

## Snapshot differences

Baseline: the supplied [native](2026-11-14-fit-snapshot-data/native.jsonl.gz) and [double](2026-11-14-fit-snapshot-data/double.jsonl.gz) archives. Final snapshots and provenance are kept as [native](2026-11-14-core5b-a-data/native.jsonl.gz), [double](2026-11-14-core5b-a-data/double.jsonl.gz), [native provenance](2026-11-14-core5b-a-data/native.provenance.json) and [double provenance](2026-11-14-core5b-a-data/double.provenance.json).

Every changed case, its category, explanation, changed fields and complete old/new result is listed in the [native difference list](2026-11-14-core5b-a-data/native-differences.jsonl.gz) or [double difference list](2026-11-14-core5b-a-data/double-differences.jsonl.gz). [classify.py](2026-11-14-core5b-a-data/classify.py) reproduces those lists and rejects missing/added baseline cases, changed old successful coefficients and unclassified differences. Per-family counts are in [difference-summary.json](2026-11-14-core5b-a-data/difference-summary.json).

| Category | Native | Double | Reason |
|---|---:|---:|---|
| Public estimate likelihood | 121 | 121 | Physical density sum at unchanged coefficients. |
| Limit payload | 195 | 194 | Physical coefficients, outward support rounding, point likelihood and inherited certificate flag. |
| Named boundary | 160 | 160 | An evaluable selected boundary becomes its named limit. |
| Core collapse | 25 | 25 | A valid degenerate sample is nonexistence, not an invalid argument/sample. |
| Core nonexistence | 15 | 15 | Core collapse status replaces a plain failure rendering. |
| Invalid-data reason | 35 | 35 | Data violate held support; the held arguments themselves are valid. |
| Non-unique maximum | 3 | 3 | Exact balanced Cauchy maximum sets and flags. |
| Finite-collapse reason | 1 | 1 | Half-atom Cauchy nonattainment is finite, not unbounded. |
| Refuted lognormal face | 7 | 7 | A feasible Feller–Pareto member strictly dominates the proposed face. |
| Unrepresentable limit | 1 | 1 | Pareto II's exponential rate exceeds binary64 range. |
| Valid-data arithmetic failure | 7 | 7 | Finite data remain valid when their numerical transformation fails. |
| Valid small-sample fit | 3 | 3 | Existing analytic two-point GEV certificate. |
| Core decision and diagnostics | 22 | 22 | Core status/diagnostic transport changes without changing the terminal estimate or outcome. |
| Failed face evaluation | 7 | 7 | An already failed SSTD search has a nonfinite attempted physical half-face. |
| Unproved scale face | 6 | 6 | Existing SSTD failure is an unresolved comparison, not non-convergence. |
| **Total** | **608** | **607** | **All differences classified; no old estimate refused or moved.** |

The required two baseline diff commands completed with their expected “differences found” exit status. Both outcome inventories report every case covered, zero outcome/emitted-kind disagreements and zero adapter overrides of a core success.

## Strict registry

The judge scripts ran with their pruning option. The two obsolete infinite-rate density keys need the narrow helper described above because the density judge does not visit a row with no valid payload. [inventory-known.py](2026-11-14-core5b-a-data/inventory-known.py) verifies that no new key was added, no unowned/out-of-scope baseline key was lost and no assigned TASK-214.15–18 key remains.

Retained entries, exactly as requested:

| Card | Family | Cases | Entries |
|---|---|---|---:|
| TASK-214.13 | bhatt | archive 000364 and 000428, paired modes | 2 |
| TASK-214.13 | dgamma | archive 001025 and class wide, paired modes | 2 |
| TASK-214.5 | bhatt | archive 000363, native and double | 2 |
| TASK-214.5 | cauchy | archive 000764 and 010216, native and double | 4 |
| TASK-214.5 | logis | archive 004689, native and double | 2 |

The final [strict log](2026-11-14-core5b-a-data/final-strict.log) ends with “strict layer: no failure outside the known list.” The paired judges have four known mode findings and eight known refinement findings. The whole-library seam check has 9,694 cases per mode: native 7,333 pass / 2,361 stop; double 7,331 pass / 2,362 stop / one already-known guarded open fit outside this assignment. It has zero new findings.

## Required suites and final-state validation

Every run below used the **workstation**, through the prescribed single-slot or four-slot wrapper. No simbox attempt was made.

| Check | Completed result | Evidence |
|---|---|---|
| Native and no-long-double assigned snapshots | 4,081 cases each; matching case inventories | Final snapshot logs and provenance in the data directory |
| Both baseline diffs and outcome inventories | 608 / 607 classified changes; zero emitted-kind disagreements or overrides | Final diff/outcome logs and difference lists |
| Paired judges with pruning; final exact just strict invocation | Passed, zero new findings | [Strict](2026-11-14-core5b-a-data/final-strict.log), judge and registry logs |
| just cpp-test | 110/110 | [Log](2026-11-14-core5b-a-data/final-cpp-test.log) |
| just cpp-test-no-long-double, GCC 15 workstation | 154/154 | [Log](2026-11-14-core5b-a-data/final-cpp-test-no-long-double.log.gz) |
| just test | FAIL 0 / WARN 0 / SKIP 157 / PASS 36,225 | [Log](2026-11-14-core5b-a-data/final-test.log.gz) |
| just test-full | FAIL 0 / WARN 0 / SKIP 1 / PASS 65,364 | [Log](2026-11-14-core5b-a-data/final-test-full.log.gz) |
| just js-build | Passed | [Log](2026-11-14-core5b-a-data/final-js-build.log) |
| just js-test | 584/584, zero failures | [Log](2026-11-14-core5b-a-data/final-js-test.log) |
| just site-check | Passed, including HTML spot checks and catalogue build | [Log](2026-11-14-core5b-a-data/final-site-check.log) |
| Final affected C++ families in both modes | cauchy, fpareto, invtrgamma, sged, sstd and sstd slow: 6/6 in each mode | [Native](2026-11-14-core5b-a-data/final-fpareto-native.log), [double](2026-11-14-core5b-a-data/final-fpareto-double.log) |
| Final native SSTD assertion rebuild/replay | Regular and slow targets: 2/2 | [Log](2026-11-14-core5b-a-data/final-sstd-native.log) |
| Final SSTD R rerun after the last family changes | FAIL 0 / WARN 0 / SKIP 12 / PASS 485 | [Log](2026-11-14-core5b-a-data/final-sstd-r.log) |
| Source scope and whitespace | Passed; settled density/truth/special/codegen sources and prohibited project files unchanged | Source manifest and final scope verification |

Initial broad runs exposed obsolete outcome assertions and real payload/face bugs; those were corrected before the green results above. The full R suite was rerun to completion. The later changes were confined to the listed family code and tests; as instructed, the relevant C++/R targets were rerun instead of repeating unrelated full suites. Final snapshots, independent checks, pruning, exact strict and site validation use the final state.

Existing data samples and substantive numerical checks were retained. Assertions changed where the requested semantics changed: proved nonexistence instead of invalid data, accepted two-point GEV fits, non-unique maximum sets, or point likelihood instead of a distinct boundary supremum. The low-budget SSTD failure check and the full oracle checks were retained; the fitter was fixed to pass them. New C++ cases are appended. No golden or truth file was regenerated.

## Open shared-protocol entry

**Shifted registered limit models need coordinate-transform metadata in the shared payload protocol.** This cannot be completed solely in the assigned family files.

The reproducible [JavaScript probe](2026-11-14-core5b-a-data/diagnose-shifted-limits.mjs), [independent mpmath check](2026-11-14-core5b-a-data/diagnose-shifted-limits.py) and [numerical diagnosis](2026-11-14-core5b-a-data/shifted-limit-diagnosis.json) establish two cases:

| Family and sample | Returned base model | Reported/shifted-law likelihood | Base law on original observations |
|---|---|---:|---:|
| fpareto: eight observations equal to 4, min held at 2, power shape2=2 and beta shape3=2 | trgamma | −4.909645111040875 | −36.274112777602196 |
| pareto2: observations 3 through 12, min held at 2 | exp | −27.047480922384253 | −30.683844558747888 |

The likelihood matches the model on excesses x−2 at 100 digits, but the registered base-model coefficient payload has no offset coordinate. The held minimum remains separately visible; generic consumers do not receive a uniform recipe for applying it to that face. These nonzero-offset variants are outside the supplied frozen matrix, so the green strict result does not establish their generic payload interpretation.

A shared owner needs to encode the affine coordinate transformation (or explicit shifted model identities) for each limit and comparison candidate in core/stop_detail.hpp, render it through bindings/common/fit.hpp and the R/JS conversions, and teach the generic density/strict consumers the same interpretation. The transformed-gamma density implementation itself is settled and was not changed. New shifted-lognormal/double-Pareto face payloads in this job already retain their physical offset; the remaining gap is the uniform protocol for existing registered base models.

No known entry was added for this gap. No shared implementation or validation Python file was modified to conceal it. The evidence records the counterexamples for the shared follow-up.

