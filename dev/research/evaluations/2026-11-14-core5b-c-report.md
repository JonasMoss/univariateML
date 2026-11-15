# core5b-c: core outcomes and strict entries

Completed on `codex/core5b-c`, from `da2174a11af7d31c47434a54a65d4108c6b00388`. All final checks passed. The 25 adapters now attach the core outcome directly; the assigned strict entries are fixed. **No old `Ok` coefficient vector changed in either precision mode.**

The final ratchet removes **48** entries: 4 under TASK-214.15, 20 under TASK-214.16 and 24 under TASK-214.17. All **34** excluded entries remain: 30 under TASK-214.5 and 4 under TASK-214.13. No known entry was added or removed from another family's allocation. [Exact removed and retained keys](2026-11-14-core5b-c-data/known-diff.json) and [validation summary](2026-11-14-core5b-c-data/validation-summary.json).

Everything ran on the workstation through the slot wrappers, with the thread limits and lowered priority they provide. simbox was not attempted. No commit, Git mutation, board edit, sub-agent, push or dependency installation was made. Protected project files, shared outcome/rendering/conversion implementations, generators, `dpqr.hpp`, `special/`, truth tables and goldens are unchanged. The generated capability CSV changes only the GPD/Pareto IV rows. [Patch scope](2026-11-14-core5b-c-data/scope-audit.json).

## Per family

E(c) means estimate, L(c) limit, N(reason) nonexistence and F(reason) failure; c is the certification flag. Arrows distinguish a core status correction from the old adapter rendering where necessary. Host argument parsing remains before the fit; post-core classification comes exclusively from `outcome()`.

The last column counts ratchet keys removed / retained, with `.15` etc. abbreviating TASK-214.15 etc.; 0 means the family had no assigned keys in that group.

| Family | Core decision, outcome and payload change | Removed / retained |
|---|---|---|
| `betapr` | Core `InvalidStart` for positive constant data with both shapes free becomes `Unbounded` → N(Unbounded), including singletons. The old adapter constancy/control classification is removed; failed gamma-face comparisons retain their structured diagnostics. | 0 / 0 |
| `burr` | The public fitter validates singleton input and controls, then returns `Unbounded` → N(Unbounded). The private certificate keeps its separate entry contract for InvBurr. Pareto/Weibull face likelihoods are evaluated after exporting physical coefficients; global certificate bounds remain separate. | .16: 8, .17: 4 / .5: 12 |
| `cmp` | The unchanged fully-held normalizer ledger and `BudgetLimit` → `NumericalFailure` policy move into `cmp_domain_mle`. Pure evaluation now produces F(Evaluation) in the core rather than being reclassified by the adapter. Its existing evaluator cap is the documented pure-evaluation exception in the work-budget design. | 0 / 0 |
| `dnorm` | Remove the adapter override and late finite-value veto. Existing core failure/budget outcomes and structured details are authoritative; no estimator change. | 0 / .5: 12 |
| `emg` | Remove data/control/payload classifiers. Finite `Tie` candidates receive the same coefficient polish and public-coordinate checks as successful fits; only checked candidates are marked accurate. A selected accurate family point can become E(false), rather than an unresolved witness. This does not prove a global winner. | 0 / .5: 2 |
| `foldnorm` | Remove the constancy/control/payload classifier and outcome override. Existing core statuses already distinguish the cases; structured failures remain attached. | 0 / 0 |
| `gb2` | Proved constant-data concentration returns core `Unbounded`, including free-power unit-shape delegates. `LognormalBoundary`: F(Evaluation) → L(false); `DoubleParetoBoundary`: F(Unsupported) → L(false), with named coefficients and point likelihoods. One-sided limits carry Pareto/power payloads; exterior held scales allow the independently moving endpoint proved below. Held endpoints are copied exactly. Invalid computed faces and logarithmic collapse of distinct data give F(Evaluation). Existing finite constrained constant-data fits remain in their specialized solvers. | .17: 4 / 0 |
| `gengamma` | Remove the post-core representation veto and override. Core representation and convergence statuses already carry the decision; no mathematical remapping. | 0 / 0 |
| `gpd` | Free-threshold decisions move into the fitter: all three coefficients free gives N(Unbounded); other unsupported free-threshold held slices give F(Unsupported), after input/control checks. Finite `ThresholdCollapse`: L → N(Unattained), retaining its supremum diagnostic without advertising a limit law. Remove the generated R hold guards; default mu=0 is unchanged. | 0 / 0 |
| `hnorm` | Empty input is core `InvalidData` → F(InvalidInput), rather than `DegenerateData` with an adapter suppression. Genuine free-scale collapse is N(Unbounded), including a singleton. Arithmetic/representation failures have `NumericalFailure` → F(Evaluation). Subnormal log likelihoods use the returned scale, not the exact-optimum n/2 identity. | .15: 2 / 0 |
| `hyper` | `Infeasible`: N(Unattained) → F(InvalidInput), because the held model has incompatible support. Remove outcome overrides and suppression of structured face/comparison stops. | 0 / 0 |
| `invgamma` | Remove the data/constancy/control classifier, late finite veto and override. Existing core statuses already own these distinctions; retain structured failure details. | 0 / 0 |
| `invpareto` | Remove the redundant classifier and override. Reciprocal-delegate statuses and diagnostics now pass directly through the core outcome. | 0 / 0 |
| `kumar` | `Unbounded`: F(InvalidInput) → N(Unbounded). Preserve the diagnostic and remove the adapter override; positive constant samples have a proved concentration escape. | .17: 4 / 0 |
| `lnorm` | Remove constancy/control classification, the finite-value veto and override. Core degeneracy, invalid-input and evaluation decisions are used directly. | 0 / 0 |
| `lomax` | An exponential limit whose rate cannot be represented changes from an invalid L payload to an arithmetic F(Evaluation). Clear its unusable coefficients and point likelihood, retaining the finite analytic face bound needed by parent certificates. Ordinary exponential limits are unchanged. | .16: 2 / 0 |
| `nhyper` | `Infeasible`: N(Unattained) → F(InvalidInput), with the specific support diagnostic. Remove the binding override; leave the excluded capped-grid accuracy work unchanged. | .17: 2 / .5: 4 |
| `pareto` | A held endpoint incompatible with the observations is now core `Incompatible` → F(InvalidInput), replacing the adapter scan over data and holds. Keep the existing support message. | 0 / 0 |
| `pareto4` | Core flags distinguish incompatible support and unsupported threshold slices. Proved free-threshold escapes give N(Unbounded); other held slices remain F(Unsupported). Nullable R min reaches the core; default min=0 stays unchanged. Shifted Pareto/Weibull payloads include min and use explicit shifted-model names. Advice retains the endpoint hold or tells the caller to translate the data. Delegated face likelihoods use exported physical coefficients. | .16: 4, .17: 10 / 0 |
| `rayleigh` | Validate every observation before the zero-likelihood decision. `ZeroLikelihood`: N(Unattained) → F(InvalidInput), retaining the specific zero-likelihood message. Nonrepresentable held evaluations return core `NumericalFailure`. Subnormal likelihoods use the returned scale rather than the exact-optimum n substitution. | .15: 2 / 0 |
| `skellam` | Remove the core-success override. Preserve structured iteration/budget failure details; existing core outcome meanings are unchanged. | 0 / 0 |
| `snorm` | Remove the classifier and late payload veto. Free half-normal endpoints use the original sample minimum/maximum, and the point likelihood uses public coefficients. A nonrepresentable face gives core `NumericalFailure` → F(Evaluation), before any empty coefficient vector can be indexed. | .16: 2 / .13: 4 |
| `std` | Remove the outcome override and retain structured BFGS failures. The normal-face point likelihood is evaluated at its exported mean/sd, and matching point-candidate bounds are updated; global comparison bounds remain separate. | .16: 4 / 0 |
| `triang` | Remove data/control classification and the override. Existing core statuses already distinguish invalid input/support, unboundedness and solver failure. The proximity warning is not promoted to a proof of non-uniqueness. | 0 / 0 |
| `yulesimon` | Initialize the stop payload kind from the core outcome and retain structured iteration failures. The existing named point-mass limit stays a limit; no adapter reclassification remains. | 0 / 0 |

The [adapter audit](2026-11-14-core5b-c-data/adapter-audit.py) verifies all 25 direct outcome attachments and the absence of data classifiers, outcome overrides, post-core finite-point vetoes and dossier-generated hold guards. Its [results](2026-11-14-core5b-c-data/adapter-audit.json) cover the architecture even where the frozen snapshots have no output delta.

The GPD/Pareto IV hold tables previously caused R to stop before the fitter. Removing those tables was necessary to make the public R route use the core decision. Their structural defaults remain mu=0 and min=0. Some held free-threshold slices remain implementation failures with reason Unsupported; they are not called mathematically nonexistent.

## Mathematical evidence

The full arguments are in [the mathematical evidence note](2026-11-14-core5b-c-data/mathematics.md). They support different claims:

- **Proof of nonexistence:** beta-prime concentration by Stirling's formula; its GB2 subfamily and the free-power concentration at scale=x; Burr's density growing linearly with free power at a constant observation; and the Kumaraswamy concentration path. These justify the constant-data Unbounded meanings, rather than an invalid-data label.
- **Proof for free thresholds:** the GPD path mu=min(x), sigma=exp(-t), xi=t has a divergent likelihood. Pareto IV has separate paths when power below one is available, or when shape1 and scale are both free. These arguments do not assert unboundedness for every held slice.
- **Proof of unattainment:** at fixed positive GPD shape, let k observations be at the threshold and m=n-k above it. When m/xi=k, the likelihood is a product of strictly decreasing factors `(sigma+xi*y)^(-1-1/xi)`. Its finite supremum is approached only as scale goes to zero. For the regression sample {0,1}, mu=0 and xi=1, it is `(1+sigma)^(-2)` with log supremum 0. No named weak limit law supplies that joint density.
- **Limit-model derivation:** the double-Pareto rates have a closed profiled form, and concavity between log-data knots reduces its location profile to a knot check. A simultaneous GB2 power/shape escape permits a free Pareto endpoint even with scale held below the sample; reciprocation gives the power counterpart. This fixes the omitted comparison that had selected a lower lognormal face. The enclosing GB2 face selection remains numerical, so these results are explicitly uncertified.
- **Likelihood identity and support:** n/2 or n may replace a quadratic term at the exact optimizing scale, but not after a subnormal scale rounds. Restoring a free endpoint through normalized coordinates can also move it inside the observed support. The corrections evaluate the public point and retain separate global bounds.
- **Arithmetic collapse, not nonexistence:** distinct binary64 observations can have identical logarithms. With held GB2 power and scale, their exact beta transforms are distinct and the strict Cauchy sufficient-statistic inequality excludes concentration. The [independent 100-digit check](2026-11-14-core5b-c-data/log-collapse-reference.py) resolves its positive gap as approximately 2.76402410762e-33. The core now reports evaluation failure when its log summary collapses, rather than falsely claiming nonexistence. Existing finite constant held-shape fits are dispatched before this branch and remain covered by the retained C++/R regressions.

The [independent point-likelihood reproducer](2026-11-14-core5b-c-data/high_precision.py) checks **100 likelihoods at 90 digits**, including the new limit payloads and two unchanged GB2 nesting references. The maximum relative error is **9.08e-14**; two unrepresentable exponential faces are rejected. [Values and errors](2026-11-14-core5b-c-data/high-precision.json). The archived nested GB2 point really exceeded the old lognormal face: its likelihood agrees with the independent reference to about 2.71e-12 absolute. This was a missing face comparison, not a bad nested likelihood.

These are point-likelihood checks, not new global MLE certificates or a re-adjudication of coefficient accuracy. Existing certificates retain their conditional computed-comparison/floating-margin standard. No outward interval claim is added. EMG's new accuracy marks use the existing successful-fit polish; they do not promote an unchecked interval witness to a verified maximizer.

## Snapshots and strict pruning

Both final snapshots contain **3,619 cases**. Native has **3,404 unchanged / 215 changed**; double-only has **3,392 unchanged / 227 changed**. All changes belong to the following categories:

| Category | Native | Double-only |
|---|---:|---:|
| Core status only | 1 | 1 |
| Stop diagnostics | 1 | 1 |
| Likelihood at public coefficients | 110 | 120 |
| Outcome correction | 93 | 93 |
| Likelihood and stop payload | 10 | 12 |
| **Total changed** | **215** | **227** |

| Family with changes | Native | Double-only |
|---|---:|---:|
| `betapr` | 2 | 2 |
| `burr` | 27 | 27 |
| `gb2` | 54 | 54 |
| `hnorm` | 2 | 2 |
| `hyper` | 13 | 13 |
| `kumar` | 13 | 13 |
| `lomax` | 1 | 1 |
| `nhyper` | 16 | 16 |
| `pareto4` | 40 | 40 |
| `rayleigh` | 2 | 2 |
| `snorm` | 15 | 27 |
| `std` | 30 | 30 |

The other assigned families have zero snapshot changes. **Every differing case**, its category, changed fields and reason is listed in [the 442-row inventory](2026-11-14-core5b-c-data/snapshot-changes.tsv); [complete before/after results](2026-11-14-core5b-c-data/snapshot-changes.json.gz) and [the classification script](2026-11-14-core5b-c-data/classify.py) make the inventory reproducible. No difference outside these stated changes remains. The script asserts that every baseline `Ok` stays `Ok` with exactly identical coefficients.

Final outcomes have zero disagreements with emitted kinds and no adapter overrides of core successes. Native reports an outcome for every case of all 25 families. Double-only has the one already-listed EMG archive 001311 audit timeout; the remaining returned fits agree. This is an audit guard, not a fitter status or a nonexistence claim. [Native outcome audit](2026-11-14-core5b-c-data/outcomes-native.log), [double outcome audit](2026-11-14-core5b-c-data/outcomes-double.log).

The final saved snapshots are [native](2026-11-14-core5b-c-data/native.jsonl.gz) and [double-only](2026-11-14-core5b-c-data/double.jsonl.gz). Full 25-family runs were followed by targeted replays after late family changes: GPD/Pareto IV frontend metadata, beta-prime/GB2/GPD payload and status updates, and finally GB2's arithmetic guard. Full and targeted provenance files are retained separately. The final directory provenance comes from the last targeted run and carries the current whole-source fingerprint; unchanged families retain their earlier per-file tags. This is combined coverage, not a claim that a one-family replay refitted all 25. [Full native provenance](2026-11-14-core5b-c-data/native-full-provenance.json), [full double provenance](2026-11-14-core5b-c-data/double-full-before-endpoint-provenance.json), [final native provenance](2026-11-14-core5b-c-data/native-provenance.json), [final double provenance](2026-11-14-core5b-c-data/double-provenance.json).

The fit, density, consistency and cached-reference judges ran with `--prune`; the seam ratchet was also pruned during the job. The final `just strict` then rebuilt/refitted both seam modes for the final source. No judge implementation or tolerance was changed. Two obsolete Lomax density-limit keys are skipped by the stock pruner once the corrected result has no limit density item. [The restricted supplementary prune](2026-11-14-core5b-c-data/prune-unrepresentable-face.py) checks only those original TASK-214.16 keys, proves with exact rational arithmetic that the exponential rate exceeds binary64's maximum, verifies the new evaluation failure and empty invalid payload, then deletes them. [Proof records](2026-11-14-core5b-c-data/pruned-unrepresentable-face.json). The excluded keys are retained unchanged.

## Validation

All entries below used the **workstation**. Full tiers were run once; after late fixes I reran affected family/shared assertion files and transitive dependants as requested. Initial failed commands remain in the evidence with their original exit codes; their final repairs are identified explicitly. Every final rerun has exit code 0. [Command ledger](2026-11-14-core5b-c-data/suite-checks.json).

| Required check | Result and final repair coverage | Evidence |
|---|---|---|
| `just cpp-test` | Initial full native dev run: 109/110. The singleton change was confined to the public Burr entry, restoring InvBurr's separate contract. Targeted repairs passed; a retained Pareto IV NaN-threshold input now correctly expects the free-threshold outcome. | [Full run](2026-11-14-core5b-c-data/cpp-test.log.gz); [targeted run](2026-11-14-core5b-c-data/cpp-targeted.log.gz); [final repair](2026-11-14-core5b-c-data/cpp-final-targeted.log.gz) |
| `just cpp-test-no-long-double` | Full workstation suite: **154/154**. After the late GB2 guard, affected full-labelled entries pass **4/4 native and 4/4 double-only**: GB2, Fpareto, Genbeta and Genbeta slow. GPD's finite-collapse regression also passed in both modes before that unrelated GB2-only fix. | [Full double run](2026-11-14-core5b-c-data/cpp-test-no-long-double.log.gz); [final dependants](2026-11-14-core5b-c-data/cpp-log-collapse.log.gz); [GPD and earlier final shards](2026-11-14-core5b-c-data/gb2-final-Cpp.log.gz) |
| `just test` | Initial R dev: 36,226 passes, 0 warnings, 157 tier skips, 2 frontend-guard failures. Removing the GPD/Pareto IV hold guards resolves both; affected families plus the complete conformance file pass **2,218**, 0 failures/warnings, 3 full-tier skips. | [Full dev run](2026-11-14-core5b-c-data/test.log.gz); [repair and conformance](2026-11-14-core5b-c-data/R-threshold-targeted.log) |
| `just test-full` | Initial full tier: **65,371** passes, 0 warnings, one pre-existing missing-reference skip, one obsolete GPD Unsupported assertion. Same input now expects proved nonexistence; an additional held-scale case retains Unsupported coverage. Final GPD/Pareto IV/outcomes rerun: **587**, no failures/warnings/skips. Final GB2/Fpareto/Genbeta rerun after the arithmetic guard: **1,645**, no failures/warnings/skips. | [Full tier](2026-11-14-core5b-c-data/test-full.log.gz); [outcome repair](2026-11-14-core5b-c-data/R-final-outcomes.log); [final dependants](2026-11-14-core5b-c-data/R-log-collapse.log.gz) |
| `just js-build`, `just js-test` | Final rebuild succeeds; **583/583** JS tests pass, including free-threshold/equality GPD and the GB2 arithmetic regression. The earlier 581/583 run had the stale GB2 constant-data message assertion and the new arithmetic test against the pre-fix module; both are resolved. | [Final build](2026-11-14-core5b-c-data/js-final-build.log); [final JS suite](2026-11-14-core5b-c-data/js-final-test.log); [earlier run](2026-11-14-core5b-c-data/js-test.log) |
| `just site-check` | PASS, including 85 family pages, resolving links, build-time MathML with no math runtime, and 85 generated snippets compiling/linking as C++17. | [Site checks](2026-11-14-core5b-c-data/site-final-check.log) |
| Snapshot commands and `just strict` | Both 3,619-case snapshots, diffs, outcome checks, point references and judges pass. Fresh seam replay: native 7,331 pass / 2,363 stop; double 7,329 pass / 2,364 stop / the one excluded EMG timeout. **No failure outside the known list.** | [Native diff](2026-11-14-core5b-c-data/diff-native.log); [double diff](2026-11-14-core5b-c-data/diff-double.log); [final strict run](2026-11-14-core5b-c-data/strict.log) |

Existing test inputs were retained. Expectations changed where this task corrects the meaning of a status or payload: constants/singletons, empty half-normal data, a free Pareto IV threshold, GPD's unattained supremum and free threshold, and the translated Pareto IV JS limit. No successful regression test was deleted, skipped or changed to accept a new refusal. New C++ cases are appended at the file ends. The late distinct-data guard has C++, R and JS coverage and an independent mathematical reference.

Large evidence files are gzip-compressed; [the compression ledger](2026-11-14-core5b-c-data/compressed-files.json) lists them. Scripts, inventories and mathematical evidence remain small plain files.

## Open integration and excluded work

1. **Fpareto owner / merge integration.** Its result borrows `Gb2Status` and forwards the new `Unbounded`, but its outcome switch lacks that case. The successful Emscripten build reports `-Wswitch` at `fpareto/mle.hpp:58`. The owning batch must map it to N(Unbounded) and transport the newly exposed GB2 faces with its held translation. Fpareto source is outside this allocation and is untouched. The other JS compile warning is the pre-existing binomial digit self-comparison.
2. **Shared limit-model registry.** `unavailable:double-pareto`, `unavailable:shifted-pareto` and `unavailable:shifted-weibull` need generic model/coefficient and density-judge registrations for automatic generic-law checking. This job supplies named payloads and checks the double-Pareto points independently; translated faces have R/JS regressions. The stock density judge's unregistered-law skips are not presented as density validation.
3. **Shared capability metadata.** Once obsolete frontend hold guards are removed, the generated GPD/Pareto IV rows default to implemented. Their actual supported/unsupported slices depend on held values. The shared catalogue should distinguish a core-delegated request from a promise that every value in a held-name mask is implemented; restoring frontend value-dependent guards would undo the core ownership change.
4. **Excluded accuracy/precision work.** All 34 TASK-214.5/.13 keys remain listed exactly as instructed: Burr 12, dnorm 12, EMG 2, negative hypergeometric 4 and snorm 4. This job corrects point likelihoods and outcome ownership, not those coefficient-accuracy or precision-mode findings.

[Exact integration locations and required actions](2026-11-14-core5b-c-data/integration-needs.json) and [the retained-key inventory](2026-11-14-core5b-c-data/known-diff.json) accompany this report. No new shared outcome kind or reason is required by the completed family changes.
