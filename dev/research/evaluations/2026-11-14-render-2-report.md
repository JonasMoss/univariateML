# Comparison outcomes and the open gates of step 5a: second pass

Consolidation step 5a, second pass (job render-2, TASK-214.14), on branch `codex/render` over the first pass `b230aaa8` (master `7ac4df29`). The first pass rendered every former `tie` stop as an estimate. This pass gives each unseparated or tied comparison the outcome its candidates support: an uncertified estimate when the better candidate is a family point that meets the accuracy contract, an uncertified limit when it is a face, a non-unique estimate only for proved non-uniqueness with accurate maximizers, and an unresolved stop otherwise. binom and lgamma now return invalid input for empty and non-finite samples.

All four snapshots classify every case into exactly one named category with zero defects, the outcome checks show no disagreement, and the strict judge's only new findings are the four gompertz cases fixed on master by the merged fix-nonfinite work. Every listed suite was run on the workstation (results below). No fit of any family changed: previous `Ok` results keep every field, and former ties keep their candidates and bounds.

Nothing was committed, pushed or installed; no agents were started; no board, NEWS, STATUS or roadmap file was edited. The `mle.hpp` files of gamma, norm, lnorm, logis, weibull, gumbel, gompertz and cauchy are untouched. simbox was not used.

## The rule as implemented

The decision is made in the core (`core/stop_detail.hpp`), and each family states two predicates next to its `outcome()`:

- `family_point(r, c)`: a candidate of the family itself (model `None`, or the family's own model, as `StopModel::Lst` for lst) whose coefficients are a point of the family.
- `accurate_point(r, c)`: a family point whose free coefficients the family's own stopping rule puts within `tol` of a maximizer, with `loglik_lower == loglik_upper` equal to the family's point log likelihood, computed as for an `Ok` result. The per-family evidence is in the table below.

`comparison_outcome` chooses the better candidate. If it is an accurate family point the outcome is `estimate(certified = false)`; if it is a face (another model, or `unavailable:*`) the outcome is `limit(certified = false)`; anything else, or no candidate, is `failure(Unresolved)`. `nonunique_outcome` handles proved non-uniqueness: `estimate(certified, unique = false)` when every listed maximizer is an accurate family point, a limit when all are faces (equal limiting suprema), and unresolved otherwise. `comparison_choice(r)` returns the same index to the bindings.

The shared binding helper `comparison_payload` (`bindings/common/fit.hpp`) builds the matching payload from the candidates `stop_failure` attached:

- **Estimate**: the chosen candidate's coefficients in public order, with held coefficients filled from the request, and its point log likelihood. A proved non-unique estimate takes the first listed maximizer, which is the canonical representative.
- **Limit**: the face's model and coefficients. `limit_loglik` is the face's point value when its bounds are equal and NaN otherwise, so a bound is never reported as a log likelihood. The message gains `The best candidate is the <model> limit; use `ml<model>`.` (route models name their `mlpois(...)` modifier, and `unavailable:*` laws are named without advice) unless the message already advises that fitter; `render_fit` adds the margin sentence to uncertified limits.
- **Unresolved**: the comparison message is unchanged.

Every candidate and the comparison bounds stay attached in all three cases. `render_fit` no longer chooses candidates. It renders an estimate that arrives without coefficients as an `unresolved` kind while leaving the outcome untouched, so `--outcomes` would report the disagreement; no case reaches this path.

Two refinements of the stated selection rule were needed. Both are deliberate and both are needed for the native and double-only builds to agree:

1. **Lower bounds are compared at rounding resolution.** Bounds within 8 ulps of each other count as equal. Observed gaps between the best two candidates are 1 to 4 ulps (19 for two betapr faces) and differ between the builds. Without this, huber 003332 was a limit in one build and unresolved in the other.
2. **Equal lower bounds prefer a family point that meets the accuracy contract**, then the first listed. A family point that cannot be returned earns no preference. In huber 003339 and 003371 the normal face and an `epsilon = 8` family point (the bracket end) have bounds that are equal in one build and one ulp apart in the other. Preferring the inaccurate family point flipped the kind between limit and unresolved; with the refinement both builds report the normal limit.

Unit tests of these rules are in `univariateML-cpp/tests/test_comparison.cpp` (new `comparison_tests` target). A family-level case is appended to `fatigue/test.cpp`.

## Per family: comparison statuses, rule and accuracy evidence

| Adapter | Comparison statuses | Does the chosen family point meet the accuracy contract? (evidence) | Outcome |
|---|---|---|---|
| bbinom | `ProfileTie`, `Unidentified` | Free-size ties: yes. Every candidate is an `Ok` fixed-size fit with that fit's log likelihood (new `size_profiles` flag). Fixed-size ties: no. The candidate is the unrefined grid root; an `Ok` fit reports the refined `enclosure.shape`. `Unidentified` lists no candidate. | Free-size: uncertified estimate. Fixed-size: unresolved, or uncertified binom limit. `Unidentified`: unresolved. |
| betapr | `BfgsFailure` + `comparison_tie` | No: certificate grid and patch points with `ll ± n·error`. | Uncertified gamma/inverse-gamma limit (all 6 observed) or unresolved. The limit message no longer says the comparison "does not select" those fitters. |
| bhatt | `Tie`; `comparison_tie` on non-failure statuses | No: interval witnesses; none marked as an accurate enclosed root. | Uncertified normal/uniform limit or unresolved. |
| binom (tbinom route) | `Unidentified` | No candidate (singleton support). | Unresolved. |
| binom (zib route) | `FaceTie` | ZIP faces only (finite-size escape proved). | Uncertified `zip` limit. |
| bnbinom | `ComparisonTie` | No: `ll ± error` bounds. | Uncertified nbinom/Poisson limit or unresolved. |
| burr | `CertificateTie` | No. `burr_uncertified` already returns an accurate search as an uncertified `Ok`; a remaining tie lists certificate witnesses with bounds. | Uncertified Weibull/Pareto limit or unresolved. |
| cauchy | `AmbiguousMaximum` | Yes: roots bracketed to `b-a ≤ 1e-10`, `|score| < 1e-7`, exactly as the `Ok` root. | Uncertified estimate. The core is unchanged; the adapter evaluates the point log likelihood as `fit_cauchy_location` does. |
| cmb | `FaceTie` | Point-valued family candidates are `Ok` fixed-size fits or exact closed forms; padded finite-size comparisons are bounds. | Estimate / uncertified limit (binom, CMP, two-point) / unresolved. All-zero counts: certified non-unique estimate (log likelihood 0, the pmf supremum). |
| emg | `Tie`; `comparison_tie` + `IterationFailure` | No: `value ± error` witnesses; root accuracy is not recorded on them. | Uncertified normal/translated-exponential limit or unresolved. |
| fatigue | `SearchFailure` + `comparison_tie` | Yes. Converged score roots (bisection to `0.01·tol` in beta), evaluated by the same `value()` as an `Ok` fit; the constant-sample pair is the closed-form root pair. | Uncertified estimate (all 10 observed). |
| fpareto, gb2 | `CertificateTie` | No: delegated Burr, inverse-Burr, beta-prime or shape-power witnesses with `ll ± error`. | Uncertified face limit or unresolved. |
| frechet | `comparison_tie` | No. A tie survives only when the uncertified finite fallback fails; candidates carry `± margin`. | Unresolved. |
| ged | `Nonunique`; `comparison_tie` | Proved ties: yes, exact sample locations with the conditional scale. Unproved: search roots with bounds. | Proved: non-unique estimate (first listed; `logLik` from `ged_detail::conditional_loglik` on the sorted sample, as an `Ok` conditional fit). Unproved: uncertified uniform limit or unresolved. |
| genbeta | `comparison_tie` | No: legacy search point or certificate roots with bounds. | Uncertified unit-gamma/power limit or unresolved. |
| gev | `comparison_tie` | No. A tie survives the uncertified fallback only when the search fails or is refuted. | Unresolved. |
| gpd | `Tie` | No: certificate roots with bounds and the `xi = -1` boundary floor. | Unresolved. |
| huber | `comparison_tie` | No: faces, proposal and profile points at bracketing shapes (often `epsilon = 8` or a collapsed scale). | Uncertified normal/Laplace limit or unresolved. |
| hyper, nhyper | `FaceTie` | The core fallback already returns any family candidate as an uncertified `Ok`; a surviving tie lists the face alone. | Uncertified face limit. |
| invburr, pareto4 | `CertificateTie` | No: transported Burr witnesses with bounds. | Uncertified face limit or unresolved. |
| invtrgamma, trgamma | `comparison_tie` | No: padded finite point against the endpoint face. | Uncertified Pareto/power/lognormal limit or unresolved. |
| lst, std | `CertificateTie`; `NonUniqueSymmetry`, `NonUniqueHeldSd` | No. Ties list witnesses with bounds; the dossiers state the symmetry witnesses are not accurate maximizers. lst candidates of model `lst` count as family points. | Ties: uncertified normal limit or unresolved. Proved non-uniqueness: unresolved. |
| nbinom (zinb route) | `ComparisonTie` | No: bounds. | Uncertified ZIP limit or unresolved. |
| norm (tnorm route) | `comparison_tie` | No: interior point padded by `1e-9` per observation. | Uncertified exponential-type/uniform limit or unresolved. The interior candidate is now named `mean, sd, a, b`. |
| pig | `comparison_tie` | Lists the Poisson face alone. | Uncertified Poisson limit. |
| pois (tpois route) | `Unidentified` | No candidate. | Unresolved. |
| pois (zip route) | `Tie` | No: rate-peak midpoints with bounds. | Unresolved. |
| sged | `CertificateFailure`, `*Boundary` + `comparison_tie` | No: certificate witnesses with bounds. | Uncertified half-GED/reflected/uniform limit or unresolved. Face-path family candidates are now named as SGED points. |
| sgomp | `IterationFailure` + `comparison_tie` | No: region centres (`b` to `1e-9`, `eta` to `1e-8`) before the polish an `Ok` fit receives. | Unresolved. |
| sn | `comparison_tie` | Proved non-identification (equal finite global bounds): exact closed forms with point values. Face tie: unpolished search point. | Proved: certified non-unique estimate (first listed, `alpha = 0`). Otherwise uncertified half-normal limit or unresolved. |
| snorm | `comparison_tie` | No: padded witnesses and roots. | Uncertified half-normal/reflected limit or unresolved. Constant data with sd held (equal faces at the sharp density bound): certified limit. |
| sstd | `CertificateFailure` + `comparison_tie` | No: enumerated scalar points with bounds. | Uncertified face limit or unresolved. |
| st | `comparison_tie` | No: faces and padded finite points. | Certified constant-location tie: certified half-t/half-normal limit (first face). Otherwise uncertified limit or unresolved. |
| tlambda | `CertificateFailure` + `comparison_tie` | No: certificate points with bounds. | Unresolved. |

Further core changes: bbinom gains a `size_profiles` flag; sn's proved case is detected by its equal finite global bounds (`sn_shape_unidentified`); snorm's proved constant-data face pair by its finite global upper bound with face-only candidates (`snorm_equal_faces`); cmb's zero-count maxima by family points with log likelihood 0 (`zero_count_maxima`). The dossier sentences the first pass added (bbinom, ged, lst, sn, st, std) and three descriptions of the public result (sn, st, snorm) now state these outcomes.

## Invalid input (binom, lgamma)

- **binom**: an empty sample now returns `BinomStatus::InvalidInput` (was `IterationFailure`); the adapter says "Binomial fitting needs nonempty count data." Integer samples cannot hold NaN or infinities. `outcome()` already mapped `InvalidInput` to `failure(InvalidInput)`.
- **lgamma**: new `LgammaStatus::InvalidInput`, appended so existing raw values keep their numbers. The gamma delegate (`gamma_raw`) already returned `GammaStatus::InvalidInput` for empty, non-finite and out-of-support (`x ≤ 1`) samples and for invalid controls; lgamma used to fold that into `IterationFailure`. `outcome()` maps it to `failure(InvalidInput)`, and the adapter message names the requirements. Besides empty, NaN, Inf and −Inf, this also moves lgamma's out-of-support class cases (negative, zeros, subnormal, tiny, wide) to invalid input. Genuine solver exhaustion stays `IterationFailure`: the existing C++ and R `iterlim = 1` tests are unchanged and pass.

## Snapshot categories

Baseline `2026-11-13-fit-snapshot-data` (master `c167a975`). The final snapshots of this tree share the source fingerprint `c0117a6202840339d8fab1ae125b2a32851a9f3f4fd04c829eea466901716864`. They were run on the workstation (GCC 15.2.0, `--jobs 4` under `run4` for the families, `--jobs 1` on one slot for the routes); the double-only runs use `--no-long-double`. [`classify.py`](2026-11-14-render-2-data/classify.py) extends the first pass's classifier. Every differing case falls in exactly one category, and every other payload field must be bit-identical.

| Category | Families native | Families double | Routes native | Routes double |
|---|---:|---:|---:|---:|
| Tie stop → estimate with point log likelihood | 14 | 14 | 0 | 0 |
| Tie stop → limit stop | 30 | 29 | 1 | 1 |
| Tie stop → unresolved stop | 8 | 8 | 0 | 0 |
| Invalid-input status (binom, lgamma) | 10 | 10 | 0 | 0 |
| Plain error → classed stop | 809 | 809 | 63 | 63 |
| Flags added to existing estimate | 8330 | 8325 | 1677 | 1677 |
| Margin sentence added to limit | 665 | 665 | 0 | 0 |
| Obsolete extra removed | 249 | 251 | 0 | 0 |
| Derived metadata on an existing classed stop | 1704 | 1707 | 193 | 193 |
| Numerical → tolerance refinement (foldnorm subnormal; outcome reason) | 1 | 1 | 0 | 0 |
| Identical timeout record (EMG 001311) | 0 | 1 | 0 | 0 |
| Total (defects) | 11820 (0) | 11820 (0) | 1934 (0) | 1934 (0) |

Former ties by family (native; double is the same except snorm, whose 006587 was already `numerical` in the double baseline):

- **Estimate (14)**: fatigue 10, cauchy 000795, cmb 000838, ged 002399 and 002400 (proved Laplace median intervals, non-unique).
- **Limit (30)**: betapr 6 (gamma or inverse gamma), huber 6 (normal or Laplace), invburr 3 (inverse Weibull), lst 2 and std 2 (normal), pareto4 2 (Pareto), sged 2 (half-GED), snorm 7 (half-normal or reflected half-normal), and the tnorm route 007779 (translated exponential).
- **Unresolved (8)**: burr 000701, huber 003356, 003372, 003386, 003387, 003388, invburr 003654, pareto4 010640. These are lone or best family points with bounds only. For burr 000701, a fit holding more coefficients reaches a log likelihood 67 above the listed candidate's bound, so it is not a maximizer.

The classifier checks, for each former tie:

- the candidates and the comparison bounds are kept; tnorm's interior candidate was relabelled from the face's names to its own, with every value kept;
- the chosen candidate has a lower bound within 8 ulps of the largest (or is the first listed, for non-unique estimates);
- held coefficients equal the request, and `fixed` lists the requested holds;
- an estimate's `logLik` is the candidate's point value. For cauchy and ged, whose candidates carry bounds, the adapter's evaluation lies within those bounds.

`--outcomes` reports 0 disagreements between outcome and emitted kind in all four snapshots; the double-only run reports 84 of 85 families because of EMG's baseline timeout. The plain `--diff` runs exit 1 because the records differ by design. Every command, exit code and machine is in [`checks.json`](2026-11-14-render-2-data/checks.json); [`collect.py`](2026-11-14-render-2-data/collect.py) reruns them.

## Judge

`fit-judge.py` on both modes, families and routes ([log](2026-11-14-render-2-data/fit-judge.log)): 13,754 cases in each mode; 20 listed as known, 4 new, 0 listed but passing.

- **Held (21 new in the first pass)**: none. The huber, pareto4 and snorm faces are now limit stops.
- **Nesting (8 new in the first pass)**: none. betapr's faces report `limit_loglik = NaN` because their candidates carry bounds; burr 000701 is unresolved.
- **Invalid input (14 new in the first pass)**: binom and lgamma pass. The 4 remaining new findings are gompertz empty and NaN in both modes. The merged fix-nonfinite work fixes them on master but is not in this tree; on the coordinator's instruction they were not added to the known list.
- **Pruned**: `mode|both|sstd|archive|006717` (TASK-214.13) now passes (both builds give the same kind) and was removed with `--prune`.

The run therefore exits 1 on this tree, solely because of the four gompertz entries, which the merge with master resolves.

## Suites

All runs were on the workstation (simbox was not used).

| Command | Result | Log |
|---|---|---|
| `just cpp-test` | PASS, 110/110 (109 + new `comparison_tests`) | [log](2026-11-14-render-2-data/cpp-test.log.gz) |
| `just cpp-test-no-long-double` | PASS, 154/154, GCC 15 | [log](2026-11-14-render-2-data/cpp-test-no-long-double.log.gz) |
| `just test` | PASS: FAIL 0, WARN 0, SKIP 157, PASS 28930 | [log](2026-11-14-render-2-data/test.log.gz) |
| `just test-full` | PASS: FAIL 0, WARN 0, SKIP 1, PASS 58069 | [log](2026-11-14-render-2-data/test-full.log.gz) |
| `just js-build` | PASS | [log](2026-11-14-render-2-data/js-build.log.gz) |
| `just js-test` | PASS, 581/581 | [log](2026-11-14-render-2-data/js-test.log.gz) |
| `just site-check` | PASS (no tracked changes under `site/`) | [log](2026-11-14-render-2-data/site-check.log.gz) |

The two skew-normal assertions in `bindings/r/distributions/snorm/test.R` ("snorm classified holds agree with independent multistart oracles") are unchanged and pass. `mlsnorm(c(-1,0,1), sd = 1.4)` is now an uncertified half-normal limit, so the held-coordinate and likelihood checks no longer meet a candidate without an `sd` coordinate.

## Changed test assertions

Every tie assertion the first pass changed was rerun, and each one whose case is now a stop was set to that stop's kind and class. Assertions on cases that are still estimates (cauchy, fatigue, ged, sn, the zero-count cmb cases) are kept. No test was deleted. Two helpers were added to the JS tests: `comparisonStop(run, kind, check)` asserts the stop's kind and runs the original candidate checks on the error. `test-outcomes.R` (a first-pass test) now checks the selection rule and the point `logLik` for cauchy, a GED median-interval non-unique estimate, and lst's symmetry proof as an unresolved stop. The SGED README-oracle loop now also takes limit and unresolved stops that keep their compared candidates.

Machine-readable list: [`changed-assertions.json`](2026-11-14-render-2-data/changed-assertions.json), produced by [`assertion-changes.py`](2026-11-14-render-2-data/assertion-changes.py) from `git diff -U0 b230aaa8`; hunks with an empty Old are additions.

| File | Test | Old | New |
|---|---|---|---|
| bindings/js/fit-test.mjs | sstd xi-only certificate, reflection, budget and half-t comparison | ` const tied=m.fitsstd(new Float64Array([-1,1]),{mean:0,sd:1,nu:5},{});assert.equal(tied.status,'Ok');assert.equal(tied.certified,false); ` | ` const tied=m.fitsstd(new Float64Array([-1,1]),{mean:0,sd:1,nu:5},{});assert.equal(tied.kind,'limit');assert.equal(tied.certified,false);assert.equal(tied.model,'unavailable:reflected-half-t'); ` |
| bindings/js/stop-test.mjs |  | (added) | ` // A comparison the certificate cannot separate that ends as a limit or an // unresolved stop (its better candidate is a face or not an accurate maximizer). function comparisonStop(run, kind, check) {   assert.throws(run, e => {     assert.equal(e.kind, kind);     return check(e) === true;   }); } ` |
| bindings/js/stop-test.mjs | Burr and reciprocal sign-uncertain-seam structured tie | ` comparisonFit(() => modifierRoute(api,family).fit(data), e => {       assert.equal(e.status,'Ok');assert.ok(e.candidates.length>=2); ` | ` comparisonStop(() => modifierRoute(api,family).fit(data), 'limit', e => {       assert.equal(e.certified,false);assert.ok(e.candidates.length>=2); ` |
| bindings/js/stop-test.mjs | discrete stops retain fitted package and non-package faces | ` comparisonFit(()=>api.cmb.fit([0,1]),e=>{     assert.equal(e.status,'Ok');assert.equal(e.model,'binom'); ` | ` comparisonStop(()=>api.cmb.fit([0,1]),'limit',e=>{     assert.equal(e.certified,false);assert.equal(e.model,'binom'); ` |
| bindings/js/stop-test.mjs | conditional snorm and skew-t ambiguities retain both compared models | ` for(const [family,held,model] of [['snorm',{mean:0,sd:1e-4},'hnorm'],['st',{xi:0,omega:1e-4,nu:1e20},'ht']])     comparisonFit(()=>modifierRoute(api,family).fit([1,2],held),e=>{       assert.equal(e.status,'Ok');assert.equal(e.candidates.length,2); ` | ` // snorm: the half-normal face is the better candidate; skew-t: the finite point.   for(const [family,held,model,kind] of [['snorm',{mean:0,sd:1e-4},'hnorm','limit'],['st',{xi:0,omega:1e-4,nu:1e20},'ht','unresolved']])     comparisonStop(()=>modifierRoute(api,family).fit([1,2],held),kind,e=>{       assert.equal(e.candidates.length,2); ` |
| bindings/js/stop-test.mjs | ZINB certificate transports competing maxima and terminal budgets | ` comparisonFit(() => modifierRoute(api,'zinb').fit([...Array(99).fill(0),10], {size:10,pi:.76839477568758077746}), e => {     assert.equal(e.status,'Ok');assert.equal(e.candidates.length,2); ` | ` comparisonStop(() => modifierRoute(api,'zinb').fit([...Array(99).fill(0),10], {size:10,pi:.76839477568758077746}), 'unresolved', e => {     assert.equal(e.candidates.length,2); ` |
| bindings/js/stop-test.mjs | PIG global certificate transports Poisson, tie, budget and numerical stops | ` comparisonFit(() => api.pig.fit([0,1,2],{mean:1+Math.sqrt(1/3)}),e=>{     assert.equal(e.status,'Ok');assert.equal(e.candidates[0].model,'pois'); ` | ` comparisonStop(() => api.pig.fit([0,1,2],{mean:1+Math.sqrt(1/3)}),'limit',e=>{     assert.equal(e.certified,false);assert.equal(e.model,'pois');assert.equal(e.candidates[0].model,'pois'); ` |
| bindings/js/stop-test.mjs | ZIP certified rate ties, budgets and numerical stops | ` comparisonFit(()=>modifierRoute(api,'zip').fit(x,{pi:.7254971052291191}),e=> { ` | ` comparisonStop(()=>modifierRoute(api,'zip').fit(x,{pi:.7254971052291191}),'unresolved',e=> { ` |
| bindings/js/stop-test.mjs | tlambda certificates keep contact ties, budget and collapse witnesses | ` comparisonFit(()=>api.tlambda.fit([-.25,.25],{sigma:1,lambda:1.5}), e=> {     assert.equal(e.status,'Ok');assert.equal(e.candidates.length,2); ` | ` comparisonStop(()=>api.tlambda.fit([-.25,.25],{sigma:1,lambda:1.5}), 'unresolved', e=> {     assert.equal(e.candidates.length,2); ` |
| bindings/js/stop-test.mjs | snorm held certificate preserves opposite-face ties and root failures | ` comparisonFit(()=>api.snorm.fit([0,0,0],{sd:1}),e=> {     assert.equal(e.status,'Ok');assert.equal(e.candidates.length,2); ` | ` comparisonStop(()=>api.snorm.fit([0,0,0],{sd:1}),'limit',e=> {     assert.equal(e.certified,true);assert.equal(e.candidates.length,2); ` |
| bindings/js/stop-test.mjs | snorm reflected finite modes remain a comparison tie | ` comparisonFit(()=>api.snorm.fit([-1,0,1],{mean:0,sd:.755}),e=> {     assert.equal(e.status,'Ok');assert.equal(e.candidates.length,2); ` | ` comparisonStop(()=>api.snorm.fit([-1,0,1],{mean:0,sd:.755}),'unresolved',e=> {     assert.equal(e.candidates.length,2); ` |
| bindings/js/stop-test.mjs | st conditional location and alpha-scale certificates retain stops | ` comparisonFit(()=>api.st.fit([-2,2],{omega:1,alpha:0,nu:1}), e=>{     assert.equal(e.status,'Ok'); ` | ` comparisonStop(()=>api.st.fit([-2,2],{omega:1,alpha:0,nu:1}), 'unresolved', e=>{ ` |
| bindings/js/stop-test.mjs | st constant-data location-skew faces retain the sharp global bound | ` comparisonFit(()=>api.st.fit([2,2,2],{omega:1,nu:4}),e=>{     assert.equal(e.status,'Ok');assert.deepEqual(e.candidates.map(c=>c.model),['ht','unavailable:reflected-half-t']); ` | ` comparisonStop(()=>api.st.fit([2,2,2],{omega:1,nu:4}),'limit',e=>{     assert.equal(e.certified,true);assert.deepEqual(e.candidates.map(c=>c.model),['ht','unavailable:reflected-half-t']); ` |
| bindings/js/stop-test.mjs | st constant-data free-nu corner keeps both half-normal faces | ` comparisonFit(()=>api.st.fit([2,2,2],{omega:1}),e=>{     assert.equal(e.status,'Ok');assert.deepEqual(e.candidates.map(c=>c.model),['hnorm','unavailable:reflected-half-normal']); ` | ` comparisonStop(()=>api.st.fit([2,2,2],{omega:1}),'limit',e=>{     assert.equal(e.certified,true);assert.deepEqual(e.candidates.map(c=>c.model),['hnorm','unavailable:reflected-half-normal']); ` |
| bindings/js/test.mjs |  | (added) | ` // A comparison the certificate cannot separate that ends as a limit or an // unresolved stop (its better candidate is a face or not an accurate maximizer). function comparisonStop(run, kind, check) {   assert.throws(run, e => {     assert.equal(e.kind, kind);     return check(e) === true;   }); } ` |
| bindings/js/test.mjs | trgamma held endpoint comparisons stop without asserting nonattainment | ` comparisonFit(() => modifierRoute(api,family).fit(data,{scale:1}),error => {       assert.equal(error.status,'Ok'); ` | ` comparisonStop(() => modifierRoute(api,family).fit(data,{scale:1}),'limit',error => {       assert.equal(error.certified,false); ` |
| bindings/js/test.mjs | lst and std fixed-scale location certificates transport equal maxima | ` comparisonFit(() => modifierRoute(api,family).fit(new Float64Array([-3, 3]), held), error => {       assert.equal(error.status,'Ok'); ` | ` comparisonStop(() => modifierRoute(api,family).fit(new Float64Array([-3, 3]), held), 'unresolved', error => { ` |
| bindings/js/test.mjs | SGED subunit scale ties and exhausted certificates stop in the common binding | ` comparisonFit(()=>api.sged.fit(new Float64Array([-1,-1,-1]),{mean: 0,nu: .5,xi},{iterlim: 400}),error=>{     assert.equal(error.status,'Ok'); ` | ` comparisonStop(()=>api.sged.fit(new Float64Array([-1,-1,-1]),{mean: 0,nu: .5,xi},{iterlim: 400}),'unresolved',error=>{ ` |
| bindings/js/test.mjs | SGED fixed-shape free-skew certificates share core routing | ` comparisonFit(()=>api.sged.fit(new Float64Array([-1,1]),{nu:.5}),error=>{     assert.equal(error.status,'Ok');assert.equal(error.candidates.length,2); ` | ` comparisonStop(()=>api.sged.fit(new Float64Array([-1,1]),{nu:.5}),'limit',error=>{     assert.equal(error.certified,false);assert.equal(error.candidates.length,2); ` |
| bindings/js/test.mjs | globally winning tied ZIP faces retain both rate candidates | ` comparisonFit(() => modifierRoute(api,'zib').fit(new Float64Array([...Array(198).fill(0),1,19]),{pi:.7254971052291191}),error => {     assert.equal(error.status,'Ok'); ` | ` comparisonStop(() => modifierRoute(api,'zib').fit(new Float64Array([...Array(198).fill(0),1,19]),{pi:.7254971052291191}),'limit',error => {     assert.equal(error.certified,false); ` |
| bindings/js/test.mjs | SGED symmetric free-shape delegate transports fits and structured stops | ` comparisonFit(()=>api.sged.fit(new Float64Array([-1,1]),{sd:.1,xi:1},{iterlim:1}),e=>{     assert.equal(e.status,'Ok');assert.equal(e.candidates.length,2); ` | ` comparisonStop(()=>api.sged.fit(new Float64Array([-1,1]),{sd:.1,xi:1},{iterlim:1}),'unresolved',e=>{     assert.equal(e.candidates.length,2); ` |
| bindings/js/test.mjs | SGED weighted median and smooth reflected half-GED ties retain their witnesses | ` comparisonFit(()=>api.sged.fit(new Float64Array([-100,...Array(3).fill(-.01),...Array(15).fill(.01),100]),{xi:2}),e=>{     assert.equal(e.status,'Ok');assert.ok(e.candidates.length>=2); ` | ` comparisonStop(()=>api.sged.fit(new Float64Array([-100,...Array(3).fill(-.01),...Array(15).fill(.01),100]),{xi:2}),'unresolved',e=>{     assert.ok(e.candidates.length>=2); ` |
| bindings/js/test.mjs | SGED weighted median and smooth reflected half-GED ties retain their witnesses | ` comparisonFit(()=>api.sged.fit(new Float64Array([-1,1]),{nu}),e=>{       assert.equal(e.status,'Ok');assert.ok(Number.isFinite(e.loglik_upper)); ` | ` comparisonStop(()=>api.sged.fit(new Float64Array([-1,1]),{nu}),'limit',e=>{       assert.equal(e.certified,false);assert.ok(Number.isFinite(e.loglik_upper)); ` |
| bindings/js/test.mjs | CMB finite-size ties retain two distinct witnesses and nu-held census certifies | (added) | ` // (1,1): padded finite-size comparison, unresolved. All-zero counts: both   // listed sizes attain log likelihood 0, a certified non-unique estimate. ` |
| bindings/js/test.mjs | CMB finite-size ties retain two distinct witnesses and nu-held census certifies | ` comparisonFit(()=>api.cmb.fit(new Float64Array(data),held),e=>{       assert.equal(e.status,'Ok');assert.match(e.message,/Distinct finite sizes/); ` | ` (data[0]===1?(run,check)=>comparisonStop(run,'unresolved',check):comparisonFit)(()=>api.cmb.fit(new Float64Array(data),held),e=>{       if(data[0]===0){assert.equal(e.status,'Ok');assert.equal(e.certified,true);assert.equal(e.unique,false);assert.equal(e.logLik,0);}       assert.match(e.message,/Distinct finite sizes/); ` |
| bindings/js/test.mjs | betapr all-free certificate returns verified fits and structured global stops | ` comparisonFit(()=>api.betapr.fit(new Float64Array([.5,1,2])),e=>{     assert.equal(e.status,'Ok');assert.ok(e.candidates.length>=2);return true; ` | ` comparisonStop(()=>api.betapr.fit(new Float64Array([.5,1,2])),'limit',e=>{     assert.equal(e.certified,false);assert.ok(e.candidates.length>=2);return true; ` |
| bindings/js/test.mjs | Student held subsets expose proved symmetry nonuniqueness | ` comparisonFit(()=>modifierRoute(api,family).fit(new Float64Array(x),held),error=>{       assert.equal(error.status,'Ok'); ` | ` // Proved non-uniqueness whose witnesses lack coefficient accuracy: unresolved.     comparisonStop(()=>modifierRoute(api,family).fit(new Float64Array(x),held),'unresolved',error=>{ ` |
| bindings/r/distributions/bbinom/test.R | faces and nonidentification are explicit | ` expect_match(attr(mlbbinom(c(0,1,1,0),size=1),"outcome_message"),"not identified")   expect_match(attr(mlbbinom(c(0,0),size=0),"outcome_message"),"not identified") ` | ` expect_error(mlbbinom(c(0,1,1,0),size=1),"not identified",class="univariateML_unresolved")   expect_error(mlbbinom(c(0,0),size=0),"not identified",class="univariateML_unresolved") ` |
| bindings/r/distributions/bbinom/test.R | held beta free-size fit has a certified tail | ` expect_match(attr(mlbbinom(c(0,0),size=NULL,beta=2),"outcome_message"),"not identified") ` | ` expect_error(mlbbinom(c(0,0),size=NULL,beta=2),"not identified",class="univariateML_unresolved") ` |
| bindings/r/distributions/bbinom/test.R | held alpha free-size certificates and stopping outcomes | ` expect_match(attr(mlbbinom(c(0,0),size=NULL,alpha=1),"outcome_message"),"not identified") ` | ` expect_error(mlbbinom(c(0,0),size=NULL,alpha=1),"not identified",class="univariateML_unresolved") ` |
| bindings/r/distributions/betapr/test.R | all-free certificate stops carry witnesses and selected faces are proper | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_limit")   expect_false(e$certified) ` |
| bindings/r/distributions/binom/test.R | discrete limit stops expose fitted faces without changing messages | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_limit")   expect_false(e$certified) ` |
| bindings/r/distributions/burr/test.R | Burr global certificate stops preserve their diagnostics | ` expect_match(attr(mlburr(x),"outcome_message"), "certificate unresolved.*sign-uncertain seam; no verified MLE") ` | ` expect_error(mlburr(x), "certificate unresolved.*sign-uncertain seam; no verified MLE", class="univariateML_limit") ` |
| bindings/r/distributions/burr/test.R | burr sign-uncertain-seam exposes structured tie diagnostics | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_limit")   expect_identical(e$model,"weibull")   expect_false(e$certified) ` |
| bindings/r/distributions/cmb/test.R | CMB finite-size ties stop with both finite witnesses | ` expect_s3_class(e,"univariateML") ` | ` if (identical(args$x,c(0,0))) {       # All-zero counts: both listed sizes attain the pmf supremum (log likelihood 0).       expect_s3_class(e,"univariateML")       expect_true(attr(e,"certified"))       expect_false(attr(e,"unique"))       expect_equal(as.numeric(logLik(e)),0)     } else expect_s3_class(e,"univariateML_unresolved") ` |
| bindings/r/distributions/gpd/test.R | GPD exhausted tail bracket is a numerical stop | ` expect_s3_class(flat,"univariateML")   expect_equal(fit_payload(flat)$kind,"estimate") ` | ` expect_s3_class(flat,"univariateML_unresolved")   expect_equal(fit_payload(flat)$kind,"unresolved") ` |
| bindings/r/distributions/gpd/test.R | GPD finite competing maxima give a structured tie | ` expect_s3_class(err,"univariateML") ` | ` expect_s3_class(err,"univariateML_unresolved") ` |
| bindings/r/distributions/huber/test.R | huber certificates stop at an unresolved finite-normal comparison | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_limit")   expect_false(e$certified) ` |
| bindings/r/distributions/invburr/test.R | invburr preserves certificate refusal diagnostics | ` expect_match(attr(mlinvburr(1/x),"outcome_message"), "certificate unresolved.*sign-uncertain seam; no verified MLE") ` | ` expect_error(mlinvburr(1/x), "certificate unresolved.*sign-uncertain seam; no verified MLE", class="univariateML_limit") ` |
| bindings/r/distributions/invburr/test.R | invburr sign-uncertain-seam exposes structured tie diagnostics | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_limit")   expect_identical(e$model,"invweibull")   expect_false(e$certified) ` |
| bindings/r/distributions/lst/test.R | held-scale location certificates retain both equal maxima | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_unresolved") ` |
| bindings/r/distributions/lst/test.R | lst symmetry proves distinct global maximizers | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_unresolved") ` |
| bindings/r/distributions/pareto4/test.R | pareto4 preserves certificate refusal diagnostics | ` expect_match(attr(mlpareto4(x,min=0),"outcome_message"), "certificate unresolved.*sign-uncertain seam; no verified MLE") ` | ` expect_error(mlpareto4(x,min=0), "certificate unresolved.*sign-uncertain seam; no verified MLE", class="univariateML_limit") ` |
| bindings/r/distributions/pig/test.R | general shape certificates transport limit, tie and budget details | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_limit")   expect_false(e$certified) ` |
| bindings/r/distributions/sged/test.R | sged P0b subsets agree with independent README oracles | ` if(inherits(fit,'error') \|\| !is.null(attr(fit,'candidates'))) {       if(!is.null(attr(fit,'candidates'))) { ` | ` # A comparison: an estimate with candidates, or a limit or unresolved     # stop that keeps the compared candidates.     comparison <- !is.null(attr(fit,'candidates')) \|\|       (inherits(fit,'univariateML_stop') && fit$kind %in% c('limit','unresolved') && NROW(fit$candidates)>=2L)     if(inherits(fit,'error') \|\| comparison) {       if(comparison) { ` |
| bindings/r/distributions/sged/test.R | sged held-shape ties and budgets have structured witnesses | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_unresolved") ` |
| bindings/r/distributions/sged/test.R | SGED subunit separated scale maxima and budgets are structured stops | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_unresolved") ` |
| bindings/r/distributions/sged/test.R | sged free skew subunit enumerates knots and both faces | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_limit")     expect_false(e$certified) ` |
| bindings/r/distributions/sged/test.R | SGED symmetric free-shape stops carry SGED and uniform coordinates | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_unresolved") ` |
| bindings/r/distributions/sged/test.R | SGED profile certificates preserve weighted median and reflected face ties | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_unresolved") ` |
| bindings/r/distributions/sged/test.R | SGED profile certificates preserve weighted median and reflected face ties | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_limit")     expect_false(e$certified) ` |
| bindings/r/distributions/sged/test.R | SGED held moments retain the two finite Laplace skew maxima | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_unresolved") ` |
| bindings/r/distributions/sged/test.R | SGED held moments certify strict-support half-GED winners | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_limit")   expect_false(e$certified) ` |
| bindings/r/distributions/sged/test.R | loose coefficient accuracy preserves separated-maxima diagnostics | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_unresolved") ` |
| bindings/r/distributions/snorm/test.R | snorm certificate ties and numerical stops are structured | ` expect_s3_class(tied,"univariateML") ` | ` expect_s3_class(tied,"univariateML_limit")   expect_true(tied$certified) ` |
| bindings/r/distributions/snorm/test.R | snorm finite reflected modes retain both witnesses | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_unresolved") ` |
| bindings/r/distributions/sstd/test.R | sstd scalar certificates preserve ties, holds and nonunit scale branches | ` expect_s3_class(mlsstd(c(-2,2),sd=1,nu=3,xi=1),"univariateML") ` | ` expect_error(mlsstd(c(-2,2),sd=1,nu=3,xi=1),class="univariateML_unresolved") ` |
| bindings/r/distributions/sstd/test.R | sstd xi-only certificate compares the complete skew range | ` expect_s3_class(mlsstd(c(-1,1),mean=0,sd=1,nu=5),"univariateML") ` | ` expect_error(mlsstd(c(-1,1),mean=0,sd=1,nu=5),class="univariateML_limit") ` |
| bindings/r/distributions/st/test.R | st conditional skew certificates preserve endpoint factors and stops | ` expect_s3_class(mlst(c(2,2,2),xi=2,omega=1,nu=.5),"univariateML") ` | ` expect_error(mlst(c(2,2,2),xi=2,omega=1,nu=.5),class="univariateML_unresolved") ` |
| bindings/r/distributions/st/test.R | st location certificates expose ties and budget witnesses | ` expect_s3_class(mlst(c(-2,2),omega=1,alpha=0,nu=1),"univariateML") ` | ` expect_error(mlst(c(-2,2),omega=1,alpha=0,nu=1),class="univariateML_unresolved") ` |
| bindings/r/distributions/st/test.R | st constant-data location-skew fit preserves both limiting witnesses | ` expect_s3_class(f,"univariateML") ` | ` expect_s3_class(f,"univariateML_limit")   expect_true(f$certified) ` |
| bindings/r/distributions/st/test.R | st constant-data free-nu corner keeps both half-normal faces | ` expect_s3_class(f,"univariateML") ` | ` expect_s3_class(f,"univariateML_limit")   expect_true(f$certified) ` |
| bindings/r/distributions/std/test.R | held sd location ties preserve std candidate coordinates | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_unresolved") ` |
| bindings/r/distributions/std/test.R | std symmetry proves distinct global maximizers | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_unresolved") ` |
| bindings/r/distributions/tlambda/test.R | tlambda certificate stops retain ties, budgets and collapse witnesses | ` expect_s3_class(tie,"univariateML")   expect_identical(fit_payload(tie)$kind,"estimate") ` | ` expect_s3_class(tie,"univariateML_unresolved")   expect_identical(fit_payload(tie)$kind,"unresolved") ` |
| bindings/r/distributions/trgamma/test.R | held endpoint comparisons do not assert nonattainment | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_limit")     expect_false(e$certified) ` |
| bindings/r/distributions/trgamma/test.R | positive near-face improvements stop as unresolved comparisons | ` expect_s3_class(e,"univariateML") ` | ` expect_s3_class(e,"univariateML_limit")     expect_identical(e$model,"lnorm") ` |
| bindings/r/modifier-tests/zib.R | zib proves escape even when the winning ZIP rate is tied | ` expect_s3_class(e,"univariateML")   expect_identical(fit_payload(e)$kind,"estimate") ` | ` expect_s3_class(e,"univariateML_limit")   expect_identical(fit_payload(e)$kind,"limit")   expect_false(e$certified) ` |
| bindings/r/modifier-tests/zinb.R | ZINB certified stops retain competing maxima and explicit budgets | ` expect_s3_class(tie,"univariateML")   expect_identical(fit_payload(tie)$kind,"estimate") ` | ` expect_s3_class(tie,"univariateML_unresolved")   expect_identical(fit_payload(tie)$kind,"unresolved") ` |
| bindings/r/modifier-tests/zip.R | ZIP certificate returns structured tie, budget and numerical stops | ` expect_s3_class(tie,"univariateML") ` | ` expect_s3_class(tie,"univariateML_unresolved") ` |
| bindings/r/tests/testthat/test-outcomes.R | ties select existing witnesses and attach their complete set | ` chosen <- which.max(witnesses$loglik_lower) ` | ` # The better candidate: the first whose lower bound is within 8 ulps of the largest.   top <- max(witnesses$loglik_lower)   chosen <- which(witnesses$loglik_lower >= top-8*.Machine$double.eps*abs(top))[1] ` |
| bindings/r/tests/testthat/test-outcomes.R | ties select existing witnesses and attach their complete set | (added) | ` # Its log likelihood is a point value at the returned coefficients, not a bound.   expect_equal(as.numeric(logLik(fit)),sum(dcauchy(c(-2,2),coef(fit)[["location"]],1,log=TRUE)),tolerance=1e-14) ` |
| bindings/r/tests/testthat/test-outcomes.R | ties select existing witnesses and attach their complete set | ` fit <- mllst(c(-100,-99,0,99,100),df=.5) ` | ` # Proved non-uniqueness with exact witnesses (GED median interval): first listed.   fit <- mlged(c(-1,1),sd=.1,iterlim=1L) ` |
| bindings/r/tests/testthat/test-outcomes.R | ties select existing witnesses and attach their complete set | (added) | ` expect_equal(as.numeric(logLik(fit)),sum(dged(c(-1,1),coef(fit)[["mean"]],.1,coef(fit)[["nu"]],log=TRUE)),tolerance=1e-12)   # Witnesses without coefficient accuracy (lst symmetry proof) are not an estimate.   stop <- tryCatch(mllst(c(-100,-99,0,99,100),df=.5),error=identity)   expect_s3_class(stop,"univariateML_unresolved")   expect_equal(nrow(stop$candidates),2L) ` |
| bindings/r/tests/testthat/test-stops-cont.R | conditional snorm and skew-t ambiguities retain compared models | ` expect_s3_class(e,"univariateML") ` | ` # snorm: the half-normal face is the better candidate; skew-t: the finite point.     expect_s3_class(e,c("univariateML_limit","univariateML_unresolved")[[i]]) ` |


## Open entries

- **NEWS.** The first pass's NEWS paragraph says "Tied candidates return an estimate with maximizers or candidates attached". It now needs to read: "Candidates the certificate cannot separate return the better one: an uncertified estimate when it is an accurate family point, otherwise an uncertified limit or an unresolved stop, with all candidates attached; proved non-uniqueness returns an estimate only when its maximizers are accurate." NEWS was not edited, as the brief requires.
- **Cauchy core.** The core keeps likelihood bounds only for `AmbiguousMaximum`. The adapter evaluates the point log likelihood exactly as `fit_cauchy_location` does. Storing that point value in `cauchy/mle.hpp` would remove the duplication but was outside this job. The balanced two-point held-scale case is symmetric, so it is a proved non-uniqueness, but the core does not claim it.
- **Witnesses that could become estimates with more core work.**
  - bhatt and EMG: their certificates know which winner roots are accurate enclosed roots but do not mark them on the candidates.
  - st's held-location constant-data alpha tie lists exact witnesses with padded bounds and no certification.
  - bbinom's fixed-size ties list the grid root instead of the refined enclosure.

  Each is unresolved today; a per-candidate accuracy mark (or the refined coefficient and its point likelihood) would let these return estimates.
- **Unidentified without a listed maximizer.** bbinom (zero size, one trial, all-zero counts) and tbinom/tpois singleton supports are unresolved stops, because no maximizer is listed. cmb maps its own size-zero `Unidentified` to invalid input, so the same phenomenon has two outcomes; the consolidation should pick one, either with a listed representative or with a reason of its own.
- **Dossier wording.** Several dossiers (emg, genbeta, gb2, huber, sged, sstd, st) still call the core's comparison status a "tie stop" in prose. The rendered outcome follows the rule above; a wording sweep remains.
- **gompertz.** The four invalid-input findings are fixed on master and will disappear at the merge.

## Files

- Core: `univariateML-cpp/include/cppunivariate/core/stop_detail.hpp`; family `outcome()` and predicates in the `mle.hpp` (or route `*_mle.hpp`/`tpois_profile.hpp`) of the 38 comparison families other than cauchy; `lgamma/mle.hpp` and `binom/mle.hpp` for invalid input.
- Bindings: `bindings/common/fit.hpp` and the 39 adapters in [`comparison-adapters.json`](2026-11-13-render-data/comparison-adapters.json), plus the binom and lgamma adapters.
- Tests: `univariateML-cpp/tests/test_comparison.cpp`, `tests/CMakeLists.txt`, `fatigue/test.cpp`; R and JS test files listed above; `dev/validation/strict-known.json` (pruned).
- Evidence: [`2026-11-14-render-2-data/`](2026-11-14-render-2-data/), with the final snapshots, provenance, classifications, outcome logs, judge log, suite logs and scripts.
