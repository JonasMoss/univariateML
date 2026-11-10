# Count-family uncertified fallback — TASK-168.1

The seven count families now retain a successful finite search result when their global or coefficient certificate cannot close, and expose its per-fit certification flag through C++, R and JavaScript. Proved stops and searches without a successful finite candidate still stop. The 1,073-case replay ends with **911 certified fits, 88 uncertified fits and 74 stops**. All 909 previously successful coefficient vectors are unchanged. All required workstation test runs finished green; the requested second-machine numerical check could not run because simbox failed its health checks.

Worktree: `univariateML.wt/fallback-count`; branch: `codex/fallback-count`; baseline: `af295963493ce0b73f678d1316e5c7355914aa89`. No commits, pushes, board edits or dependency installations were made. Changes cover the seven owned fitters, their common binding adapters, tests and dossiers, and the owned `ztbinom/face_certificate.hpp`. Shared converters, other families, core, d/p/q/r, special functions, truth tables, code generation, goldens and project status/roadmap files were left unchanged.

**Behavior and implementation.** Each result carries `bool certified`; each owned common adapter emits numeric `certified`. The existing shared R conversion supplies the logical list element and `attr(estimates,"certified")`; JavaScript receives 0/1. There are no family-specific R conversion shims. A finite fallback reports `Ok` with `certified=false`, explicitly giving up the global or requested coefficient-accuracy claim. It is not a verified MLE. Local solve failures, nonfinite candidates and exhausted searches with no candidate remain failures. A certificate establishing that a finite candidate loses still prevents its return as an MLE.

- **bbinom:** successful score-gated fixed-size solves survive coefficient inclusion, curvature/outside exclusion, face comparison and certificate-cell exhaustion. Supported held-shape free-size routes retain a plain fixed-size prefix winner, including the old held-beta preflight refusal. Search and certificate work share the cell allowance. The adapter no longer lets a leftover budget diagnostic override an `Ok` fallback. The existing unsupported unrestricted-size/both-shapes-free subset is retained and discussed below.
- **hyper / nhyper:** recovery separates the global growing-urn certificate from finite prefix enumeration; truncated grids and inconclusive face comparisons return their best feasible prefix. Hyper's held-`m`/held-`k` symmetry is preserved through recovery, including face reset. Nhyper uses representable lower stopping counts and reassociated population bounds; omitted branches prevent certification. Its exact constant-law construction also recovers a feasible point missed by the old initial bounds.
- **tbinom:** the free-size profile uses mode-relative kernels and a relative normalizer, avoiding subtraction of huge factorial logs. Normalizer and observation recurrences consume the enumeration allowance. A completed finite profile survives an unclosed face or prefix proof. Held probability avoids an unnecessary free-probability mean solve. Fixed-size default fitting remains on its original path. The current public tbinom interface has no requested `tol`/`iterlim` argument; its existing internal 512-iteration allowance is retained. No control was added or renamed.
- **ztbinom / zib / zmbinom:** positive-count or mixture profiles retain successful integer-prefix results through sign, prefix and certificate-budget failures. Held-probability scans account for `n + sum(counts)` work using saturating arithmetic. If even the first evaluation is unaffordable, they stop with a budget failure and no invented finite witness. Zmbinom preserves the finite zero-mass contribution and the positive fitter's budget diagnostics. Complete integer-size coverage in zib cannot upgrade an unresolved probability-root enclosure to certified.
- **Coefficient precision:** ztbinom, zib and zmbinom conservatively mark nondegenerate estimated interior floating coefficients uncertified for requested tolerance below eight double epsilons. Examples are `p=2/3` and `p0`/`pi=1/3` at `1e-18`; no stored double can meet that relative target. Degenerate laws and held coefficients are treated separately. The guard can leave exactly representable interior coefficients uncertified too. Coefficients themselves are unchanged.

**Stop-path inventory.** The table inventories status/adaptor branches, including the former stops converted here. “Plain” means a binding error without a specific error kind; the study labels this `input-or-search`. “Unclosed” denotes missing certification, not established nonexistence. Multiple causes sharing a status are separated when they require different behavior. The machine-readable inventory is [stop-paths.json](2026-11-10-fallback-count-data/stop-paths.json).

| # | Family | Status / route | Error kind | Classification | Trigger and resulting behavior |
|---|---|---|---|---|---|
| 1 | all | binding validation | plain | proved stop | Missing required designs, NA/nonfinite held values, held values outside the coefficient domain, noninteger/negative observations; tbinom additionally empty data or incompatible bounds. **After:** stop. |
| 2 | bbinom | unsupported free size and both shapes | plain | search failure | No shape held with size free: no implemented unrestricted search. Fixed-size face escapes do not provide a successful finite joint-shape profile to reuse; global size/shape face comparisons are open. This is the existing declared structural not-implemented subset, not a nonexistence theorem. **After:** existing structural stop retained; a safe general joint search is open. |
| 3 | bbinom | InvalidInput | plain | proved stop | Empty/negative counts; invalid size, shape, tolerance or iteration limit. **After:** stop. |
| 4 | bbinom | SizeTooSmall | plain | proved stop | Held size smaller than an observation. **After:** stop. |
| 5 | bbinom | BinomialLimit | limit | proved stop | Signed near-face exclusion plus global tangent/chord exclusion prove the binomial face strictly wins. **After:** stop. |
| 6 | bbinom | TwoPointLimit | limit | proved stop | Both endpoints observed and no interior counts, both shapes free. **After:** stop. |
| 7 | bbinom | PointMass | limit | proved stop | All zero/all size with a shape free; or constant positive data with size and one shape free. **After:** stop. |
| 8 | bbinom | Unidentified | tie | proved stop | Size zero; size one with both shapes free; all zeros with size and a shape free. **After:** stop. |
| 9 | bbinom | ProfileTie | tie | unclosed certificate | Finite/face likelihoods or size-profile runners cannot be separated by the practical margin. **After:** Ok, certified=false, best successful finite candidate. |
| 10 | bbinom | FaceUnresolved | unresolved | unclosed certificate | Near-binomial strip cannot close, or held-alpha size profile has no strict negative-binomial face separation. **After:** Ok, certified=false when a finite candidate exists. |
| 11 | bbinom | SizeBudget / stop_detail.budget_exhausted | budget | unclosed certificate | Tail preflight, global exclusion, final-cell evaluation or size coverage exhausts allowance after a successful finite solve. **After:** Ok, certified=false. |
| 12 | bbinom | SizeBudget / IterationFailure | budget / numerical | search failure | Allowance exhausted before a successful finite candidate, nonfinite search/shape bounds, or local root refinement fails. **After:** stop. |
| 13 | bbinom | IterationFailure | numerical | unclosed certificate | Successful local search but coefficient inclusion, curvature region, outside exclusion or certificate arithmetic cannot close. **After:** Ok, certified=false. |
| 14 | hyper | InvalidInput | plain | proved stop | Empty data; nonwhole/nonfinite/out-of-range counts or designs; invalid urn inequalities. **After:** stop. |
| 15 | hyper | Infeasible | nomle | proved stop | No admissible integer tuple gives every observation positive mass (hyper support, or nhyper draws from r through N-m+r). **After:** stop. |
| 16 | hyper | FaceUnresolved / FaceTie | unresolved / tie; fully-free adapter uses plain errors | unclosed certificate | Exploratory finite prefix is not strictly above a binomial/Poisson face (hyper) or shifted negative-binomial face (nhyper); no global escape established. **After:** Ok, certified=false, prefix winner. |
| 17 | hyper | NumericalFailure + budget | budget | unclosed certificate | Finite grid or global growing-urn coverage does not fit the allowance; large-count correction preflight refuses the global certificate. **After:** Ok, certified=false, finite prefix winner. |
| 18 | hyper | NumericalFailure | plain (budget overrides when present) | search failure | No finite representable candidate or nonfinite likelihood before a usable candidate. **After:** stop. |
| 19 | nhyper | InvalidInput | plain | proved stop | Empty data; nonwhole/nonfinite/out-of-range counts or designs; invalid urn inequalities. **After:** stop. |
| 20 | nhyper | Infeasible | nomle | proved stop | No admissible integer tuple gives every observation positive mass (hyper support, or nhyper draws from r through N-m+r). **After:** stop. |
| 21 | nhyper | FaceUnresolved / FaceTie | unresolved / tie | unclosed certificate | Exploratory finite prefix is not strictly above a binomial/Poisson face (hyper) or shifted negative-binomial face (nhyper); no global escape established. **After:** Ok, certified=false, prefix winner. |
| 22 | nhyper | NumericalFailure + budget | budget | unclosed certificate | Finite grid or global growing-urn coverage does not fit the allowance; large-count correction preflight refuses the global certificate. **After:** Ok, certified=false, finite prefix winner. |
| 23 | nhyper | NumericalFailure | plain (budget overrides when present) | search failure | No finite representable candidate or nonfinite likelihood before a usable candidate. **After:** stop. |
| 24 | hyper | AllZero | limit | proved stop | Population free, positive held m and k, all observations zero: population must diverge. **After:** stop. |
| 25 | hyper | TooLarge | plain | search failure | Required one-coordinate maximizer exceeds the exactly represented integer range 2^53-1. **After:** stop. |
| 26 | hyper | BothFree | plain | search failure | Obsolete unsupported-design enum, unreachable through current joint-search dispatch. **After:** unreachable. |
| 27 | tbinom | InvalidInput | plain | proved stop | Invalid size/probability, bounds a>=b, observations outside a<x<=b, or conditioning event with zero probability. **After:** stop. |
| 28 | tbinom | LowerPointMass / UpperPointMass | limit | proved stop | Constant sample at an excluded lower/upper probability endpoint; held interior probability with a growing-size upper-edge escape unless finite singleton support is feasible. **After:** stop. |
| 29 | tbinom | Unidentified | tie | proved stop | Singleton conditioning support in the underlying truncated fitter; the public fixed-size wrapper already supplies a representative. **After:** not normally reachable as a stop. |
| 30 | tbinom | TpoisLimit | limit | proved stop | Signed tail and full prefix, exact rational moment/saturation, or untruncated-binomial reduction proves a strictly winning truncated-Poisson face. **After:** stop. |
| 31 | tbinom | NumericalFailure / provisional TpoisLimit | numerical / unresolved / budget | unclosed certificate | Zero/higher-order moment or prefix comparison, certificate representation or allowance cannot close after a finite profile succeeds. **After:** Ok, certified=false. |
| 32 | tbinom | NumericalFailure | numerical | search failure | No successful finite normalizer/mean solve; no finite candidate. **After:** stop. |
| 33 | tbinom | NumericalFailure (proposal) | numerical | unclosed certificate | Held-probability integer scan or free-size expansion/refinement stops after successful finite evaluations. **After:** Ok, certified=false; global pass may subsequently certify. |
| 34 | ztbinom | InvalidInput / InvalidSize | plain | proved stop | Empty/invalid counts or controls, invalid held probabilities, held size below max(x), or incompatible held point/zero mass. **After:** stop. |
| 35 | ztbinom | PoissonLimit | limit | proved stop | Signed/relaxed face certificate and full finite-prefix exclusion prove the positive Poisson face strictly wins. **After:** stop. |
| 36 | ztbinom | IterationFailure / provisional PoissonLimit | numerical / unresolved / budget | unclosed certificate | Successful finite profile but unclosed sign, prefix, retained-support cutoff, allowance, or coefficient/proposal comparison. **After:** Ok, certified=false. |
| 37 | ztbinom | IterationFailure | numerical (budget if no candidate and exhausted) | search failure | Failed probability root/relative bracket/score gate, nonfinite likelihood, or no successful finite candidate. zmbinom preserves the positive-count failure. **After:** stop. |
| 38 | zib | InvalidInput / InvalidSize | plain | proved stop | Empty/invalid counts or controls, invalid held probabilities, held size below max(x), or incompatible held point/zero mass. **After:** stop. |
| 39 | zib | ZipLimit | limit | proved stop | Signed/relaxed face certificate and full finite-prefix exclusion prove the ZIP face strictly wins. **After:** stop. |
| 40 | zib | IterationFailure / provisional ZipLimit | numerical / unresolved / budget | unclosed certificate | Successful finite profile but unclosed sign, prefix, retained-support cutoff, allowance, or coefficient/proposal comparison. **After:** Ok, certified=false. |
| 41 | zib | IterationFailure | numerical (budget if no candidate and exhausted) | search failure | Failed probability root/relative bracket/score gate, nonfinite likelihood, or no successful finite candidate. zmbinom preserves the positive-count failure. **After:** stop. |
| 42 | zmbinom | InvalidInput / InvalidSize | plain | proved stop | Empty/invalid counts or controls, invalid held probabilities, held size below max(x), or incompatible held point/zero mass. **After:** stop. |
| 43 | zmbinom | PoissonLimit | limit | proved stop | Signed/relaxed face certificate and full finite-prefix exclusion prove the zero-modified Poisson face strictly wins. **After:** stop. |
| 44 | zmbinom | IterationFailure / provisional PoissonLimit | numerical / unresolved / budget | unclosed certificate | Successful finite profile but unclosed sign, prefix, retained-support cutoff, allowance, or coefficient/proposal comparison. **After:** Ok, certified=false. |
| 45 | zmbinom | IterationFailure | numerical (budget if no candidate and exhausted) | search failure | Failed probability root/relative bracket/score gate, nonfinite likelihood, or no successful finite candidate. zmbinom preserves the positive-count failure. **After:** stop. |
| 46 | zib | FaceTie | tie | proved stop | All finite sizes excluded; globally winning ZIP face branches cannot be ordered. Finite nonattainment is already proved. **After:** stop; uncertified finite fallback if finite exclusion was not proved. |
| 47 | zib | IterationFailure (turning discriminant) | numerical | unclosed certificate | Held-inflation turning discriminant falls within the arithmetic margin. **After:** Exact integer recheck when safe; otherwise bracketed stationary search returns Ok, certified=false. |
| 48 | nhyper | NumericalFailure (initial represented r/N bound) | plain | search failure | Held m=2^53-1, N and r free, constant draws c<=m: the old initial-r/population-bound arithmetic missed the feasible tuple r=c, N=m. This was an implementation search failure, not nonexistence. **After:** Ok, certified=true at the exact attained point law; other omitted representability branches remain uncertified when a finite candidate exists. |

Hyper/nhyper `Tie` and `Unidentified` outcomes that already return representatives with warnings are not stops. Zmbinom's `AllZero` and `TwoPoint` outcomes likewise already return with warnings and remain unchanged. The obsolete Hyper `BothFree` enum and underlying truncated-fitter singleton status are included above to distinguish dispatch-unreachable stop messages from live routes. Invalid controls are still checked before search.

**What the additional certification work closes.** These are mathematical arguments with practical floating-point checks where coefficients or comparisons must be evaluated; they are not interval-arithmetic guarantees.

1. **Attained finite saturated laws.** Positive observations restricted to 1 and 2 can be represented at size 2 and attain the empirical multinomial entropy bound. The corresponding zmbinom reduction inherits the result. For tbinom, the smallest feasible size can leave exactly two atoms even when the stated upper truncation bound extends further: `[2000001,2000002]`, `a=2000000`, `b=2000005` is now certified, rather than refused by the old work/normalization path. Positive all-one samples with held tiny probability have the attained size-one law; this closes the ztbinom and zmbinom cases. Constant nhyper data `c<=m`, with `N,r` free, attain probability one at `N=m,r=c`, including `m=2^53-1`.
2. **Turning-discriminant rechecks in zib.** A scaled integer discriminant can be signed exactly when its intermediate integers fit the working mantissa with a two-bit reserve. Otherwise vertex-assisted stationary brackets retain a finite solution uncertified. The primitive table with 17 zeros, 2 ones and 15 twos, size 2 and held inflation `.2`, closes; multiplying its counts by 2,000 exercises the unresolved arithmetic route. A zero turning discriminant is insufficient to enclose a flat probability root: the same primitive table at `pi=54/343` stays uncertified. Near-turning roots need opposite padded scores at radius `.25*min(tol,1)*p` and separation from competing likelihoods.
3. **Positive-Poisson first-gap recheck.** A mean bracket sharpens the positive-Poisson gap comparison. For a nonconstant finite positive-integer sample, an exactly zero first gap would force a nonzero rational Poisson rate: the sample mean and factorial second moment are rational. The conditional-mean equation would then make its negative exponential rational, contradicting Lindemann's theorem. This rules out exact zero in that mathematical setting; it supplies no uniform practical separation and no exclusion of every unseen integer size. Zmbinom inherits the positive-count argument.
4. **Mixed ZIP first coefficient cannot be exactly zero at rational interior inflation.** For `z>0` zeros, `k>0` positive counts, `S=sum(x)`, `T=sum(x(x-1))`, held rational `0<pi<1` and an interior stationary ZIP rate `lambda`, let `w=(1-pi) exp(-lambda)/(pi+(1-pi) exp(-lambda))`. The score gives `S=lambda(k+z*w)`; the first profile coefficient is `(lambda*S-T)/2`. Zero would imply nonzero rational `lambda=T/S`, rational `w`, and rational `exp(-lambda)=pi*w/((1-pi)(1-w))`, again contradicting Lindemann. `T=0` would force the inadmissible zero rate for mixed positive data. The proof applies to actual finite binary inflation inputs; `pi=0` and no-zero reductions are outside its hypotheses. It establishes neither usable separation near zero nor global prefix exclusion. The family dossier records the proof.

The 80-digit [rechecks](2026-11-10-fallback-count-data/rechecks.json) show why the adjacent ZIP inputs still need fallback. For five zeros and positive counts `[1,1,2,2,4]`, held inflation `.3772146069463776` gives a first coefficient about `-2.3683300855e-16`; the adjacent `.37721460694637765` gives about `+1.3626331389e-16`. These are numerical local signs, not a proof ordering the complete finite-size profile against the face. Finite-window higher-order tables `[50,41,33,3,3]`, `[104,0,0,608,112]` and `[1,2,2]` were also rechecked numerically at sizes 5, 10, 100 and 1,000; existing proved higher-order stops remain intact. None of those finite grids is presented as a global proof. All seven dossier front-matter values remain `certified: partial`.

**Seeded before/after study.** [Inputs](2026-11-10-fallback-count-data/cases.json), [baseline outcomes](2026-11-10-fallback-count-data/before-final.tsv), [new outcomes](2026-11-10-fallback-count-data/after-final.tsv), [summary](2026-11-10-fallback-count-data/summary.json) and [provenance](2026-11-10-fallback-count-data/provenance.json) are retained. The study includes all 925 cases for these families from the frozen audit inventory reused by the November 9 rerun, plus 120 pseudorandom cases and 28 directed route cases, seed 1681. The original 1,069-case prefix was frozen before the four added precision examples; [prefix verification](2026-11-10-fallback-count-data/inventory-extension-check.json) confirms that its inputs and both outcome prefixes are unchanged. The frozen baseline headers are included for reproduction.

The baseline had no per-fit flag for these families. Its TSV label `certified` therefore means **unflagged successful fit**, not a retrospective certificate. Eight baseline cases hit an external 60-second per-case guard; these are censored observations, not library budget failures or nonexistence claims.

| Outcome | Before | After |
|---|---:|---:|
| Successful, no per-fit flag | 909 | — |
| Certified | — | 911 |
| Uncertified | — | 88 |
| Stop: limit | 34 | 34 |
| Stop: nomle | 22 | 22 |
| Stop: budget | 33 | 2 |
| Stop: unresolved | 19 | 0 |
| Stop: numerical | 15 | 0 |
| Stop: untyped input-or-search | 33 | 16 |
| Censored by external wall guard | 8 | 0 |
| Total | 1,073 | 1,073 |

Of 156 baseline library stops, 82 become successful: 5 certified and 77 uncertified. The other 74 retain their stop outcomes. All eight censored baseline cases now return uncertified. Three former unflagged successes become explicitly uncertified because of their sub-machine coefficient target. The five newly certified former stops are the nhyper representation-boundary constant law, large-count two-atom tbinom, small exactly rechecked zib turning table, size-one ztbinom and corresponding zmbinom case.

| Family | Cases | Before success / library stop / censored | After certified / uncertified | Remaining stops |
|---|---:|---|---|---|
| bbinom | 166 | 132 / 34 / 0 | 132 / 22 | limit 10, budget 2 |
| hyper | 143 | 98 / 45 / 0 | 98 / 35 | nomle 10 |
| nhyper | 139 | 119 / 20 / 0 | 120 / 7 | nomle 12 |
| tbinom | 255 | 219 / 33 / 3 | 220 / 5 | limit 14, input-or-search 16 |
| zib | 148 | 140 / 6 / 2 | 139 / 9 | none |
| zmbinom | 143 | 130 / 11 / 2 | 131 / 6 | limit 6 |
| ztbinom | 79 | 71 / 7 / 1 | 71 / 4 | limit 4 |

Directed additions deliberately hit certificate routes ordinary random samples often miss:

| Route label | Certified | Uncertified | Stops |
|---|---:|---:|---:|
| route-regression | 0 | 9 | 1 |
| held-probability-budget | 0 | 3 | 0 |
| certificate-window | 0 | 1 | 0 |
| coefficient-inclusion | 0 | 3 | 0 |
| zero-turning-discriminant | 1 | 0 | 0 |
| unclosed-turning-discriminant | 0 | 1 | 0 |
| representable-stopping-count | 1 | 0 | 0 |
| attained-two-atom-large-count | 1 | 0 | 0 |
| attained-one-positive-atom | 2 | 0 | 0 |
| flat-probability-root-inclusion | 0 | 1 | 0 |
| sub-machine-coefficient-target | 0 | 4 | 0 |

The full route breakdown, including the twelve random cohorts, is in the summary. There are zero finite-success/held-value contract violations. [Estimate preservation](2026-11-10-fallback-count-data/estimate-preservation.json) checks all 909 previously successful coefficient vectors: parsed double values are identical, with zero coefficient drift. There are 101 small log-likelihood rounding changes, predominantly from the tbinom normalization arithmetic; maximum `abs(new-old)/(1+abs(old))` is `1.9139447479709795e-13`. Thus the evidence supports unchanged coefficients and negligible recorded likelihood rounding, not bitwise identity of every output. No golden sample or reference was regenerated.

The study's `run-study.sh` builds baseline and current runners, replays cases, checks preservation and reruns high-precision comparisons. Scratch resume rows are invalidated when the binary, case file or runner identity changes. Run it through a local single-slot wrapper with the worktree temporary-directory variables set; no dependency installation is needed. The source/evidence SHA-256 inventories distinguish the initial frozen study from the final precision refinements.

**Existing test expectations converted.** These are the old refusal assertions changed under the explicit fallback rule. The cases themselves remain. Proved-stop assertions, unfinished-optimizer assertions and unsupported-subset assertions remain. C++ additions were appended at the ends of family test files.

| Family / tests | Sample and controls | Former refusal and reason for conversion |
|---|---|---|
| bbinom C++ and R | Free size, held `beta=1e9`, `[0,1,2,4]`; R guard also `[0,1,2,4,8]` | Tail preflight/certificate budget does not negate a successful finite shape solve: now uncertified. |
| bbinom C++ and R | Free size, `alpha=1`, `[0,1001]` and `[0,0,0,10]` (including structured duplicate C++ assertion) | Size allowance or unresolved negative-binomial face: successful finite prefix now uncertified. |
| bbinom C++ | `[0,1,1,2,2,3,4,5,7,10]`, size 10, allowance one cell below reference usage | Certificate final-cell exhaustion after finite convergence: now uncertified. |
| bbinom C++, R and JS `stop-test.mjs` | Same size-10 sample, `1e-18` tolerance, both shapes free, or `alpha=2`, or `beta=3` | Coefficient inclusion cannot close after a successful solve: now finite and uncertified. |
| hyper C++ and R | Fully free `[0,1]`; `[0,0,0,5,5,5]` fully free and held `k=5` | Unseparated binomial/Poisson faces are not proved global escapes: finite prefix now uncertified. |
| hyper C++ and R | Fully free `[8,8,9,9]`; fully free `[0,100001]` (including C++ empty-witness check); `[0,1]` with `k=100001`; `[0,1]` with `N=10000`, `k,m` free | Certificate allowance, large-count preflight or grid coverage: now finite prefix and uncertified. |
| nhyper C++ and R | `[1,1,1,1,1,30]`, held `r=1`; C++ also `r` free | Unclosed shifted-negative-binomial face comparison: now uncertified. |
| nhyper C++ and R | `[1,100001]`, `r=1` including C++ payload check; `[1,100]`, `m=1e7,r=1`; R `[1,2]`, `N=1e7,r` free | Old preflight/grid budget refusal: now successful prefix and uncertified. |
| tbinom C++ and R | `[2000001,2000002]`, `a=2000000,b=2000005`, free size | Old work/numerical refusal replaced by stable profile and attained two-atom certificate: now certified. |
| zib C++, R and JS `test.mjs` higher-order mixed test | Five zeros plus `[1,1,2,2,4]`, held `pi=.75*exp(-1.6)/(.25+.75*exp(-1.6))`, free size | Ambiguous local face coefficient: now finite and uncertified. Only the zib branch changed; the tbinom proved-limit branch remains. |
| zib C++, R and JS `test.mjs` adjacent-critical test | Same data, held `pi=.3772146069463776` and `.37721460694637765` | Near-zero comparisons do not prove nonattainment: both now uncertified. |

New C++ and R coverage includes the held-probability prefix allowance in ztbinom, zib and zmbinom (`p=1e-9`, iteration allowance 1, positives `[1,2,3]` with a zero for zero-altered families); saturated size-two and size-one laws; the nhyper representation boundary; the oversized tbinom certificate window (`[1,1,1,1,1,12]`, `a=0,b=1e100`); large held-probability counts; zib ambiguous turning and flat-root cases; and the sub-machine coefficient examples. The latter cover ztbinom `[1,2]` free size, zib `[1,1,2]` with size 2/inflation 0 and `[0,1,1]` with size 1/probability 1, and zmbinom `[0,1,1]` with held tiny probability. The new job-level JS flag test checks hyper's uncertified 0 and fixed-size ztbinom's certified 1. The R full tier executes the newly appended flag cases. Existing tests outside these owned refusal conversions were not rewritten to accept new stops.

**Independent likelihood checks and limits of the evidence.** [Point checks](2026-11-10-fallback-count-data/point-checks.json) evaluate 15 returned points at 80 decimal digits using the reported binary-double coefficients; none was skipped. The new tbinom path's largest scaled likelihood discrepancy is `2.860167060026931e-12`. These point evaluations check likelihood arithmetic, not global maximality. Three inherited zero-truncated binomial log-PMF discrepancies exceed `1e-10`; all affected fits already carry `certified=false`:

| Case ID | Family | Scaled likelihood discrepancy |
|---|---|---:|
| 629 | zmbinom | `4.429008592523626e-8` |
| 681 | zmbinom | `4.00776747680479e-8` |
| 733 | ztbinom | `4.429008592523626e-8` |

All use `[1000000,1000001,1000003,1000008,1000011]`. Case 629 reports log likelihood `-14.363697396130272` at size `1000023`, probability `.99998160042319029`, zero mass 0; the independent value is approximately `-14.36369671567082418`. The source is inherited factorial-log cancellation in `zero_detail::bin_logpmf`, in unowned `ztpois/profile.hpp`; it was not modified. This limitation is recorded rather than hidden by relaxing the independent check. Regression tests and matching baseline coefficients alone do not establish coefficient accuracy or global optimality for uncertified results.

**Finished validation.** All commands ran in the foreground, through the prescribed slot wrappers. All started commands have terminated. Every final numerical run below used the workstation (GCC 15.2.0; R 4.6.1); the successful local runs are the prescribed fallback for simbox exit 75. Slot wrappers enforce the compute allocation. No test process remains running.

| Command / check | Result | Retained log |
|---|---|---|
| `run4 just cpp-test` | 106/106 dev shards | [C++ dev](2026-11-10-fallback-count-data/logs/cpp-dev.log.gz) |
| `run4 just cpp-test-no-long-double` | 130/130 shards | [No long double](2026-11-10-fallback-count-data/logs/cpp-no-long-double.log.gz) |
| Final affected native and no-long-double shards | 5/5 each after final zib/ztbinom/zmbinom guards | [Affected shards and study](2026-11-10-fallback-count-data/logs/affected-and-study.log.gz) |
| Focused seven-family C++ checks | 188 cases, 224,826 assertions; zero failures, two filtered cases | [Same final log](2026-11-10-fallback-count-data/logs/affected-and-study.log.gz) |
| `run just js-build` | Green | [JS build](2026-11-10-fallback-count-data/logs/js-build.log.gz) |
| `run just js-test` | 596/596 | [JS tests](2026-11-10-fallback-count-data/logs/js-test.log.gz) |
| `run just site-check` after final dossier proof | 99 C++17 snippets compile/link; HTML checks green | [Site check](2026-11-10-fallback-count-data/logs/site-check.log.gz) |
| `run4 just test` | 23,026 passes; 0 failures, 0 warnings; 154 tier skips | [R dev](2026-11-10-fallback-count-data/logs/r-dev.log.gz) |
| Final affected R dev refresh | 384 passes; 0 failures, 0 warnings; 9 full-tier skips | [Affected R dev](2026-11-10-fallback-count-data/logs/r-dev-affected.log.gz) |
| `run4 just test-full` on current complete source | 45,849 passes; 0 failures, 0 warnings, 0 skips | [R full](2026-11-10-fallback-count-data/logs/r-full.log.gz) |
| `git diff --check` | Green; only the 39 declared source/test/dossier files plus this report/evidence changed | — |
| Markdown links | Owned report and seven dossiers: zero broken links. Repository-wide scan: 44 pre-existing issues in unchanged files | [Link audit](2026-11-10-fallback-count-data/markdown-links.json), [global output](2026-11-10-fallback-count-data/logs/markdown-links.log.gz) |
| Final 1,073-case replay and preservation | Zero finite/held contract violations; all 909 prior coefficient vectors identical | [Summary](2026-11-10-fallback-count-data/summary.json), [preservation](2026-11-10-fallback-count-data/estimate-preservation.json) |

The broad C++ dev/no-long-double runs and broad R dev run preceded the last three-family precision guards; the final affected native/no-long-double/R-dev checks cover those refinements, and the complete final R full suite, JS build and JS tests use them. The isolated affected R checker copies generated package sources inside this worktree, overlays the current three headers/tests and runs one worker without changing a concurrently tested package; its source hashes and per-test rows are [recorded](2026-11-10-fallback-count-data/r-flags-dev-validation.json). Static site inspection also confirms the final ZIP proof is present, with 111 MathML elements and no MathJax script ([HTML check](2026-11-10-fallback-count-data/zip-proof-html-check.json)). Machine-readable [validation results](2026-11-10-fallback-count-data/validation-results.json) and deterministic compressed logs are preserved.

Simbox's no-long-double command was attempted five times. The first four attempts failed filesystem readiness; the fifth reported the host unreachable. Each exited 75 before the numerical suite. [Attempt logs](2026-11-10-fallback-count-data/logs/simbox-attempts.log.gz) document the failures. No GCC 13 result is claimed.

**Open entries after implementation and testing.**

- **Global certificates:** practical-margin face ordering, unseen integer-prefix exclusion, omitted represented branches and some probability-root/coefficient enclosures remain unclosed. The new exact-zero exclusions and discriminant checks close specific mathematics, not all supported subsets. Completing those certificates requires family-specific global bounds or stronger comparison/enclosure methods; current successful searches return uncertified.
- **Existing bbinom unrestricted subset:** free size with both shapes free remains the declared structural not-implemented case. Fixed-size shape profiles can themselves escape, so trying a generic integer prefix does not provide a generally successful finite joint-shape search. A supported unrestricted fitter needs a joint search and its binomial/two-point/size-face treatment. Existing unsupported tests and structural `holds` exception remain; this is not a proved nonexistence claim and was not introduced by the fallback change.
- **Inherited likelihood arithmetic:** cases 629, 681 and 733 need stable large-count `bin_logpmf` arithmetic in shared `ztpois/profile.hpp` and corresponding d/p/q/r truth coverage. That shared family and settled d/p/q/r seam are outside this job's permitted edits. The affected fits are explicitly uncertified, and their independent discrepancies remain in the evidence.
- **Baseline censoring:** IDs 317, 345, 358 (tbinom), 551, 603 (zib), 655, 707 (zmbinom) and 757 (ztbinom) were censored at 60 seconds on the baseline. Their true terminal library outcomes would require longer or instrumented baseline runs. They all use the million-count stress sample above; no terminal error kind is inferred from the external guard.
- **Second compiler:** GCC 13 no-long-double validation on simbox is missing because all five readiness attempts failed. The full workstation no-long-double suite and final affected refresh passed. Rerun the same remote command when simbox is usable; no machine repair or out-of-worktree mutation was attempted.

- **Historical Markdown links:** the repository-wide checker reports 44 missing targets or signature-shaped false positives across six historical files. All six are byte-identical to `HEAD`; the report and seven owned dossiers have no broken destinations. Fixing historical reports, the nbinom dossier or inference-note routes is outside this job.

- **Artifact-pane delivery:** the required `bin/show` call returned HTTP 403: “That path is outside every readable root.” The viewer lists the main checkout but not this worktree. The report remains at its requested path; viewer roots were not changed and no copy was written outside the worktree. This is a display configuration issue, not a report/build failure.
