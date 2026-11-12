# TASK-150.2.5.2: continuous uncertified-fit accuracy

Baseline: `bc8ff7be2e3bc4b9186a56fc4f2e8ee45c163714`. Worktree: `acc-fb-cont`, branch `codex/acc-fb-cont`. The requested November 11 report filename is retained; the job continued into November 12 in Europe/Oslo.

Every returned fit in the numerical audit meets the requested coefficient bound: 391 relevant returned fits in each arithmetic mode, with maximum scaled errors **7.760132369047211e-12 native** and **7.307141975597569e-12 double-only**. R and WASM each returned 79 checked fits, with maximum error **2.6717662099863066e-11**. All 995 native and 994 double-only baseline-certified rows retain text-identical recorded coefficients, likelihoods and certification flags.

**Review round completed: all requested checks are green.** The nine regular/slow C++ shards for Burr, inverse Burr, Pareto IV, Bhatt and GED pass in each arithmetic mode; the five R files pass at full tier; the complete JavaScript suite passes 615/615. The five expectation conflicts were resolved under Jonas's existing accuracy/fallback rules and the reviewer's explicit authorization. Original samples and fit arguments are unchanged.

GED already fixes SGED unit-skew case 31101 in this tree. New GED-owned C++ and R regressions verify the parent and delegated child; independent 100-digit comparison in native, double-only, R and WASM has maximum scaled error **9.429118e-14**. No production fitter change was needed in this review.

The implementation changes only owned family fitters/helpers, family-specific dossier prose, appended C++ regressions, the explicitly authorized C++/R/JS expectation replacements, and this evaluation bundle. Genbeta and skew-normal already have adequate local finishing steps and were audited without changes. Controls, dossier front matter, goldens, d/p/q/r, special functions, core/common/production binding sources, NEWS, status files and roadmap files were preserved. No commits, board edits, sub-agents, dependency installations, pushes or publication were performed.

## Return-route inventory

The inventory follows both flag assignments and delegates; the retained scans are `flag-inventory.txt` and `flag-inventory-final.txt`. “No guarantee” describes the old stopping rule, even where the sampled old point happened to be accurate. An inconclusive global certificate still retains a successful search after local accuracy is established.

| Route | Why certification is withheld | Stopping rule before this job | Contract assessment and final return step |
|---|---|---|---|
| Burr, joint | Joint cover, tail comparison, arithmetic or certificate allowance unclosed | Profile/BFGS nomination and search convergence | No guarantee; analytic local Newton finish, compensated shape/rate information near the Weibull seam, all three native coefficients checked |
| Burr, held scale | Conditional enclosure/sign/allowance unclosed | Historical moment/Newton proposal and score/iteration stopping | No guarantee; refine the conditional shape coordinates and bound their native errors |
| Burr, held power | Lomax face/cover unclosed while the existing scalar search succeeds | Total transformed score norm below `1e-3` | No guarantee; signed local profile bracket, derivative bound, compensated raw powers, and coefficient images for both shape and scale |
| Inverse Burr, joint | Reciprocal Burr fallback | Parent stopping rule plus rounded reciprocal data/scale | No native-data guarantee; accurate parent followed by an inverse raw-likelihood correction and reciprocal-scale error propagation |
| Inverse Burr, held scale | Conditional reciprocal Burr fallback | Parent conditional proposal | No native-data guarantee; held scale stays literal; refine the free shape coordinates on original observations |
| Inverse Burr, held power | Reciprocal Lomax/Burr fallback | Parent `1e-3` score gate | No guarantee; inverse-orientation profile/bracket and native scale sensitivity |
| Inverse Burr, held first shape | Parent or transformation comparison remains unresolved | Conditional parent result with certification withdrawn after transport | No transport error bound; check/refine original inverse likelihood with the held coefficient removed from the solve |
| Pareto IV, joint / held scale / held power | Shifted Burr fallback | Parent stopping rule after rounded subtraction | No native-data guarantee; accurate Burr parent plus compensated excesses and correction of every free coefficient |
| Paralogistic, joint | Manifold certificate/cover unclosed | Existing joint/profile Newton search | No parameter-error gate; centred raw-log shape/rate Newton finish and native projection |
| Paralogistic, held scale | Conditional certificate unclosed | Existing scalar search proposal | Sampled roots were accurate, but the fallback had no independent error gate; add observed-information finishing |
| Inverse paralogistic, joint / held scale | Parent fallback, or inverse transport remains unresolved | Parent proposal and reciprocal conversion | No native-data guarantee; verify the original inverse likelihood and reciprocal scale, retaining holds |
| Beta-prime, joint | Joint exterior or Gamma/inverse-Gamma comparison unclosed | Multistart/profile search and `1e-3` score gate | No guarantee; analytic final solve, compensated beta/Gamma scores when scale is free |
| Beta-prime, conditional shape, scale free | Conditional face/sign/allowance unclosed | Shape/scale search score gate | No guarantee; compensated free-coordinate information and shape/scale Jacobian; covers either orientation |
| Beta-prime, unit shape | Lomax/inverse-Lomax reduction cannot close its certificate | Parent scalar nomination | No guarantee; Burr unit-power local finish; reciprocal direction checked on raw observations |
| Beta-prime, scale held | Failed conditional search/certificate followed by accepted fallback nomination | Fixed-scale BFGS score gate | No guarantee; analytic log-shape Newton finish; held scale stays exact. Three forced return-boundary masks were checked separately |
| GB2, third shape one | Burr delegate is uncertified | Parent result | Inherits the corrected Burr finish; remaining direct coefficient transport checked |
| GB2, first shape one | Inverse Burr delegate is uncertified | Parent result | Inherits inverse Burr accuracy and raw-data transport checks |
| GB2, known power / small power / reverse orientation | Beta-prime delegate is uncertified | Rounded powers, parent proposal and scale back-transform | Parent accuracy alone is insufficient; raw-observation beta-prime finish with native `1/a` scale sensitivity and orientation change |
| Feller Pareto, Burr / inverse Burr / known-power beta-prime reductions | GB2 parent is uncertified | Parent stopping rule after a shifted-data transform | Parent finish plus compensated raw shifted likelihood; the same small-power and inverse sensitivities enter the final error map |
| Pareto III, parent transport | Log-logistic parent is uncertified | A failed outer polish could return the unchecked parent | No guarantee; remove the bypass, refine the raw excess likelihood, check scale projection and recompute the physical likelihood |
| GED, smooth free-shape subsets | Certificate comparison/enclosure unclosed | Existing numerical search point | No guarantee; analytic location/scale/shape information, native location sensitivity and final projection |
| GED, held shape | Conditional certificate unclosed | Smooth search, or exact normal/knot special case | Exact special cases already meet the rule; other fallback coordinates receive the analytic finish with the held shape literal |
| GED, subunit knot / Laplace restriction | Enumeration/boundary comparison unclosed | Exact knot or median and conditional scale; boundary shape could need a sign check | Preserve the exact location; refine remaining smooth coordinates and verify the free-shape one-sided condition at the restriction |
| Bhatt, finite width | Compact enclosure or normal/uniform comparison unclosed | Existing latent-moment/profile/simplex search and diameter/score convergence | No guarantee; analytic convolution derivatives, narrow-component/tail expansions, total-variance coordinates and compensated weak-width information |
| Bhatt, normal member | Existing search supplies `a=0` while global comparison is unresolved | Closed Gaussian coordinates without a sufficient one-sided width check | Gaussian coordinates are closed form; a free width additionally needs the quadratic or profiled quartic sign. Refuted normal members are refined to finite local maxima |
| Bhatt, held-scale/width extreme location | Same global fallback with tiny or large latent width | Existing conditional location proposal | Use symmetry, conditional-mean or Gaussian-tail enclosures when they directly bound native coefficient error; otherwise use analytic information |
| Genbeta, free-power conditional/all-free fallback | `allfree_certified=false`; global all-free cover is unclosed | Existing product-coordinate analytic Newton finish | Already conforming; retained. The flag applies to all-free globality, not removal of local accuracy |
| Genbeta, held power or scalar reductions | The all-free flag is not an applicable certificate for these subsets | Existing beta reduction, signed scalar coefficient brackets or closed form | Already conforming; retained and checked in the census |
| Skew-normal | No public `certified=false` field; the requested finite route is unflagged | Existing public analytic-information polish and native projection | Already conforming; retained. Its internal scalar `certified=false` branch returns failure, not an unchecked `Ok` |

The delegated paths were checked through their original-data likelihoods, not solely by comparing rounded parent coefficients. There are no free integer coefficients in these thirteen families.

## Local accuracy and weak-limit formulations

The smooth finish follows the supplied `likelihood_polish` convention: positive observed information and small **undamped** Newton corrections, with two small corrections or observed quadratic contraction. The coefficient gate has a factor-16 step margin. Damping and likelihood improvement propose steps; they do not establish acceptance. The compensated solver additionally propagates componentwise score arithmetic/truncation allowances through inverse information and the native coefficient Jacobian. Modified information pivots can propose a step but cannot accept it. Solver loops use the existing `solver_budget(iterlim)` convention; finite polynomial evaluations carry explicit remainder bounds.

These are local claims, conditional on the computed comparisons and the stated practical floating-point margins. They do not establish global optimality or outward-rounded interval certificates.

### Burr and its transports

For known power `c`, normalize raw observations to `Y=(x/e)^c` and define

\[
G(r)=E\log(1+rY),\qquad H(r)=E\frac{rY}{1+rY}.
\]

The profiled equation is `(H-G+G*H)/r^2=0`; its coefficient images are `k=1/G(r)` and `s=e*r^(-1/c)`. The local signed bracket and derivative bound control both coefficients, including `d log(s)=-d log(r)/c`. Moment evaluation retains the small departure from the exponential face. Compensated raw powers and subtraction residuals prevent rounded log data from moving the root outside the target. The inverse orientation uses `Y=(e/x)^c` and the reversed scale map.

With both shapes free near the Weibull face, use `(u,v,D)=(log(k),log(c),u-c*log(s/unit))`. The native scale differential is

\[
d\log s=\{du-dD-(u-D)\,dv\}/c.
\]

The weak shape score and observed information are evaluated with two-word binary64 arithmetic. A held shape removes its direction; a held scale ties `D=u` and combines the corresponding score/information directions before solving. The coupled, reciprocal and shifted checks retain the unrounded latent shape in the scale map.

### Beta-prime, GB2 and Feller Pareto

Scale-free fallback points use compensated beta/Gamma information, including two comparable large shapes. Stable digamma/trigamma increments and a Gamma-relative coordinate separate the finite rate from the large shape. In a known-power delegate, native log scale has sensitivity `1/a`; the raw-data finish propagates that sensitivity rather than trusting rounded powered observations. Parent bits can be retained only when the correction lies safely inside the remaining error allowance.

The Gamma-seam checks extend to shape about `7.16e7`. Score remainder bounds are dependency-specific: a held small shape does not contribute its digamma remainder to the large-shape or elementary rate score. This distinction is needed to recover the deepest two finite cases without an artificial numerical stop. Search nomination bounds remain search bounds; the final likelihood is evaluated uncapped at the refined point.

With scale held, three separately seeded fallback masks (both shapes free, first shape held, second shape held) reduce loose-search errors from at most `7.3182e-6` to at most `3.2913e-12` in both arithmetic modes. These force the private uncertified return boundary; ordinary public fixed-scale calls can instead certify their beta reduction.

### Bhatt normal-width seams

With total variance `S^2=sigma^2+a^2/3` and `eta=a/S`, the matching-Gaussian density ratio has Hermite coefficients generated by `exp(-t^2/6)*sinh(t)/t`. The quadratic term vanishes. Its first likelihood terms are

\[
-\frac{\sum H_4(z_i)}{180}\eta^4+
 \frac{\sum H_6(z_i)}{2835}\eta^6.
\]

When both sums are negative, `eta^2=10.5*sum(H4)/sum(H6)` supplies a small-width start. The width information is sixth order, so a fixed cubic coordinate scale conditions the solve. A positive finite search nomination remains the starting point; moment expressions supply conditioning and starts for a normal-member nomination, not a replacement global search.

With Gaussian scale held, the unprofiled ratio uses coefficients `1/(2j+1)!`. Write

\[
H_2=\sum H_2(z_i),\qquad
Q=\sum\{H_4(z_i)/120-H_2(z_i)^2/72\}.
\]

For `H2>0, Q<0`, the local start is `(a/sigma)^2=-H2/(12Q)`; the coordinate scale is quadratic. Held sigma remains literal. Raw centring, Hermite recurrences and the information calculation are compensated; a Cauchy bound supplies the discarded-series allowance and its derivative margins. Every accepted correction is mapped back to location, Gaussian scale and width.

The fourteen new 180-digit checks include symmetric and asymmetric samples, held location, and held sigma. Their final maximum scaled errors are below `8.3e-17` in both builds. They recover finite widths down to about `2.44e-7`. The previous analytic finish returned `Ok` on the deepest held-sigma double-only case with width error **1.10825e-10**, exceeding the `1e-10` bound; the final error is **1.79e-23**. Its independent reference differentiates raw Gaussian CDF endpoint probabilities, not the production Hermite expansion. Positive information is checked at the reference; symmetry fixes location exactly where used.

Normal members require a one-sided width test. At held sigma, the quadratic moment sign uses exact binary64-product sums; 365 independent rational comparisons agree in native, double-only and WASM. With scale free, a separated positive fourth Hermite sum proves a strict local normal maximum. A negative sum refutes that nomination and triggers finite refinement. Conditional location enclosures and exact symmetry remain useful where a displacement is below representable endpoint resolution.

## Replay and independent-reference results

Inputs combine the uncertified rows from the specified fallback-cont `after.tsv` and `delegate-after.tsv`, the Genbeta and Bhatt censuses, the unflagged skew-normal case, seeded route checks and seventeen default-control checks. The complete matrix has **1,395 rows per build**; the strict seeded/default replay has **91 rows**. References use the exact retained binary64 observations, mpmath working precision of at least 80 digits (up to 700 where offsets require it), and independent raw-density scores. The new Bhatt references use 180 digits; the supplemental fixed-scale beta-prime references use 100.

For positive coefficients the comparisons use relative error. Zero-capable coefficients use the contract floor: fitted scale for location and 1 for Bhatt width or skew-normal shape. Every free public coefficient is compared; held values must remain equal to their supplied binary64 values. The reference identifies a local solution; these comparisons do not supply a global certificate.

The table reports maxima over comparable finite points. Four rejected nominations have no reference inside their requested coefficient box; their refutations are described below. `N` includes those nominations. Detailed per-coefficient errors and identities are in `coefficient-errors.json`, `route-errors.json`, `certified-current-identity.json` and `binding-errors.json`.

| Route | N | Before native | After native | Before double-only | After double-only |

|---|---:|---:|---:|---:|---:|

| betapr / conditional shape | 13 | 9.891e-01 | 1.060e-12 | 9.891e-01 | 1.065e-12 |

| betapr / joint | 15 | 7.483e-07 | 1.811e-12 | 2.352e-07 | 1.595e-13 |

| betapr / unit shape | 5 | 2.403e-03 | 1.432e-12 | 1.575e-03 | 7.260e-17 |

| bhatt / finite width | 79 | 1.447e-01 | 1.715e-12 | 6.308e-01 | 5.067e-13 |

| bhatt / normal member | 72 | 3.081e-01 | 9.674e-17 | 3.081e-01 | 7.819e-16 |

| burr / held power | 10 | 6.738e-08 | 1.763e-12 | 4.863e-08 | 7.207e-17 |

| burr / held scale | 5 | 3.665e-07 | 4.792e-16 | 3.665e-07 | 4.792e-16 |

| burr / joint | 2 | 1.256e-07 | 2.204e-15 | 1.256e-07 | 2.038e-15 |

| fpareto / Burr delegate | 3 | 1.256e-07 | 1.433e-12 | 1.256e-07 | 2.038e-15 |

| fpareto / beta-prime power delegate | 4 | 3.522e-07 | 3.933e-13 | 3.522e-07 | 3.928e-13 |

| fpareto / beta-prime reverse-power delegate | 1 | 2.534e-10 | 8.398e-17 | 2.534e-10 | 8.398e-17 |

| fpareto / beta-prime small-power delegate | 5 | 5.140e-01 | 7.731e-13 | 5.140e-01 | 6.603e-14 |

| fpareto / inverse Burr delegate | 1 | 3.212e-08 | 5.659e-17 | 3.212e-08 | 3.893e-16 |

| gb2 / Burr delegate | 3 | 1.256e-07 | 1.433e-12 | 1.256e-07 | 2.038e-15 |

| gb2 / beta-prime power delegate | 1 | 1.161e-08 | 2.352e-16 | 1.238e-08 | 1.242e-16 |

| gb2 / beta-prime reverse-power delegate | 1 | 2.534e-10 | 8.398e-17 | 2.534e-10 | 8.398e-17 |

| gb2 / beta-prime small-power delegate | 3 | 5.140e-01 | 3.046e-14 | 5.140e-01 | 5.512e-13 |

| gb2 / inverse Burr delegate | 1 | 3.212e-08 | 5.659e-17 | 3.212e-08 | 3.893e-16 |

| ged / free shape | 13 | 9.863e-07 | 4.343e-12 | 9.863e-07 | 4.008e-12 |

| ged / held shape | 2 | 1.357e-16 | 1.152e-16 | 1.357e-16 | 1.152e-16 |

| ged / knot/Laplace | 2 | 5.376e-16 | 1.504e-16 | 5.376e-16 | 2.795e-16 |

| genbeta / existing local accuracy | 90 | 5.388e-12 | 5.388e-12 | 5.937e-12 | 5.937e-12 |

| invburr / held first shape | 1 | 6.796e-17 | 2.212e-15 | 6.796e-17 | 2.212e-15 |

| invburr / held power | 6 | 8.741e-09 | 7.055e-13 | 1.001e-08 | 2.932e-12 |

| invburr / held scale | 2 | 1.802e-14 | 3.586e-17 | 1.802e-14 | 3.586e-17 |

| invburr / joint | 3 | 8.822e-01 | 9.212e-17 | 8.822e-01 | 9.212e-17 |

| invparalogis / held scale | 1 | 7.524e-17 | 7.867e-17 | 7.524e-17 | 7.867e-17 |

| invparalogis / joint | 13 | 1.000e+00 | 7.760e-12 | 1.000e+00 | 7.307e-12 |

| paralogis / held scale | 2 | 7.524e-17 | 8.717e-17 | 7.524e-17 | 2.411e-16 |

| paralogis / joint | 12 | 1.000e+00 | 2.033e-13 | 1.000e+00 | 8.566e-13 |

| pareto3 / parent transport | 6 | 1.140e-04 | 3.374e-15 | 1.251e-01 | 3.374e-15 |

| pareto4 / held power | 7 | 6.738e-08 | 8.816e-13 | 4.863e-08 | 9.713e-13 |

| pareto4 / held scale | 5 | 5.547e-13 | 7.724e-17 | 5.547e-13 | 7.724e-17 |

| pareto4 / joint | 5 | 3.830e-03 | 1.772e-16 | 3.830e-03 | 4.064e-14 |

| sn / existing local accuracy | 1 | 6.892e-17 | 6.892e-17 | 6.892e-17 | 6.892e-17 |

| beta-prime / held scale, forced private nominations | 3 extra | 7.318e-06 | 3.291e-12 | 7.318e-06 | 3.291e-12 |

The largest complete-matrix error in each mode is `delegate-audit-3926` (inverse paralogistic). There are no missing references for returned relevant fits. Each binding check contains 84 rows: 79 returned fits and five explicitly verified baseline search stops; the stops are not counted as accurate fits.

The weak-limit supplemental probes also cover arbitrary held Burr powers (including a non-dyadic power), coupled free-shape Burr points near shape `1e8`, held first shapes near `1e300`, held scales near `1e100`, beta-prime shapes near `2.2e5` in both shape coordinates, and known-power transports with power `1e-4`. Those comparisons use independent 100–120 digit stationary points and retain the original observations through shifts and reciprocals.

## Refuted nominations and precision failure

The complete matrix has exactly four newly rejected prior-`Ok` nominations per arithmetic mode:

| Input ID | Evidence | Outcome |
|---|---|---|
| `route-burr-nearly-constant` | A signed profiled shape bound excludes every stationary point in the requested coefficient box | Numerical failure with the old nomination retained as a witness |
| `route-invburr-nearly-constant` | The same bound on the original inverse likelihood | Numerical failure |
| `route-pareto4-nearly-constant` | The same bound on shifted excesses | Numerical failure |
| `route-betapr-unrepresentable-face-1000` | Exact normalized moment coefficient `F0=0`, positive next coefficient `F1=2/81`, and a positive local derivative bound exclude the old finite small-rate root | Numerical failure; no unproved named limit selected |

For the Burr bound, after profiling scale let `v_i=n*x_i^c/sum(x_j^c)` and `A(c)=sum(v_i^2)/2-n`. For `k>=100*n`,

\[
\left|k\frac{\partial L}{\partial\log k}+A(c)\right|\le64n^3/k.
\]

Across the requested relative power box, an upper bound on `A` follows from the weight factor `exp(E)`, with `E=c*log(xmax/xmin)*tol/(1-tol)`. The upper bound remains strictly negative after the analytic tail and practical floating-point margin. The near-constant cases have `A` around `-0.03056`, far from the margin. This proves local refutation of the proposals. Independent 420-digit Weibull closure comparisons provide improving witnesses (gaps about `4.62e-13` for Burr/Pareto IV and `3.44e-14` for inverse Burr). The profile grid is numerical exploration. Neither the grid nor these witnesses proves global Weibull domination; a named limit would require the missing global cover.

The forced zero-certificate-budget Burr test has another refuted nomination: its returned scale is below every observation throughout the accuracy box. The joint shape equations cannot both vanish there. It therefore cannot preserve the old `Ok` assertion merely because the certificate allowance is zero.

The Bhatt tolerance test is a genuine binary64 obstruction. For `x={-3,-1,0,1,2}`, held `sigma=1,a=2`, strict location concavity gives one root,

`mu* = -0.2966871018622317685538106428555293...`.

A 120-digit signed enclosure of radius `1e-60` gives a nearest-binary64 distance bounded below by **2.60373852785e-17**. The requested bound is `1e-30` (location floor 1). No public binary64 coefficient can meet it. `NumericalFailure` is required; `certified=false` cannot make that `Ok` assertion valid.

These refutations and the quantization enclosure are retained in `burr-local-refutation.json`, `burr-face-evidence.json`, `forced-burr-evidence.json`, `unit-seam-evidence.json` and `bhatt-quantization.json`.

## Original full validation, before expectation review

The original full runs used the local workstation **JonasBI**. Simbox was not used, as the brief declares it unreachable. Toolchains: GCC **15.2.0**, R **4.6.1**, Node **22.22.1**, and the Justfile's Emscripten toolchain. Builds and computations ran through the shared slot wrappers, under `nice`, with BLAS/OpenMP pinned to one thread; our processes were restricted to CPUs 0–3. Commands were waited for in the foreground.

| Check | Final result | Exit |
|---|---|---:|
| Focused C++, native | 35/35 cases, 291/291 assertions | 0 |
| Focused C++, double-only | 35/35 cases, 291/291 assertions | 0 |
| Strict replay and independent measurements | 91 inputs each mode; all returned coefficients pass; five preserved search stops | 0 |
| Complete replay and measurements | 1,395 inputs each mode; 391 returned relevant fits pass; four refuted nominations | 0 |
| Supplemental held-scale beta-prime | Three masks in each build; maximum error `3.292e-12` | 0 |
| Exact normal-sign comparisons | 365 each native, double-only and WASM; zero mismatches | 0 |
| `just cpp-test` | 105/107 shards; only `distribution_burr`, `distribution_bhatt` fail | 8 |
| `just cpp-test-no-long-double` | 132/134 shards; the same two fail | 8 |
| `just test` | PASS 23206, FAIL 6, WARN 0, SKIP 154 | 1 |
| `just test-full` | PASS 46033, FAIL 6, WARN 0, SKIP 0 | 1 |
| `just js-build` | Complete | 0 |
| `just js-test` | 613/615; the two old conflicts below | 1 |
| `just site-check` | Complete | 0 |
| R and WASM spots plus coefficient measurements | 79 fits and five preserved search stops in each; maximum error `2.672e-11` | 0 |
| Source/scope/test-prefix integrity | No violations | 0 |

These historical manifests record the pre-review nonzero broad-suite exits explicitly. The final requested review checks and their green results follow below. The orchestration's completion marker does not mean those suites passed. The final binding measurement was repeated after correcting only the fourteen new Bhatt fixtures from the nonexistent `gradtol` argument to their existing `reltol` control. No fitter, dossier or test source changed; source hashes were checked before and after the rerun. The failed measurement manifest and superseded checks are retained for audit.

## Review round: authorized expectations and final results

The reviewer confirmed that the five conflicts follow from decisions already made by Jonas: a proved refutation or failed search may stop; an unreachable tolerance is a numerical-accuracy failure. Only the specified expectations were replaced. No production fitter header, control, message, dossier, golden, d/p/q/r source or unrelated test changed during this review.

| Authorized change | New assertion and retained evidence |
|---|---|
| Nearly-constant Burr / inverse Burr / Pareto IV in C++, R and JS | Numerical/refutation outcome; the old nomination remains a finite, positive, bounded-likelihood witness. C++ matches its coefficients exactly to the private nomination; R/JS identify the retained large-shape nomination. Weibull/inverse-Weibull closure witnesses, when carried, retain finite positive coefficients and valid likelihood bounds |
| Zero-certificate-allowance Burr cases | Numerical refutation, not a budget-conditioned `Ok`; the original nomination remains a witness and its scale box stays below the smallest observation |
| Bhatt `tol=1e-30` in C++, R and JS | Numerical-accuracy failure; the finite best candidate retains held `sigma=1,a=2`, finite location and valid physical likelihood bounds |
| Bhatt `maxit=1` | Local-search iteration failure; no unverified finite replacement is accepted |
| Bhatt private/public bit identity at three unit scales | Each public coefficient satisfies the `1e-10` bound against the independent references, its physical likelihood is checked, and the private proposal remains finite. A separate source-identity audit proves `bhatt_detail::legacy_candidate` is byte-identical to `bc8ff7be` |

The Weibull-closure improvements are smaller than the floating likelihood enclosures. The tests preserve both witnesses and consistent bounds; they do not claim that those intervals strictly separate. The independent 420-digit comparisons above supply the improvement evidence.

### GED / SGED case 31101

Inputs remain `(0.7321666961121761, 2.2766042947262384, -0.5491691631533211)`, with held `sd=1.0178559902092952` and SGED `xi=1`. Independent differentiation of the raw GED log density at 100 digits gives

`mean* = 0.8637175657864586408862805910629875338...`

`nu* = 27.49733661683168433866852526549805668...`.

The reference information is positive definite. The existing GED local finish already corrects the old `27.497341303893556` nomination; no additional GED algorithm change was necessary. The appended GED C++ regression checks both public fitters in each arithmetic mode, including literal holds and exact child/parent transport at identical controls. The GED R regression checks the public parent and unit-skew delegate. This resolves acc-fb-t open item 2 for this source tree; no SGED file was edited.

| Backend / family | Returned nu | Mean scaled error | Nu relative error |
|---|---:|---:|---:|
| native / ged | 27.497336616829312 | 1.256e-29 | 8.628e-14 |
| double / ged | 27.497336616829177 | 1.091e-16 | 9.119e-14 |
| R / ged | 27.497336616829312 | 1.256e-29 | 8.628e-14 |
| R / sged | 27.497336616829312 | 1.256e-29 | 8.628e-14 |
| WASM / ged | 27.497336616829092 | 1.091e-16 | 9.429e-14 |
| WASM / sged | 27.497336616829092 | 1.091e-16 | 9.429e-14 |

### Final requested review checks (local workstation)

| Check | Result | Exit |
|---|---|---:|
| Burr, inverse Burr, Pareto IV, Bhatt, GED C++ regular/slow shards, native | 9/9 passed | 0 |
| Same C++ shards, double-only | 9/9 passed | 0 |
| The five R test files, full tier | PASS 2088, FAIL 0, WARN 0, SKIP 0 | 0 |
| WASM rebuild and complete JavaScript suite | 615/615 passed; zero failures, cancellations or skips | 0 |
| Case 31101 independent-reference and R/WASM comparisons | Six backend/family rows pass; maximum scaled error 9.429e-14 | 0 |
| Scope, original-data/fit-argument and private-proposal audits | No violations | 0 |

All commands ran through the local slot wrappers and were waited for in the foreground. The validation driver freezes the authorized test files and checks all other pre-review source hashes before and after each command. The original full numerical replay results remain applicable because all production fitter files are unchanged. The original pre-authorization full-suite failures above are historical; the requested final review checks are all green.

**Preserved existing search limitation: five supplemental coupled Burr transports.** Both baseline and final native/double builds stop with the same budget classification and diagnostics on:

- `strict-seed-coupled-burr-0`;
- `strict-seed-coupled-pareto4-0`;
- `strict-seed-coupled-burr-1`;
- `strict-seed-coupled-invburr-1`;
- `strict-seed-coupled-pareto4-1`.

Independent high-precision finite local maxima are available and the direct final-step checks pass. The existing public search supplies no successful nomination. A structural proposal/search change is missing; that belongs to the global/search work preserved by this brief. These are not new accuracy refusals, and they remain in the R/WASM spot inputs with explicit expected-stop evidence in `preserved-search-stops.json`.

**Open artifact-pane configuration.** The required `bin/show` call returned HTTP 403: this worktree is outside every viewer-readable root. A supported read-only thread-list refresh confirmed that the viewer has no registered thread for this worktree. Display requires the user-owned viewer configuration to include this directory, or a viewer-registered thread there. No new agent thread, server-configuration change or copy into another checkout was made. The completed report remains at the requested path.

## Evidence and reproduction

The original full-validation records are [2026-11-11-acc-fb-cont-data/validation-summary.json](2026-11-11-acc-fb-cont-data/validation-summary.json), [complete-validation.json](2026-11-11-acc-fb-cont-data/complete-validation.json), [final-js-site-checks.json](2026-11-11-acc-fb-cont-data/final-js-site-checks.json), [current-native.tsv](2026-11-11-acc-fb-cont-data/current-native.tsv.gz), [current-double.tsv](2026-11-11-acc-fb-cont-data/current-double.tsv.gz), [coefficient-errors.json](2026-11-11-acc-fb-cont-data/coefficient-errors.json.gz), [binding-errors.json](2026-11-11-acc-fb-cont-data/binding-errors.json) and [source-integrity.json](2026-11-11-acc-fb-cont-data/source-integrity.json). Earlier `probe*`, `after*`, `*-analytic-step*` and `validation-before-*` records document development stages and are not the final validation result.

The final review records are `review-validation.json`, `review-summary.json`, `review-scope-audit.json`, `review-private-proposal-identity.json` and `review-ged-31101-reference.json` / `review-ged-31101-results.json` in the same data directory. Reproduce the requested review checks from this worktree with:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run4 taskset -c 0-3 nice -n 10 python3 dev/research/evaluations/2026-11-11-acc-fb-cont-data/review_validate.py
```

`review_validate.py` runs the targeted C++/R checks, WASM build, complete JS suite and delegated GED measurements. The final run used `review_refresh_js.py` to add the separate WASM build and repeat JS, GED measurements and the source audit after the initial JS pass. R measurement values use 17 significant digits, preserving the exact held binary64 scale. The pre-correction JSON-serialization failure is retained in `review-validation-before-ged-serialization-fix.json`; no fit or test failed in that measurement attempt. `review_audit.py` checks authorized test-block changes, unchanged original data/fit arguments and private-proposal source identity. `review_ged_reference.py` retains the independent 100-digit calculation. The original `validate_all.py` and pre-review manifests remain historical records of the original full validation; their original no-test-rewrite guard predates the review authorization. Large scratch builds/logs remain under `build/scratch/`.


Final report assembled at 2026-11-12 02:36 UTC. No command started for validation remained running when the report was assembled.
