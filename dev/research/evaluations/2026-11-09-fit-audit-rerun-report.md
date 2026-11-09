# Fit audit replay on current master — TASK-101.7.2

The completed replay of **11,185 frozen cases, 99 families and 579 held subsets** at `a99ca0dc` finds **354 silent-failure hits under the original classification rules**, down from 533. **344 old silent cases still fail; 10 cases are newly silent.** The separate **1e-10 coefficient-contract view has 895 hits**, adding 541 independently checked coefficient-only cases. These constructed stress-grid counts are not prevalence estimates or counts of independent bugs.

Of the 533 old silent cases, 144 now pass the replay checks, 5 now stop loudly with finite-attainment evidence, 39 now stop without an audit proof of finite attainment or escape, and 1 remains numerically open. The latter 40 are not counted as fixes. No fitter, golden, dossier, binding, generated package, production test, status or roadmap file was changed. No commits, writing Git commands, board edits, package installs or sub-agents were used.

## Certified routes to address first

The strict list contains 622 findings on runtime/declared/conditional certified routes, 5 fully-held evaluations and 268 uncertified numerical routes. [Every ranked case, exact reproduction, returned/reference point, certificate basis, source seam and likely fix](2026-11-09-fit-audit-rerun-data/silent-failures.md) is retained. The machine-readable list is [ranked-silent.json](2026-11-09-fit-audit-rerun-data/ranked-silent.json.gz).

- **Beta-prime held scale, cases 248/249:** the current result still reports logLik 80 and about −5.0466e10 on tightly clustered samples. The saved independent values are about 84.44195 and 130.49369. The current seam is `betapr/mle.hpp:138–142`: the stable beta shape fit is followed by a cancelling raw likelihood/log-beta reconstruction. This is a certified-route payload defect; any shared `special/` repair needs a separate task.
- **NSBeta held shape, case 5348:** the current returned shape2 is still 4.27914928881e14, versus the saved unique-root reference 4.73493829873e150. The held branch at `nsbeta/mle.hpp:22–25` still differences digammas/lgammas and omits the inner maxit. Reuse of the 350-digit bracket and 700-digit sign check is valid because the frozen input, held values and returned point match exactly; see the [saved focused reference](2026-11-09-fit-audit-rerun-data/focused-reference.json).
- **EMG location-only, newly silent cases 1374/1375:** current `location_certificate.hpp:67–69` scales its coordinate target by absolute location. A 1e8 translation permits order-one location uncertainty at default tol=1e-8. Case 1375 returns mu 100000000.89101982, while a feasible stationary point at 100000000.60179031 improves logLik from −25.26599180 to −24.97609356. Case 1374 has a much larger extreme-scale loss. The conditional enclosure may satisfy its declared tolerance; the public likelihood/1e-10 acceptance contract still fails. Use compensated local coordinates and propagate coefficient and likelihood bounds.
- **Triangular and GEV:** exact rational comparisons retain represented-support exclusions; triangular also retains finite likelihood/improvement witnesses. Recheck restored coefficients against the original observations and recompute the raw closed-support likelihood before Ok.
- **Transformed families:** inverse-gamma, log-gamma, Nakagami, lognormal, logit-normal, inverse Burr and log-logistic transfers retain arithmetic seams where rounded transformed observations lose information. Preserving a stable base solver does not recover that information.

A certified-route flag here identifies the executed analytic/certificate route; it does not assert that an exact-arithmetic theorem was disproved. Certificates are conditional on computed comparisons and practical margins. Some coefficient findings exceed the new 1e-10 contract while lying inside an older documented default tolerance or enclosure. Relative location tolerance can also allow material likelihood loss after a large translation, as in EMG. Nonfinite core Ok results caught by a binding guard remain core findings; this audit does not claim every such result reaches R or JS.

## Per-family transition table

Transitions below use the original rules, including the 1e-8 coefficient threshold. `Fixed` means old silent → current ok (a passing numerical/proved-stop check, not a new global proof). `Still` is silent → silent; `New SF` is non-silent → silent; `New loud` is any previously non-loud → loud. `New open` counts numerical opens only. `New existence-open` counts newly unresolved native non-Ok stops separately. Columns can overlap outside the old-silent partition. The last column is the separate 1e-10 view; its additions are not labelled regressions.

| Family | Cases | Old SF | Fixed | Still | New SF | New loud | New open | New existence-open | Current SF (old rules) | SF at 1e-10 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| bbinom | 125 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| beta | 88 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 2 |
| betapr | 184 | 34 | 24 | 10 | 0 | 0 | 0 | 0 | 10 | 11 |
| bhatt | 144 | 8 | 4 | 0 | 0 | 0 | 0 | 16 | 0 | 11 |
| binom | 64 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| bnbinom | 112 | 4 | 3 | 0 | 0 | 0 | 0 | 1 | 0 | 39 |
| burr | 192 | 2 | 0 | 2 | 0 | 0 | 0 | 0 | 2 | 9 |
| cauchy | 72 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 4 |
| cmb | 112 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 15 |
| cmp | 56 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| dgamma | 56 | 3 | 0 | 3 | 0 | 0 | 0 | 0 | 3 | 15 |
| dlaplace | 56 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| dnorm | 56 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 7 |
| dunif | 64 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| dweibull | 56 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| emg | 184 | 36 | 35 | 0 | 2 | 0 | 0 | 10 | 2 | 39 |
| exp | 42 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| fatigue | 88 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 3 |
| foldnorm | 96 | 13 | 0 | 13 | 0 | 0 | 0 | 0 | 13 | 27 |
| fpareto | 544 | 49 | 9 | 33 | 0 | 0 | 2 | 32 | 33 | 69 |
| frechet | 180 | 32 | 13 | 15 | 0 | 0 | 0 | 4 | 15 | 24 |
| gamma | 96 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 1 | 5 |
| gb2 | 544 | 27 | 5 | 17 | 0 | 0 | 0 | 29 | 17 | 56 |
| ged | 144 | 18 | 8 | 0 | 0 | 4 | 0 | 10 | 0 | 0 |
| genbeta | 192 | 6 | 2 | 4 | 0 | 0 | 0 | 0 | 4 | 21 |
| geom | 28 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| gev | 152 | 20 | 3 | 16 | 0 | 0 | 0 | 3 | 16 | 46 |
| gompertz | 84 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 1 | 1 |
| gpd | 80 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| gumbel | 84 | 2 | 0 | 2 | 0 | 0 | 0 | 0 | 2 | 4 |
| hcauchy | 100 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| hnorm | 82 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| ht | 200 | 23 | 0 | 23 | 0 | 0 | 0 | 0 | 23 | 24 |
| huber | 144 | 1 | 0 | 0 | 0 | 3 | 0 | 2 | 0 | 0 |
| hyper | 116 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| invburr | 192 | 3 | 0 | 3 | 0 | 0 | 0 | 0 | 3 | 10 |
| invexp | 44 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| invgamma | 96 | 3 | 0 | 3 | 0 | 0 | 0 | 0 | 3 | 6 |
| invgauss | 100 | 2 | 0 | 2 | 0 | 0 | 0 | 0 | 2 | 3 |
| invparalogis | 88 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| invpareto | 88 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| invtrgamma | 152 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| invweibull | 84 | 9 | 0 | 9 | 0 | 0 | 0 | 0 | 9 | 17 |
| kumar | 80 | 2 | 0 | 2 | 0 | 0 | 0 | 0 | 2 | 4 |
| laplace | 72 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| lgamma | 80 | 2 | 0 | 2 | 0 | 0 | 0 | 0 | 2 | 6 |
| lgser | 28 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| llogis | 88 | 3 | 0 | 3 | 0 | 0 | 0 | 0 | 3 | 10 |
| lnorm | 100 | 3 | 0 | 3 | 0 | 0 | 0 | 0 | 3 | 5 |
| logis | 72 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 2 |
| logitnorm | 88 | 2 | 0 | 2 | 0 | 0 | 0 | 0 | 2 | 4 |
| lomax | 76 | 2 | 0 | 2 | 0 | 0 | 0 | 0 | 2 | 3 |
| lst | 144 | 6 | 0 | 6 | 2 | 0 | 0 | 0 | 8 | 10 |
| naka | 96 | 3 | 0 | 3 | 0 | 0 | 0 | 0 | 3 | 10 |
| nbinom | 64 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 2 |
| nhyper | 124 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| norm | 72 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| nsbeta | 100 | 6 | 0 | 6 | 0 | 0 | 0 | 0 | 6 | 9 |
| paralogis | 76 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| pareto | 82 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| pareto2 | 88 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| pareto3 | 76 | 1 | 0 | 1 | 0 | 0 | 0 | 0 | 1 | 4 |
| pareto4 | 176 | 2 | 0 | 2 | 0 | 0 | 0 | 0 | 2 | 5 |
| pig | 56 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 14 |
| pois | 28 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| power | 84 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| rayleigh | 48 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| sged | 256 | 17 | 4 | 13 | 0 | 0 | 0 | 0 | 13 | 35 |
| sgomp | 88 | 6 | 0 | 6 | 0 | 0 | 0 | 0 | 6 | 11 |
| skellam | 56 | 4 | 0 | 4 | 0 | 0 | 0 | 0 | 4 | 26 |
| slash | 64 | 9 | 0 | 9 | 0 | 0 | 0 | 0 | 9 | 19 |
| sn | 160 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 29 |
| snorm | 128 | 20 | 16 | 0 | 0 | 4 | 0 | 1 | 0 | 0 |
| sstd | 256 | 24 | 5 | 16 | 1 | 0 | 0 | 3 | 17 | 37 |
| st | 256 | 51 | 9 | 41 | 3 | 1 | 1 | 0 | 44 | 68 |
| std | 144 | 8 | 2 | 6 | 2 | 0 | 0 | 0 | 8 | 20 |
| tbinom | 240 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 |
| tlambda | 176 | 2 | 1 | 1 | 0 | 0 | 0 | 1 | 1 | 1 |
| tnorm | 256 | 2 | 0 | 2 | 0 | 2 | 0 | 0 | 2 | 9 |
| tpois | 116 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| trgamma | 176 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| triang | 168 | 10 | 0 | 10 | 0 | 0 | 0 | 0 | 10 | 10 |
| unif | 94 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| weibull | 96 | 5 | 0 | 5 | 0 | 0 | 0 | 0 | 5 | 15 |
| yulesimon | 16 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| zeta | 16 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| zib | 128 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| zinb | 128 | 2 | 0 | 2 | 0 | 0 | 0 | 0 | 2 | 2 |
| zip | 56 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| zipf | 56 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 |
| zmbinom | 128 | 2 | 0 | 0 | 0 | 0 | 0 | 2 | 0 | 0 |
| zmgeom | 56 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| zmlogarithmic | 56 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| zmnbinom | 128 | 24 | 0 | 24 | 0 | 0 | 0 | 0 | 24 | 38 |
| zmpois | 56 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| ztbinom | 64 | 1 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 |
| ztgeom | 16 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| ztnbinom | 64 | 16 | 0 | 16 | 0 | 0 | 0 | 0 | 16 | 18 |
| ztpois | 28 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |

Full [transition CSV](2026-11-09-fit-audit-rerun-data/transitions.csv) and [JSON](2026-11-09-fit-audit-rerun-data/transitions.json) include strict-target additions and old silent → unresolved-stop counts. Totals: 14 newly loud, 4 newly numerical-open and 115 newly existence-open cases.

## Newly silent under unchanged rules

| Case | Family | Previous classification | Holds / sample | Current evidence |
|---:|---|---|---|---|
| 1374 | emg | non-Ok | `{'sigma': 1.0518147225573364, 'lambda': 1.209185930245896}` / scale-1e+150 | feasible higher-likelihood point (70 digits); E=1 |
| 1375 | emg | ok | `{'sigma': 1.0518147225573364, 'lambda': 1.209185930245896}` / offset-1e8 | feasible higher-likelihood point (70 digits); E=0.011474 |
| 4941 | lst | ok | `{'mu': 0.0745507089520688, 'sigma': 1.0270947160838733}` / tiny-5 | stationary local refinement: coefficient relative error exceeds 1e-8; E=1.278e-07 |
| 4952 | lst | ok | `{'mu': 0.0745507089520688, 'sigma': 1.0270947160838733}` / limit-outlier | stationary local refinement: coefficient relative error exceeds 1e-8; E=1.4415e-08 |
| 6736 | sstd | ok | `{'mean': 0.012734271748561354, 'sd': 1.0184483076867987}` / limit-outlier | stationary local refinement: coefficient relative error exceeds 1e-8; E=2.0133e-07 |
| 6960 | st | ok | `{'xi': -0.036539023191020197}` / limit-outlier | stationary local refinement: coefficient relative error exceeds 1e-8; E=5.5723e-08 |
| 6962 | st | ok | `{'omega': 0.9203517925594821}` / interior | stationary local refinement: coefficient relative error exceeds 1e-8; E=3.5999e-08 |
| 7026 | st | ok | `{'omega': 0.9203517925594821, 'alpha': 0.10872382324228061}` / interior | stationary local refinement: coefficient relative error exceeds 1e-8; E=2.195e-08 |
| 7220 | std | ok | `{'sd': 0.8395065634734545}` / tiny-3 | stationary local refinement: coefficient relative error exceeds 1e-8; E=2.4359e-08 |
| 7249 | std | ok | `{'mean': -0.0062603717426506856, 'sd': 0.8395065634734545}` / limit-concentration | stationary local refinement: coefficient relative error exceeds 1e-8; E=1.0619e-08 |

## Severity ranking

The complete list puts likelihood/ascent first, wrong status or invalid payload second, and coefficient-only cases third. Certified routes come first within each severity tier and are flagged separately above. Finite normalized likelihood error or demonstrated gain (including the reused focused NSBeta root) ranks likelihood witnesses; verified relative coefficient displacement ranks coefficient witnesses. A support-excluded point has exactly zero likelihood even when a one-ULP outward restoration would repair feasibility. Coefficient-only errors are local isolated-reference comparisons, not claims of global uniqueness.

| Rank | Case | Family | Route | Category | Error |
|---:|---:|---|---|---|---:|
| 1 | [257](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-257) | betapr | declared certified | likelihood / ascent | 3.6141 |
| 2 | [5348](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-5348) | nsbeta | declared certified | likelihood / ascent | 2.2326 |
| 3 | [256](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-256) | betapr | declared certified | likelihood / ascent | 1.9676 |
| 4 | [255](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-255) | betapr | declared certified | likelihood / ascent | 1.8958 |
| 5 | [251](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-251) | betapr | declared certified | likelihood / ascent | 1.6051 |
| 6 | [252](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-252) | betapr | declared certified | likelihood / ascent | 1 |
| 7 | [249](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-249) | betapr | declared certified | likelihood / ascent | 1 |
| 8 | [1374](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-1374) | emg | declared certified | likelihood / ascent | 1 |
| 9 | [10791](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-10791) | nsbeta | declared certified | likelihood / ascent | 0.2547 |
| 10 | [11011](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-11011) | nsbeta | declared certified | likelihood / ascent | 0.13905 |
| 11 | [248](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-248) | betapr | declared certified | likelihood / ascent | 0.055524 |
| 12 | [10792](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-10792) | nsbeta | declared certified | likelihood / ascent | 0.042323 |
| 13 | [1375](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-1375) | emg | declared certified | likelihood / ascent | 0.011474 |
| 14 | [11130](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-11130) | triang | declared certified | likelihood / ascent | 0.003443 |
| 15 | [260](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-260) | betapr | declared certified | likelihood / ascent | 0.0010208 |
| 16 | [10799](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-10799) | nsbeta | declared certified | likelihood / ascent | 5.7779e-05 |
| 17 | [11122](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-11122) | triang | declared certified | likelihood / ascent | 3.3343e-05 |
| 18 | [11119](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-11119) | triang | declared certified | likelihood / ascent | 3.0247e-05 |
| 19 | [8194](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-8194) | triang | declared certified | likelihood / ascent | 1.9078e-05 |
| 20 | [254](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-254) | betapr | declared certified | likelihood / ascent | 8.2132e-06 |
| 21 | [5666](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-5666) | pareto4 | declared certified | likelihood / ascent | 3.3463e-06 |
| 22 | [5669](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-5669) | pareto4 | declared certified | likelihood / ascent | 3.3463e-06 |
| 23 | [8289](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-8289) | triang | declared certified | likelihood / ascent | 1.0973e-06 |
| 24 | [3759](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-3759) | invgamma | declared certified | likelihood / ascent | 1.0034e-06 |
| 25 | [3762](2026-11-09-fit-audit-rerun-data/silent-failures.md#case-3762) | invgamma | declared certified | likelihood / ascent | 1.0034e-06 |

All 895 ranked reproductions are in [silent-failures.md](2026-11-09-fit-audit-rerun-data/silent-failures.md). Each contains only its frozen small sample/holds and one exact standalone command. Each one-record replay is a minimal executable reproducer with no R or testing-framework dependency. Exact causal operations not isolated by this audit are listed as open nominations with the missing trace, rather than presented as proved causes.

## Method and evidence

The frozen `cases.json` is byte-identical to the decompressed original, including the appended limit-sample sequences. The 118 saved finite-parameter sequences span 44 families; their binary64 data are replayed, not regenerated from SciPy. Held values and feasible golden seeds come from the frozen case inventory, not current golden estimates. Current binding controls and core routes are retained. The generated observer preserves 17 digits of each result, sees the core status before binding conversion and records optional top-level `certified`/`global_certified` flags separately. A missing flag is not a false flag; partial families without flags use the reviewed held-subset dispatch in `route_rules.py`.

The only interface adaptation is audit-local handling of CMP’s lambda-built final result, preserving its fully-held ledger route and current defaults. No production harness was edited. Existing generation/GB2 mapping and structured-error adaptations were retained.

Exactly 9354 old reference records have an exact native status, returned-coefficient and reported-logLik match, on byte-identical inputs. The other 1831 points received new reference work. Non-Ok classification uses the same frozen sufficient attainment/nonexistence rules. The initial core guard is 3 seconds; 68 guarded calls received 20-second retries, leaving 20 without a terminal status. Reference gaps were retried at 20 seconds; incomplete work remains open.

The independent parser uses dossier `log_density`, not golden-specific specifications. mpmath precision is at least 70 digits, escalates with magnitude, and finite silent likelihood/ascent witnesses agree at doubled precision to relative 1e-20. Strict support exclusions use exact rational binary64 inputs. Original likelihood threshold: 1e-8 × max(1, |reported logLik|); score threshold: 1e-3 with a feasible ascent witness. The transition coefficient threshold stays 1e-8. The supplementary threshold is 1e-10, with unchanged positive/signed-coordinate denominators, strictly better likelihood, per-observation stationarity residual ≤1e-18 and positive local information. Root residual/information proxies must be below both 1e-10 and one hundredth of the measured coefficient displacement. These are numerical local consistency checks, not outward-rounded curvature enclosures or a global absence-of-failure proof.

The supplementary pass rechecks eligible saved/refined stationary points; it does not tighten every black-box search threshold or prove that every unflagged Ok is within 1e-10. Fully held evaluations have no coefficient target. Default fit controls and goldens were not changed.

## Open entries and remaining work

The final strict census is: {'ok': 7773, 'non-Ok': 2305, 'silent failure': 895, 'loud failure': 182, 'open': 30}. The [open register](2026-11-09-fit-audit-rerun-data/open.md) enumerates all 30 numerical opens and 2305 native stops whose existence/escape classification is not settled by the frozen sufficient rules. It records the exact case, holds, native status, effort and missing evidence. A numerical failure or exhausted budget is allowed by the library contract and is not a silent wrong result. The [loud list](2026-11-09-fit-audit-rerun-data/loud-failures.md) records finite-attainment evidence for all 182 loud outcomes.

Remaining fixes are follow-up work: preserve raw-data information in transformations; evaluate log normalizers/interval masses without cancellation; recheck original-data support after restoration; propagate certificate widths into public coordinates; complete global coverage for the named numerical profiles or stop. Exact causal attribution for generic coefficient misses is an explicit open trace task in the register. No density/quantile/sampling, special-function, truth-table or generated d/p/q/r change was made. Any needed shared special-function work is only nominated here.


Storage: `cases.json` is byte-identical to the 2026-11-07 audit's and is not duplicated here (decompress `2026-11-07-fit-audit-data/cases.json.gz`). The largest data files (`reference.jsonl`, `reference-legacy.jsonl`, `fits.csv`, `fits-initial.csv`, `ranked-silent.json`, `cpp-test-simbox.log`) are gzipped; the scripts use the plain names.

## Finished runs, provenance and reproduction

- **Audit:** workstation, GCC 15; standalone C++17 `-O1`, single-thread native sweep/retries, exact-point reference reuse, single-thread high-precision pass and four-worker cache reconciliation. Every numerical worker used the shared slot wrapper, nice priority and single-thread BLAS/OpenMP. The queued four-slot pass was waited to completion.
- **Independent checks:** 41/41 oracle checks; 142 independent normal/exponential/Poisson likelihood checks; complete 11,185-case/99-family/579-subset accounting; exact frozen input hash; all 241 compiled held literals match the frozen inventory; status consistency; all coefficient witnesses passed information/stationarity and root-proxy gates.
- **Required C++ test:** simbox, GCC 13, `just cpp-test`: **106/106 dev shards passed**, 54.16 seconds test time. The normal Boost-discovery fallback occurred during configuration; the final build and ctest both succeeded. No further package suites were required by this task.

[Run provenance](2026-11-09-fit-audit-rerun-data/run-provenance.json), [validation](2026-11-09-fit-audit-rerun-data/validation.json), [oracle validation](2026-11-09-fit-audit-rerun-data/reference-validation.json), [root checks](2026-11-09-fit-audit-rerun-data/root-precision-check.json), [source SHA-256](2026-11-09-fit-audit-rerun-data/source-sha256.json), [native log](2026-11-09-fit-audit-rerun-data/native-run.log) and [C++ test log](2026-11-09-fit-audit-rerun-data/cpp-test-simbox.log.gz) retain the evidence. [summary.json](2026-11-09-fit-audit-rerun-data/summary.json) contains the headline counts.

From the worktree root, `bash dev/research/evaluations/2026-11-09-fit-audit-rerun-data/build_harness.sh` rebuilds the current-interface observer. Then run any frozen case using the slot-wrapped `reproduce.py INDEX` command above. `run.sh` documents the complete resumable replay, with reservations acquired at each compute stage and no nested slot locks. Do not reuse fit/reference caches after production sources change; compare the saved source hashes first. No result claims exhaustive coverage of all datasets, hold values, multimodal branches or limit faces.

The first read-only host lock-owner diagnostic timed out in automatic permission review. One allowed retry succeeded and confirmed active validation work; no lock or reservation was changed.
