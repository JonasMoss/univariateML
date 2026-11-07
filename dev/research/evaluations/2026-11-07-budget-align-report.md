# TASK-143: work budgets and iterlim

Source base: `f8f65f9b080c394d11ca9dc178a93219fffee210` (`codex/budget-align`, uncommitted worktree). No commits, dependency installations, board edits, golden changes, or policy-note changes were made. Numerical source was frozen before the final validation; subsequent edits corrected stale budget wording in four dossiers. Each run retains its actual complete-source fingerprint. The final numerical-header SHA-256 is `7349c83fd8ae9cc29c52d42593b53160ba0dbb512a36f265cc84779f652e036f`.

## Implemented behavior

The [work-budget policy](../../design/work-budgets.md) is unchanged. The common C++17 helpers widen before multiplication and compute `I = max(0, min(iterlim, 100000))`, solver allowance `I`, certificate allowance `128 I`, and enumeration allowance `100000 I`. The largest allowance is 10,000,000,000 cells, which fits in signed 64-bit arithmetic and exceeds 32-bit range. Invalid controls remain invalid inputs. Public controls reach local solvers, bracketing/backtracking, held-coordinate profiles, and delegates; BFGS line searches now use the caller's iteration allowance. Local solver exhaustion remains a convergence failure.

Certificate and enumeration exhaustion set `budget_exhausted`, retain the available candidate coefficients, likelihood bounds and limiting-family witnesses, and map to R class `univariateML_budget`. Existing diagnostic prefixes are retained; budget messages add “Raising iterlim may resolve this stop.” A budget stop never returns its best-so-far candidate as a successful estimate. Representability, inconclusive comparisons and failed residual checks remain numerical failures rather than being renamed budget stops.

The principal global changes are the NB/Kumar certificates (16 to 128 evaluations per iteration), ZINB finite and face covers (32/64 to 128), Burr, Student, paralogistic, skew-normal and slash fixed certificates, Cauchy, discrete gamma, folded normal, Skellam and transformed-gamma covers. BB and CMB use cumulative cell ledgers. CMB's fixed-size normalizers, mean matching and free-size rescue share the parent's allowance; BB charges support/range preparation and profile work. Held-probability ZIB/ZTBinom scans count sample cells rather than million-profile reservations. GB2, beta-prime and BNB propagate the caller's control and unresolved delegate failures. Transformed-gamma witnesses survive the reciprocal and GB2/Fpareto transformations.

## Inventory and work units


The retained inventory contains 1173 loop records and 119 adaptive-cap/delegate rows across 68 aligned families. [loop-inventory.csv](2026-11-07-budget-align-data/loop-inventory.csv) gives paths, line numbers, old/new expressions, units and default-cap calculations; [cap-table.csv](2026-11-07-budget-align-data/cap-table.csv) is the machine-readable table below. [defaults.json](2026-11-07-budget-align-data/defaults.json) records all public controls, including the protected CMP exception.


The scan deliberately includes evaluator and quantile helpers, inference-only helpers, and structural loops as a superset of fitter dependencies. It is a source audit, not a formally proved call graph. Data reductions, dimension loops, fixed approximation formulas and finite sets of starting points are not additional adaptive search caps. Reconnaissance points that call a metered certificate evaluator spend that evaluator's ledger. [structural-loop-review.csv](2026-11-07-budget-align-data/structural-loop-review.csv) records the retained formula/grid/domain expressions; [protected-loop-inventory.csv](2026-11-07-budget-align-data/protected-loop-inventory.csv) records settled evaluator dependencies. Fixed domain limits (including CMB size at most 10,000,000 and Student shapes above 1e14) remain numerical refusals, never fitted maxima. Pure quantile loops are outside fitting.


In this table `I` means the saturated caller control above; old numeric caps use the old default, and new numeric caps use the final default. Compound old expressions and previously unbudgeted scans are stated explicitly. Per-loop solver expressions and fixed inner caps are resolved in the linked loop inventory.


| Family | Loop | Unit | Old default cap | New default cap | New cap |
|---|---|---|---|---|---|
| bbinom | shape profile certificate | certificate evaluation | 200000 | 64000 | 128*I |
| bbinom | size range and support scans, shared ledger | enumeration cell | 1000 size points; no aggregate cell cap | 50000000 | 100000*I |
| bbinom | held-shape fixed-size likelihood scans | enumeration cell | unbudgeted finite support | 50000000 | 100000*I |
| burr | Weibull bracket doubling | solver iteration | 80 | 1000 | I |
| burr | nuisance enclosure widening | solver iteration | 6 | 1000 | I |
| gb2 | adaptive shape-box expansion | solver iteration | 3 | 300 | I |
| huber | cold full-reduction Newton trajectory | solver iteration | min(I,max(12,16000000/n)) | 200 | I |
| huber | fallback scale bracket doubling | solver iteration | 1024 | 200 | I |
| dweibull | inner bracket doubling | solver iteration | 65 | 200 | I |
| gpd | root-center Newton polish | solver iteration | 8 | 200 | I |
| lomax | two root-center Newton polishes (each) | solver iteration | 8 | 100 | I |
| paralogis | certificate center Newton polish | solver iteration | 8 | 100 | I |
| slash | scale-bound bracket halving | solver iteration | 8 | 500 | I |
| tlambda | support rounding solves (each) | solver iteration | 8 | 1000 | I |
| burr | all-free global cover | certificate evaluation | 4096 | 128000 | 128*I |
| burr | held-scale profile points | certificate evaluation | 1000 | 128000 | 128*I |
| cauchy | held-scale location cover | certificate evaluation | 6400 | 12800 | 128*I |
| cmb | fixed-size normalizers and nested mean matches, shared ledger | enumeration cell | unbudgeted N+1 tables per solve | 200000000 | 100000*I |
| cmb | free-size monotone rescue normalizers | enumeration cell | (I+110)*(N+1)*230 reserved cells | 200000000 | 100000*I |
| cmb | three free-size normalizer covers | enumeration cell | 10000000 | 200000000 | 100000*I |
| cmb | both natural coordinates held, size cover | enumeration cell | 4000000 | 200000000 | 100000*I |
| dgamma | shape global cover | certificate evaluation | 400000 | 51200 | 128*I |
| foldnorm | held-location and free covers | certificate evaluation | 102400 | 25600 | 128*I |
| gpd | held-threshold covers | certificate evaluation | 25600 | 25600 | 128*I |
| kumar | free-shape score certificate | certificate evaluation | 1600 | 12800 | 128*I |
| lomax | global profile cover | certificate evaluation | 12800 | 12800 | 128*I |
| lst | all-free t cover | certificate evaluation | 4096 | 25600 | 128*I |
| nbinom | size score certificate | certificate evaluation | 1600 | 12800 | 128*I |
| paralogis | free and held-scale covers including rescue | certificate evaluation | 32768 | 12800 | 128*I |
| slash | held and free covers | certificate evaluation | 1000000 | 64000 | 128*I |
| skellam | joint-rate finite cover including tail nominations | certificate evaluation | 150000 | 19200 | 128*I |
| trgamma | shared free-shape segment cover | certificate evaluation | depth 40; no aggregate cap | 12800 | 128*I |
| invtrgamma | reciprocal transformed-gamma segment cover | certificate evaluation | depth 40; no aggregate cap | 12800 | 128*I |
| sn | shape cover | certificate evaluation | 20000 | 12800 | 128*I |
| std | all-free t cover | certificate evaluation | 4096 | 25600 | 128*I |
| zinb | held-probability size cover | certificate evaluation | 6400 | 25600 | 128*I |
| zinb | joint finite and ZIP face covers (each) | certificate evaluation | 12800 | 25600 | 128*I |
| ztnbinom | size cover | certificate evaluation | 3200 | 25600 | 128*I |
| ztnbinom | recurrence support | enumeration cell | 100000 | 20000000 | 100000*I |
| zib | held-probability integer scan | enumeration cell | 1000000 profile points | 20000000 | 100000*I |
| ztbinom | held-probability integer scan | enumeration cell | 1000000 profile points | 20000000 | 100000*I |
| invburr | certified delegates: burr | certificate evaluation | 4096 all-free; caller I held-scale | 128000 | 128*I |
| invparalogis | certified delegates: paralogis | certificate evaluation | 32768 | 12800 | 128*I |
| invpareto | certified delegates: lomax | certificate evaluation | 12800 | 12800 | 128*I |
| pareto2 | certified delegates: lomax | certificate evaluation | 128000 | 128000 | 128*I |
| pareto4 | certified delegates: burr | certificate evaluation | 4096 all-free; caller I held-scale | 128000 | 128*I |
| betapr | certified delegates: lomax | certificate evaluation | 128000 | 128000 | 128*I |
| gb2 | certified delegates: burr/lomax/trgamma | certificate evaluation | 4096 all-free Burr; caller I held-scale Burr; 128*I Lomax; depth-40 trgamma with solver I=100 | 38400 | 128*I |
| fpareto | certified delegates: gb2 | certificate evaluation | 4096 all-free Burr; caller I held-scale Burr; 128*I Lomax; depth-40 trgamma with solver I=100 | 128000 | 128*I |
| zmnbinom | certified delegates: ztnbinom | certificate evaluation | 3200 | 25600 | 128*I |
| bnbinom | certified delegates: nbinom | certificate evaluation | 3200 | 76800 | 128*I |
| bbinom | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 500 | 1*min(I,100000) |
| beta | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| betapr | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 1000 | 1*min(I,100000) |
| bhatt | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 500 | 1*min(I,100000) |
| binom | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| bnbinom | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 600 | 1*min(I,100000) |
| burr | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 1000 | 1*min(I,100000) |
| cauchy | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| cmb | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 2000 | 1*min(I,100000) |
| dgamma | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 400 | 1*min(I,100000) |
| dnorm | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| dweibull | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| emg | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 500 | 1*min(I,100000) |
| fatigue | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| foldnorm | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| fpareto | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 1000 | 1*min(I,100000) |
| frechet | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| gamma | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| gb2 | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 300 | 1*min(I,100000) |
| ged | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| genbeta | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 300 | 1*min(I,100000) |
| gev | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| gompertz | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| gpd | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| gumbel | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| huber | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| invburr | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 1000 | 1*min(I,100000) |
| invgamma | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| invparalogis | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| invpareto | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| invtrgamma | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| invweibull | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| kumar | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| lgamma | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| llogis | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| logis | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| lomax | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| lst | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| naka | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| nbinom | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| nsbeta | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 1000 | 1*min(I,100000) |
| paralogis | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| pareto2 | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 1000 | 1*min(I,100000) |
| pareto3 | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 1000 | 1*min(I,100000) |
| pareto4 | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 1000 | 1*min(I,100000) |
| pig | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| sged | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| sgomp | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| skellam | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 150 | 1*min(I,100000) |
| slash | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 500 | 1*min(I,100000) |
| sn | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| sstd | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| st | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| std | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| tlambda | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 1000 | 1*min(I,100000) |
| tnorm | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| trgamma | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| triang | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| weibull | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| yulesimon | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| zeta | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| zib | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| zinb | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| zipf | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 100 | 1*min(I,100000) |
| zmbinom | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| zmnbinom | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| ztbinom | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |
| ztnbinom | local solves, bracketing and backtracking (details in loop inventory) | solver iteration | I or fixed inner caps (source expressions retained in inventory) | 200 | 1*min(I,100000) |


## Defaults and ordinary-sample regressions


| Family | Old → new iterlim | Reason / retained evidence |
|---|---|---|
| bbinom | 100 → 500 (C++, R, JS) | Golden data with alpha held and size free stop at iterlim 100 and 200; 500 succeeds with logLik -1129.5877962989728 (bbinom-default.log / .mjs). Full native/R/JS tests pass at 500. |
| cmb | 100 → 2000 (C++, R, JS) | The shared ledger exposed nested normalizer cost. Hand samples with held nu pass at 1000, but R RNG seed 1142026, rcmb(100,4,.4,.5), still stopped at 1000 and succeeds at 2000. |
| slash | C++ 200 → 500; R/JS remain 500 | The native sample {0,1} stops on budget at 200 and certifies at 500 (slash-probe.log / .cpp). Align C++ with existing binding defaults. |

The default probes are retained in [bbinom-default.log](2026-11-07-budget-align-data/bbinom-default.log) and [slash-probe.log](2026-11-07-budget-align-data/slash-probe.log). The CMB seed and complete diagnostic samples are retained in [cmb-seed-probe.log](2026-11-07-budget-align-data/cmb-seed-probe.log), [cmb-r-default-probe.log](2026-11-07-budget-align-data/cmb-r-default-probe.log) and [cmb-r-default-probe-local.log](2026-11-07-budget-align-data/cmb-r-default-probe-local.log). The R-seed replay [r-cmb-target10.log](2026-11-07-budget-align-data/r-cmb-target10.log) independently checks size 4 against the finite size grid through 300 at likelihood tolerance 1e-8. That grid is numerical corroboration, not a replacement for the production tail bound. Other families keep their defaults. All golden/truth files remain byte-for-byte untouched. The full suites cover seeded cases beyond the golden fixtures; no newly failing sample was hidden with a skip.


## Mathematical and numerical checks

The work ledgers are accounting bounds, not a new proof that every existing fitter has a global maximum. Existing certificate inequalities, monotonicity and face comparisons retain their family-specific scope. The certificate convention remains exact arithmetic conditional on computed comparisons, with the dossier's practical floating-point margin; outward-rounded interval arithmetic is not claimed.

For CMB, the exact score in canonical log odds is `sum(x) - n E[X]`, whose magnitude is at most `n N`. Consequently, converting canonical odds to represented probability changes log likelihood by at most `n N |delta_eta|`. Candidate bounds now include that coordinate-rounding allowance plus a practical floating-point pad; held probability is preserved exactly. Normalizer cells and nested mean matches consume one shared allowance. This removes the former solver-count-dependent rescue reservation without accepting an unchecked mean match. Face/CMP caveats below remain explicit.

The beta held-shape bracket uses the analytic inequality `psi'(z) <= 1/z + 1/z^2 <= 2/z` for `z >= 1`. Integrating gives `psi(b+a)-psi(b) <= 2a/b` for `b >= 1`; thus `max(1,2a/T)` supplies an upper bracket for positive target `T`, conditional on represented endpoint signs. This preserves the protected tiny-complement regression with a root near 1e100 under the ordinary iteration default instead of requiring hundreds of doublings. The endpoint arithmetic is checked rather than treating an overflowed bracket as a root.

Transformed-gamma inner shape proposals require represented relative equation residual at most 1e-12. An unresolved inner solve cannot silently fall back to a finite face. GB2 stops on unresolved box expansion and checks representable progress; it does not repeat identical floating-point steps until the cap. Huber retains its former direct/recovery branch selection for ordinary workloads, while both solver paths receive the full caller allowance. The previous 16-million-observation heuristic is no longer an iteration cap. Exact-prefix regression checks protect the ordinary Huber path.

Regression tests cover the 64-bit maximum allowance, exhaustion and retry behavior, finite and empty witness lists, transformed/delegated witnesses, and ordinary fits. Student C++/R/JS tests force a certificate budget stop at iterlim 7 and succeed at 20; Skellam uses [-1e9,0,1e9] to force a budget stop. CMB native/JS tests cross the one-iteration cell allowance at size 100001; R uses one free coordinate to enter the fitter rather than its fully held density shortcut. BB tests distinguish preflight exhaustion from a search with retained candidates. Solver-failure tests remain distinct from these budget tests.

## NB-family workload replay


The [NB-family certification report](2026-11-06-nbfam-cert-report.md)'s 89 workloads were replayed on the workstation with seed 1165, explicit iterlim 200 (the ZTNBinom/ZMNBinom/ZINB default), NumPy 2.4.6, and mpmath 1.3.0 at 60 decimal digits. [nbfam-final9.json](2026-11-07-budget-align-data/nbfam-final9.json) retains every result; [nbfam-probe.cpp](2026-11-07-budget-align-data/nbfam-probe.cpp) and [nbfam-check.py](2026-11-07-budget-align-data/nbfam-check.py) are the reproducible probe and oracle. There are **2 actual budget stops**, down from the original 11: 78 finite fits and 9 Poisson-face results complete. Both default stops use 25,599 counted evaluations. At iterlim 2000, poisson-0 stops numerically after 49,581 evaluations with `budget_exhausted=false`; edge remains a budget stop after 255,999. A generic unresolved status is therefore not counted as a budget stop.


The maximum parameter errors are 4.7779383e-07 and 3.6480721e-07; maximum likelihood error per observation is 1.7632164e-13. These meet 2e-6 parameter and 1e-7 likelihood-per-observation tolerances. 72 independent sampled score/derivative interval checks pass. These are numerical comparisons and sampled enclosure checks, not universal floating-point proofs. The retained held-pi example also has two separated maxima, so a single unimodal scalar search is not substituted for the cover.


## n = 100000 timing census

Seed 143 is used throughout. The census contains 272 cases in 68 families; 27 cases in 10 families remain censored. Each family has four constructed samples: repeated golden data, small seeded dithering, a near-face mixture, and a held-coordinate configuration (details and family-specific choices are in the probes). The supplementary solver-family probe keeps perturbations within the observed range. Discrete normal and discrete Weibull were replayed with rounded integer perturbations; the original fractional-input diagnostic runs remain retained in stress-solver-final11.log and are replaced by the valid retry measurements in the summary. Timing excludes slot waiting and compilation; queue-inclusive wall times are stored separately. Each fit runs in a subprocess, and a timed-out fit is terminated by the measurement harness; it is never reported as a completed budget stop.

The 32 certificate/enumeration-family measurements ran on simbox under nice and single-thread BLAS. The additional 36 solver-family measurements ran on the workstation under run4, also with single-thread BLAS. CMB was rerun at its final default of 2000. The earlier probe's other 31 family headers are byte-identical to the final headers; phase8-equivalence.json verifies the sole intervening numerical-source difference was CMB's default. These machines' times should not be treated as a controlled speed comparison.


[stress-summary.csv](2026-11-07-budget-align-data/stress-summary.csv) and [stress-combined.json](2026-11-07-budget-align-data/stress-combined.json) retain the complete census; [stress.cpp](2026-11-07-budget-align-data/stress.cpp), [stress-solver.cpp](2026-11-07-budget-align-data/stress-solver.cpp), [stress-run.py](2026-11-07-budget-align-data/stress-run.py) and [stress-extended.py](2026-11-07-budget-align-data/stress-extended.py) retain constructions and limits. [stress-solver-phase11.cpp](2026-11-07-budget-align-data/stress-solver-phase11.cpp) preserves the exact original supplementary probe; [phase8-equivalence.json](2026-11-07-budget-align-data/phase8-equivalence.json) records the numerical-source equivalence check. “None observed” means these four constructions did not produce a completed budget stop; it is not a worst-case bound. “Censored” leaves the eventual status and completion time open.


| Family | Slowest observed budget stop (s) | Longest completed fit (s) | Unresolved timing modes |
|---|---|---|---|
| bbinom | None observed | 0.243 | — |
| beta | None observed | 0.001 | — |
| betapr | None observed | 6.100 | — |
| bhatt | None observed | 53.144 | 0>60s,1>60s,2>60s |
| binom | None observed | 0.001 | — |
| bnbinom | None observed | 0.432 | — |
| burr | 14.552 | 28.554 | — |
| cauchy | None observed | 0.131 | — |
| cmb | 24.336 | 24.336 | — |
| dgamma | None observed | 0.502 | — |
| dnorm | None observed | 0.002 | — |
| dweibull | None observed | 0.002 | — |
| emg | None observed | 44.667 | 0>60s,1>60s,2>60s |
| fatigue | None observed | 1.784 | — |
| foldnorm | None observed | 4.987 | — |
| fpareto | None observed | 44.566 | — |
| frechet | None observed | 0.006 | — |
| gamma | None observed | 0.001 | — |
| gb2 | None observed | 17.236 | — |
| ged | None observed | 22.864 | — |
| genbeta | None observed | 0.325 | — |
| gev | None observed | 32.424 | — |
| gompertz | None observed | 0.175 | — |
| gpd | None observed | 1.644 | — |
| gumbel | None observed | 0.194 | — |
| huber | None observed | 0.007 | — |
| invburr | 15.675 | 15.765 | — |
| invgamma | None observed | 0.002 | — |
| invparalogis | None observed | 0.462 | — |
| invpareto | None observed | 0.258 | — |
| invtrgamma | None observed | 1.255 | — |
| invweibull | None observed | 0.006 | — |
| kumar | None observed | 0.225 | — |
| lgamma | None observed | 0.001 | — |
| llogis | None observed | 0.096 | — |
| logis | None observed | 0.037 | — |
| lomax | None observed | 0.260 | — |
| lst | None observed | 0.067 | 1>180s |
| naka | None observed | 0.001 | — |
| nbinom | None observed | 0.319 | — |
| nsbeta | None observed | 0.001 | — |
| paralogis | None observed | 0.487 | — |
| pareto2 | None observed | 1.466 | — |
| pareto3 | None observed | 0.071 | — |
| pareto4 | 14.814 | 39.700 | — |
| pig | None observed | 0.000 | — |
| sged | None observed | — | 0>60s,1>60s,2>60s,3>60s |
| sgomp | None observed | 3.810 | — |
| skellam | None observed | 27.642 | — |
| slash | None observed | — | 0>180s,1>60s,2>60s,3>60s |
| sn | None observed | 38.722 | 2>60s,3>60s |
| sstd | None observed | 4.993 | 3>60s |
| st | None observed | — | 0>60s,1>60s,2>60s,3>60s |
| std | None observed | 0.066 | 1>180s |
| tlambda | None observed | — | 0>60s,1>60s,2>60s,3>60s |
| tnorm | None observed | 0.000 | — |
| trgamma | None observed | 0.842 | — |
| triang | None observed | 4.568 | — |
| weibull | None observed | 0.005 | — |
| yulesimon | None observed | 0.004 | — |
| zeta | None observed | 0.001 | — |
| zib | None observed | 0.008 | — |
| zinb | 1.565 | 1.565 | — |
| zipf | None observed | 0.000 | — |
| zmbinom | None observed | 0.008 | — |
| zmnbinom | 1.493 | 1.493 | — |
| ztbinom | None observed | 0.013 | — |
| ztnbinom | 2.515 | 2.515 | — |

Default budget stops exceed a few seconds for Burr (14.552 s), inverse Burr (15.675 s), Pareto IV (14.814 s), and CMB with nu held (24.336 s). No family-specific multiplier was tuned. Completed non-budget fits above three seconds and all censoring bounds are also exposed in the table. The Student, slash and skew-normal unresolved cases were given real timed runs, including 180-second extensions for the first censored case per family; they cannot be assigned a budget-stop time without completing those fits.


## Final validation

All commands below completed green on the workstation. Each was a separate foreground `/tmp/umvl-cpu/run4` invocation, with nice priority, BLAS/OpenMP pinned to one thread and temporary directories inside this worktree. The final-local.sh driver invokes each command sequentially and stops on failure. Earlier simbox and fallback runs are retained as supplementary evidence; the authoritative final runs below are local as requested. Generation was rerun after the final documentation corrections. No command remains running.


| Command | Result | Queue-inclusive seconds | Evidence |
|---|---|---|---|
| just cpp-test | 106 / 106 shards | 1606.0 | [cpp-dev-final11.json](2026-11-07-budget-align-data/cpp-dev-final11.json) / [cpp-dev-final11.log](2026-11-07-budget-align-data/cpp-dev-final11.log) |
| env UMVL_TEST_TIER=full just cpp-test | 127 / 127 shards | 205.2 | [cpp-full-final11.json](2026-11-07-budget-align-data/cpp-full-final11.json) / [cpp-full-final11.log](2026-11-07-budget-align-data/cpp-full-final11.log) |
| just cpp-test-no-long-double | 127 / 127 shards | 172.3 | [cpp-no-long-double-final11.json](2026-11-07-budget-align-data/cpp-no-long-double-final11.json) / [cpp-no-long-double-final11.log](2026-11-07-budget-align-data/cpp-no-long-double-final11.log) |
| just test | [ FAIL 0 \| WARN 0 \| SKIP 154 \| PASS 20877 ] | 2833.4 | [r-dev-final11.json](2026-11-07-budget-align-data/r-dev-final11.json) / [r-dev-final11.log](2026-11-07-budget-align-data/r-dev-final11.log) |
| just test-full | [ FAIL 0 \| WARN 0 \| SKIP 0 \| PASS 43697 ] | 446.3 | [r-full-final11.json](2026-11-07-budget-align-data/r-full-final11.json) / [r-full-final11.log](2026-11-07-budget-align-data/r-full-final11.log) |
| env EMCC_CORES=4 just js-build | green | 471.8 | [js-build-final11.json](2026-11-07-budget-align-data/js-build-final11.json) / [js-build-final11.log](2026-11-07-budget-align-data/js-build-final11.log) |
| just js-test | 436 passed; 0 failed | 298.0 | [js-test-final11.json](2026-11-07-budget-align-data/js-test-final11.json) / [js-test-final11.log](2026-11-07-budget-align-data/js-test-final11.log) |
| just site-check | green | 133.0 | [site-check-final11.json](2026-11-07-budget-align-data/site-check-final11.json) / [site-check-final11.log](2026-11-07-budget-align-data/site-check-final11.log) |
| just gen | green | 446.4 | [gen-final12.json](2026-11-07-budget-align-data/gen-final12.json) / [gen-final12.log](2026-11-07-budget-align-data/gen-final12.log) |
| just relations | 157 records; 0 failed | 193.1 | [relations-final12.json](2026-11-07-budget-align-data/relations-final12.json) / [relations-final12.log](2026-11-07-budget-align-data/relations-final12.log) |
| bash dev/research/evaluations/2026-11-07-budget-align-data/environment.sh | versions recorded | 324.6 | [environment-final12.json](2026-11-07-budget-align-data/environment-final12.json) / [environment-final12.log](2026-11-07-budget-align-data/environment-final12.log) |

The workstation uses GCC 15.2.0, Node 22.22.1, R 4.6.1, Rcpp 1.1.2, BH 1.90.0.1 and testthat 3.3.2. Simbox measurements use GCC 13.3.0 and WSL; R package compilations there use its configured g++15 toolchain. Full-tier tests include cases omitted from the dev tier; dev skips are tier selections, not failures. [validation.json](2026-11-07-budget-align-data/validation.json) records commands, machines and hashes; [source-sha256.txt](2026-11-07-budget-align-data/source-sha256.txt) records the final changed-source manifest. Existing compiler warnings are separate from R test warnings. `git diff --check` passes. The settled special/dpqr, truth, goldens, code generators, policy, NEWS, status, known-issues and roadmap files have no changes.


## Open entries and retained exceptions

These entries identify the concrete work that needs a separately authorized settled-evaluator change or a policy interpretation. They are not implemented by quietly altering the settled files.


| Path / family | Loop / cap retained | What is missing |
|---|---|---|
| cmp — special/cmp.hpp:295,382,406,488 | max_terms=1000000 per call; maxit; backtracking1024 → unchanged (protected) | Needs uint64 aggregate ledger, saturated solver control and distinct work/domain failure plus retained witnesses. |
| cmb — distributions/cmb/mle.hpp:603,837 | remaining/(2*(I+1)^2); budget/I → unchanged child reservation; widened arithmetic | Cannot replace per-call reservation while guaranteeing aggregate cap without CMP ledger API. |
| dgamma — distributions/dgamma/dpqr.hpp:14; special/special.hpp:441 | 100000 lower-tail / 1000000 upper-tail terms → unchanged (protected) | Likelihood calls protected gamma-tail series; evaluator needs budget/status propagation. |
| skellam — distributions/skellam/dpqr.hpp:49,51 | mode-sized lower sum; convergence-only upper sum → unchanged (protected) | Likelihood series needs caller ledger and checked exhaustion. Fixed polynomial terms are structural. |
| pig — distributions/pig/dpqr.hpp:13,64 | 200 asymptotic terms; mass recurrence through observed count → unchanged (protected) | Protected evaluator requires metering contract before changing recurrence or asymptotic truncation. |
| R fully held models — bindings/r/scripts/codegen_wrappers.R:275; bindings/r/R/frozen.R:15 | no fitter control → unchanged (protected generator/evaluators) | Decide whether pure evaluation is metered; then forward shared budget without changing held-model semantics. |

The CMP free-nu child reservation can decrease as iterlim rises: before prior work, the per-call allowance is at most 490 terms at iterlim 100 and 24 at 2000. Held-nu division gives 100,000 terms per iteration. Widening the reservation arithmetic prevents wasm overflow but does not fix this monotonicity defect. The protected kernel needs a consumable parent ledger, saturated iteration control, a portable 64-bit work counter, separate numerical-domain and exhaustion statuses, and witness propagation. Its WorkLimit also covers the mode-above-9e15 numerical refusal, so renaming every WorkLimit to budget would be incorrect.


Families without an iterlim argument, including hyper, nhyper, HT, truncated-binomial/Poisson, log-series, ZIP and ZTPoisson, retain their fixed caps under the explicit policy exception. The complete source-expression list is [no-iterlim-exceptions.csv](2026-11-07-budget-align-data/no-iterlim-exceptions.csv); HT keeps its 100000-profile certificate, and hyper/nhyper keep their shared 10000000-cell fixed grid ledger. Their original messages are retained because raising an absent iterlim control cannot resolve those stops; changing that interface requires a separate decision. Fixed approximation degree, a predetermined seed mesh, and supported numerical domains do not acquire a new meaning merely by replacing their constants with I: that could change formulas, initialization or the supported model. Their unchanged expressions are listed in [structural-loop-review.csv](2026-11-07-budget-align-data/structural-loop-review.csv). In particular, the held-scale transformed-gamma sign-change mesh, EMG log-concave-profile mesh and multi-start meshes retain their existing numerical scope; this task does not establish a missing global uniqueness/exhaustiveness theorem for those paths. For the held-scale transformed-gamma 720-point mesh (trgamma/mle.hpp), a stronger certification claim needs bounds excluding additional stationary points between mesh points and beyond the searched range; EMG logconcave_profile.hpp similarly needs an exhaustiveness argument for its 65 shape points. Merely replacing mesh cardinality by 128 I would change resolution without supplying those bounds. A uniform metering contract for such initialization/evaluator work is still an explicit policy-scope decision.


Timing remains open exactly for the censored family/mode entries in the census table: the retained input construction, seed, timeout and machine are sufficient to resume longer measurements. No finite runtime bound or eventual stop class is inferred from censoring. No observed budget stop for a family is not evidence that a slower one cannot be constructed. The four measured slow budget-stop families need a performance decision; the common multipliers were left unchanged as instructed.

Artifact-pane delivery was attempted after validation. The viewer returned HTTP 403 because this worktree is absent from its readable roots. The report remains at the requested worktree path; no files were copied outside it.
