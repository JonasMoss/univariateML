# Controls inventory and migration design for consolidation step 5c

2026-11-14. Inventory only; no code changed. Sources: `bindings/common/distributions/<id>/fit.hpp`
(the common adapter, the single place where C++ and binding defaults meet), `bindings/r/distributions/<id>/adapter.cpp`,
dossier front matter `r.control`, `bindings/r/scripts/codegen_wrappers.R`, `bindings/js/generate.py`,
`bindings/r/R/modifiers.R`. Collected by grep and a small script over all 85 families with `mle.hpp`.

Target ([accuracy contract, Controls](../../design/accuracy-contract.md#controls)):
`ml<id>(x, <held>, tol = 1e-10, iterlim = ...)`; no `reltol`, `abstol`, `gradtol`, `<param>0`; `zero`, `truncate` explicit formals.

## 1. Per-family table

Columns. **common/adapter controls**: what `fit_<id>` takes, with defaults (the R adapter has identical defaults for every
family; `generate.py` fails on a mismatch, so the two cannot drift). **dossier**: control-like keys in `r.control`
(holds are omitted; `codegen_wrappers.R` fails if `r.control` plus held coefficients differ from the adapter formals, so the
dossier always equals the adapter). **C++**: the fitter's own signature. **class**: see section 2.

Findings in the dossier column: it equals the adapter in all 85 families, with two quirks. `huber`'s `epsilon` is a
coefficient-like tuning constant held by the user (not a control; stays). `power`'s `epsilon` is a documented
"deprecated compatibility argument, validated but ignored" (remove). Start values `shape10 shape20 scale0 shape0 lambda0 a0`
exist in 10 families: betapr (3), burr (3), invburr (3), invparalogis, paralogis, invpareto, invweibull, kumar, lomax, weibull.
In `fit.hpp` they are `Held x0 = {}` (adapter: `Nullable<double> = R_NilValue`, except betapr, which uses plain `double = 0`).

| id | common/adapter controls | dossier controls | C++ fitter | class |
|---|---|---|---|---|
| bbinom | reltol=1e-10, iterlim=500 | reltol, iterlim | tol+maxit | A |
| beta | reltol=1.490116e-08, iterlim=100 | reltol, iterlim | tol+maxit | B-reltol |
| betapr | gradtol=1e-6, iterlim=2000, reltol=1e-13, abstol=1e-10 ; starts: shape10, shape20, scale0 | gradtol, reltol, abstol, shape10, shape20, scale0, iterlim | tol+maxit | D |
| bhatt | reltol=1e-8, iterlim=500 | reltol, iterlim | tol+maxit | B-reltol |
| binom | abstol=1.220703e-4, iterlim=100 | abstol, iterlim | tol+maxit | B-abstol |
| bnbinom | gradtol=1e-10, iterlim=600 | gradtol, iterlim | tol+maxit | A-grad |
| burr | gradtol=1e-8, iterlim=1000, reltol=1e-12, abstol=1.220703e-4 ; starts: shape10, shape20, scale0 | abstol, gradtol, iterlim, shape10, shape20, scale0, reltol | tol+maxit | D |
| cauchy | abstol=1.220703e-4, iterlim=100 | abstol, iterlim | tol+maxit | B-abstol |
| cmb | reltol=1e-10, iterlim=2000 | reltol, iterlim | tol+maxit | A |
| cmp | reltol=1e-10, iterlim=100 | reltol, iterlim | control struct | A |
| dgamma | reltol=1e-10, iterlim=400 | reltol, iterlim | tol+maxit | A |
| dlaplace | - | - | none (closed form) | Z |
| dnorm | reltol=1e-9, iterlim=200 | reltol, iterlim | tol+maxit | B-reltol |
| dunif | - | - | none (closed form) | Z |
| dweibull | reltol=1e-12, iterlim=200 | reltol, iterlim | tol+maxit | C-reltol |
| emg | reltol=1e-10, iterlim=500 | reltol, iterlim | tol+maxit | A |
| exp | - | - | none (closed form) | Z |
| expweibull | tol=1e-10, iterlim=100 | tol, iterlim | tol+maxit | A |
| fatigue | abstol=1e-10, iterlim=100 | abstol, iterlim | tol+maxit | A |
| foldnorm | reltol=1e-10, iterlim=200 | reltol, iterlim | tol+maxit | A |
| fpareto | reltol=1e-7, iterlim=1000 | reltol, iterlim | tol+maxit | B-reltol |
| frechet | reltol=1e-10, iterlim=100 | reltol, iterlim | tol+maxit | A |
| gamma | abstol=1.220703e-4, iterlim=100 | abstol, iterlim | tol+maxit | B-abstol |
| gb2 | gradtol=1e-10, iterlim=300 | gradtol, iterlim | tol+maxit | A-grad |
| ged | gradtol=1.220703e-4, iterlim=200 | gradtol, iterlim | tol+maxit | B-gradtol |
| genbeta | gradtol=1e-10, iterlim=300 | gradtol, iterlim | tol+maxit | A-grad |
| gengamma | tol=1e-10, iterlim=100 | tol, iterlim | tol+maxit | A |
| geom | - | - | none (closed form) | Z |
| gev | reltol=1e-10, iterlim=100 | reltol, iterlim | tol+maxit | A |
| gompertz | reltol=1.220703e-4, iterlim=100 | reltol, iterlim | tol+maxit | B-reltol |
| gpd | reltol=1e-12, iterlim=200 | reltol, iterlim | tol+maxit | C-reltol |
| gumbel | reltol=1e-10, iterlim=100 | reltol, iterlim | tol+maxit | A |
| hcauchy | - | - | none (closed form) | Z |
| hnorm | - | - | none (closed form) | Z |
| ht | - | - | none (closed form) | Z |
| huber | gradtol=1e-10, iterlim=200 | epsilon, gradtol, iterlim | tol+maxit | A-grad |
| hyper | - | - | none (closed form) | Z |
| invburr | gradtol=1e-8, iterlim=1000, reltol=1e-12, abstol=1.220703e-4 ; starts: shape10, shape20, scale0 | abstol, gradtol, iterlim, shape10, shape20, scale0, reltol | tol+maxit | D |
| invexp | - | - | none (closed form) | Z |
| invgamma | abstol=1.220703e-4, iterlim=100 | abstol, iterlim | tol+maxit | B-abstol |
| invgauss | - | - | none (closed form) | Z |
| invparalogis | gradtol=1.220703e-4, iterlim=100 ; starts: shape0 | shape0, gradtol, iterlim | tol+maxit | B-gradtol |
| invpareto | reltol=1.220703e-4, iterlim=100 ; starts: scale0 | scale0, reltol, iterlim | tol+maxit | B-reltol |
| invtrgamma | reltol=1e-10, iterlim=100 | reltol, iterlim | tol+maxit | A |
| invweibull | reltol=1.220703e-4, iterlim=100 ; starts: shape0 | shape0, reltol, iterlim | tol+maxit | B-reltol |
| kumar | reltol=1e-10, iterlim=100 ; starts: a0 | a0, reltol, iterlim | tol+maxit | A |
| laplace | - | - | none (closed form) | Z |
| lgamma | abstol=1.220703e-4, iterlim=100 | abstol, iterlim | tol+maxit | B-abstol |
| lgser | - | - | none (closed form) | Z |
| llogis | abstol=1.220703e-4, iterlim=100 | abstol, iterlim | tol+maxit | B-abstol |
| lnorm | - | - | none (closed form) | Z |
| logis | abstol=1.220703e-4, iterlim=100 | abstol, iterlim | tol+maxit | B-abstol |
| logitnorm | - | - | none (closed form) | Z |
| lomax | reltol=1e-10, iterlim=100 ; starts: lambda0 | lambda0, reltol, iterlim | tol+maxit | A |
| lst | gradtol=1.220703e-4, iterlim=200 | gradtol, iterlim | tol+maxit | B-gradtol |
| naka | abstol=1.220703e-4, iterlim=100 | abstol, iterlim | tol+maxit | B-abstol |
| nbinom | reltol=1.220703e-4, iterlim=100 | reltol, iterlim | tol+maxit | B-reltol |
| nhyper | - | - | none (closed form) | Z |
| norm | - | - | none (closed form) | Z |
| nsbeta | reltol=1e-10, iterlim=1000 | reltol, iterlim | tol+maxit | A |
| paralogis | gradtol=1.220703e-4, iterlim=100 ; starts: shape0 | shape0, gradtol, iterlim | tol+maxit | B-gradtol |
| pareto | - | - | none (closed form) | Z |
| pareto2 | reltol=1e-10, iterlim=1000 | reltol, iterlim | tol+maxit | A |
| pareto3 | reltol=1e-10, iterlim=1000 | reltol, iterlim | tol+maxit | A |
| pareto4 | reltol=1e-10, iterlim=1000 | reltol, iterlim | tol+maxit | A |
| pig | reltol=1e-10, iterlim=200 | reltol, iterlim | tol+maxit | A |
| pois | - | - | none (closed form) | Z |
| power | epsilon=1.490116e-08 | epsilon | none | E |
| rayleigh | - | - | none (closed form) | Z |
| sged | gradtol=1e-10, iterlim=200 | gradtol, iterlim | tol+maxit | A-grad |
| sgomp | reltol=1e-10, iterlim=200 | reltol, iterlim | tol+maxit | A |
| skellam | reltol=1e-10, iterlim=150 | reltol, iterlim | tol+maxit | A |
| slash | reltol=1e-10, iterlim=500 | reltol, iterlim | tol+maxit | A |
| sn | reltol=1e-10, iterlim=100 | reltol, iterlim | tol+maxit | A |
| snorm | iterlim=200 | iterlim | check: iterlim only | E |
| sstd | gradtol=1e-10, iterlim=200 | gradtol, iterlim | tol+maxit | A-grad |
| st | reltol=1e-10, iterlim=200 | reltol, iterlim | tol+maxit | A |
| std | gradtol=1.220703e-4, iterlim=200 | gradtol, iterlim | tol+maxit | B-gradtol |
| tlambda | reltol=1e-7, iterlim=1000 | reltol, iterlim | tol+maxit | B-reltol |
| trgamma | reltol=1e-10, iterlim=100 | reltol, iterlim | tol+maxit | A |
| triang | reltol=1e-12, iterlim=100 | reltol, iterlim | tol+maxit | C-reltol |
| unif | - | - | none (closed form) | Z |
| weibull | reltol=1e-10, iterlim=100 ; starts: shape0 | shape0, reltol, iterlim | tol+maxit | A |
| yulesimon | reltol=1e-10, iterlim=200 | reltol, iterlim | tol+maxit | A |
| zeta | reltol=1e-10, iterlim=200 | reltol, iterlim | tol+maxit | A |

C++ column: "tol+maxit" = the fitter has `tol` and `maxit` (62 families); "none (closed form)" = no iteration (the 20 class Z families); 62 + 20 + `cmp`, `snorm`, `power` = 85; `cmp` takes a `CmpFitControl` struct (reltol is copied in by the
adapter); `snorm` has only an `iterlim` in the adapter (fit.hpp has no tolerance; its C++ `tol` is internal; needs a look in
the migration job); `power` has no iteration (adapter `epsilon` is ignored).

`iterlim` defaults today: 100 (29 families), 200 (16), 1000 (8), 500 (4), 2000 (2), 300 (2), 600, 400, 150 (one each),
none (21). The contract text says `iterlim = 100`; a different default is a change in budget, not in the stopping rule.
Decision needed: keep per-family defaults (documented in the dossier, what the work-budget design assumes) or unify to 100.
Unifying lowers the budget of 42 families and can turn `Ok` into an exhausted-budget stop. I assume per-family defaults stay.

## 2. Families whose default fits change when `tol = 1e-10`

How `tol` enters today (grep over each family's headers; the rule is family-specific, so the migration job must read each
`mle.hpp` before swapping the default). Counted families, by mechanism:

| mechanism in C++ | examples | note |
|---|---|---|
| absolute step in `newton_raphson_1d_abs` or scalar Newton: `abs(step) < tol` | gamma, invgamma, lgamma, naka, llogis, logis, cauchy, kumar | default 1.220703e-4 absolute becomes relative 1e-10: a different and much stricter rule; the contract requires the stopping rule to certify relative accuracy, so these are rewritten, not only re-defaulted |
| relative-plus-absolute step `abs(step) < abs(x)*tol + tol` | gompertz, nbinom, kumar | rule already mixes both; becomes 1e-10 both |
| BFGS `grad_tol = tol` (norm of gradient) | betapr, burr, invburr, ged, lst, paralogis, std, sged, sstd | gradient norm is not a parameter error; the contract rejects it alone. Changing the default to 1e-10 would exceed double-precision gradient noise on finite-difference BFGS, so these need the Newton-step estimate (or fail with the tolerance/conditioning stop) |
| internal cap `min(tol, 1e-10)` or floor `max(tol, 1e-14)` | beta, bhatt, burr, cmb, fatigue, foldnorm, gamma, genbeta, gumbel, lomax, nbinom, paralogis, slash, weibull and others (about 30 files) | the user's `tol` is already clipped to 1e-10 internally, so the default change is mostly a no-op; only the user-visible knob changes |
| ignores `tol` (`(void)tol`) | binom, betapr (some routes) | exact enumeration; defaults change nothing |

Grouped by kind of change (class labels in the table):

| class | families | default today | what changes for a default fit |
|---|---|---|---|
| A, A-grad: same value, new name | bbinom, cmb, cmp, dgamma, emg, expweibull, fatigue, foldnorm, frechet, gengamma, gev, gumbel, invtrgamma, kumar, lomax, nsbeta, pareto2, pareto3, pareto4, pig, sgomp, skellam, slash, sn, st, trgamma, weibull, yulesimon, zeta (29, `reltol`/`tol`/`abstol` = 1e-10); bnbinom, gb2, genbeta, huber, sged, sstd (6, `gradtol` = 1e-10) | 1e-10 | rename only; no change in default fits expected (fatigue is `abstol`, absolute meaning to check). `gradtol` ones: the meaning changes from gradient norm to parameter accuracy, results may move at the 1e-10 level where the stop was gradient-based |
| B-abstol: absolute step 1.220703e-4 | binom (ignored), cauchy, gamma, invgamma, lgamma, llogis, logis, naka | 1.220703e-4 abs | stricter and relative; fits move by up to about 1e-4 absolute where the old rule stopped early; new work: more Newton steps, budget stops possible near iterlim = 100 |
| B-reltol: loose relative | beta (1.490116e-08), bhatt (1e-8), dnorm (1e-9), fpareto (1e-7), gompertz (1.220703e-4), invpareto (1.220703e-4), invweibull (1.220703e-4), nbinom (1.220703e-4), tlambda (1e-7) | listed | stricter; same remark as above. gompertz, nbinom, invpareto, invweibull: today's default is the CRAN `1.220703e-4` |
| B-gradtol: loose gradient | ged, lst, std, invparalogis, paralogis (1.220703e-4) | 1.220703e-4 on gradient norm | stricter and a different quantity; BFGS rewrite (see mechanism table) |
| C-reltol: tighter than 1e-10 | dweibull (1e-12), gpd (1e-12), triang (1e-12) | 1e-12 | relaxed to 1e-10: fits may move by up to 1e-10 relative; the claim "within 1e-10" still holds, but goldens and reference comparisons at 1e-12 will show diffs |
| D: several tolerances | betapr (gradtol 1e-6, reltol 1e-13, abstol 1e-10), burr and invburr (gradtol 1e-8, reltol 1e-12, abstol 1.220703e-4) | three knobs | collapse to one; burr and invburr go from a loose abstol on a one-dimensional route and 1e-8 gradient on BFGS; the `mle.hpp` of each reads three tolerances (7 mentions in burr) |
| E: no tolerance today | snorm (iterlim only), power (`epsilon`, ignored) | - | snorm gains `tol` (the root-isolation rule must be given a relative bound); power drops `epsilon` |
| Z: closed form or exact, no control | dlaplace, dunif, exp, geom, hcauchy, hnorm, ht, hyper, invexp, invgauss, laplace, lgser, lnorm, logitnorm, nhyper, norm, pareto, pois, rayleigh, unif (20) | no controls | no fit changes; they gain `tol`/`iterlim` formals only if the R signature must be uniform. Recommendation: accept and ignore (documented "closed form: ignored"), since `model_select(..., tol = )` and `ml_check_args` then work uniformly; alternative: leave them out. Needs Jonas's decision |

The family count that can change numerically: 8 + 9 + 5 + 3 + 3 + 1 (snorm) = 29 families plus `gradtol`-meaning change for 6, i.e.
up to 35. Class A needs only the fit snapshot to be identical.

## 3. Where each piece lives

| piece | location | what must change |
|---|---|---|
| dossier controls | `r.control` in 85 `README.md`; schema text in `README_SCHEMA.md` lines 54-55 (names `reltol abstol gradtol iterlim <param>0`) | rename keys to `tol`, drop `<param>0`; schema text |
| C++ fitters | `distributions/<id>/mle.hpp` (13 files use the old names, 29 mentions; 19 files mention start values, 114 mentions: burr 18, dgamma 13, lomax 12, kumar 12, paralogis 10) | `tol` default and semantics (section 2); remove start parameters where they exist |
| shared C++ | `core/newton.hpp` (`reltol` in the relative-step helper), `core/bfgs.hpp` (`grad_tol` doc), `univariateML-cpp/tests/test_support.hpp` (`kFitTol = 1.220703e-4`, used 67 times in 25 `test.cpp`) | helper naming; test tolerance constants |
| common adapters | `bindings/common/distributions/<id>/fit.hpp` (68 files, 225 mentions); modifier routes `*_fit.hpp` (15: tbinom, tnorm, tpois, zib, zinb, zip, zm*, zt*); `bindings/common/modifiers.hpp`, `fit.hpp` | rename formals; remove starts; the modifier routes keep their own `reltol` name today |
| R adapters | `bindings/r/distributions/<id>/adapter.cpp` (61 files, 127 mentions; start values in 10); `bindings/r/modifier-adapters/*.cpp` (15, `reltol`) | formals `tol`, `iterlim`; adapter formals order is arbitrary today (control before or after holds) |
| R wrapper generator | `bindings/r/scripts/codegen_wrappers.R` (`check_control`, `gen_doc` control items, `gen_wrapper`), `load_distributions.R` line 359 (`control = f$r$control`) | see section 4 |
| R conversion | `bindings/r/R/frozen.R` line 24 (`c("reltol","abstol","gradtol","iterlim")`), `modifiers.R` (`modifier_controls`, `reltol`/`abstol` validation lines 68-73, `tol <- dots$reltol ... abstol`), `code_helpers.R` `ml_check_args` (allowed set = adapter formals), `code_misc.R` `newton_raphson_1d` (R-internal, `reltol` default `eps^0.25`; check whether anything still calls it), `outcomes.R` (classes only) | rename; `modifiers.R` validation |
| model_select, bootstrapml | `code_model_select.R` passes `...` to every fitter (a family without the control would error; closed forms must accept `tol`/`iterlim` for `model_select(x, tol = 1e-8)` to work) | decision on class Z |
| JS | `bindings/js/generate.py` (control names from `fit.hpp` formals, checked against dossier `r.control` and adapter defaults; emits `generated/fits.hpp` with `control(c,"reltol",...)` and `family-types.d.ts`: 61 control mentions, 10 starts), `bindings.cpp` (`control()` helper), `index.mjs` (zero/truncate are keys of the `control` object, lines 53-60), `modifiers.mjs` (lines 132-138 validate and read `reltol`/`abstol`), `index.d.ts` (`ModifierControls { reltol?, iterlim? }`), `modifier-test-routes.mjs`, `modifier-metadata.mjs` | JS already routes `zero`/`truncate` through the `control` object; rename `reltol` to `tol` in the d.ts and `modifiers.mjs`; generated files regenerate |
| modifier metadata | `bindings/common/modifier-laws.json` (8 control entries), `bindings/r/R/modifier_law_metadata.R`, `codegen_metadata.R` | rename keys |
| site | `site/scripts/data.py` line 47 (placeholder controls `tol reltol abstol gradtol epsilon` = 1e-8) | drop old names |
| docs | `AGENTS.md` (2 mentions), `README_SCHEMA.md`, `dev/design/modifiers.md` ("Existing controls retain their names ... `reltol` and `iterlim`", "named entries in `...`"), `dev/design/accuracy-contract.md`, `dev/roadmap/2026-10-28-cran-argument-names.md` (decision D3, superseded; 26 mentions), `dev/roadmap/decisions.md`, `bindings/r/NEWS.md`, `bindings/r/vignettes/*.Rmd` (no mentions of the old names), `bindings/js/README.md` | NEWS entry for 2.0; schema; modifiers design text; history files in `dev/research/` (about 50 files) are not edited |
| validation tools | `dev/validation/diagnostics-audit.cpp` (137 mentions: it carries its own copy of every `route_<id>` signature with `reltol`, `gradtol`), `beta-free-fit-check.py/.cpp`, `fit-snapshot.py` (builds on `fit_<id>(x, Held...)` and default controls only; unaffected by renames) | the audit copies must follow the signatures |

### Tests passing old controls (counts of `reltol|abstol|gradtol`; start values separately)

| area | files | mentions | notes |
|---|---|---|---|
| `bindings/r/distributions/<id>/test.R` | 71 | 422 (reltol 280, gradtol 86, abstol 62 over all R test.R) | top: sged 41, triang 38, invweibull 21, invburr 21, gev 18, naka 13, llogis 13, lomax 11, bhatt 11, sstd 10 |
| start values in `test.R` | 8 | 25 | kumar 5, lomax 4, burr 4, paralogis 3, invburr 3, weibull 2, invweibull 2, betapr 2; also `testthat/test_argument_names.R` (2: a regression that `mlweibull(x, shape = 2)` is not a start `shape0`; keep as "unknown argument" check) |
| `bindings/r/modifier-tests/*.R` | 14 | 28 | zib 6, ztbinom 4, zmbinom 3, others 1-2 |
| `bindings/r/tests/testthat/` | 4 | 10 | `test_modifiers.R` 6, `test_argument_names.R` 2, `test-outcomes.R` 1, `test-stops-cont.R` 1 |
| JS | `test.mjs` 22, `stop-test.mjs` 10, `fit-test.mjs` 3, `family-types.d.ts` (generated) 61 | 35 + generated | iterlim and gradtol budget stops, e.g. `{gradtol:1e-8, iterlim:1}` |
| C++ | `test.cpp` use `kFitTol` (67 uses, 25 files); `test_support.hpp` 3 | 70 | `golden.json` files pass no controls |
| `dev/validation/diagnostics-audit.cpp` | 1 | 137 | not a test; an audit tool |

## 4. Explicit `zero` and `truncate`

Today (`codegen_wrappers.R`, `gen_wrapper`): every wrapper is `mlX <- function(x, na.rm = FALSE, ...)`. The body collects
`modifier_args <- list(...)`, and routes to `ml_modified_call(...)` if `"zero" %in% names(modifier_args)` (only emitted for
families with `modifier_eligibility$zero`: 11 integer bases plus lgser), or `"truncate" %in% names(modifier_args)`, or `x` is a
two-column matrix or `Surv`. It then pulls `zero` (default `"none"`) and `truncate` out of the list, removes them from `...`,
and calls `ml_modified_call(name, x, missing(x), na.rm, modifier_args, zero, truncate, match.call())`. The documentation
lists `zero` and `truncate` as items of `@param ...` (`control_items`). `ml_check_args(mlX_, list(...), "mlX")` runs after
this and rejects names not in the adapter's formals (so ineligible `zero` falls to "no argument `zero`").

`modifiers.R`: `ml_modified_call` already takes `zero` and `truncate` as separate arguments; it parses `dots` only for
holds (`meta$names`, `p0`, `pi`) and controls (`modifier_controls`: `reltol`, `iterlim`, `abstol` checks at lines 66-73; line 98
`tol <- reltol else abstol else 1e-10`). `frozen.R` line 24 lists the control names. JS is already explicit
(`control.zero`, `control.truncate`).

What changes:

- generator: add `zero = "none"` (eligible families only) and `truncate = NULL` after `...`, replace the `list(...)`
  sniffing with `!identical(zero, "none") || !is.null(truncate) || interval x`, document them as separate `@param`s, and drop
  the two `control_items` lines.
- `modifiers.R`: validation of `zero`, `truncate` stays (move the `match.arg`-like check next to the wrapper); the
  `modifier_controls` and the `reltol`/`abstol` branch become `tol`; the message "Modifier holds and controls must be named"
  remains for `...`.
- `ml_check_args` is unchanged (formals after `...` do not appear in `list(...)`); no partial matching, since formals after `...` need exact names.
- positional use after `...` is impossible; `mlpois(x, TRUE, ...)` still means `na.rm`.

R signature of mlpois, proposed (minimal; holds and `tol`, `iterlim` stay in `...`, as the dossier documents them):

```r
mlpois <- function(x, na.rm = FALSE, ..., zero = "none", truncate = NULL)
```

with `lambda`, `p0`, `pi`, `tol`, `iterlim` named in `...`. Non-zero-eligible families get
`function(x, na.rm = FALSE, ..., truncate = NULL)`. Alternative (fully explicit, as the contract text
`ml<id>(x, <held coefficients>, tol, iterlim)` suggests): `mlpois(x, na.rm = FALSE, lambda = NULL, tol = 1e-10, iterlim = 100,
zero = "none", truncate = NULL, p0 = NULL, pi = NULL)`; this makes holds visible in `args()` but needs the generator to know
which coefficients and zero atoms each family holds and breaks `ml_check_args`' role. Recommended: the minimal signature now,
the fully explicit one as a later job. Decision for Jonas.

## 5. Proposed jobs (non-overlapping file ownership)

Common contract fixed first (no code): C++ and adapter name `tol` then `iterlim` after the holds; start values removed;
the generators read names from files, so each family can migrate on its own.

| job | owns | does | checks |
|---|---|---|---|
| 1. rename-only families (class A, A-grad, Z, 55 families) | per family: `README.md` front matter, `mle.hpp` (where a start or `reltol` name exists), `bindings/common/distributions/<id>/fit.hpp`, `bindings/r/distributions/<id>/adapter.cpp`, `test.R`, `test.cpp` | rename `reltol`/`gradtol`/`abstol` to `tol`; remove starts in kumar, lomax, weibull; class Z gains `tol`/`iterlim` if the decision says so | `fit-snapshot.py --diff` against `2026-11-14-fit-snapshot-data`: must be empty for A (no fits change); for A-grad, diffs only where a gradient stop was in force, each inspected; `just test-changed`; `just regen` and the codegen consistency checks (adapter equals `r.control`); `just cpp-test` |
| 2. families whose default changes (B, C, D, E, 30 families) | the same per-family files | new defaults with the stopping rule justified relative to `tol` (section 2 mechanisms); collapse burr, invburr and betapr to one tolerance; remove starts in betapr, burr, invburr, invparalogis, invpareto, invweibull, paralogis; snorm gains `tol`; power drops `epsilon`; dossier text; regenerate goldens only after inspecting diffs | snapshot `--diff` per family with the diff listed in the evidence file (expected moves of the size in section 2; any case that turns `Ok` into a stop is a finding); `just quality <ids>`; reference MLEs via `just golden-reference <id>` (50-digit) for classes C and B; `just test-changed` |
| 3. R infrastructure and modifiers | `bindings/r/scripts/codegen_wrappers.R`, `load_distributions.R`, `bindings/r/R/{modifiers,frozen,code_helpers,code_misc,code_model_select}.R`, `bindings/r/R/modifier_law_metadata.R`, `bindings/r/modifier-adapters/*`, `modifier-tests/*`, `bindings/common/distributions/*/{z*,t*}_fit.hpp`, `bindings/common/modifier-laws.json`, `bindings/common/{fit,modifiers}.hpp`, `bindings/r/tests/testthat/*`, `bindings/r/NEWS.md`, vignettes | explicit `zero`/`truncate` formals; `tol` for modifier routes (they pass `reltol` today); `ml_check_args`; frozen controls list; testthat updates | `just regen`, then `just test` (R tier) and the modifier tests; fit snapshot with `--modifier-routes` against `routes-native` and `routes-double`: must be identical (it calls the C++ route directly) |
| 4. JS, docs, tools | `bindings/js/*` (including generated `family-types.d.ts`), `site/scripts/data.py`, `AGENTS.md`, `README_SCHEMA.md`, `dev/design/{modifiers,accuracy-contract,consolidation}.md`, `dev/roadmap/*`, `dev/validation/diagnostics-audit.cpp`, `univariateML-cpp/tests/test_support.hpp` (`kFitTol` becomes `1e-10`-based only after job 2 lands) | rename in the facade, d.ts and tests; schema text; the design docs; audit signature copies | JS test suite (`bindings/js/run-tests.py`), `python3 dev/tools/check_links.py`, `generate.py` consistency (it errors on mismatch between `fit.hpp` and adapter defaults) |

Order and conflicts. Jobs 1, 2 and 3 are independent at the file level (modifier route headers in family folders are
assigned to job 3, not to the family jobs: `norm/tnorm_fit.hpp`, `pois/*`, `binom/*`, `nbinom/*`, `geom/*`, `lgser/*`). Job 4
owns the places that read all of them (generators in `bindings/js`, the audit copy) and should merge last, with its tests run after 1 to 3.
Classes A and Z are the safe first merge. Job 2 changes numbers and should be split per class if too large (B-abstol and
B-gradtol are the risky ones; each can be its own branch, with distinct family sets).

Open decisions (assumptions made above): (a) per-family `iterlim` defaults stay; (b) closed-form fitters accept and ignore
`tol` and `iterlim`; (c) minimal `...` plus `zero`/`truncate` signature; (d) fatigue's `abstol = 1e-10` is treated as class A
pending a check of its stopping rule.
