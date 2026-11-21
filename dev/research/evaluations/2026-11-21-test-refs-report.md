# Independent references for five C++ fit tests

TASK-150.6, 2026-11-21. Baseline `67ff1d73`.

The four seeded-outlier cases and the three-observation Cauchy case pass at the reference tolerance in both workstation builds. Across their ten coefficient comparisons, the largest relative error against the rounded references was `2.88e-15` in each build.

## Reference calculation

The probe exports the observations used by C++ as hexadecimal binary64 values. It loads the four outlier fixtures through the test loader and reads the three-observation declaration from the Cauchy test. The mpmath calculation solves the likelihood scores in location and log-scale coordinates using those exact binary64 inputs. For log-logistic it solves the logistic scores on the high-precision log observations, then transforms the result to shape and reciprocal scale before rounding. Separate 70- and 100-digit solves produced the same binary64 values for all ten coefficients. These score roots and the precision comparison provide numerical evidence. [Earlier tolerance study](2026-11-20-ctrl-js-report.md#c-test-tolerance), [reproduction script](2026-11-21-test-refs-data/fit-references.py), [native measurements](2026-11-21-test-refs-data/native.csv), [double-only measurements](2026-11-21-test-refs-data/double.csv).

From the worktree root, reproduce both CSVs with `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-21-test-refs-data/fit-references.py`.

## Changed comparisons

Every row used nominal doctest epsilon `1e-4` with the default unit scale. Every row now uses `mltest::kFitReferenceTol` (`1.0000011102230248e-10`) with `.scale(0)`. The measured errors below divide the absolute difference by the absolute rounded reference. Paths are relative to `univariateML-cpp/include/cppunivariate/distributions/`. The four seeded-outlier cases store their references in their respective `fixture-outlier.json` files. The three-point values are C++ literals.

| Assertion | Coefficient | Old value | New binary64 reference | Native error | Double-only error |
|---|---|---:|---:|---:|---:|
| `cauchy/test.cpp:25` | Outlier location | `-0.10763969158718215` | `-0.10763995599296099` | `1.29e-16` | `1.29e-16` |
| `cauchy/test.cpp:26` | Outlier scale | `1.095846270988029` | `1.0958464877966498` | 0 | 0 |
| `cauchy/test.cpp:36` | Three-point location | `-2.369470147` | `-2.3694702471283882` | `1.87e-16` | `1.87e-16` |
| `cauchy/test.cpp:37` | Three-point scale | `0.272740500` | `0.27274197625455088` | 0 | 0 |
| `gumbel/test.cpp:44` | Location | `0.51315461590301448` | `0.51315340083467331` | `1.08e-15` | `1.08e-15` |
| `gumbel/test.cpp:45` | Scale | `5.2452414775139751` | `5.2452412433113649` | `2.88e-15` | `2.88e-15` |
| `llogis/test.cpp:39` | Shape | `2.0711285238630479` | `2.0711284767861744` | `6.43e-16` | `6.43e-16` |
| `llogis/test.cpp:40` | Reciprocal scale | `1.0805730872851835` | `1.0805730234767765` | 0 | 0 |
| `logis/test.cpp:24` | Location | `0.24676943769340498` | `0.24676819243505332` | `1.12e-16` | `1.12e-16` |
| `logis/test.cpp:25` | Scale | `5.2535732389093095` | `5.2535735994085826` | 0 | 0 |

## Comment edits

- `distributions/gamma/test.cpp:13` describes the current `1e-10` coefficient request and the relative-step stopping rule.
- `distributions/invparalogis/test.cpp:7` describes the golden comparison in inverse-paralogistic coordinates.
- `core/bfgs.hpp:55` describes the objective-change backstop and the gradient tolerance in their own units.

## Other loose coefficient comparisons

I searched C++ doctest checks, including the modifier tests and shared tolerances, and the R family, modifier and shared tests. The inventory covers frozen coefficient references, known analytic values and stored generating parameters used in direct recovery checks. It includes default numeric comparisons and scale offsets that allow more than `1e-8` relative.

### C++

Family paths are relative to `univariateML-cpp/include/cppunivariate/distributions/`. Paths beginning with `tests/` are relative to `univariateML-cpp/`. Default doctest `Approx` uses epsilon `100 * epsilon(float)` (about `1.19e-5`) and adds a unit scale unless overridden. The table gives the nominal epsilon for those mixed absolute and relative comparisons.

| File and line | Fitted coefficient and stored comparison | Tolerance |
|---|---|---:|
| `bbinom/test.cpp:399–400,420–421` | Alpha and beta against 25000 and 18749.5 in the concentrated cases | `1e-7`, `1e-6` |
| `betapr/test.cpp:264–265,290–291,355–356` | Beta shape and scale against two-point, bounded-ratio and upper-face references | `1e-6` |
| `bhatt/test.cpp:112–114,116–118` | Location, scale and smoothing width against golden coefficients | `kBhattGoldenTol` (double-width `2e-6`, WASM `5e-7`) |
| `bhatt/test.cpp:273` | Held-scale location root against a fixed high-precision value | `1e-8` plus unit scale |
| `bhatt/test.cpp:636,643,650,657,664,669,671,676,678,685,695,702,709,716,723,730` | Small smoothing widths or locations against stored literals, with unit `Approx` scale | `1e-10` plus the unit scale gives more than `1e-8` relative |
| `bnbinom/test.cpp:252,277,291–292,314` | Held-subset coefficients against stored roots or the multiplicity formula | `1e-8` plus unit scale |
| `bnbinom/test.cpp:694–696` | Legacy-route size and both shapes against frozen coefficients | `2e-6` |
| `cauchy/test.cpp:246` | Absolute location of each tied maximum against `sqrt(3)` | `1e-7` |
| `cmb/test.cpp:270` | Shape against the fixed-probability formula `log(18)/log(2)` | Default |
| `cmp/test.cpp:53` | Geometric-face rate against `2.5/3.5` | Default |
| `dlaplace/test.cpp:9` | Scale against the closed-form expression in the mean absolute deviation 1.75 | Default |
| `exp/test.cpp:184` | Rate divided by `1e-308` against one | Default |
| `expweibull/test.cpp:238–240` | Scale, shape and exponent against exponentiated reference log coefficients | `1e-6` |
| `foldnorm/test.cpp:29,32` | Scale against `sqrt(.2)` and 2 | Default |
| `gamma/test.cpp:77,85` | Scale against one with shape held | Default |
| `gb2/test.cpp:191–192,194,447–448` | Free shapes and scale against the Burr nested golden, and tail shapes against fixed literals | `kBurrGoldenTol` (`1e-6`), `1e-7` |
| `ged/test.cpp:414–416,474–475` | Location, scale and shape against conditional fixtures, and tied-candidate shapes against stored roots | `1e-5` |
| `genbeta/test.cpp:578` | Shape against 0.00628923008455203 | `1e-10` plus unit scale |
| `genbeta/test.cpp:90,440–442` | Ridge shape product against its analytic value, and three shapes against a conditional golden | Default, `1e-5` |
| `gengamma/test.cpp:308` | Join location against 0.5 | Default |
| `gev/test.cpp:922–923` | Location and scale against 2 and 1 | Default |
| `gpd/test.cpp:45` | Scale against 1.58 for the exponential submodel | Default |
| `hnorm/test.cpp:17,46–47` | Scale against 2.5, `sqrt(5/3)` and 3 | Default |
| `ht/test.cpp:183` | Degrees of freedom against 0.41508 | `1e-3` |
| `huber/test.cpp:180–181,546` | Cutoff and scale against generating values, and near-tie scale against `3.889e-19` | `.02`, `1e-3` |
| `invburr/test.cpp:223–225` | Both shapes and scale against fixed asymmetric-interior references | `1e-6` |
| `invtrgamma/test.cpp:114` | Scale against `sqrt(3)/2` | Default |
| `kumar/test.cpp:58` | First shape against 0.007765975729959528 | `1e-8` plus unit scale |
| `laplace/test.cpp:40–41` | Seeded outlier location and scale against stored fixture estimates | `1e-4` |
| `lnorm/test.cpp:22–23,92` | Seeded outlier log mean and log SD against fixture estimates, and large-input log SD against `1e300` | `1e-4`, default |
| `lomax/test.cpp:63–64` | Rate and shape against near-exponential reference arrays | `5e-6` |
| `lst/test.cpp:132,342` | Fitted scale against `sqrt(2.748)` and tied locations against `±sqrt(6)` | `1e-8` plus unit scale |
| `lst/test.cpp:350,497,521,541` | Location against fixed literals, normalized scale against `sqrt(2)`, and degrees of freedom against a finite reference | `1e-6` or `1e-7` |
| `norm/test.cpp:21–22` | Seeded outlier mean and SD against stored fixture estimates | `1e-4` |
| `pareto3/test.cpp:86` | Scale against 2 | Default |
| `pig/test.cpp:40,64,447` | Shape against 0.036799 and a conditional golden, and mean against 1.5 | `1e-4`, `1e-7`, `1e-6` |
| `sged/test.cpp:696` | Asymmetric held-skew fit shape against 3.12977540545 | `2e-8` |
| `sgomp/test.cpp:21` | Exponential-face rate against `5/11` | Default |
| `skellam/test.cpp:27` | Fitted mean difference against 1.125 | Default |
| `std/test.cpp:206,295–296` | Fitted scale against `sqrt(2.748)`, and tied locations and scale against `±sqrt(8)` and 1 | `1e-8` plus unit scale, default |
| `std/test.cpp:280–281,285,326,353` | Degrees of freedom, scale and location against finite-sample references | `1e-7` |
| `sstd/test.cpp:365–366,377` | Tied locations against `±sqrt(3)` and log scale against 5.894765235143508 | `1e-8` plus unit scale |
| `st/test.cpp:178` | Slant against 0.16456272666935215 | `1e-8` plus unit scale |
| `tlambda/test.cpp:202–203` | Tied fitted locations against `±5/12` | Default |
| `trgamma/test.cpp:284` | Fixed-shape scale against `2/sqrt(3)` | Default |
| `triang/test.cpp:23,105,136,141` | Fitted support endpoints against fixed endpoint references | Default |
| `weibull/test.cpp:102` | Fitted scale against 4 | Default |
| `tests/test_cmp.cpp:155–156` | Shape and rate against the generating Poisson parameters | `.002` |
| `tests/modifiers/tnorm.cpp:82–83` | Mean and SD against generating parameters for quantile samples | `.1` with scale offset |
| `tests/modifiers/zinb.cpp:170` | Fitted probability against the stored held-size references | `1e-8` plus unit scale |
| `tests/modifiers/zinb.cpp:201–202,221` | Probability, inflation and size against fixture references | `2e-7` |
| `tests/modifiers/zip.cpp:39–40` | Rate and inflation against stored arrays | `1e-4` |

### R

Family paths are relative to `bindings/r/distributions/`. Paths beginning with `modifier-tests/` or `tests/` are relative to `bindings/r/`. Numeric `expect_equal` defaults to `sqrt(epsilon(double))` (about `1.49e-8`) in this test suite. Its vector comparison can aggregate the errors across coefficients.

| File and line | Fitted coefficient and stored comparison | Tolerance |
|---|---|---:|
| `bbinom/test.R:35,122,149` | Beta shapes at the Bernoulli member and integer size on two finite grids | Default |
| `bbinom/test.R:167,181` | Alpha and beta against 25000 and 18749.5 | `1e-7`, `1e-6` |
| `bhatt/test.R:107,110,239,279,324` | Constant-sample location or smoothing width and attained normal-member coefficients | Default |
| `binom/test.R:24,34,79,264` | Free size and boundary probability against analytic or fixed integer values | Default |
| `bnbinom/test.R:280` | Size and both shapes against legacy-route values | `2e-6` |
| `cauchy/test.R:109,176,287` | One representative of balanced two-point maxima against `(1.5,.5)`, location against 2 on constant data, and tied locations against `±sqrt(3)` | Default, `1e-7` |
| `cmb/test.R:75–76,117,119,228,231,244,248,250–251,262,372,408–410` | Size, probability and dispersion against boundary or finite-grid values | Default |
| `cmp/test.R:99` | Geometric-face rate and shape against `(2.5/3.5,0)` | Default |
| `dlaplace/test.R:19` | Smallest tied integer median against −3 | Default |
| `dweibull/test.R:97,102` | Second shape against generating values 1.5 and 0.3 | `.02`, `.1` |
| `exp/test.R:35` | Scaled fitted rate against one | Default |
| `foldnorm/test.R:70,86–88,132` | Conditional location and scale against analytic values, and free location and scale against stored `mu` and `sqrt(v)` arrays | Default, `1e-5` |
| `frechet/test.R:237,251` | Restricted shape against one for two-point data | Default |
| `gb2/test.R:233,235` | Free shapes against fixed tail references in both orientations | `1e-7` |
| `ged/test.R:60,286,295,303–304` | Fitted Laplace-face shape or location against known constants | Default |
| `ged/test.R:359,511–512` | Shape against a held-subset fixture, and scale and shape against fallback references | `1e-5`, default |
| `genbeta/test.R:301,312` | Both routes’ coefficient vectors against the held-scale fixture | `1e-5` |
| `geom/test.R:4,74` | Probability against the mean-based expression and the all-zero boundary | Default |
| `gev/test.R:235,254,263,270` | Attained two-point or fixed-parameter coefficients against analytic values | Default |
| `gompertz/test.R:89` | Free rate against 1 for zero data at held shape | Default |
| `gumbel/test.R:150` | Constant-sample location against 2 with scale held | Default |
| `ht/test.R:99,107,115` | Fitted threshold against 2, 4 and 5 | Default |
| `ht/test.R:187` | Degrees of freedom against 0.41508 | `1e-3` |
| `huber/test.R:85` | Fitted cutoff against 0.8 | `.08` |
| `laplace/test.R:104–105` | Location and scale against constant-sample conditional values | Default |
| `lnorm/test.R:113` | Large-input log SD against `1e300` | Default |
| `logis/test.R:171` | Constant-sample location against 2 with scale held | Default |
| `lomax/test.R:114` | Rate and shape against near-exponential reference arrays | `5e-6` |
| `lst/test.R:149` | Conditional location against 2 for constant data | `1e-7` |
| `nhyper/test.R:25,46,116–117,227` | Free success counts, stopping count and population against finite-grid values | Default |
| `norm/test.R:59,113–114` | Scaled SD or conditional coefficients against analytic values | Default |
| `pig/test.R:8–9,125` | Mean or shape against zero-data member values | Default |
| `sged/test.R:647,1020` | Held-skew shape against 3.12977540545, and fallback location against a stored score root | `2e-8`, `10` relative |
| `sgomp/test.R:40–41,90` | Gompertz shape and rate against attained exponential or constant-data values | Default |
| `skellam/test.R:57–59,121–123,130–131` | Both rates against one-sided Poisson and zero boundaries | Default |
| `slash/test.R:90` | Constant-sample location against 2 with scale held | `1e-7` |
| `std/test.R:309,391–392` | Conditional location against 2, then degrees of freedom and location against finite Cauchy-side references | `1e-7`, `1e-6` |
| `tlambda/test.R:245` | Constant-sample conditional location against 2 | `1e-6` |
| `triang/test.R:52,60,62,79–80` | Fitted modes and endpoints against analytically known values | Default |
| `weibull/test.R:134` | Fitted scale against 4 with shape held | Default |
| `modifier-tests/tbinom.R:74,185,222` | Probability and free sizes against finite-support reference values | Default |
| `modifier-tests/zip.R:19–20` | Rate and inflation against stored arrays | `1e-4` relative |
| `modifier-tests/zmbinom.R:181` | Zero mass against 0.25 | Default |
| `modifier-tests/zmnbinom.R:181` | Zero mass against `1/3` | Default |
| `tests/testthat/test_golden_regression.R:254–265` | All Bhattacharjee coefficients against the golden estimates, using the entry at line 118 | `2e-6` |

The R analogues of the outlier and three-point checks compute their references during the test and apply a componentwise `1e-4` threshold. They compare fitted location and scale in `cauchy/test.R:87,99`, `gumbel/test.R:73`, `laplace/test.R:45` and `logis/test.R:106`, both fitted log-scale normal coefficients in `lnorm/test.R:42`, mean and SD in `norm/test.R:42`, and shape and scale in `llogis/test.R:55`.

## Verification

Both suites ran on the workstation with GCC 15.2.0 under `run4`. CMake used the recipes' installed BH fallback for Boost headers.

| Check | Result |
|---|---|
| `just cpp-test` | 110/110 passed. CTest took 294.32 s. |
| `just cpp-test-no-long-double` | 154/154 passed. CTest took 284.20 s. |
| `python3 dev/tools/check_links.py` | Passed with zero new broken links and 47 inherited broken links. |

## Open

None for the five updated cases. The other comparisons above are follow-up work.
