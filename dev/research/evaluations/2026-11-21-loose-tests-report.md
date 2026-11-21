# Coefficient reference tests

TASK-150.7, 2026-11-21. Baseline `8b3b2dba`.

193 assertion sites now use the reference threshold or an exact comparison. 9 recovery sites also check the MLE of the same sample. 8 sites retain the original check with a measured-error TODO. 4 sites already checked exact values.

## Reference construction

Analytic targets use the formulas, exact finite-grid values or boundary members identified in the earlier inventory and the surrounding tests. Independent binomial, truncated binomial and CMB profile grids verified the specified integer winners on the recorded ranges. The adjacent negative hypergeometric population likelihood ratios were checked as exact rationals. The grid and neighbor checks complement the existing tail-certificate tests. An exact zero is used for attained structural zeros, held zeros and exact zero endpoints. The expected coefficient is represented exactly in those routes. R checks vector lengths and applies the bound to each coordinate. Integer sizes checked individually use equality at tolerance zero.

The numerical script converts the decimal test inputs to binary64 before solving the likelihood scores at 70 and 100 decimal digits. Both calculations must round to the same binary64 reference. The input dumps retain exact C++ quantile values and R seeded draws in hexadecimal. The C++ and R log-logistic fixtures have different samples and coordinate conventions. The public R check compares scale. The Laplace references use the upper median and mean absolute deviation about it. Score solves establish numerical stationarity for the indicated likelihood branches. The test and dossier arguments for choosing a branch still apply. [`references.py`](2026-11-21-loose-tests-data/references.py), [C++ quantile exporter source](2026-11-21-loose-tests-data/export-quantiles.cpp), [rounded coefficients and roots](2026-11-21-loose-tests-data/references.csv), [native R inputs](2026-11-21-loose-tests-data/r-native-inputs.txt.gz), [double-only R inputs](2026-11-21-loose-tests-data/r-double-inputs.txt.gz).

Run from the repository root. The input dumps are included so the R references can be recomputed without loading the package.

```sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-21-loose-tests-data/references.py
```

## Comparison inventory

Line numbers refer to the baseline. The error is the maximum over the cases and coefficients on each line, relative to the rounded reference. Zeros use absolute error. C++ uses the rounded-reference threshold `1.0000011102230248e-10` and R uses `1e-10`. A dash indicates that the comparison branch had no returned coefficient. Quantile and seeded input references were checked separately when the builds produced different inputs. Each stored numerical target was checked against both builds. [C++ measurements](2026-11-21-loose-tests-data/cpp-measurements.csv), [R measurements](2026-11-21-loose-tests-data/r-measurements.csv).

| C++ site | Reference class | Previous comparison | Current comparison | Native error | Double-only error |
|---|---|---|---|---:|---:|
| `bbinom/test.cpp:399–400,420–421` | analytic | `1e-7`, `1e-6` | `kFitReferenceTol`, scale 0 | 2.51e-12 | 2.51e-12 |
| `betapr/test.cpp:264–265,290–291,355–356` | frozen | `1e-6` | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `bnbinom/test.cpp:252` | frozen | `1e-8` plus unit scale | Retained with TODO | 3.84e-09 | 3.84e-09 |
| `bnbinom/test.cpp:277` | analytic | `1e-8` plus unit scale | Retained with TODO | 3.23e-09 | 3.23e-09 |
| `bnbinom/test.cpp:291–292,314` | frozen | `1e-8` plus unit scale | `kFitReferenceTol`, scale 0 | 3e-15 | 2.76e-14 |
| `bnbinom/test.cpp:694–696` | frozen | `2e-6` | `kFitReferenceTol`, scale 0 | 5.96e-14 | 8.94e-15 |
| `cauchy/test.cpp:246` | analytic | `1e-7` | `kFitReferenceTol`, scale 0 | 2.66e-12 | 2.66e-12 |
| `cmb/test.cpp:270` | analytic | Default | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `cmp/test.cpp:53` | analytic | Default | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `dlaplace/test.cpp:9` | analytic | Default | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `exp/test.cpp:184` | analytic | Default | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `expweibull/test.cpp:238–239` | frozen | `1e-6` | `kFitReferenceTol`, scale 0 | 1.39e-15 | 7.07e-12 |
| `expweibull/test.cpp:240` | frozen | `1e-6` | Retained with TODO | 3.98e-14 | 2.12e-10 |
| `foldnorm/test.cpp:29,32` | analytic | Default | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `gamma/test.cpp:77,85` | analytic | Default | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `ged/test.cpp:414–416,474` | frozen | `1e-5` | `kFitReferenceTol`, scale 0 | 1.53e-11 | 1.53e-11 |
| `ged/test.cpp:475` | frozen | `1e-5` | Retained with TODO | 3.47e-10 | 3.47e-10 |
| `genbeta/test.cpp:90` | analytic | Default, `1e-5` | `kFitReferenceTol`, scale 0 | 0 | 2.03e-16 |
| `genbeta/test.cpp:440–442` | frozen | Default, `1e-5` | `kFitReferenceTol`, scale 0 | 6.52e-16 | 3.48e-15 |
| `genbeta/test.cpp:578` | frozen | `1e-10` plus unit scale | `kFitReferenceTol`, scale 0 | 2.97e-12 | 2.97e-12 |
| `gev/test.cpp:922–923` | analytic | Default | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `gpd/test.cpp:45` | analytic | Default | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `hnorm/test.cpp:17,46–47` | analytic | Default | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `ht/test.cpp:183` | frozen | `1e-3` | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `huber/test.cpp:180–181` | generating | `.02`, `1e-3` | Recovery check plus independent reference | 0 | 2.49e-15 |
| `huber/test.cpp:546` | frozen | `.02`, `1e-3` | `kFitReferenceTol`, scale 0 | 0 | 2.48e-16 |
| `invburr/test.cpp:223–225` | frozen | `1e-6` | `kFitReferenceTol`, scale 0 | 2.22e-15 | 2.22e-15 |
| `invtrgamma/test.cpp:114` | analytic | Default | `kFitReferenceTol`, scale 0 | 1.28e-16 | 1.28e-16 |
| `laplace/test.cpp:40–41` | frozen | `1e-4` | `kFitReferenceTol`, scale 0 | 1.5e-16 | 1.5e-16 |
| `lnorm/test.cpp:22–23` | frozen | `1e-4`, default | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `lnorm/test.cpp:92` | analytic | `1e-4`, default | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `lomax/test.cpp:63–64` | frozen | `5e-6` | `kFitReferenceTol`, scale 0 | 1.74e-15 | 2.76e-14 |
| `lst/test.cpp:132,342` | analytic | `1e-8` plus unit scale | `kFitReferenceTol`, scale 0 | 6.07e-12 | 6.07e-12 |
| `lst/test.cpp:350,541` | frozen | `1e-6` or `1e-7` | `kFitReferenceTol`, scale 0 | 4.51e-11 | 4.51e-11 |
| `lst/test.cpp:497` | analytic | `1e-6` or `1e-7` | `kFitReferenceTol`, scale 0 | 1.57e-16 | 1.57e-16 |
| `lst/test.cpp:521` | frozen | `1e-6` or `1e-7` | Retained with TODO | 3.48e-10 | 3.48e-10 |
| `norm/test.cpp:21–22` | frozen | `1e-4` | `kFitReferenceTol`, scale 0 | 0 | 5.19e-16 |
| `pareto3/test.cpp:86` | analytic | Default | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `pig/test.cpp:40,447` | frozen | `1e-4`, `1e-7`, `1e-6` | `kFitReferenceTol`, scale 0 | 4.43e-13 | 4.43e-13 |
| `pig/test.cpp:64` | frozen | `1e-4`, `1e-7`, `1e-6` | Retained with TODO | 1.61e-10 | 1.61e-10 |
| `sged/test.cpp:696` | frozen | `2e-8` | `kFitReferenceTol`, scale 0 | 5.79e-12 | 5.79e-12 |
| `skellam/test.cpp:27` | analytic | Default | `kFitReferenceTol`, scale 0 | 0 | 7.89e-16 |
| `std/test.cpp:206,295–296` | analytic | `1e-8` plus unit scale, default | `kFitReferenceTol`, scale 0 | 8.83e-12 | 8.83e-12 |
| `std/test.cpp:280–281,285,353` | frozen | `1e-7` | `kFitReferenceTol`, scale 0 | 2.7e-11 | 2.7e-11 |
| `std/test.cpp:326` | frozen | `1e-7` | Retained with TODO | 3.48e-10 | 3.48e-10 |
| `tlambda/test.cpp:202–203` | analytic | Default | `kFitReferenceTol`, scale 0 | 1.33e-16 | 1.33e-16 |
| `trgamma/test.cpp:284` | analytic | Default | `kFitReferenceTol`, scale 0 | 1.92e-16 | 1.92e-16 |
| `triang/test.cpp:23,105,136,141` | analytic | Default | `kFitReferenceTol`, scale 0 | 1.85e-16 | 1.85e-16 |
| `weibull/test.cpp:102` | analytic | Default | `kFitReferenceTol`, scale 0 | 0 | 0 |
| `tests/test_cmp.cpp:155–156` | generating | `.002` | Recovery check plus independent reference | 5.92e-16 | 3.56e-13 |
| `tests/modifiers/tnorm.cpp:82–83` | generating | `.1` with scale offset | Recovery check plus independent reference | 9.46e-12 | 3.13e-11 |
| `tests/modifiers/zinb.cpp:170` | frozen | `1e-8` plus unit scale | `kFitReferenceTol`, scale 0 | 7.5e-13 | 7.5e-13 |
| `tests/modifiers/zinb.cpp:201` | frozen | `2e-7` | Retained with TODO | 1.14e-10 | 1.14e-10 |
| `tests/modifiers/zinb.cpp:202,221` | frozen | `2e-7` | `kFitReferenceTol`, scale 0 | 3.61e-12 | 3.61e-12 |
| `tests/modifiers/zip.cpp:39–40` | frozen | `1e-4` | `kFitReferenceTol`, scale 0 | 1.56e-16 | 1.56e-16 |

| R site | Reference class | Previous comparison | Current comparison | Native error | Double-only error |
|---|---|---|---|---:|---:|
| `bbinom/test.R:35` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `bbinom/test.R:122,149` | analytic | Default | Exact integer | 0 | 0 |
| `bbinom/test.R:167,181` | analytic | `1e-7`, `1e-6` | `1e-10` componentwise | 2.51e-12 | 2.51e-12 |
| `binom/test.R:24` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `binom/test.R:34` | analytic | Default | Exact zero | 0 | 0 |
| `binom/test.R:79,264` | analytic | Default | Exact integer | 0 | 0 |
| `bnbinom/test.R:280` | frozen | `2e-6` | `1e-10` componentwise | 5.96e-14 | 8.94e-15 |
| `cauchy/test.R:87,99` | calculated | `1e-4` relative | `1e-10` componentwise | 1.87e-16 | 1.87e-16 |
| `cauchy/test.R:109,176,287` | analytic | Default, `1e-7` | `1e-10` componentwise | 2.66e-12 | 2.66e-12 |
| `cmb/test.R:75–76,117,119,228,231,244,251,262,409–410` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `cmb/test.R:248,250,372,408` | analytic | Default | Exact integer | 0 | 0 |
| `cmp/test.R:99` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `dlaplace/test.R:19` | analytic | Default | Exact integer | 0 | 0 |
| `dweibull/test.R:97,102` | generating | `.02`, `.1` | Recovery check plus independent reference | 7.41e-16 | 7.41e-16 |
| `exp/test.R:35` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `foldnorm/test.R:70,86,88` | analytic | Default, `1e-5` | `1e-10` componentwise | 0 | 0 |
| `foldnorm/test.R:87` | analytic | Default, `1e-5` | Exact zero | 0 | 0 |
| `foldnorm/test.R:132` | frozen | Default, `1e-5` | `1e-10` componentwise | 9.67e-15 | 9.67e-15 |
| `frechet/test.R:237,251` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `ged/test.R:60,286,295,304` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `ged/test.R:303` | analytic | Default | Exact zero | 0 | 0 |
| `ged/test.R:359` | frozen | `1e-5`, default | `1e-10` componentwise | 4.42e-16 | 8.83e-16 |
| `ged/test.R:511–512` | held-exact | Exact comparison | Already exact | 0 | 0 |
| `genbeta/test.R:301` | frozen | `1e-5` | `1e-10` componentwise | 6.52e-16 | 3.48e-15 |
| `genbeta/test.R:312` | frozen | `1e-5` | Conditional branch not reached | — | — |
| `geom/test.R:4` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `geom/test.R:74` | held-exact | Exact comparison | Already exact | 0 | 0 |
| `gev/test.R:235,254,263,270` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `gumbel/test.R:73` | calculated | `1e-4` relative | `1e-10` componentwise | 1.52e-15 | 1.52e-15 |
| `gumbel/test.R:150` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `ht/test.R:99,107,115` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `ht/test.R:187` | frozen | `1e-3` | `1e-10` componentwise | 0 | 0 |
| `huber/test.R:85` | generating | `.08` | Recovery check plus independent reference | 0 | 1.5e-15 |
| `laplace/test.R:45` | analytic (calculated) | `1e-4` relative | `1e-10` componentwise | 0 | 0 |
| `laplace/test.R:104–105` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `lnorm/test.R:42` | calculated | `1e-4` relative | `1e-10` componentwise | 0 | 0 |
| `lnorm/test.R:113` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `logis/test.R:106` | calculated | `1e-4` relative | `1e-10` componentwise | 1.12e-16 | 1.12e-16 |
| `logis/test.R:171` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `lomax/test.R:114` | frozen | `5e-6` | `1e-10` componentwise | 1.74e-15 | 2.76e-14 |
| `lst/test.R:149` | analytic | `1e-7` | `1e-10` componentwise | 0 | 0 |
| `nhyper/test.R:25,46,116–117,227` | analytic | Default | Exact integer | 0 | 0 |
| `norm/test.R:42` | calculated | `1e-4` relative | `1e-10` componentwise | 0 | 5.19e-16 |
| `norm/test.R:59,113–114` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `pig/test.R:8–9,125` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `sged/test.R:647` | frozen | `2e-8`, `10` relative | `1e-10` componentwise | 5.79e-12 | 5.79e-12 |
| `sged/test.R:1020` | frozen | `2e-8`, `10` relative | Conditional branch not reached | — | — |
| `skellam/test.R:57–58,121–123,130–131` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `skellam/test.R:59` | analytic | Default | Exact zero | 0 | 0 |
| `slash/test.R:90` | analytic | `1e-7` | `1e-10` componentwise | 0 | 0 |
| `std/test.R:309` | analytic | `1e-7`, `1e-6` | `1e-10` componentwise | 0 | 0 |
| `std/test.R:391–392` | frozen | `1e-7`, `1e-6` | `1e-10` componentwise | 4.19e-14 | 4.19e-14 |
| `tlambda/test.R:245` | analytic | `1e-6` | `1e-10` componentwise | 0 | 0 |
| `triang/test.R:52` | analytic | Default | Exact zero | 0 | 0 |
| `triang/test.R:60,62,79–80` | analytic | Default | `1e-10` componentwise | 1.85e-16 | 1.85e-16 |
| `weibull/test.R:134` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `modifier-tests/tbinom.R:74` | held-exact | Exact comparison | Already exact | 0 | 0 |
| `modifier-tests/tbinom.R:185,222` | analytic | Default | Exact integer | 0 | 0 |
| `modifier-tests/zip.R:19–20` | frozen | `1e-4` relative | `1e-10` componentwise | 1.56e-16 | 1.56e-16 |
| `modifier-tests/zmbinom.R:181` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `modifier-tests/zmnbinom.R:181` | analytic | Default | `1e-10` componentwise | 0 | 0 |
| `llogis/test.R:55` | calculated | `1e-4` relative | `1e-10` componentwise | 4.28e-16 | 4.28e-16 |

## Updated reference values

Pairs use binary64 decimal representations. For recovery checks the first value is the generating parameter, which the tests still check. For the seeded outliers the first value is the former runtime comparison. Replicates with identical pairs appear once. The measurement CSVs retain the errors coefficient by coefficient.

| Language | Site | Previous target → independent target | Native error | Double-only error |
|---|---|---|---:|---:|
| C++ | `betapr/test.cpp:264` | `1.6969258659791764` → `1.6969258489725776`<br>`0.6398504179662102` → `0.6398504186728861`<br>`0.48632248971440617` → `0.48632248957995083` | 0<br>0<br>0 | 0<br>0<br>0 |
| C++ | `betapr/test.cpp:265` | `3.092136533992608` → `3.092136488380654`<br>`0.12184378490069656` → `0.12184378512012611`<br>`0.021291062908283555` → `0.02129106290070557` | 0<br>0<br>0 | 0<br>0<br>0 |
| C++ | `betapr/test.cpp:290` | `0.22150182242584585` → `0.2215018227316948` | 0 | 0 |
| C++ | `betapr/test.cpp:291` | `2.5493370924264878` → `2.5493371032271748` | 0 | 0 |
| C++ | `betapr/test.cpp:355` | `95.516952` → `95.51695233754258` | 0 | 0 |
| C++ | `betapr/test.cpp:356` | `78.232314` → `78.23231367734707` | 0 | 0 |
| C++ | `bnbinom/test.cpp:291` | `1.15097395202066` → `1.1509739520206237` | 1.93e-15 | 2.3e-14 |
| C++ | `bnbinom/test.cpp:292` | `4.50374529147196` → `4.503745291472128` | 5.92e-16 | 2.76e-14 |
| C++ | `bnbinom/test.cpp:314` | `1.1088158545389952` → `1.1088158545386362` | 3e-15 | 2.2e-15 |
| C++ | `bnbinom/test.cpp:694` | `1.0672234787479615` → `1.067223439275812` | 2.89e-14 | 3.75e-15 |
| C++ | `bnbinom/test.cpp:695` | `2.141979154644717` → `2.1419792202550636` | 1.93e-14 | 3.11e-15 |
| C++ | `bnbinom/test.cpp:696` | `5.36473383510874` → `5.364734296253785` | 5.96e-14 | 8.94e-15 |
| C++ | `expweibull/test.cpp:239` | `23551.466260172245` → `23551.466260172223` | 1.39e-15 | 7.07e-12 |
| C++ | `ht/test.cpp:183` | `0.41508` → `0.4150800049142386` | 0 | 0 |
| C++ | `huber/test.cpp:180` | `0.8` → `0.8012237746838412` | 0 | 2.49e-15 |
| C++ | `huber/test.cpp:181` | `3.0` → `3.0028965585830516` | 0 | 2.22e-15 |
| C++ | `huber/test.cpp:546` | `3.889e-19` → `3.8886717919813423e-19` | 0 | 2.48e-16 |
| C++ | `invburr/test.cpp:223` | `1.1998579109829286` → `1.1998576948631954` | 2.22e-15 | 2.22e-15 |
| C++ | `invburr/test.cpp:224` | `2.1538589870062492` → `2.1538588889405816` | 6.19e-16 | 6.19e-16 |
| C++ | `invburr/test.cpp:225` | `0.30782991760094586` → `0.3078299526668148` | 1.62e-15 | 1.62e-15 |
| C++ | `laplace/test.cpp:41` | `5.905504256086694` → `5.905504256086693` | 1.5e-16 | 1.5e-16 |
| C++ | `lnorm/test.cpp:22` | `0.1658876904889516` → `0.16588752430803472` | 0 | 0 |
| C++ | `lnorm/test.cpp:23` | `1.062058874053751` → `1.0620589278165107` | 0 | 0 |
| C++ | `lomax/test.cpp:63` | `0.772199` → `0.7721986394822588`<br>`1394.08` → `1394.0822389719701`<br>`1039.29` → `1039.2879097707425`<br>`0.236987` → `0.23698695319903593`<br>`0.0359623` → `0.03596229860503347` | 0<br>0<br>0<br>0<br>1.74e-15 | 1.73e-15<br>8.15e-16<br>2.19e-16<br>3.51e-16<br>2.76e-14 |
| C++ | `lomax/test.cpp:64` | `0.405536` → `0.40553600650270355`<br>`0.161888` → `0.1618879518227893`<br>`0.196081` → `0.1960813444978726`<br>`0.878708` → `0.8787077236096766`<br>`6.07162` → `6.0716127841124825` | 0<br>0<br>1.42e-16<br>0<br>1.46e-15 | 5.48e-16<br>0<br>1.42e-16<br>1.26e-16<br>2.37e-14 |
| C++ | `lst/test.cpp:541` | `3.0242427136283037` → `3.024242709426846` | 3.45e-11 | 3.45e-11 |
| C++ | `norm/test.cpp:21` | `5.131381767233879` → `5.131348747913124` | 0 | 5.19e-16 |
| C++ | `norm/test.cpp:22` | `70.53074690727387` → `70.5307462207029` | 0 | 0 |
| C++ | `pig/test.cpp:40` | `0.036799` → `0.036798885007980536` | 5.34e-14 | 5.34e-14 |
| C++ | `sged/test.cpp:696` | `3.12977540545` → `3.1297754054334574` | 5.79e-12 | 5.79e-12 |
| C++ | `std/test.cpp:280` | `539.46057298` → `539.460572974731` | 9.57e-12 | 8.77e-12 |
| C++ | `std/test.cpp:281` | `1.00131588021` → `1.0013158802128972` | 1.69e-14 | 1.69e-14 |
| C++ | `std/test.cpp:285` | `539.46057298` → `539.460572974731` | 4.26e-14 | 2.82e-14 |
| C++ | `tests/test_cmp.cpp:155` | `1.0` → `1.0000898833697407` | 4.44e-16 | 2.78e-13 |
| C++ | `tests/test_cmp.cpp:156` | `3.0` → `3.0002854407158877` | 5.92e-16 | 3.56e-13 |
| C++ | `tests/modifiers/tnorm.cpp:82` | `0.3` → `0.30005475654592983`<br>`0.0` → `4.9201486651696174e-05`<br>`2.0` → `2.0001409681324773`<br>`0.0` → `0.044309398226104725`<br>`0.0` → `-0.024199702212838764`<br>`5.0` → `4.941593353830312`<br>`-3.0` → `-2.9885314308952964` | 0<br>9.46e-12<br>0<br>1.76e-13<br>1.69e-13<br>1.26e-15<br>8.92e-16 | 1.11e-15<br>3.13e-11<br>2.22e-16<br>3.43e-14<br>3.8e-13<br>1.26e-14<br>8.92e-16 |
| C++ | `tests/modifiers/tnorm.cpp:83` | `0.25` → `0.24990707486621472`<br>`1.0` → `0.9999137252059462`<br>`1.0` → `0.9984543032073503`<br>`1.0` → `0.9873450754647232`<br>`1.0` → `0.9911980863583388`<br>`3.0` → `2.9767248206760697`<br>`2.0` → `1.9918901327460004` | 3.33e-16<br>5.55e-16<br>0<br>2.14e-15<br>1.01e-15<br>1.04e-15<br>1e-15 | 1.22e-15<br>2.22e-16<br>0<br>0<br>2.35e-15<br>5.52e-15<br>1.11e-15 |
| C++ | `tests/modifiers/zinb.cpp:170` | `0.957450207` → `0.9574502070263265`<br>`0.506823257326` → `0.5068232573268997` | 7.5e-13<br>2.4e-13 | 7.5e-13<br>2.4e-13 |
| C++ | `tests/modifiers/zip.cpp:39` | `1.423265` → `1.423262352113118`<br>`3.920695` → `3.9206903948728864` | 1.56e-16<br>0 | 1.56e-16<br>0 |
| C++ | `tests/modifiers/zip.cpp:40` | `0.4730435` → `0.47304163643022323`<br>`0.6599249` → `0.6599238401795402` | 1.17e-16<br>0 | 1.17e-16<br>0 |
| R | `bnbinom/test.R:280` | `1.0672234787479615` → `1.067223439275812`<br>`2.141979154644717` → `2.1419792202550636`<br>`5.36473383510874` → `5.364734296253785` | 2.89e-14<br>1.93e-14<br>5.96e-14 | 3.75e-15<br>3.11e-15<br>8.94e-15 |
| R | `cauchy/test.R:87` | `-0.10763969158718215` → `-0.10763995599296099`<br>`1.095846270988029` → `1.0958464877966498` | 1.29e-16<br>0 | 1.29e-16<br>0 |
| R | `cauchy/test.R:99` | `-2.3694702843585906` → `-2.369470247128388`<br>`0.2727418943707882` → `0.2727419762545509` | 1.87e-16<br>0 | 1.87e-16<br>0 |
| R | `dweibull/test.R:97` | `1.5` → `1.498451288480568` | 7.41e-16 | 7.41e-16 |
| R | `dweibull/test.R:102` | `0.3` → `0.29962729679958416` | 7.41e-16 | 7.41e-16 |
| R | `foldnorm/test.R:132` | `0.31650829337731573` → `0.3165082933773172`<br>`0.28698193776778447` → `0.28698193776778286` | 7.72e-15<br>9.67e-15 | 7.72e-15<br>9.67e-15 |
| R | `gumbel/test.R:73` | `0.5763761423233421` → `0.5763765079152982`<br>`5.255287778481466` → `5.25528718403262` | 1.35e-15<br>1.52e-15 | 1.93e-16<br>1.52e-15 |
| R | `ht/test.R:187` | `0.41508` → `0.4150800049142386` | 0 | 0 |
| R | `huber/test.R:85` | `0.8` → `0.8159453415711106` | 0 | 1.5e-15 |
| R | `lnorm/test.R:42` | `0.1658876904889516` → `0.16588752430803472`<br>`1.062058874053751` → `1.0620589278165107` | 0<br>0 | 0<br>0 |
| R | `logis/test.R:106` | `0.24676943769340498` → `0.24676819243505332`<br>`5.2535732389093095` → `5.253573599408583` | 1.12e-16<br>0 | 1.12e-16<br>0 |
| R | `lomax/test.R:114` | `0.772199` → `0.7721986394822588`<br>`0.405536` → `0.40553600650270355`<br>`1394.08` → `1394.0822389719701`<br>`0.161888` → `0.1618879518227893`<br>`1039.29` → `1039.2879097707425`<br>`0.196081` → `0.1960813444978726`<br>`0.236987` → `0.23698695319903593`<br>`0.878708` → `0.8787077236096766`<br>`0.0359623` → `0.03596229860503347`<br>`6.07162` → `6.0716127841124825` | 0<br>0<br>0<br>0<br>0<br>1.42e-16<br>0<br>0<br>1.74e-15<br>1.46e-15 | 1.73e-15<br>5.48e-16<br>8.15e-16<br>0<br>2.19e-16<br>1.42e-16<br>3.51e-16<br>1.26e-16<br>2.76e-14<br>2.37e-14 |
| R | `norm/test.R:42` | `5.131381767233879` → `5.131348747913124`<br>`70.53074690727387` → `70.5307462207029` | 0<br>0 | 5.19e-16<br>0 |
| R | `sged/test.R:647` | `3.12977540545` → `3.1297754054334574` | 5.79e-12 | 5.79e-12 |
| R | `std/test.R:391` | `218.257164105` → `218.25716410505748` | 4.19e-14 | 4.19e-14 |
| R | `std/test.R:392` | `-0.0242634536213` → `-0.024263453621344095` | 1.43e-16 | 1.43e-16 |
| R | `modifier-tests/zip.R:19` | `1.423265` → `1.423262352113118`<br>`3.920695` → `3.9206903948728864` | 1.56e-16<br>0 | 1.56e-16<br>0 |
| R | `modifier-tests/zip.R:20` | `0.4730435` → `0.47304163643022323`<br>`0.6599249` → `0.6599238401795402` | 1.17e-16<br>0 | 1.17e-16<br>0 |
| R | `llogis/test.R:55` | `2.0734828916218055` → `2.0734828096959386`<br>`1.096032862562148` → `1.0960327912206853` | 4.28e-16<br>2.03e-16 | 4.28e-16<br>2.03e-16 |

## Findings

Each item preserves its previous assertion and records the mismatch in a `TODO(TASK-150.7)` comment. Several tests deliberately ask for a looser solver tolerance than the independent comparison threshold.

| Site | Native error | Double-only error | Context |
|---|---:|---:|---|
| `bnbinom/test.cpp:252` | 3.84e-09 | 3.84e-09 | Scalar-root test requests `1e-8`. |
| `bnbinom/test.cpp:277` | 3.23e-09 | 3.23e-09 | Multiplicity-formula test requests `1e-8`. |
| `expweibull/test.cpp:240` | 3.98e-14 | 2.12e-10 | Relaxed-tolerance test requests `1e-6`. The double-only exponent misses the reference threshold. |
| `ged/test.cpp:475` | 3.47e-10 | 3.47e-10 | The test requests `1e-6` and compares a separated candidate. |
| `lst/test.cpp:521` | 3.48e-10 | 3.48e-10 | The test requests `1e-7`. The relative reference threshold is also stricter than the location scale floor here. |
| `pig/test.cpp:64` | 1.61e-10 | 1.61e-10 | Held-shape test requests `1e-9`. |
| `std/test.cpp:326` | 3.48e-10 | 3.48e-10 | The test requests `1e-7`. The relative reference threshold is also stricter than the location scale floor here. |
| `tests/modifiers/zinb.cpp:201` | 1.14e-10 | 1.14e-10 | The hold mask fixing inflation alone misses. The fixed-size entry point requests its default `1e-12`. |

## Verification

All final runs used the workstation. C++ used GCC 15.2.0 and the installed BH headers. The double-only mode defines `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`. R used the locally assembled package and testthat. The changed-family dry run selected the full shared path because modifier and standalone tests are outside family directories. The final R runs used the existing `just --no-deps test` recipe with the selected files staged in the already compiled generated package. The development run also included the underscore-named shared tests, which the initial file selector left in place. The full-tier run selected Foldnorm and ZIP. The original double-only R probe started parallel testthat against an uncompiled package copy and was interrupted during the workstation memory incident. The rerun completed `just regen` with two make jobs before starting the stock test recipe with one make job. It exercised the assertions needed for the measurements. The generated package currently holds this double-only build.

| Check | Result |
|---|---|
| `just cpp-test` | 110/110 passed in 294.11 s of CTest time. |
| `just cpp-test-no-long-double` | 154/154 passed in 452.59 s of CTest time. |
| R development tier, touched family and modifier files | 23163 passes, 4 warnings, 80 skips, zero failures. |
| R full tier, `dist-foldnorm` and `modifier-kernel-zip` | 407 passes, 0 warnings, 0 skips, zero failures. |
| R double-only coefficient measurement probe | 1813 passes, 0 warnings, 0 skips, zero failures. |
| `references.py` | All 193 entries agree after rounding from 70 and 100 digits. |
| `python3 dev/tools/check_links.py` | Passed with zero new broken links and 47 inherited broken links. |

The development-tier run included these family and modifier files plus the shared tests.

```text
test-dist-bbinom.R
test-dist-betapr.R
test-dist-binom.R
test-dist-bnbinom.R
test-dist-cauchy.R
test-dist-cmb.R
test-dist-cmp.R
test-dist-dlaplace.R
test-dist-dweibull.R
test-dist-exp.R
test-dist-expweibull.R
test-dist-foldnorm.R
test-dist-frechet.R
test-dist-gamma.R
test-dist-ged.R
test-dist-genbeta.R
test-dist-geom.R
test-dist-gev.R
test-dist-gpd.R
test-dist-gumbel.R
test-dist-hnorm.R
test-dist-ht.R
test-dist-huber.R
test-dist-invburr.R
test-dist-invtrgamma.R
test-dist-laplace.R
test-dist-llogis.R
test-dist-lnorm.R
test-dist-logis.R
test-dist-lomax.R
test-dist-lst.R
test-dist-nhyper.R
test-dist-norm.R
test-dist-pareto3.R
test-dist-pig.R
test-dist-sged.R
test-dist-skellam.R
test-dist-slash.R
test-dist-std.R
test-dist-tlambda.R
test-dist-trgamma.R
test-dist-triang.R
test-dist-weibull.R
test-modifier-kernel-tbinom.R
test-modifier-kernel-tnorm.R
test-modifier-kernel-zinb.R
test-modifier-kernel-zip.R
test-modifier-kernel-zmbinom.R
test-modifier-kernel-zmnbinom.R
```

## Open

- `genbeta/test.R:312` is the fit-return branch for a `tol = 1e-20` request. Both builds reported a tolerance stop on the fixture, so a returned coefficient vector for that branch remains unmeasured.
- `sged/test.R:1020` is the fit-return branch for a `tol = 10` request. Both builds reported a numerical stop on the fixture, so the fallback coefficient error remains unmeasured. [Observed R statuses](2026-11-21-loose-tests-data/conditional-statuses.txt).

Rows assigned to the other jobs remain follow-up work.

- C++ `bhatt/test.cpp:112–114,116–118,273,636,643,650,657,664,669,671,676,678,685,695,702,709,716,723,730`.
- C++ `gb2/test.cpp:191–192,194,447–448`.
- C++ `gengamma/test.cpp:308`.
- C++ `kumar/test.cpp:58`.
- C++ `sgomp/test.cpp:21`.
- C++ `sstd/test.cpp:365–366,377`.
- C++ `st/test.cpp:178`.
- R `bhatt/test.R:107,110,239,279,324`.
- R `gb2/test.R:233,235`.
- R `gompertz/test.R:89`.
- R `sgomp/test.R:40–41,90`.
- R `tests/testthat/test_golden_regression.R:254–265`.
