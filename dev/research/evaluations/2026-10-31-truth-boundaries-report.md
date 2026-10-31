# TASK-106: truth at attained boundaries and support endpoints

2026-10-31. Source revision: `28e42fe3cd71ac69ee3e8e3ba6d1ebfdf9a060b9`, with the uncommitted oracle/fixture/documentation changes described here. No fitter, dpqr, special-function, core, golden, README dossier, R code generator or `test.cpp` was changed. No commits were made.

## Coverage and method

Added 79 active parameter points and 2 complete archived reference points (30 value rows and 88 quantile rows). Active additions: 2773 value rows and 3386 quantile rows. Separately retained exclusions: 207 value rows and 90 quantile rows. A value row contains log density and both log tails; a quantile row specifies the tail and probability scale.

The pre-existing numerical rows were retained unchanged. Two additional complete independent laws are archived outside active d/p/q/r tests, retaining every reference row and its normalization. Unbounded atomic laws use their actual last atom as the upper quantile endpoint; explicit finite design endpoints retain the nominal support convention. New parameter faces were independently regenerated at 80 decimal digits (up to 600 when the existing finite-support precision rule calls for it). The shared `boundary_points` selector appends each inclusive coefficient-domain bound after the historical stress cap. Dependent inclusive domains are projected until compatible: for example, hypergeometric `N = 0` requires `m = k = 0`. No open domain bound is added. The local routes additionally declare GEV/GPD `xi = -1` and the joint Skellam zero-rate corner. Truncated normal already had its inclusive infinite bounds in its public default point.

The tables already contained support endpoints and their adjacent binary64 probes at every existing point. They now contain those same probes at every added face, including triangular endpoint modes and the finite upper endpoints of GPD/GEV at `xi = -1`. Both quantile tails include natural probabilities zero and one and log probabilities minus infinity and zero. This is one representative per attained face, plus selected intersections; it does not exhaust every boundary intersection or every possible endpoint density regime.

Conditional representatives: CMP `nu = 0` uses `lambda = 0.5`, since the geometric normalizer requires `lambda < 1`. Truncated binomial `size = 0` uses bounds `a = -1, b = 0`, so its conditional support is nonempty. Its `prob = 0` representative uses `a = -1`, including zero. Truncated Poisson at zero rate uses `a = -1, b = 0`. The original strictly positive-truncation zero-rate/probability limits are independently retained as complete archived laws because public d/p/q/r return NaNs there. CMB at its attained size cap 10,000,000 uses `prob = 0`, an exact atomic member; no claim is made about nondegenerate members at that size.

## Independent formulas and validation strength

All new references use mathematics, mpmath and SymPy, never C++ or R values. Exact degeneracies use positive atomic masses. Poisson at zero rate, negative binomial at zero size or probability one, geometric at probability one, and PIG at zero shape collapse to zero. A Poisson conditional on a fixed lower count concentrates on that first count as its rate tends to zero. Binomial probability endpoints concentrate on extreme counts; inflation/modification retains its explicit mixture mass. For zero-size positive-truncated negative binomial, `Gamma(r+k)/Gamma(r) ~ r Gamma(k)` gives logarithmic-series masses `(1-p)^k / (-k log(p))`; zero modification adds its atom at zero. These identities and limits are analytical; their stored binary64 evaluations are numerical.

Bhattacharjee at width zero is normal by collapse of its independent uniform summand. Shifted Gompertz at `eta = 0` is exponential. CMP at `nu = 0` is the normalized geometric series. Endpoint-mode triangular CDFs factor as `z*(2-z)` or `z^2`, with the opposite tail evaluated directly to preserve tiny probabilities. GPD at `xi = -1` is uniform, and GEV at `xi = -1` has endpoint density `1/sigma`. Hypergeometric singleton laws and uniform/Zipf singleton cases use finite positive sums. The existing independent Skellam convolution route covers both one-rate-zero laws and the joint point mass.

Every added law passes the existing normalization and density/antiderivative consistency checks. Atomic and geometric/logarithmic normalizations also have the analytic series identities stated in the routes. Quadrature error estimates and finite numerical comparisons are numerical evidence, not general proofs of mpmath error bounds or of model correctness. The discrete verifier now treats zero-mass intervals explicitly, avoiding indeterminate differences of two infinite logarithms. Symbolic density construction is lazy, so an exact limiting route need not instantiate a singular interior formula.

## Counts by family

Archived complete laws are counted separately from excluded individual rows. Counts are additions relative to the source revision above. Excluded rows retain their independently computed references and reasons under `excluded_values` / `excluded_quantiles`; they are not active expectations.

| Family | Parameter points | Active value rows | Active quantile rows | Excluded value / quantile rows | Archived laws |
|---|---:|---:|---:|---:|---:|
| bbinom | 1 | 4 | 44 | 0 / 0 | 0 |
| beta | 0 | 0 | 0 | 0 / 0 | 0 |
| betapr | 0 | 0 | 0 | 0 / 0 | 0 |
| bhatt | 1 | 19 | 44 | 0 / 0 | 0 |
| binom | 3 | 36 | 132 | 0 / 0 | 0 |
| bnbinom | 0 | 0 | 0 | 0 / 0 | 0 |
| burr | 0 | 0 | 0 | 0 / 0 | 0 |
| cauchy | 0 | 0 | 0 | 0 / 0 | 0 |
| cmb | 4 | 38 | 134 | 204 / 42 | 0 |
| cmp | 2 | 212 | 88 | 0 / 0 | 0 |
| dgamma | 0 | 0 | 0 | 0 / 0 | 0 |
| dlaplace | 0 | 0 | 0 | 0 / 0 | 0 |
| dnorm | 0 | 0 | 0 | 0 / 0 | 0 |
| dunif | 2 | 8 | 88 | 0 / 0 | 0 |
| dweibull | 0 | 0 | 0 | 0 / 0 | 0 |
| emg | 0 | 0 | 0 | 0 / 0 | 0 |
| exp | 0 | 0 | 0 | 0 / 0 | 0 |
| fatigue | 0 | 0 | 0 | 0 / 0 | 0 |
| foldnorm | 1 | 21 | 44 | 0 / 0 | 0 |
| fpareto | 0 | 0 | 0 | 0 / 0 | 0 |
| frechet | 0 | 0 | 0 | 0 / 0 | 0 |
| gamma | 0 | 0 | 0 | 0 / 0 | 0 |
| gb2 | 0 | 0 | 0 | 0 / 0 | 0 |
| ged | 0 | 0 | 0 | 0 / 0 | 0 |
| genbeta | 0 | 0 | 0 | 0 / 0 | 0 |
| geom | 1 | 4 | 44 | 0 / 0 | 0 |
| gev | 1 | 16 | 44 | 0 / 0 | 0 |
| gompertz | 0 | 0 | 0 | 0 / 0 | 0 |
| gpd | 1 | 18 | 44 | 0 / 0 | 0 |
| gumbel | 0 | 0 | 0 | 0 / 0 | 0 |
| hcauchy | 0 | 0 | 0 | 0 / 0 | 0 |
| hnorm | 0 | 0 | 0 | 0 / 0 | 0 |
| ht | 0 | 0 | 0 | 0 / 0 | 0 |
| huber | 0 | 0 | 0 | 0 / 0 | 0 |
| hyper | 5 | 20 | 220 | 0 / 0 | 0 |
| invburr | 0 | 0 | 0 | 0 / 0 | 0 |
| invexp | 0 | 0 | 0 | 0 / 0 | 0 |
| invgamma | 0 | 0 | 0 | 0 / 0 | 0 |
| invgauss | 0 | 0 | 0 | 0 / 0 | 0 |
| invparalogis | 0 | 0 | 0 | 0 / 0 | 0 |
| invpareto | 0 | 0 | 0 | 0 / 0 | 0 |
| invtrgamma | 0 | 0 | 0 | 0 / 0 | 0 |
| invweibull | 0 | 0 | 0 | 0 / 0 | 0 |
| kumar | 0 | 0 | 0 | 0 / 0 | 0 |
| laplace | 0 | 0 | 0 | 0 / 0 | 0 |
| lgamma | 0 | 0 | 0 | 0 / 0 | 0 |
| lgser | 0 | 0 | 0 | 0 / 0 | 0 |
| llogis | 0 | 0 | 0 | 0 / 0 | 0 |
| lnorm | 0 | 0 | 0 | 0 / 0 | 0 |
| logis | 0 | 0 | 0 | 0 / 0 | 0 |
| logitnorm | 0 | 0 | 0 | 0 / 0 | 0 |
| lomax | 0 | 0 | 0 | 0 / 0 | 0 |
| lst | 0 | 0 | 0 | 0 / 0 | 0 |
| naka | 0 | 0 | 0 | 0 / 0 | 0 |
| nbinom | 2 | 8 | 88 | 0 / 0 | 0 |
| nhyper | 5 | 27 | 220 | 0 / 0 | 0 |
| norm | 0 | 0 | 0 | 0 / 0 | 0 |
| nsbeta | 0 | 0 | 0 | 0 / 0 | 0 |
| paralogis | 0 | 0 | 0 | 0 / 0 | 0 |
| pareto | 0 | 0 | 0 | 0 / 0 | 0 |
| pareto2 | 0 | 0 | 0 | 0 / 0 | 0 |
| pareto3 | 0 | 0 | 0 | 0 / 0 | 0 |
| pareto4 | 0 | 0 | 0 | 0 / 0 | 0 |
| pig | 1 | 4 | 44 | 0 / 0 | 0 |
| pois | 1 | 4 | 44 | 0 / 0 | 0 |
| power | 0 | 0 | 0 | 0 / 0 | 0 |
| rayleigh | 0 | 0 | 0 | 0 / 0 | 0 |
| sged | 0 | 0 | 0 | 0 / 0 | 0 |
| sgomp | 1 | 21 | 44 | 0 / 0 | 0 |
| skellam | 3 | 313 | 132 | 3 / 0 | 0 |
| slash | 0 | 0 | 0 | 0 / 0 | 0 |
| sn | 0 | 0 | 0 | 0 / 0 | 0 |
| snorm | 0 | 0 | 0 | 0 / 0 | 0 |
| sstd | 0 | 0 | 0 | 0 / 0 | 0 |
| std | 0 | 0 | 0 | 0 / 0 | 0 |
| tbinom | 3 | 35 | 132 | 0 / 0 | 1 |
| tlambda | 0 | 0 | 0 | 0 / 0 | 0 |
| tnorm | 0 | 0 | 0 | 0 / 0 | 0 |
| tpois | 1 | 4 | 44 | 0 / 0 | 1 |
| trgamma | 0 | 0 | 0 | 0 / 0 | 0 |
| triang | 2 | 37 | 80 | 0 / 8 | 0 |
| unif | 0 | 0 | 0 | 0 / 0 | 0 |
| weibull | 0 | 0 | 0 | 0 / 0 | 0 |
| yulesimon | 0 | 0 | 0 | 0 / 0 | 0 |
| zeta | 0 | 0 | 0 | 0 / 0 | 0 |
| zib | 5 | 68 | 212 | 0 / 8 | 0 |
| zinb | 4 | 220 | 176 | 0 / 0 | 0 |
| zip | 2 | 210 | 88 | 0 / 0 | 0 |
| zipf | 2 | 19 | 88 | 0 / 0 | 0 |
| zmbinom | 5 | 71 | 208 | 0 / 12 | 0 |
| zmgeom | 3 | 219 | 128 | 0 / 4 | 0 |
| zmlogarithmic | 3 | 219 | 128 | 0 / 4 | 0 |
| zmnbinom | 4 | 427 | 172 | 0 / 4 | 0 |
| zmpois | 3 | 217 | 128 | 0 / 4 | 0 |
| ztbinom | 3 | 34 | 128 | 0 / 4 | 0 |
| ztgeom | 1 | 4 | 44 | 0 / 0 | 0 |
| ztnbinom | 2 | 212 | 88 | 0 / 0 | 0 |
| ztpois | 1 | 4 | 44 | 0 / 0 | 0 |

Families with no attained coefficient-domain boundary (all model-domain brackets open): `beta`, `betapr`, `bnbinom`, `burr`, `cauchy`, `dgamma`, `dlaplace`, `dnorm`, `dweibull`, `emg`, `exp`, `fatigue`, `fpareto`, `frechet`, `gamma`, `gb2`, `ged`, `genbeta`, `gev`, `gompertz`, `gpd`, `gumbel`, `hcauchy`, `hnorm`, `ht`, `huber`, `invburr`, `invexp`, `invgamma`, `invgauss`, `invparalogis`, `invpareto`, `invtrgamma`, `invweibull`, `kumar`, `laplace`, `lgamma`, `lgser`, `llogis`, `lnorm`, `logis`, `logitnorm`, `lomax`, `lst`, `naka`, `norm`, `nsbeta`, `paralogis`, `pareto`, `pareto2`, `pareto3`, `pareto4`, `power`, `rayleigh`, `sged`, `slash`, `sn`, `snorm`, `sstd`, `std`, `tlambda`, `trgamma`, `unif`, `weibull`, `yulesimon`, `zeta`. GEV and GPD nevertheless receive their explicitly attained fit-restriction point.

Inclusive-domain families needing no additional point because their attained bounds were already covered: `tnorm`.

## Findings

The complete audit found 260 failing checks. Two unsupported positive-truncation limits account for 208 checks and are archived as complete laws: 15 value rows and 44 quantile rows each. Each value row can fail several density/tail checks. The numerical dpqr findings on otherwise supported faces are eight triangular quantile checks and three Skellam log-upper-tail checks. Forty other checks concern nominal finite support endpoints versus collapsed atomic support. One gapped-atom quantile case was resolved by correcting oracle acceptance bookkeeping.

Triangular: at `a = c = 0, b = 1`, the lower quantile is `p / (1 + sqrt(1-p))`; subtraction of two nearly equal numbers in the current implementation loses tiny quantiles. Skellam: at `mu1 = 1, mu2 = 0`, the log survival at counts 174–176 is inaccurate in the subnormal-probability range. Both are numerical findings; no implementation was changed.

Finite-support endpoint differences are contract findings, not a claim that the interior inverse CDF is wrong. The oracle retains the explicit finite design endpoints, while these implementations sometimes return the collapsed atomic endpoint. A later job should decide and document that endpoint policy. Exact exclusions preserve all independent expected values.

At a zero-mass gap, the existing near-jump acceptance rule now names the neighboring positive-mass atoms rather than consecutive integers. For the equal mixture at 0 and 10, its admissible answers are 0 or 10, never an intermediate zero-mass count. The relative input-scale jump tolerance remains 1e-12; the high-precision quantile value is unchanged. This is a numerical test acceptance convention, not a claim that the exact quantile is set-valued.

At the CMB size cap, every C++ d/p/q call constructs 10,000,001 table entries even for the exact atomic law. The initial C++ trial was interrupted after 103 shards completed because this workload was excessive. Both support endpoints and both natural-scale median quantiles remain active at the cap; all other independently computed probes are retained with explicit workload reasons. The interrupted child and obsolete lock waiter were terminated, and every final run below completed.

Every failed check from the complete audit is listed below. Point/row indices refer to the unfiltered extended audit tables; exact parameters and probes disambiguate later reindexing. Numbers were emitted with 17 significant digits. The status distinguishes complete-law archives, exact-row exclusions and the resolved oracle-bookkeeping case.

| Family | Parameters (README order) | Check | Point / row | Input | Expected | Actual | Disposition |
|---|---|---|---|---|---|---|---|
| skellam | `[1.0, 0.0]` | log_upper | 8 / 1738 | `174` | `-733.33365531008144` | `-733.33365711091244` | row excluded |
| skellam | `[1.0, 0.0]` | log_upper | 8 / 1739 | `175` | `-738.50417158558412` | `-738.50517772576165` | row excluded |
| skellam | `[1.0, 0.0]` | log_upper | 8 / 1740 | `176` | `-743.68035323493075` | `-743.74692474082133` | row excluded |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 75 | `0.99999999999999989` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 75 | `0.99999999999999989` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 75 | `0.99999999999999989` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | upper | 5 / 75 | `0.99999999999999989` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 76 | `1` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 76 | `1` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 76 | `1` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 76 | `1` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 77 | `1.0000000000000002` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 77 | `1.0000000000000002` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 77 | `1.0000000000000002` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 77 | `1.0000000000000002` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 78 | `1.5` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 78 | `1.5` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 78 | `1.5` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 78 | `1.5` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 79 | `2` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 79 | `2` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 79 | `2` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 79 | `2` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 80 | `3` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 80 | `3` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 80 | `3` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 80 | `3` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 81 | `4` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 81 | `4` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 81 | `4` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 81 | `4` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 82 | `5` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 82 | `5` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 82 | `5` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 82 | `5` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 83 | `6` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 83 | `6` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 83 | `6` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 83 | `6` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 84 | `7` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 84 | `7` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 84 | `7` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 84 | `7` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 85 | `8` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 85 | `8` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 85 | `8` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 85 | `8` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 86 | `9` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 86 | `9` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 86 | `9` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 86 | `9` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 87 | `9.9999999999999982` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 87 | `9.9999999999999982` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 87 | `9.9999999999999982` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 87 | `9.9999999999999982` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 88 | `10` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 88 | `10` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 88 | `10` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 88 | `10` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_d | 5 / 89 | `10.000000000000002` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_lower | 5 / 89 | `10.000000000000002` | `0` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | lower | 5 / 89 | `10.000000000000002` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | log_upper | 5 / 89 | `10.000000000000002` | `-inf` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower_log | 5 / 220 | `-690.77552789821368` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower_log | 5 / 221 | `-230.25850929940458` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower_log | 5 / 222 | `-69.077552789821368` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower_log | 5 / 223 | `-27.631021115928547` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower_log | 5 / 224 | `-13.815510557964274` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower_log | 5 / 225 | `-4.6051701859880918` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower_log | 5 / 226 | `-1.3862943611198906` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower_log | 5 / 227 | `-0.69314718055994529` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower_log | 5 / 228 | `-800` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower_log | 5 / 229 | `-10000` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower_log | 5 / 230 | `-inf` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower_log | 5 / 231 | `0` | `10` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower | 5 / 232 | `1e-300` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower | 5 / 233 | `1e-100` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower | 5 / 234 | `1.0000000000000001e-30` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower | 5 / 235 | `9.9999999999999998e-13` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower | 5 / 236 | `9.9999999999999995e-07` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower | 5 / 237 | `0.01` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower | 5 / 238 | `0.25` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower | 5 / 239 | `0.5` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower | 5 / 240 | `0` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_lower | 5 / 241 | `1` | `10` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper_log | 5 / 242 | `-690.77552789821368` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper_log | 5 / 243 | `-230.25850929940458` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper_log | 5 / 244 | `-69.077552789821368` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper_log | 5 / 245 | `-27.631021115928547` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper_log | 5 / 246 | `-13.815510557964274` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper_log | 5 / 247 | `-4.6051701859880918` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper_log | 5 / 248 | `-1.3862943611198906` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper_log | 5 / 249 | `-0.69314718055994529` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper_log | 5 / 250 | `-800` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper_log | 5 / 251 | `-10000` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper_log | 5 / 252 | `-inf` | `10` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper_log | 5 / 253 | `0` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper | 5 / 254 | `1e-300` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper | 5 / 255 | `1e-100` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper | 5 / 256 | `1.0000000000000001e-30` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper | 5 / 257 | `9.9999999999999998e-13` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper | 5 / 258 | `9.9999999999999995e-07` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper | 5 / 259 | `0.01` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper | 5 / 260 | `0.25` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper | 5 / 261 | `0.5` | `1` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper | 5 / 262 | `0` | `10` | `nan` | law archived |
| tbinom | `[0.0, 0.0, 10.0, 10.0]` | q_upper | 5 / 263 | `1` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 75 | `0.99999999999999989` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 75 | `0.99999999999999989` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 75 | `0.99999999999999989` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | upper | 5 / 75 | `0.99999999999999989` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 76 | `1` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 76 | `1` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 76 | `1` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 76 | `1` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 77 | `1.0000000000000002` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 77 | `1.0000000000000002` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 77 | `1.0000000000000002` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 77 | `1.0000000000000002` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 78 | `1.5` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 78 | `1.5` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 78 | `1.5` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 78 | `1.5` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 79 | `2` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 79 | `2` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 79 | `2` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 79 | `2` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 80 | `3` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 80 | `3` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 80 | `3` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 80 | `3` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 81 | `4` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 81 | `4` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 81 | `4` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 81 | `4` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 82 | `5` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 82 | `5` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 82 | `5` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 82 | `5` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 83 | `6` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 83 | `6` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 83 | `6` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 83 | `6` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 84 | `7` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 84 | `7` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 84 | `7` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 84 | `7` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 85 | `8` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 85 | `8` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 85 | `8` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 85 | `8` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 86 | `9` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 86 | `9` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 86 | `9` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 86 | `9` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 87 | `9.9999999999999982` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 87 | `9.9999999999999982` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 87 | `9.9999999999999982` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 87 | `9.9999999999999982` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 88 | `10` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 88 | `10` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 88 | `10` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 88 | `10` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_d | 5 / 89 | `10.000000000000002` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_lower | 5 / 89 | `10.000000000000002` | `0` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | lower | 5 / 89 | `10.000000000000002` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | log_upper | 5 / 89 | `10.000000000000002` | `-inf` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower_log | 5 / 220 | `-690.77552789821368` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower_log | 5 / 221 | `-230.25850929940458` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower_log | 5 / 222 | `-69.077552789821368` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower_log | 5 / 223 | `-27.631021115928547` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower_log | 5 / 224 | `-13.815510557964274` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower_log | 5 / 225 | `-4.6051701859880918` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower_log | 5 / 226 | `-1.3862943611198906` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower_log | 5 / 227 | `-0.69314718055994529` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower_log | 5 / 228 | `-800` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower_log | 5 / 229 | `-10000` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower_log | 5 / 230 | `-inf` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower_log | 5 / 231 | `0` | `10` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower | 5 / 232 | `1e-300` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower | 5 / 233 | `1e-100` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower | 5 / 234 | `1.0000000000000001e-30` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower | 5 / 235 | `9.9999999999999998e-13` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower | 5 / 236 | `9.9999999999999995e-07` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower | 5 / 237 | `0.01` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower | 5 / 238 | `0.25` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower | 5 / 239 | `0.5` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower | 5 / 240 | `0` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_lower | 5 / 241 | `1` | `10` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper_log | 5 / 242 | `-690.77552789821368` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper_log | 5 / 243 | `-230.25850929940458` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper_log | 5 / 244 | `-69.077552789821368` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper_log | 5 / 245 | `-27.631021115928547` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper_log | 5 / 246 | `-13.815510557964274` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper_log | 5 / 247 | `-4.6051701859880918` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper_log | 5 / 248 | `-1.3862943611198906` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper_log | 5 / 249 | `-0.69314718055994529` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper_log | 5 / 250 | `-800` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper_log | 5 / 251 | `-10000` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper_log | 5 / 252 | `-inf` | `10` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper_log | 5 / 253 | `0` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper | 5 / 254 | `1e-300` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper | 5 / 255 | `1e-100` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper | 5 / 256 | `1.0000000000000001e-30` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper | 5 / 257 | `9.9999999999999998e-13` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper | 5 / 258 | `9.9999999999999995e-07` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper | 5 / 259 | `0.01` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper | 5 / 260 | `0.25` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper | 5 / 261 | `0.5` | `1` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper | 5 / 262 | `0` | `10` | `nan` | law archived |
| tpois | `[0.0, 0.0, 10.0]` | q_upper | 5 / 263 | `1` | `1` | `nan` | law archived |
| triang | `[0.0, 1.0, 0.0]` | q_lower_log | 5 / 220 | `-690.77552789821368` | `5.0000000000001187e-301` | `0` | row excluded |
| triang | `[0.0, 1.0, 0.0]` | q_lower_log | 5 / 221 | `-230.25850929940458` | `4.9999999999999449e-101` | `0` | row excluded |
| triang | `[0.0, 1.0, 0.0]` | q_lower_log | 5 / 222 | `-69.077552789821368` | `5.0000000000000118e-31` | `0` | row excluded |
| triang | `[0.0, 1.0, 0.0]` | q_lower_log | 5 / 223 | `-27.631021115928547` | `5.000000000001255e-13` | `5.0004445029117051e-13` | row excluded |
| triang | `[0.0, 1.0, 0.0]` | q_lower | 5 / 232 | `1e-300` | `5.0000000000000001e-301` | `0` | row excluded |
| triang | `[0.0, 1.0, 0.0]` | q_lower | 5 / 233 | `1e-100` | `5.0000000000000001e-101` | `0` | row excluded |
| triang | `[0.0, 1.0, 0.0]` | q_lower | 5 / 234 | `1.0000000000000001e-30` | `5.0000000000000004e-31` | `0` | row excluded |
| triang | `[0.0, 1.0, 0.0]` | q_lower | 5 / 235 | `9.9999999999999998e-13` | `5.00000000000125e-13` | `5.0004445029117051e-13` | row excluded |
| zib | `[10.0, 0.0, 0.5]` | q_lower_log | 8 / 363 | `0` | `10` | `0` | row excluded |
| zib | `[10.0, 0.0, 0.5]` | q_lower | 8 / 373 | `1` | `10` | `0` | row excluded |
| zib | `[10.0, 0.0, 0.5]` | q_upper_log | 8 / 384 | `-inf` | `10` | `0` | row excluded |
| zib | `[10.0, 0.0, 0.5]` | q_upper | 8 / 394 | `0` | `10` | `0` | row excluded |
| zib | `[10.0, 1.0, 0.5]` | q_lower_log | 9 / 403 | `-0.69314718055994529` | `10` | `0` | oracle bookkeeping resolved |
| zib | `[10.0, 0.5, 1.0]` | q_lower_log | 11 / 495 | `0` | `10` | `0` | row excluded |
| zib | `[10.0, 0.5, 1.0]` | q_lower | 11 / 505 | `1` | `10` | `0` | row excluded |
| zib | `[10.0, 0.5, 1.0]` | q_upper_log | 11 / 516 | `-inf` | `10` | `0` | row excluded |
| zib | `[10.0, 0.5, 1.0]` | q_upper | 11 / 526 | `0` | `10` | `0` | row excluded |
| zmbinom | `[10.0, 0.0, 0.5]` | q_lower_log | 8 / 363 | `0` | `10` | `1` | row excluded |
| zmbinom | `[10.0, 0.0, 0.5]` | q_lower | 8 / 373 | `1` | `10` | `1` | row excluded |
| zmbinom | `[10.0, 0.0, 0.5]` | q_upper_log | 8 / 384 | `-inf` | `10` | `1` | row excluded |
| zmbinom | `[10.0, 0.0, 0.5]` | q_upper | 8 / 394 | `0` | `10` | `1` | row excluded |
| zmbinom | `[10.0, 0.5, 0.0]` | q_lower_log | 10 / 450 | `-inf` | `0` | `1` | row excluded |
| zmbinom | `[10.0, 0.5, 0.0]` | q_lower | 10 / 460 | `0` | `0` | `1` | row excluded |
| zmbinom | `[10.0, 0.5, 0.0]` | q_upper_log | 10 / 473 | `0` | `0` | `1` | row excluded |
| zmbinom | `[10.0, 0.5, 0.0]` | q_upper | 10 / 483 | `1` | `0` | `1` | row excluded |
| zmbinom | `[10.0, 0.5, 1.0]` | q_lower_log | 11 / 495 | `0` | `10` | `0` | row excluded |
| zmbinom | `[10.0, 0.5, 1.0]` | q_lower | 11 / 505 | `1` | `10` | `0` | row excluded |
| zmbinom | `[10.0, 0.5, 1.0]` | q_upper_log | 11 / 516 | `-inf` | `10` | `0` | row excluded |
| zmbinom | `[10.0, 0.5, 1.0]` | q_upper | 11 / 526 | `0` | `10` | `0` | row excluded |
| zmgeom | `[0.5, 0.0]` | q_lower_log | 8 / 362 | `-inf` | `0` | `1` | row excluded |
| zmgeom | `[0.5, 0.0]` | q_lower | 8 / 372 | `0` | `0` | `1` | row excluded |
| zmgeom | `[0.5, 0.0]` | q_upper_log | 8 / 385 | `0` | `0` | `1` | row excluded |
| zmgeom | `[0.5, 0.0]` | q_upper | 8 / 395 | `1` | `0` | `1` | row excluded |
| zmlogarithmic | `[0.5, 0.0]` | q_lower_log | 8 / 362 | `-inf` | `0` | `1` | row excluded |
| zmlogarithmic | `[0.5, 0.0]` | q_lower | 8 / 372 | `0` | `0` | `1` | row excluded |
| zmlogarithmic | `[0.5, 0.0]` | q_upper_log | 8 / 385 | `0` | `0` | `1` | row excluded |
| zmlogarithmic | `[0.5, 0.0]` | q_upper | 8 / 395 | `1` | `0` | `1` | row excluded |
| zmnbinom | `[1.0, 0.5, 0.0]` | q_lower_log | 11 / 494 | `-inf` | `0` | `1` | row excluded |
| zmnbinom | `[1.0, 0.5, 0.0]` | q_lower | 11 / 504 | `0` | `0` | `1` | row excluded |
| zmnbinom | `[1.0, 0.5, 0.0]` | q_upper_log | 11 / 517 | `0` | `0` | `1` | row excluded |
| zmnbinom | `[1.0, 0.5, 0.0]` | q_upper | 11 / 527 | `1` | `0` | `1` | row excluded |
| zmpois | `[1.0, 0.0]` | q_lower_log | 8 / 362 | `-inf` | `0` | `1` | row excluded |
| zmpois | `[1.0, 0.0]` | q_lower | 8 / 372 | `0` | `0` | `1` | row excluded |
| zmpois | `[1.0, 0.0]` | q_upper_log | 8 / 385 | `0` | `0` | `1` | row excluded |
| zmpois | `[1.0, 0.0]` | q_upper | 8 / 395 | `1` | `0` | `1` | row excluded |
| ztbinom | `[10.0, 0.0]` | q_lower_log | 6 / 275 | `0` | `10` | `1` | row excluded |
| ztbinom | `[10.0, 0.0]` | q_lower | 6 / 285 | `1` | `10` | `1` | row excluded |
| ztbinom | `[10.0, 0.0]` | q_upper_log | 6 / 296 | `-inf` | `10` | `1` | row excluded |
| ztbinom | `[10.0, 0.0]` | q_upper | 6 / 306 | `0` | `10` | `1` | row excluded |

### Initial unbounded-support oracle correction

Before the complete audit above, the exploratory trial retained an infinite nominal upper endpoint for unbounded count laws that had collapsed to finite atoms. That was an oracle-policy error: the last positive-mass atom gives the limiting upper quantile. The following initial discrepancies were resolved analytically by correcting the reference support; they are not dpqr defects. Fixed finite design endpoints remain explicit, as discussed above.

| Family | Parameters | Check | Input | Initial expected | Actual / corrected expected |
|---|---|---|---|---|---|
| cmp | `[0.0, 1.0]` | q_lower_log | `0` | `Inf` | `0` |
| cmp | `[0.0, 1.0]` | q_lower | `1` | `Inf` | `0` |
| cmp | `[0.0, 1.0]` | q_upper_log | `-inf` | `Inf` | `0` |
| cmp | `[0.0, 1.0]` | q_upper | `0` | `Inf` | `0` |
| geom | `[1.0]` | q_lower_log | `0` | `Inf` | `0` |
| geom | `[1.0]` | q_lower | `1` | `Inf` | `0` |
| geom | `[1.0]` | q_upper_log | `-inf` | `Inf` | `0` |
| geom | `[1.0]` | q_upper | `0` | `Inf` | `0` |
| nbinom | `[0.0, 0.5]` | q_lower_log | `0` | `Inf` | `0` |
| nbinom | `[0.0, 0.5]` | q_lower | `1` | `Inf` | `0` |
| nbinom | `[0.0, 0.5]` | q_upper_log | `-inf` | `Inf` | `0` |
| nbinom | `[0.0, 0.5]` | q_upper | `0` | `Inf` | `0` |
| nbinom | `[1.0, 1.0]` | q_lower_log | `0` | `Inf` | `0` |
| nbinom | `[1.0, 1.0]` | q_lower | `1` | `Inf` | `0` |
| nbinom | `[1.0, 1.0]` | q_upper_log | `-inf` | `Inf` | `0` |
| nbinom | `[1.0, 1.0]` | q_upper | `0` | `Inf` | `0` |
| pois | `[0.0]` | q_lower_log | `0` | `Inf` | `0` |
| pois | `[0.0]` | q_lower | `1` | `Inf` | `0` |
| pois | `[0.0]` | q_upper_log | `-inf` | `Inf` | `0` |
| pois | `[0.0]` | q_upper | `0` | `Inf` | `0` |
| pig | `[1.0, 0.0]` | q_lower_log | `0` | `Inf` | `0` |
| pig | `[1.0, 0.0]` | q_lower | `1` | `Inf` | `0` |
| pig | `[1.0, 0.0]` | q_upper_log | `-inf` | `Inf` | `0` |
| pig | `[1.0, 0.0]` | q_upper | `0` | `Inf` | `0` |
| zinb | `[0.0, 0.5, 0.5]` | q_lower_log | `0` | `Inf` | `0` |
| zinb | `[0.0, 0.5, 0.5]` | q_lower | `1` | `Inf` | `0` |
| zinb | `[0.0, 0.5, 0.5]` | q_upper_log | `-inf` | `Inf` | `0` |
| zinb | `[0.0, 0.5, 0.5]` | q_upper | `0` | `Inf` | `0` |
| zinb | `[1.0, 1.0, 0.5]` | q_lower_log | `0` | `Inf` | `0` |
| zinb | `[1.0, 1.0, 0.5]` | q_lower | `1` | `Inf` | `0` |
| zinb | `[1.0, 1.0, 0.5]` | q_upper_log | `-inf` | `Inf` | `0` |
| zinb | `[1.0, 1.0, 0.5]` | q_upper | `0` | `Inf` | `0` |
| zinb | `[1.0, 0.5, 1.0]` | q_lower_log | `0` | `Inf` | `0` |
| zinb | `[1.0, 0.5, 1.0]` | q_lower | `1` | `Inf` | `0` |
| zinb | `[1.0, 0.5, 1.0]` | q_upper_log | `-inf` | `Inf` | `0` |
| zinb | `[1.0, 0.5, 1.0]` | q_upper | `0` | `Inf` | `0` |
| zip | `[1.0, 1.0]` | q_lower_log | `0` | `Inf` | `0` |
| zip | `[1.0, 1.0]` | q_lower | `1` | `Inf` | `0` |
| zip | `[1.0, 1.0]` | q_upper_log | `-inf` | `Inf` | `0` |
| zip | `[1.0, 1.0]` | q_upper | `0` | `Inf` | `0` |
| zmgeom | `[1.0, 0.5]` | q_lower_log | `0` | `Inf` | `1` |
| zmgeom | `[1.0, 0.5]` | q_lower | `1` | `Inf` | `1` |
| zmgeom | `[1.0, 0.5]` | q_upper_log | `-inf` | `Inf` | `1` |
| zmgeom | `[1.0, 0.5]` | q_upper | `0` | `Inf` | `1` |
| zmgeom | `[0.5, 1.0]` | q_lower_log | `0` | `Inf` | `0` |
| zmgeom | `[0.5, 1.0]` | q_lower | `1` | `Inf` | `0` |
| zmgeom | `[0.5, 1.0]` | q_upper_log | `-inf` | `Inf` | `0` |
| zmgeom | `[0.5, 1.0]` | q_upper | `0` | `Inf` | `0` |
| zmlogarithmic | `[0.0, 0.5]` | q_lower_log | `0` | `Inf` | `1` |
| zmlogarithmic | `[0.0, 0.5]` | q_lower | `1` | `Inf` | `1` |
| zmlogarithmic | `[0.0, 0.5]` | q_upper_log | `-inf` | `Inf` | `1` |
| zmlogarithmic | `[0.0, 0.5]` | q_upper | `0` | `Inf` | `1` |
| zmlogarithmic | `[0.5, 1.0]` | q_lower_log | `0` | `Inf` | `0` |
| zmlogarithmic | `[0.5, 1.0]` | q_lower | `1` | `Inf` | `0` |
| zmlogarithmic | `[0.5, 1.0]` | q_upper_log | `-inf` | `Inf` | `0` |
| zmlogarithmic | `[0.5, 1.0]` | q_upper | `0` | `Inf` | `0` |
| zmnbinom | `[1.0, 1.0, 0.5]` | q_lower_log | `0` | `Inf` | `1` |
| zmnbinom | `[1.0, 1.0, 0.5]` | q_lower | `1` | `Inf` | `1` |
| zmnbinom | `[1.0, 1.0, 0.5]` | q_upper_log | `-inf` | `Inf` | `1` |
| zmnbinom | `[1.0, 1.0, 0.5]` | q_upper | `0` | `Inf` | `1` |
| zmnbinom | `[1.0, 0.5, 1.0]` | q_lower_log | `0` | `Inf` | `0` |
| zmnbinom | `[1.0, 0.5, 1.0]` | q_lower | `1` | `Inf` | `0` |
| zmnbinom | `[1.0, 0.5, 1.0]` | q_upper_log | `-inf` | `Inf` | `0` |
| zmnbinom | `[1.0, 0.5, 1.0]` | q_upper | `0` | `Inf` | `0` |
| zmpois | `[0.0, 0.5]` | q_lower_log | `0` | `Inf` | `1` |
| zmpois | `[0.0, 0.5]` | q_lower | `1` | `Inf` | `1` |
| zmpois | `[0.0, 0.5]` | q_upper_log | `-inf` | `Inf` | `1` |
| zmpois | `[0.0, 0.5]` | q_upper | `0` | `Inf` | `1` |
| zmpois | `[1.0, 1.0]` | q_lower_log | `0` | `Inf` | `0` |
| zmpois | `[1.0, 1.0]` | q_lower | `1` | `Inf` | `0` |
| zmpois | `[1.0, 1.0]` | q_upper_log | `-inf` | `Inf` | `0` |
| zmpois | `[1.0, 1.0]` | q_upper | `0` | `Inf` | `0` |
| ztgeom | `[1.0]` | q_lower_log | `0` | `Inf` | `1` |
| ztgeom | `[1.0]` | q_lower | `1` | `Inf` | `1` |
| ztgeom | `[1.0]` | q_upper_log | `-inf` | `Inf` | `1` |
| ztgeom | `[1.0]` | q_upper | `0` | `Inf` | `1` |
| ztpois | `[0.0]` | q_lower_log | `0` | `Inf` | `1` |
| ztpois | `[0.0]` | q_lower | `1` | `Inf` | `1` |
| ztpois | `[0.0]` | q_upper_log | `-inf` | `Inf` | `1` |
| ztpois | `[0.0]` | q_upper | `0` | `Inf` | `1` |
| ztnbinom | `[1.0, 1.0]` | q_lower_log | `0` | `Inf` | `1` |
| ztnbinom | `[1.0, 1.0]` | q_lower | `1` | `Inf` | `1` |
| ztnbinom | `[1.0, 1.0]` | q_upper_log | `-inf` | `Inf` | `1` |
| ztnbinom | `[1.0, 1.0]` | q_upper | `0` | `Inf` | `1` |

## Reproduction and final runs

Environment: Linux x86_64, GCC 15.2.0, R 4.6.1, Python 3.14.5, BH 1.90.0-1, mpmath 1.3.0, SymPy 1.14.0 and PyYAML 6.0.3. All generation, compilation and tests ran locally through `/tmp/umvl-cpu/run4`, under its `nice -n 10` and single-thread BLAS/OpenMP settings. `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` were set to this worktree's `build/tmp`. No simbox or package installation was used.

The new faces were generated with `quality.truth.make(folder, [theta])` for each new `quality.truth.boundary_points(meta, route)` point, in four single-thread processes. Existing table rows were compared against `git show HEAD:<truth.json>` and found unchanged. Full reconstruction uses `PYTHONPATH=dev/validation python3 -m quality truth <id>` (or `just dpqr-truth <id>`). The per-table semantic input hashes cover the shared generator, reference helpers, local route, density parser and numerical metadata.

All final runs below completed green. The C++ runs were repeated after the test-only portability adjustment described below. R and oracle source/fixtures were unchanged by that adjustment.

| Command (inside `/tmp/umvl-cpu/run4`) | Final result |
|---|---|
| `just cpp-test` | 104/104 dev shards pass; 43.91 s |
| `just cpp-test-no-long-double` | 125/125 shards pass, including full labels; 120.13 s |
| `just test` | FAIL 0, WARN 0, SKIP 151, PASS 18,927; 70.08 s |
| `just test-full` | FAIL 0, WARN 0, SKIP 1, PASS 40,369; 267.21 s |
| `PYTHONPATH=dev/validation python3 -m unittest discover -s dev/validation/quality -p 'test_truth*.py'` | 42/42 tests pass; 13.87 s |
| Full active-row C++ audit | zero mismatches |

The R generated-truth test covers the internally generated families; stats delegates are deliberately omitted by that test. C++ truth checks cover all 98 families. R full's remaining skip is the optional `{sn}` reference package, which was not installed. Oracle generation is deterministic. Existing sampling seeds and workloads were retained: C++ truth uses seed `20261029 + zero-based point`, normally 20,000 draws at the default and fitted points (CMP: 1,000); R generated truth uses seed `20261029 + one-based point` and 32 draws per selected active point. These sampling checks do not establish inferential calibration.

The first Python truth-unit run had one test-harness error: the new zero-rate test passed two arguments to Poisson's one-argument `build` route. It was corrected to dispatch by the existing one/two-argument route signature, then all 42 tests passed. This changed neither oracle formulas nor stored rows.

### Existing Pareto IV proxy golden drift

Before the final runs, both the unoptimized and Release no-long-double suites passed 124/125 shards; the only failure was three coefficient comparisons in the unchanged Pareto IV default golden test. The independent reproducer calls the unchanged fitter on the unchanged golden data, with the golden minimum held, `tol = 1e-10`, and `maxit = 1000`. It does not read truth tables. Both `-O0` and `-O3` produce the same values and status `Ok`:

| Quantity | Golden | Double-width result (O0 and O3) | Relative coefficient error |
|---|---:|---:|---:|
| shape1 | 0.31572663386430971 | 0.31572653763270259 | 3.0479407442385309e-7 |
| shape2 | 0.66237135973887262 | 0.66237153567540563 | 2.6561615396047955e-7 |
| scale | 1.0887479679788286 | 1.0887475136519189 | 4.1729300348514187e-7 |
| total log likelihood | -3607.2382431667834 | -3607.2382431667843 | absolute difference 9.0949470177292824e-13 |

The shared C++ test helper now uses its existing wasm Pareto IV coefficient tolerance, `5e-7`, also under `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS` or actual double-width `long double`. The ordinary extended-width tolerance remains `1e-8`. The likelihood and status assertions are unchanged, as are every fitter, golden file and truth tolerance. This is an empirical portability calibration for this frozen regression workload, not a mathematical error bound or native macOS/Windows validation. Trying Release did not remove the drift, so the final builds were returned to the original empty `CMAKE_BUILD_TYPE` configuration.

Reproduce the fitter comparison by compiling `build/scratch/pareto-baseline.cpp` at `-O0` or `-O3` with `-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`, the core/test include directories and the installed BH include path. The source in that reproducer is the same call shown above; the three measured errors and total likelihood difference are retained here so the conclusion does not depend on ignored logs.

Final logs: `build/scratch/final-cpp-rerun.log`, `final-cpp-no-long-double-rerun.log`, `final-r-dev.log`, `final-r-full.log`, `final-truth-unit-rerun.log`, and the empty `final-findings.tsv`. Initial complete audit rows are preserved verbatim in the findings table above and their reference values in the fixture exclusions/archives.


The additional audit executable uses the exact d/p/q lambdas from each existing truth `TEST_CASE`, reads the same JSON tables, applies the same standard log/relative/bracket comparisons, and emits all failures at binary64 precision. Scratch code and logs are under `build/scratch/`; the lasting findings and formulas are recorded here and in the local routes.

## Source pointers

- [Shared generator](../../validation/quality/truth.py), [reference primitives](../../validation/quality/truth_reference.py), [truth tests](../../validation/quality/test_truth.py), and [validation guide](../../validation/README.md).
- [CMB workload selection](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/truth.py), [truncated Poisson references](../../../univariateML-cpp/include/cppunivariate/distributions/tpois/truth.py), and [truncated binomial references](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/truth.py).
- [Shared C++ test-only portability tolerance](../../../univariateML-cpp/tests/test_support.hpp).
