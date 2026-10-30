# TASK-104: standard-member defaults

Base: `5f11fb9e878219ac47a9a372e985eb69da6198e3`, branch `codex/defaults`, plus the tested working-tree diff below. No commits or Git writes.

129 of 237 coefficients changed across 76 of 98 families; 108 coefficients and 22 whole families are unchanged. Values in each row correspond in order.

| Family | Coefficient | Old | New | Reason |
|---|---|---|---|---|
| bbinom | alpha, beta | 2, 3 | 1, 1 | simple interior shape |
| beta | shape1, shape2 | 2.0, 3.0 | 1, 1 | simple interior shape |
| betapr | shape1, shape2 | 2.0, 3.0 | 1, 1 | simple interior shape |
| bhatt | a | 2 | 1 | natural design / conventional member |
| bnbinom | alpha, beta | 2, 5 | 1, 1 | simple interior shape |
| burr | shape2 | 2.0 | 1 | simple interior shape |
| cmb | prob, nu | 0.3, 1.5 | 0.5, 1 | balanced probability; simple interior shape |
| cmp | lambda, nu | 4, 1.5 | 1, 1 | unit rate; simple interior shape |
| dgamma | shape | 2 | 1 | simple interior shape |
| dlaplace | scale | 0.4 | 0.5 | Geometric probability 0.5; 1 is outside domain. |
| dunif | min | 2.0 | 0 | natural integer range |
| dweibull | shape1, shape2 | 0.8, 1.5 | 0.5, 1 | balanced probability; simple interior shape |
| fatigue | alpha, beta | 3.0, 2.0 | 1, 1 | simple interior shape; unit scale |
| foldnorm | mu | 2 | 1 | interior parent location; zero is nonregular boundary |
| fpareto | shape1, shape2, shape3 | 0.3, 2, 0.4 | 2, 1, 2 | Symmetric simple shapes; unit triple selects trgamma limit in seeded fit. |
| frechet | lambda | 2 | 1 | simple interior shape |
| gamma | shape, scale | 2.0, 0.5 | 1, 1 | simple interior shape; unit scale |
| gb2 | shape1, shape2, shape3 | 0.3, 2.0, 0.4 | 2, 1, 2 | Symmetric simple shapes; unit triple selects trgamma limit in seeded fit. |
| ged | nu | 3.0 | 2 | Gaussian member |
| genbeta | shape1, shape2, shape3 | 0.4, 0.3, 2 | 1, 2, 1 | Interior shapes; shape2=2 avoids unit-triple nonidentifiability. |
| gev | xi | 0.3 | 0 | Gumbel/exponential member |
| gpd | xi | 0.3 | 0 | Gumbel/exponential member |
| gumbel | mu, sigma | 3.0, 3.0 | 0, 1 | origin; unit scale |
| ht | nu | 3 | 5 | t with five degrees of freedom |
| hyper | m, N, k | 20, 50, 10 | 5, 10, 5 | small feasible balanced design |
| invburr | shape2 | 2.0 | 1 | simple interior shape |
| invgamma | alpha, beta | 3.0, 4.0 | 1, 1 | simple interior shape; unit scale |
| invgauss | mean, shape | 3.0, 4.0 | 1, 1 | Positive unit mean and shape (zero mean is outside domain). |
| invpareto | shape | 2 | 1 | simple interior shape |
| invtrgamma | shape1, shape2, scale | 3.0, 1.5, 0.5 | 1, 1, 1 | simple interior shape; unit scale |
| invweibull | shape, scale | 2.0, 0.3333333333333333 | 1, 1 | simple interior shape; unit scale |
| kumar | a, b | 2.0, 3.0 | 1, 1 | simple interior shape |
| lgamma | shapelog, ratelog | 2.0, 3.0 | 1, 1 | simple interior shape; unit rate |
| lgser | theta | 0.9 | 0.5 | interior bounded rate |
| llogis | scale | 0.5 | 1 | unit scale |
| logitnorm | mu, sigma | 2.0, 3.0 | 0, 1 | origin; unit scale |
| lomax | kappa | 2 | 1 | simple interior shape |
| lst | df | 3.0 | 5 | t with five degrees of freedom |
| naka | shape, scale | 2.0, 3.0 | 1, 1 | natural design / conventional member; unit scale |
| nbinom | size, prob | 10.0, 0.3 | 1, 0.5 | geometric member; balanced probability |
| nhyper | m, N | 20, 50 | 5, 10 | small feasible balanced design |
| nsbeta | shape1, shape2 | 2, 3 | 1, 1 | simple interior shape |
| pareto | b | 2.0 | 1 | natural support origin |
| pareto2 | shape | 0.4 | 1 | simple interior shape |
| pareto3 | shape | 2 | 1 | simple interior shape |
| pareto4 | shape1, shape2 | 0.3, 0.7 | 1, 1 | simple interior shape |
| pig | mean, shape | 2.0, 3.0 | 1, 1 | simple interior shape |
| pois | lambda | 2.0 | 1 | unit rate |
| power | beta | 2.0 | 1 | simple interior shape |
| rayleigh | sigma | 2.0 | 1 | unit scale |
| sged | nu, xi | 3.0, 3.0 | 2, 1 | Gaussian member; symmetric member |
| sgomp | eta | 3 | 1 | simple interior shape |
| skellam | mu1, mu2 | 4, 3 | 1, 1 | unit rate |
| sn | alpha | 3 | 0 | Gaussian member |
| snorm | xi | 3.0 | 1 | symmetric member |
| sstd | nu, xi | 3.0, 2.0 | 5, 1 | t with five degrees of freedom; symmetric member |
| std | nu | 3.0 | 5 | t with five degrees of freedom |
| tbinom | prob, size | 0.4, 12 | 0.5, 10 | balanced probability; natural design / conventional member |
| tlambda | lambda | 0.14 | 0 | logistic member |
| tpois | lambda | 3 | 1 | unit rate |
| trgamma | shape1, shape2, scale | 3.0, 1.5, 2.0 | 1, 1, 1 | simple interior shape; unit scale |
| triang | c | 0.4 | 0.5 | symmetric unit interval |
| weibull | shape, scale | 3.0, 4.0 | 1, 1 | simple interior shape; unit scale |
| yulesimon | rho | 2 | 1 | simple interior shape |
| zib | size, prob, pi | 8.0, 0.4, 0.4 | 10, 0.5, 0.5 | small design; balanced probability |
| zinb | size, prob, pi | 2.0, 0.4, 0.3 | 1, 0.5, 0.5 | geometric member; balanced probability |
| zipf | N, s | 3.0, 0.5 | 10, 1 | small design / standard exponent |
| zmbinom | size, prob, p0 | 8.0, 0.4, 0.3 | 10, 0.5, 0.5 | small design; balanced probability |
| zmgeom | prob, p0 | 0.4, 0.3 | 0.5, 0.5 | balanced probability |
| zmlogarithmic | prob, p0 | 0.4, 0.3 | 0.5, 0.5 | balanced probability |
| zmnbinom | size, prob, p0 | 2.0, 0.4, 0.3 | 1, 0.5, 0.5 | geometric member; balanced probability |
| zmpois | lambda, p0 | 2.0, 0.3 | 1, 0.5 | unit rate; balanced probability |
| ztbinom | size, prob | 8.0, 0.4 | 10, 0.5 | small design; balanced probability |
| ztgeom | prob | 0.4 | 0.5 | balanced probability |
| ztnbinom | size, prob | 2.0, 0.4 | 1, 0.5 | geometric member; balanced probability |
| ztpois | lambda | 2.0 | 1 | unit rate |

Unchanged: cauchy, dnorm, emg, hcauchy, hnorm, laplace, lnorm, logis, norm, slash (standard location/scale); exp, invexp (unit rate/scale); geom (prob=0.5); binom, zip (small design/unit rate and balanced probability); gompertz, invparalogis, paralogis (unit shapes/rates/scales); tnorm (standard untruncated normal); unif (unit interval); huber (standard location/scale, conventional 1.345 tuning); zeta (interior s=2).

Exceptions: dunif max=10 is a natural count range; hyper (m,N,k)=(5,10,5) and nhyper (m,N,r)=(5,10,3) are small feasible designs; Pareto's positive lower endpoint is 1; folded-normal mu=1 avoids nonregular boundary zero. GB2/fpareto use symmetric shapes (2,1,2), because (1,1,1) selects a limit in the seeded example. Genbeta shape2=2 avoids nonidentifiability at the all-unit triple.

Sampler checks: every table explicitly selects the README simulate vector and golden-fit vector through two sampler_points indices. Historical stress-grid construction is retained, with the README point appended if absent (up to nine points). C++ uses seed 20261029+point, 20,000 draws per selected point (CMP 1,000), the existing validity and five-binomial-standard-error checks: 196 runs, 3,882,000 draws. The R shell still visits every stored point.

Truth schema 3: quantile rows are [point, lower, log_input, input, true_q, lo, hi, jump_ambiguous, zero_floor]. For discrete interior probes with representable adjacent integers (abs(q)<2^53), evaluate the independent CDF or survival at q-1 and q on the probe's own natural/log scale. Relative distance <=1e-12 marks a jump; lo and hi store its two adjacent acceptable support points. C++ and R accept either endpoint in addition to the existing 1e-9 relative quantile tolerance; d/p tolerances are unchanged. No probability-specific C++ tie-breaking is needed.

Continuous zero rule: only when the true quantile is exactly zero inside the support, store absolute floor 1e-12/f(0), proportional to the model scale. Both consumers use max(relative tolerance, stored floor). Nonzero near-median log quantiles retain relative checks; natural and rounded-log probes may differ. The exact symmetric reductions are retained.

Other oracle fixes: scale-aware differentiation for paralogis/invparalogis at shape 0.1; Kumaraswamy y=x^a -> Beta(1,b) normalization with bounded omitted mass; exact normal-reference inversion for sn alpha=0. A guard of abs(logp)*mp.eps^0.75 stabilizes exact discrete jumps far below binary64 resolution. Generation status goes to scratch via --report-dir; historical notes are unchanged.

C++ changes kept: sn alpha=0 uses the normal quantile; snorm/sged/sstd xi=1 use their symmetric base quantile; the shared ztpois core uses geometric survival for NB size=1. These identities hold for every probability input. Removed all task-added hyper/nhyper probability-constant branches, the bnbinom unit-member branch, yulesimon's unit-shape delegate and dweibull's extended-precision branch. Those five dpqr.hpp files are byte-identical to the base revision. No mle.hpp changes. No correctness decision relies on extra long-double precision.

Tests changed: test_support.hpp and test-generated-dpqr.R consume the ambiguity/floor fields; quality/test_truth.py checks sampler indexing, oracle validation, probe-scale jump detection and zero-floor scaling. bnbinom/yulesimon/dweibull/hyper/nhyper C++ regressions check exact answers away from jumps and either adjacent answer at jumps. bnbinom/yulesimon R regressions do likewise. sn/snorm/sged/sstd and ztnbinom closed-form reduction checks remain. Yulesimon inverse inequalities allow eight epsilons for rounded CDF outputs, with independent integer truth checks retained. mllst joins the existing profile-fit 2e-5 coefficient tolerance; its strict likelihood check remains. The pre-existing half-t/half-Cauchy Python fixture now supplies mu=0. Closed-form test identities: unit bnbinom S(k)=1/(k+2), unit Yule-Simon S(k)=1/(k+1), geometric discrete Weibull S(k)=shape1^(k+1), and geometric positive NB S(k)=(1-prob)^k. Exact integer expectations use probes away from jumps; jump probes admit adjacent points.

Examples: all 98 retained. Held design literals in hyper, nhyper, tbinom, zib, zmbinom and ztbinom match the new defaults. Validation evaluates every README block and separately uses set.seed(1), rml(200, mlX()) and its final example fit. Golden fit_args and data remain untouched.

Regeneration: all 98 tables, 0 gaps; schema 3 throughout. Parameter points 572 -> 647; d/p value rows 35,122 -> 40,097; quantile rows 25,168 -> 28,468. There are 132 jump-ambiguous rows in 18 families and 110 zero-floor rows in 16 families. Every table matches its README SHA-256. All changes are generator output, with no manual numerical-row edits.

Numerical-row diff by parameter/input key: d/p values 34,706 identical, 17 changed, 399 removed, 5,374 added; q answer/bound cores 22,802 identical, 1,442 changed, 924 removed, 4,224 added. All quantile rows gain the two explicit acceptance fields. Regeneration also refreshes stale numerical rows, normalization timings and checksums.

Validation: Python 38/38 tests pass, including jump checks in both tails/scales and scale proportionality of zero floors. C++ just cpp-test: 6/6 targets pass; 937 cases and 1,292,405 assertions total, including 915 distribution cases and 1,281,358 distribution assertions; 0 failures/skips (199.64 s). Fallback Python also passes 38/38. Fallback C++ also passes all 6 targets, with the same 937 cases and 1,292,405 assertions (323.28 s). Fallback uncapped R: 39,035 passes, 0 failures/warnings, 1 skip (optional external sn density comparison; sn is not installed). Fallback just check: Status OK, 0 warnings/notes; installed tests 39,022 passes, 0 failures/warnings, 2 skips (external sn and explicit On CRAN test_special.R), 1,023.924 s installed-test elapsed. Both local 98-family README and seed-1 rml(200)+example-fit diagnostics pass without warnings/errors.

Commands: initially /tmp/umvl-simbox/run; after simbox I/O failure /tmp/umvl-cpu/run with TMPDIR=/home/jonas/.cache/umvl-tmp, MAKEFLAGS=-j1, CMAKE_BUILD_PARALLEL_LEVEL=1. just dpqr-truth <all 98 IDs> --report-dir /tmp/umvl-codex/defaults/review-truth-status (three serial-per-worker groups); just cpp-test; just regen; python3 -m unittest quality.test_truth quality.test_truth_gaps quality.test_check. Final R: export TMPDIR=$HOME/umvl/tmp UMVL_DISTRIBUTIONS_DIR=$PWD/cppunivariate/include/cppunivariate/distributions; in univariateML/package run Rscript -e 'options(testthat.progress.max_fails = Inf); testthat::test_local(".", stop_on_failure = TRUE)'; just check.

Environment: truth generation and initial C++ validation on simbox Linux x86_64, GCC 15.2.0, R 4.6.1, Python 3.12.3/mpmath 1.2.1/SymPy 1.12/PyYAML 6.0.1. Simbox R/check were interrupted by filesystem I/O errors (even coreutils failed), not counted as completed validations. Authorized workstation fallback: GCC 15.2.0, R 4.6.1, Python 3.14.5/mpmath 1.3.0/SymPy 1.14.0/PyYAML 6.0.3. Long jobs run nice=10; BLAS/OpenMP=1; no more than four compute cores together. Package check uses an isolated copy of the same source to avoid changing the package under the concurrent uncapped R suite.

Invariants: all 98 golden.json files remain byte-identical by SHA-256; no just goldens. The isolated check copy matches 1,028 relevant source files by SHA-256. Protected-file and design/ diffs are empty; git diff --check passes. Defaults remain the approved 129 changes across 76 families. No dependency installs, board edits or protected-file edits; no files added under design/. The withdrawn design note is deleted; its lasting numerical conclusions are recorded above. Portability: no native macOS arm64 execution was available. The five restored files are byte-identical to baseline; ambiguity acceptance no longer requires extended long-double precision to select one prescribed neighbour.

Tested tracked diff SHA-256: `7f67f00368afbc53b07f067d44a4e169c915d23a248843f0754742e7997e9904`.
