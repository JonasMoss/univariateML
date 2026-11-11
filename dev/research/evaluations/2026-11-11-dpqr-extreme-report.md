# TASK-189: extreme-domain d/p/q audit

Baseline: `bcea25d29800d4ddf058182c1749584b704cba3b`. Worktree: `codex/dpqr-extreme`. Measurements made on 2026-11-11. Changes remain uncommitted, as instructed. No fitter source, golden sample, protected status/roadmap file, Fréchet file, or inverse-Weibull file was edited. No dependency installation, board mutation, publication, or delegation was performed.

## Result and validation boundary

The expanded truth replay has **336,560 comparisons and zero active failures**, in workstation native and double-only modes and simbox GCC 13 modes. Baseline failures fall from **2,661 to 210** including exclusions. The remaining 210 failing comparisons are 208 pre-existing zero-probability truncation-boundary checks and two newly documented SGED natural-tail precision exclusions. All 99 golden likelihood seams agree; all finite d/p/q disagreements found in the 9,251-record archived/golden seam replay have been resolved against independent references.

**The required full validation is not all green.** Correct SGED density evaluation exposes three existing fitter likelihood assertions. GCC 13 double-only testing also reproduces three inherited beta-negative-binomial bitwise-symmetry assertions on untouched baseline headers. These fitter issues were investigated and independently reproduced; the brief explicitly forbids repairing fitters or rewriting their tests. Their exact evidence and final suite outcomes appear below. No failing assertion was suppressed.

Separate targeted measurement of the out-of-scope Fréchet and inverse-Weibull implementations finds 99 failures in 120 checks. There are 131 explicit high-precision reference gaps. Passing a finite grid does not prove the contract at every binary64 input; these gaps and named NaN regions remain open. No claim of a completed uniform-domain proof is made.

## Generator and table coverage

The historical parameter, value, quantile, normalization and sampler prefixes are retained. The shared generator adds uncapped one-coordinate factors 1e-8 and 1e8, projects dependent domains to representable interior values, adds signed location/scale offsets, and stresses nonunit scales with concentrating shapes. Family routes supply coupled limit paths: fixed-mean gamma/beta/negative-binomial concentration; GEV/GPD shape approaching zero; Lomax/Pareto concentration with scale/shape fixed; small-shape reciprocal powers with finite product; and thin two-piece skew limits. Both tails, log scales, binary64 quantile cuts, support endpoints and round trips retain the existing row design.

The coverage inventory has 580 requested extreme points across 101 families, with explicit joint paths in 41 families. After overlap with historical points and explicit reference failures, 399 new parameter points are retained (3.95 per family on average). The two TASK-187 tables are measured copies under this report directory, not writes to their family folders. See [coverage.json](2026-11-11-dpqr-extreme-data/coverage.json.gz) and [declared-limits.json](2026-11-11-dpqr-extreme-data/declared-limits.json.gz).

| Quantity | Before | After | Growth |
|---|---:|---:|---:|
| JSON bytes | 9,449,554 | 13,720,451 | 45.2% |
| Parameter points | 759 | 1,158 | 52.6% |
| Active value rows | 43,724 | 59,333 | 35.7% |
| Active quantile rows | 33,396 | 50,936 | 52.5% |

The after value count excludes the two retained SGED rows in `excluded_values`. No pre-existing row or exclusion was removed. [final-sizes.json](2026-11-11-dpqr-extreme-data/final-sizes.json) records per-family byte and row counts; [metadata-migration.json](2026-11-11-dpqr-extreme-data/metadata-migration.json) records the checksum-preserving metadata migration. Frozen sampler selectors remain separate from newly added d/p/q stresses.

The references use exact binary64 inputs converted to multiprecision, at least 160 decimal digits for generation, higher precision when concentration requires it, and independent normalization/antiderivative checks. Conditional count references use positive finite products, avoiding a symbolic factorial/summation failure. Endpoints are resolved before singular symbolic gamma evaluation. Quantile bracketing no longer accepts a chart cap or an early binary64 rounding exit. Each point is isolated with a 120-second/1 GiB reference budget; a failed worker is an oracle gap, never a passing row. All families were regenerated on simbox, at most six workers; generation receipts are in `generation/`. Five focused generator tests pass on both platforms.

## Ranked truth failures

Counts include retained exclusions. Rank is by baseline failing scalar comparisons. Every failing row records family, function, exact inputs, parameters, expected/actual value, normalized error, category and exclusion reason in [audit-failures.json](2026-11-11-dpqr-extreme-data/audit-failures.json.gz). The contract is unchanged: log errors use 1e-10*max(1,abs(reference)); natural probabilities use relative 1e-10; quantiles use relative 1e-9 or the existing cut/resolution floor.

| Family | Before | After | Active after |
|---|---:|---:|---:|
| bnbinom | 481 | 0 | 0 |
| tbinom | 224 | 104 | 0 |
| fpareto | 190 | 0 | 0 |
| ztnbinom | 181 | 0 | 0 |
| nbinom | 180 | 0 | 0 |
| zinb | 180 | 0 | 0 |
| zmnbinom | 180 | 0 | 0 |
| fatigue | 131 | 0 | 0 |
| weibull | 121 | 0 | 0 |
| tpois | 104 | 104 | 0 |
| betapr | 97 | 0 | 0 |
| pareto3 | 93 | 0 | 0 |
| gb2 | 77 | 0 | 0 |
| llogis | 70 | 0 | 0 |
| sged | 64 | 2 | 0 |
| bbinom | 54 | 0 | 0 |
| nsbeta | 30 | 0 | 0 |
| beta | 28 | 0 | 0 |
| snorm | 26 | 0 | 0 |
| invtrgamma | 25 | 0 | 0 |
| sstd | 23 | 0 | 0 |
| trgamma | 23 | 0 | 0 |
| bhatt | 18 | 0 | 0 |
| genbeta | 13 | 0 | 0 |
| emg | 8 | 0 | 0 |
| huber | 8 | 0 | 0 |
| tnorm | 7 | 0 | 0 |
| pareto4 | 4 | 0 | 0 |
| cmb | 3 | 0 | 0 |
| invburr | 3 | 0 | 0 |
| burr | 2 | 0 | 0 |
| hcauchy | 2 | 0 | 0 |
| hyper | 2 | 0 | 0 |
| logitnorm | 2 | 0 | 0 |
| pareto2 | 2 | 0 | 0 |
| triang | 2 | 0 | 0 |
| unif | 2 | 0 | 0 |
| expweibull | 1 | 0 | 0 |

## Numerical repairs and their evidence

Weibull and log-logistic were repaired first. Near a scale, Sterbenz subtraction retains the exact displacement before `log1p`; multiplying separately rounded logarithms by a large shape loses the required digits. Density terms and inverse hazards retain their small residuals rather than subtracting large rounded terms. The 68 independent priority checks have maximum normalized error 3.43e-16; nine centered Weibull checks improve from a worst 0.402 to zero at binary64 output resolution.

The remaining repairs follow measured failures: centered gamma and beta deviances replace normalizer cancellation; a bounded gamma quadrature covers the concentrating regime where Boost returns an inaccurate tail; exact beta submodels and compensated beta log odds preserve asymmetric concentration. Gamma reciprocal-power limits retain the finite product a*c even when the latent log variable overflows. Count densities use beta identities, finite products or Stirling deviances; truncated count laws sum positive weights relative to their retained mode with geometric remainder control. Their quantiles compare conditional odds, retaining signs around a rounded half probability. Affine inverses and shifted ratios retain physical displacements before scaling; Gaussian, GED and Student skew moments use compensated two-word arithmetic with propagated practical error gates.

Each changed dossier contains its family-specific error argument. [change-manifest.json](2026-11-11-dpqr-extreme-data/change-manifest.json) separates the generator, shared primitives and 41 family numerical files for later commits; no commits were made because the job forbids git writes. [nan-regions.json](2026-11-11-dpqr-extreme-data/nan-regions.json) collects the added precision-region discussions. Certificates here mean exact-arithmetic identities and bounds conditional on computed comparisons, with stated practical floating-point margins; they are not outward-rounded interval certificates.

| Independent witness set | Checks | Result |
|---|---:|---|
| Centered gamma density | 111 | max error 3.98e-16 |
| Gamma tail quadrature | 74 | max error 3.23e-15 |
| Centered beta density | 63 | max error 4.00e-16 |
| Exact beta submodel, each machine mode | 612 | max error 2.51e-13 |
| Asymmetric beta-family density | 114 | zero finite failures and NaNs; max 5.28e-15 |
| Compensated normal/Laplace skew cases | 1,242 | max error 4.89e-15 |
| General GED/Student skew cases | 4,907 | zero finite failures; 307 named NaNs; max resolved 6.25e-13 |
| Multiprecision skew moments | 22 | all valid; zero failures |
| Thin logit-normal | 672 | zero failures and NaNs; max 1.06e-13 |
| Concentrating binomial/negative-binomial | 219 | max error 8.23e-14 |
| Reciprocal-power finite-product limits | 2,070 | max error 1.66e-13 |
| Fatigue centered displacement | 48 | worst 9.08e-5 before, 1.89e-15 after |
| Fatigue inverse-product underflow | 4 | max error 2.13e-16 |

The general skew study is an independent 240-digit transformation, not a copy of the production moment implementation. The baseline fails 1,222 of those checks (734 SGED, 488 SSTD). Current named NaNs are 195 SGED and 112 SSTD; their exact inputs and margins remain in [skew-smoke.json](2026-11-11-dpqr-extreme-data/skew-smoke.json.gz). All 384 initially failing scalar likelihood-seam witnesses now pass, with zero NaNs: [seam-row-replay.json](2026-11-11-dpqr-extreme-data/seam-row-replay.json).

## Likelihood seam and fitter findings

The seam covers 99 golden samples and 9,152 successful archived fit payloads from the November 9 fit-audit replay. Every disagreement beyond 1e-10 relative was evaluated independently at 100–1,600 digits for the exact returned coefficients. Agreement between the fitter and d/p/q alone is not independent evidence that either is correct. Final workstation and GCC 13 attribution counts are:

| Attribution | Workstation | GCC 13 |
|---|---:|---:|
| dossier-excluded model | 4 | 4 |
| fitter | 305 | 305 |
| invalid fit parameters | 104 | 104 |
| named precision-region NaN; fitter | 13 | 13 |

There is no unresolved reference attribution. These counts concern archived returned coefficients and likelihoods; the archive was not refitted wholesale. The fresh cross-binding SGED witnesses below confirm a current defect. “Invalid fit parameters” includes archived boundary/unsupported coefficient vectors and is not a claim that every one is a finite interior fitter bug. The four dossier-excluded models are zero-probability conditioning events. A named precision-region NaN is distinguished from a finite wrong density; 13 such cases also have an independently wrong reported fitter likelihood. Exact data, coefficients, values, errors, archive indices and flags are in [seam-final.json](2026-11-11-dpqr-extreme-data/seam-final.json.gz) and [gcc13-seam-final.json](2026-11-11-dpqr-extreme-data/gcc13-seam-final.json.gz).

Fitter likelihood errors by family (including the named-NaN cases whose fitter side is wrong):

| Family | Cases |
|---|---:|
| sged | 65 |
| sstd | 61 |
| gb2 | 31 |
| ged | 29 |
| fpareto | 26 |
| snorm | 19 |
| betapr | 11 |
| gev | 8 |
| nsbeta | 8 |
| genbeta | 7 |
| tnorm | 7 |
| trgamma | 4 |
| triang | 4 |
| frechet | 3 |
| invburr | 3 |
| invgamma | 3 |
| invparalogis | 3 |
| burr | 2 |
| foldnorm | 2 |
| invgauss | 2 |
| llogis | 2 |
| logitnorm | 2 |
| naka | 2 |
| weibull | 2 |
| zmbinom | 2 |
| beta | 1 |
| binom | 1 |
| invtrgamma | 1 |
| nbinom | 1 |
| paralogis | 1 |
| pareto3 | 1 |
| slash | 1 |
| st | 1 |
| tpois | 1 |
| ztbinom | 1 |

A fresh SGED witness uses data {-3,-1,-0.2,0,0.3,0.4,2}, held mean 0.1, nu 0.3, xi 0.3. The fit returns sd=4.801023165942154 and reported log likelihood -14.405685463919742. The 300-digit exact-coefficient likelihood is -14.406001430120535; corrected d/p/q sums to -14.406001430120533. The fitter reports the ideal cusp value while its rounded coefficients incur a finite non-Lipschitz cusp loss. Six of nine fresh cusp fits miss the seam contract, while none of the corrected density sums does. Three existing assertions fail at their looser 2e-8 tolerance. See [sged-cusp-witnesses.json](2026-11-11-dpqr-extreme-data/sged-cusp-witnesses.json) and [sged-cusps.py](2026-11-11-dpqr-extreme-data/sged-cusps.py). The R dev and full tiers each fail the same three cusp assertions at tolerance 2e-7. A separate exact-coefficient R replay also finds six fitter likelihood errors and zero density errors: [sged-cusp-R.json](2026-11-11-dpqr-extreme-data/sged-cusp-R.json). JavaScript passes 614 of 615 tests; its one failure is the same likelihood-versus-density cusp check. Exact JavaScript coefficients are checked independently in [sged-cusp-js.json](2026-11-11-dpqr-extreme-data/sged-cusp-js.json).

The simbox beta-negative-binomial symmetry failure is inherited: untouched baseline headers with GCC 13 and BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS give differences up to 1.6e-12 between symmetric fits. The existing assertion demands bitwise equality. It fails at held values 5, 1.0672234787479615 and 5.3647338351087397. Reordering the fitter’s r+alpha+beta sum is a prospective repair, not performed here. See [bnbinom-symmetry-baseline.txt](2026-11-11-dpqr-extreme-data/bnbinom-symmetry-baseline.txt) and [bnbinom-symmetry.cpp](2026-11-11-dpqr-extreme-data/bnbinom-symmetry.cpp).

## The fourteen stats routes

Routing is unchanged. On this workstation, stats fails 386 of 32,305 comparisons across the fourteen routed families; 27 functions miss the same consumer contract. Every witness is in [stats-findings.json](2026-11-11-dpqr-extreme-data/stats-findings.json.gz).

| Function | Failures |
|---|---:|
| `stats::pweibull` | 92 |
| `stats::dweibull` | 49 |
| `stats::phyper` | 37 |
| `stats::dcauchy` | 24 |
| `stats::qhyper` | 24 |
| `stats::dbinom` | 19 |
| `stats::pbinom` | 19 |
| `stats::dhyper` | 16 |
| `stats::qweibull` | 15 |
| `stats::dnbinom` | 14 |
| `stats::qcauchy` | 12 |
| `stats::qlogis` | 10 |
| `stats::qnorm` | 10 |
| `stats::dpois` | 7 |
| `stats::dgeom` | 5 |
| `stats::pnbinom` | 5 |
| `stats::pbeta` | 4 |
| `stats::qbeta` | 4 |
| `stats::pexp` | 3 |
| `stats::pgamma` | 3 |
| `stats::dlnorm` | 3 |
| `stats::qnbinom` | 3 |
| `stats::dgamma` | 2 |
| `stats::qlnorm` | 2 |
| `stats::qunif` | 2 |
| `stats::dbeta` | 1 |
| `stats::punif` | 1 |

For example, at Weibull shape 1e12, scale 1000 and x=1000.000000003319, the exact log density is -3.5897335166067403 and stats gives -3.587793966514038 (normalized error 5.40e-4). At x=1000.0000000065378, the upper natural-tail relative error is about 6.18%. `qweibull(-800,shape=1e8,scale=1,log.p=TRUE)` returns zero instead of approximately 0.999992. These R routing defects are open by explicit scope.

## Sampling and existing test intent

All 202 frozen selectors, 12,928 draws, are bitwise unchanged. The extended study has 214 selectors and 13,696 draws. Seven added stress selectors change 256 outputs: six TRgamma/inverse-TRgamma inverse-fallback selectors (192 draws) and one subnormal two-cell truncated-binomial selector (64 formerly NaN draws). They consume the same primitive RNG draws; required inverse arithmetic changes the returned values. [sampler-streams.json](2026-11-11-dpqr-extreme-data/sampler-streams.json) records both hexadecimal streams.

New C++ regression cases are appended. Three old d/p/q expectations were corrected with independent evidence: the tiny-shape Nakagami upper tail needs the Euler constant term; the beta-negative-binomial unit-shape median previously expected NaN but its exact rational survival proves zero; a Student affine quotient previously expected NaN from the overflowing raw subtraction, but half-scaled subtraction gives the representable exact quotient, with both d and p confirmed at 240 digits. No fitter failure was converted into a new expected stop. The expweibull check was tightened to the common 1e-10 tolerance. R sampling retains every historical case without extending sampling into the d/p/q stress grid.

## Final required runs

All commands below were awaited in the foreground through the required CPU wrappers. Workstation builds use four reserved slots, single-thread numerical studies use one, and simbox commands run nice with BLAS/OpenMP pinned. No command was left running when this report was written. Full logs are retained in [final-logs/](2026-11-11-dpqr-extreme-data/final-logs/).

| Command | Machine | Exit | Outcome |
|---|---|---:|---|
| `just cpp-test` | workstation | 8 | 107/108 shards pass; distribution_sged |
| `just cpp-test-no-long-double` | workstation | 8 | 134/135 shards pass; distribution_sged |
| `just js-build` | workstation | 0 | pass |
| `just js-test` | workstation | 1 | 614/615 tests pass |
| `just site-check` | workstation | 0 | pass |
| `just relations` | workstation | 0 | pass |
| `just cpp-test-no-long-double` | simbox | 8 | 133/135 shards pass; distribution_bnbinom, distribution_sged |
| `just test` | simbox | 1 | PASS 25484, FAIL 3, WARN 0, SKIP 154 |
| `just test-full` | simbox | 1 | PASS 51623, FAIL 3, WARN 0, SKIP 0 |

The C++, R and JavaScript failures are the SGED exact-coefficient cusp likelihood assertions above, plus the inherited GCC 13 BNB symmetry assertions on simbox. All 332,757 generated JavaScript truth comparisons pass; the fitter assertion is separate. The concentrating Genbeta truth case itself takes 0.134 seconds on the workstation double-only binary (6,762 assertions); the long family shard time includes fitter tests. The timing is recorded in [truth-timing.json](2026-11-11-dpqr-extreme-data/truth-timing.json). Final exact shard/assertion counts are recorded in [final-validation.json](2026-11-11-dpqr-extreme-data/final-validation.json). R dev/full, JS, site and relations outcomes must be read from the table; a red required command is not represented as green by a narrower replay.

## Open work

1. **SGED natural tails at two exact new rows.** At nu=3.8792797496374307, xi=1e8, mean=0, sd=1, x=-1.5461574810135936 and -1.5461574810135932, reference lower probabilities are 4.111206646631721e-108 and 1.5600541083276566e-34. Propagated log-tail margins are 3.1645e-10 and 9.1662e-11. The natural-tail guard returns NaN; a sharper moment enclosure is needed to close the reserved coordinate budget. These rows remain `truth_exclusions` with measured reasons; logarithmic evaluations pass.

2. **Other named precision regions.** The 307 skew witness NaNs and the explicit SN near-mode sign witness are retained, as are guarded concentrating-beta/logit coordinates and bounded conditional-count summations. See [nan-regions.json](2026-11-11-dpqr-extreme-data/nan-regions.json) for exact dossier text and [skew-smoke.json](2026-11-11-dpqr-extreme-data/skew-smoke.json.gz) for every measured skew input. No practical margin failure is advertised as a finite accurate answer.

3. **Reference coverage: 131 parameter points.** The isolated reference process exhausted its budget or could not verify its antiderivative/normalization. [oracle-gaps.json](2026-11-11-dpqr-extreme-data/oracle-gaps.json) gives every family, exact parameters, requested path and diagnostic; the count table follows. These are missing oracle rows, not passing tests or fabricated exclusions. Some finite-mean/concentrating limits therefore remain unverified in the generator even when the additional independent witness passes.

| Family | Unsettled reference points |
|---|---:|
| st | 7 |
| invtrgamma | 6 |
| sged | 6 |
| trgamma | 6 |
| sstd | 5 |
| burr | 4 |
| gamma | 4 |
| ged | 4 |
| gengamma | 4 |
| ht | 4 |
| invgamma | 4 |
| pig | 4 |
| invburr | 3 |
| lgamma | 3 |
| lst | 3 |
| naka | 3 |
| sn | 3 |
| std | 3 |
| tnorm | 3 |
| cmp | 2 |
| frechet | 2 |
| invparalogis | 2 |
| nbinom | 2 |
| paralogis | 2 |
| pareto4 | 2 |
| skellam | 2 |
| zinb | 2 |
| zmnbinom | 2 |
| ztnbinom | 2 |
| beta | 1 |
| betapr | 1 |
| binom | 1 |
| bnbinom | 1 |
| cmb | 1 |
| dgamma | 1 |
| dlaplace | 1 |
| dnorm | 1 |
| dweibull | 1 |
| fpareto | 1 |
| gb2 | 1 |
| geom | 1 |
| invpareto | 1 |
| invweibull | 1 |
| lgser | 1 |
| lomax | 1 |
| nhyper | 1 |
| pareto | 1 |
| pareto2 | 1 |
| pois | 1 |
| power | 1 |
| tlambda | 1 |
| yulesimon | 1 |
| zib | 1 |
| zip | 1 |
| zmbinom | 1 |
| zmgeom | 1 |
| zmlogarithmic | 1 |
| zmpois | 1 |
| ztbinom | 1 |
| ztgeom | 1 |
| ztpois | 1 |

4. **TASK-187 families.** Fréchet has 69 and inverse-Weibull 30 targeted d/p failures, with source files intentionally unchanged. At shape 1e12, scale 1e100, location zero and x=1.0000000000005e100, both return upper probability 0.46034516262436276 instead of 0.45474657691840953 (1.231% relative error). [external-family-findings.json](2026-11-11-dpqr-extreme-data/external-family-findings.json) contains all 120 checks. Their measured extreme tables remain under `measured-tables/`.

5. **Fitter repairs and all-green validation.** The independently attributed likelihood defects, three current SGED assertions and inherited GCC 13 BNB symmetry assertions require work outside the authorized fitter scope. Exact cases are above. Repairing d/p/q to reproduce the fitter’s inaccurate cusp value would violate the binary64-input accuracy decision.

6. **stats routing.** The 27 listed functions remain unchanged by scope. Their failed rows are actionable independently of the generated C++ bindings.

7. **Uniform domain coverage.** The new finite grid and targeted analytic/numerical witnesses substantially widen coverage but do not certify every input or every path named in the prose inventory. [coverage.json](2026-11-11-dpqr-extreme-data/coverage.json.gz) preserves every declared-limit line beside the actual attempted points, including explicit oracle gaps. Completing unmeasured paths and a whole-domain proof or exhaustive precision-region classification remains separate work; the passing rows are not evidence for untested inputs.

The consumer also omits natural outputs at or below 1e-300. Five 160-digit exact-input Weibull/exponential witnesses are retained in [subnormal-forward.json](2026-11-11-dpqr-extreme-data/subnormal-forward.json). At shape=scale=1 and upper-tail input x=737.123, the closest binary64 probability has relative error 1.40497e-4; at x=744.3 it is 0.130704, and at x=800 underflow to zero has relative error one against the positive mathematical probability. No binary64 output can meet a literal 1e-10 real-valued relative target in these three cases. The current functions retain rounded subnormal/underflow outputs; a NaN policy and function-specific consumer metadata for this regime remain open. Existing log rows remain active. This boundary is additional to the active-grid results, not covered by their zero-failure count.

## Reproduction

Run [reproduce.sh](2026-11-11-dpqr-extreme-data/reproduce.sh) from this worktree (slot wrappers required). [final-numerics.sh](2026-11-11-dpqr-extreme-data/final-numerics.sh), [final-supplements.sh](2026-11-11-dpqr-extreme-data/final-supplements.sh), [close-remote.sh](2026-11-11-dpqr-extreme-data/close-remote.sh) and [local-final.sh](2026-11-11-dpqr-extreme-data/local-final.sh) reproduce the final studies and suite matrices. Preserve the baseline commit and reference cache receipts; the baseline is rebuilt using `git archive`, not a stored header archive. [report-data.json](2026-11-11-dpqr-extreme-data/report-data.json) is the strict machine-readable summary. No source archive is stored with the evidence.
