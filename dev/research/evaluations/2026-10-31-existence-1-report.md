# Existence-1: exponential, Poisson, geometric, binomial and beta

Completed 2026-10-31 for TASK-101.6.4.1 in branch `codex/existence-1`.
Tested source: `bfdf257a2ec2c44fb0b986d2ade376239e9c0d55` plus the five
uncommitted dossier edits described here. No commits, board edits, installs,
other-worktree changes or sub-agents were used.

## Scope and outcomes

Only the five owned distribution READMEs changed, plus this report. Added
`existence`, rewrote reader-facing descriptions and limit clauses, and moved
dated diagnostic material into Decision history. Binomial R calling conventions
and work-budget guidance now live under R notes. The existing exponential
algorithm history remains accessible.

All protected front-matter fields compare equal to HEAD after excluding the
new existence records and the permitted `limits.when` and `limits.detect`
text. There are no status changes, held-set changes, numerical changes or
golden changes. None of these families has parameter `advice` or structural
hold reasons requiring a message update. No common binding strings or R test
regexes needed changes. There are no fitting-domain restrictions in this group.

All statements concern nonempty finite samples in the declared support and
mathematical likelihoods. Existence is distinct from success in finite arithmetic.

## Exponential

**Statement:** The free-rate MLE is finite and unique exactly when the sample
sum is positive; holding a finite positive rate allows all-zero samples too.

**Grade: proved.** With sample size m and sum S, the log likelihood is
`m log(lambda) - lambda S`, strictly concave in the positive rate. Its score
vanishes only at `m/S` when S is positive. When S is zero it increases without
bound. The dossier gives this argument explicitly.

**Status and consistency:** `Unbounded` remains correct for all-zero data
with a free rate. The limit and existence statement agree. Historical wording
about a supremum at infinite rate now explicitly says the likelihood is
unbounded. No missing limit, hold or fitting-domain condition was found.

**Open:** No mathematical existence or uniqueness question remains for the
supported free and held cases. This is not a universal floating-point accuracy
claim.

## Poisson

**Statement:** Every nonnegative integer sample has the unique estimate
`lambda = mean(x)`, including zero; a held zero rate requires all-zero data.

**Grade: proved.** For positive S, `S log(lambda) - m lambda` is strictly
concave with its unique maximum at S/m. For S = 0 it decreases strictly on
positive rates and attains its maximum at the included endpoint zero.

**Status and consistency:** No changes or disagreements. Zero is already in
the parameter domain, so this is an attained boundary member, not an omitted
nonexistence limit. Incompatible held values are rejected.

**Open:** None for mathematical existence or uniqueness in the declared domain.

## Geometric

**Statement:** Every nonnegative integer sample has the unique estimate
`prob = 1/(1 + mean(x))`; holding probability one requires all-zero data.

**Grade: proved.** `m log(p) + S log(1-p)` is strictly concave with its unique
interior root at m/(m+S) for S > 0. For S = 0 it increases strictly up to the
included endpoint p = 1.

**Status and consistency:** No changes or disagreements. The point mass at
zero is already included at probability one. The description explains the
failure-count convention, with its R-specific attribution under R notes.

**Open:** None for mathematical existence or uniqueness in the declared domain.

## Binomial

**Statement:** With both coefficients free and positive sample mean, a finite
maximum exists exactly when the mean exceeds the variance with divisor equal
to sample size; all-zero data attain nonidentifiable maxima, and integer or
boundary submodels need not have unique coefficient estimates.

**Grade: proved for the cited existence criterion and unimodality; proved by
an explicit argument for held cases; unknown for a universal classification
of joint integer-size ties.** The dossier therefore uses `uniqueness: unknown`
rather than claiming uniqueness for every supported subset.

The verified primary source is DeRiggi, D. F. (1983), “Unimodality of Likelihood
Functions for the Binomial Distribution”, Journal of the American Statistical
Association, 78(381), 181–183,
[doi:10.1080/01621459.1983.10477949](https://doi.org/10.1080/01621459.1983.10477949).
The publisher abstract states the existence criterion and unimodality. This
job verified that record; it did not independently reconstruct the full paper's
proof. The zero-mean case is handled separately because the included point
masses attain a maximum.

For a held interior probability, the adjacent log-likelihood difference is
`m log(1-p) - sum(log(1-x_i/(n+1)))`. It decreases strictly if any count is
positive and has a negative limit. Hence an integer maximum exists, with at
most two adjacent maximizers. Counts all equal to one and held p = 1/2 give
exactly equal likelihoods at sizes one and two. Fixed positive size has a
unique probability estimate. Fixed zero size leaves probability unidentified.
Fixed probability zero leaves size unidentified for zero data; fixed
probability one requires constant counts and identifies size. Fully held
models require positive likelihood for the data.

**Status and consistency:** No status change. The `Underdispersed` enum still
reports the Poisson escape even though its name is counterintuitive. The old
limit detector mentioned only free size. That wording was too broad: a held
interior probability has a finite integer maximum even on overdispersed data.
The detector now explicitly requires both coefficients free. The limit path
now explicitly includes probability tending to zero at a fixed positive mean.
The old opening sentence implied size need not be supplied; R notes now state
that the argument must be supplied, with `NULL` requesting estimation.

The old claim that raising `iterlim` always resolves every finite MLE was
stronger than finite arithmetic supports. The replacement retains the lack
of a fixed free-size magnitude cap, while distinguishing work-budget exhaustion
from integer-resolution limitations. The held-probability fitter has an
explicit integer-resolution guard. No fitter code was changed.

The retained estimation-alternative citations were also checked against
publisher records: Carroll, R. J. and Lombard, F. (1985), “A Note on N Estimators
for the Binomial Distribution”, Journal of the American Statistical Association,
80(390), 423–426,
[doi:10.1080/01621459.1985.10478134](https://doi.org/10.1080/01621459.1985.10478134);
and DasGupta, A. and Rubin, H. (2005), “Estimation of binomial parameters when
both n, p are unknown”, Journal of Statistical Planning and Inference,
130(1–2), 391–404,
[ScienceDirect record](https://www.sciencedirect.com/science/article/pii/S0378375804002733).
No reference-list fields were changed. Three search queries were used in total.

**Open:** A complete exact characterization of ties in the all-free integer
profile was not established here. Unimodality alone is not a proof of unique
integer maximization. The cited finite-existence theorem and held-case
arguments suffice for the statements made. Arbitrarily large-size numerical
certification beyond floating-point integer resolution remains outside this
prose-only job; the replacement text does not promise it.

## Beta

**Statement:** With both shapes free, a finite unique MLE exists exactly for
nonconstant samples inside (0,1); with either finite positive shape held,
the remaining shape always has a finite unique MLE.

**Grade: proved.** The minimal regular exponential-family statistic is
`(log(x), log(1-x))`. Its convex support has interior `exp(u)+exp(v)<1`.
Strict Jensen inequalities put the empirical statistic there exactly for
nonconstant samples, establishing existence; strict concavity gives
uniqueness. For constant c, shapes `(t*c, t*(1-c))` make the density at c
grow as the square root of t by Stirling's formula, proving unboundedness.
With one positive shape h held, the free-shape score is
`a + digamma(z+h) - digamma(z)`, for a negative mean log observation a.
Its derivative is negative because trigamma decreases. Its endpoint limits
are positive infinity and a, giving exactly one root. Fully held shapes
require only the declared open support and valid shapes.

**Status and consistency:** `Degenerate` remains correct on constant data
when both shapes are free. The limit path now states the fixed ratio needed
to collapse onto the observed c. Merely sending both shapes to infinity
without that ratio would not establish the stated likelihood divergence.
No disagreement with held cases or fitting domains remains.

**Open:** None for mathematical existence or uniqueness in these subsets.
The numerical Newton and bracket solvers remain subject to their failure
statuses and finite precision; strict concavity is not a claim that every
numerical invocation succeeds.

## Validation

Environment: Linux workstation, R 4.6.1 (2026-06-24), Python 3.14.5,
GNU C++ 15.2.0. Temporary files and logs are under `build/scratch/` and
`build/tmp/` in this worktree. Build/test commands used the shared CPU slot
locks, `nice -n 10`, and one-thread BLAS/OpenMP settings. No new random
workloads, seeds, tolerance changes or tests were introduced. Existing tests
retain their own seeds and tolerances; these checks validate regression and
integration, rather than proving the mathematical results above.

Commands and final results:

- `/tmp/umvl-cpu/run4 nice -n 10 just regen`: exit 0. Regenerated R wrappers,
  metadata and help. Log: `build/scratch/regen.log`.
- From `univariateML-r`, `/tmp/umvl-cpu/run4 nice -n 10 env
  UMVL_DISTRIBUTIONS_DIR=$PWD/../univariateML-cpp/include/cppunivariate/distributions
  Rscript -e "testthat::test_local('.', filter =
  'dist-(exp|pois|geom|binom|beta)|conformance|restricted|generated-dpqr|parameter',
  stop_on_failure = TRUE)"`: exit 0, FAIL 0, WARN 0, SKIP 8, PASS 7642.
  The unanchored requested filter also includes beta-prime tests. The eight
  skips require the full tier. Log: `build/scratch/targeted.log`.
- From `univariateML-r`, the requested `tools::checkRd` loop over `man/ml*.Rd`,
  filtering Non-ASCII diagnostics: exit 0, no diagnostics for these five
  families. One unrelated diagnostic remains in `ml_check_args.Rd`: its
  title ends in a period. This file is outside the assigned scope.
  Script and log: `build/scratch/check-help.R`, `build/scratch/check-help.log`.
- `/tmp/umvl-cpu/run nice -n 10 python3 site/scripts/data.py`: exit 0,
  derived 98 families. Log: `build/scratch/site-data.log`.
- `/tmp/umvl-cpu/run4 nice -n 10 just cpp-test`: exit 0, 105/105 dev shards
  passed. Log: `build/scratch/cpp-dev.log`.
- Protected YAML comparison against HEAD: exit 0 for all five families.
  Reproducer and log: `build/scratch/check-fields.py`,
  `build/scratch/check-fields.log`.
- Final `/tmp/umvl-cpu/run4 nice -n 10 just test`: exit 0, FAIL 0, WARN 0,
  SKIP 151, PASS 19843; four workers, 71.112 seconds elapsed for the R tests
  after regeneration. Log: `build/scratch/r-dev.log`.
- `git diff --check`: passed.

The full tier was not required or run: changes are confined to dossier
metadata text and prose, with no fitting, d/p/q/r or test-infrastructure
change. Full-tier oracle skips are recorded above, not counted as passes.
