# Existence-2: existence statements and reader-facing prose

Date: 2026-10-31. Task: TASK-101.6.4.1. Worktree: `existence-2`, branch `codex/existence-2`.
Tested source: `bfdf257a2ec2c44fb0b986d2ade376239e9c0d55` plus the five dossier edits described here. No commits were made.

## Changes and scope

Added `existence: {uniqueness, statement, reason}` to `lnorm`, `invgauss`, `rayleigh`, `invgamma` and `nbinom`. Each statement covers free fitting, conditional fitting and all-held evaluation. All five use `uniqueness: proved`, with the explicitly stated exceptions for nonexistence or nonunique coefficients. The label does not assert uniqueness in those exceptions.

Rewrote reader-facing prose with coefficient symbols, separated R interface details into `## R notes`, and moved existing dated implementation material into `## Decision history`. The existing lognormal and inverse-Gaussian limit records now separate the parameter path from the sample condition. The negative-binomial limit condition now explicitly requires a positive sample mean and defines variance with divisor n.

No statuses, held sets, support, coefficient definitions, restrictions, fitting controls, examples, relations, catalog fields or numerical implementation changed. Existing references and their indices are unchanged; the newly verified negative-binomial citation is given in the existence reason. There were no hold reasons, fit-domain reasons or advice fields to change in these five dossiers, so no binding message or R test regex changes were necessary. Default fits and goldens are unchanged.

## Lognormal (`lnorm`)

Statement: A positive nonconstant sample has a unique joint MLE; held log mean requires a positive squared log residual total, while held positive log scale permits constant samples.

Grade: **proved**. Put y_i = log(x_i). The Jacobian does not involve the coefficients. For any positive scale the free mean uniquely maximizes at the sample log mean. With Q the squared residual total about a supplied or fitted mean, the remaining scale log-likelihood is −n log(sigma) − Q/(2 sigma²) plus a constant. Its unique maximum is sigma² = Q/n when Q > 0. When Q = 0, the likelihood grows without bound as sigma tends to zero. This also settles every one-free-coordinate case.

Status changes: none. README disagreements: the old `when` contained a data condition instead of a parameter path; that wording is corrected. The existing limit record is scoped to held `meanlog`, whereas the same collapse occurs with both coefficients free on constant data. The new existence statement and body describe both cases without changing the protected held set.

Open: no mathematical existence or uniqueness question remains for these subsets. The machine-readable limits still omit the both-free collapse; adding a separate record would require a metadata change outside this prose-only scope. Floating-point logarithms can merge distinct nearby observations, so the exact theorem is not a guarantee of representability for every floating-point sample.

## Inverse Gaussian (`invgauss`)

Statement: A positive nonconstant sample has a unique joint MLE; held mean requires at least one observation different from that mean, while held positive shape permits constant samples.

Grade: **proved**. Define Q(mu) = sum((x_i − mu)²/(mu² x_i)). Apart from a constant, the log-likelihood is (n/2) log(lambda) − lambda Q(mu)/2. Since Q'(mu) = 2(n mu − sum(x_i))/mu³, the unique free mean is the sample mean for every positive shape. For a held or fitted mean, Q > 0 gives the unique shape lambda = n/Q. If Q = 0, increasing lambda makes the likelihood unbounded. These calculations prove the conditional claims as well. The Lévy limit at infinite mean cannot beat the finite sample-mean maximizer.

Status changes: none. README disagreements: the old `when` supplied a sample condition; it now supplies the parameter path. As with the lognormal, the existing held-mean limit record does not inventory the both-free constant-sample collapse. The existence statement supplies that missing explanation without changing the held set. The prior prose about the Lévy limit is retained and its lack of a likelihood escape is explained.

Open: no mathematical existence or uniqueness question remains for these subsets. A separate machine-readable both-free collapse record remains a metadata follow-up. The closed-form mathematical result does not establish floating-point error bounds for all extreme inputs.

## Rayleigh (`rayleigh`)

Statement: Every nonempty strictly positive sample has a unique scale MLE; if any observation is zero, all positive scales tie at zero likelihood and the fitter rejects the sample.

Grade: **proved**. Set t = sigma² and S = sum(x_i²). For positive data, S > 0 and the scale log-likelihood is −n log(t) − S/(2t) plus a constant. Its derivative changes sign once, at t = S/(2n). At an observation of zero, the density is zero for every scale, so the likelihood is identically zero. This is a flat-likelihood case, not an unbounded likelihood or an unattained maximum. Holding the scale leaves only evaluation and preserves the implementation's rejection of zeros.

Status changes: none; `ZeroLikelihood` is consistent with the mathematics. README disagreements: none. The statement now explicitly distinguishes failure of uniqueness on zero-containing samples from nonexistence, and explains positive singleton and constant samples.

Open: no mathematical existence or uniqueness question remains. There is no machine-readable record for the flat zero-likelihood rejection; it is documented in the body and existence field. A metadata extension should preserve its distinction from an escape.

## Inverse gamma (`invgamma`)

Statement: A positive nonconstant sample has a unique joint MLE; holding either positive coefficient gives a unique finite estimate of the other even for constant samples.

Grade: **proved**. Reciprocals y_i = 1/x_i give a gamma likelihood with shape alpha and rate beta, with a coefficient-independent Jacobian. The profiled rate is beta = alpha/mean(y). The shape equation is log(alpha) − digamma(alpha) = log(mean(y)) − mean(log(y)). The left side strictly decreases from infinity to zero; Jensen's inequality makes the right side positive exactly for nonconstant samples. For constant samples the profile diverges as alpha grows at fixed beta/alpha. Held shape keeps the analytic rate solution. Held rate gives a unique digamma root because digamma is strictly increasing onto the real line.

Status changes: none. README disagreements: none; the existing statement that holding either coefficient permits constant data is retained and justified. The existing `fit` note and new argument agree.

Open: no mathematical existence or uniqueness question remains. The original dossier has no `limits` record for its both-free constant-sample `Degenerate` failure; adding one remains a metadata follow-up. Reciprocal overflow and solver exhaustion are numerical failures, not counterexamples to the existence theorem.

## Negative binomial (`nbinom`)

Statement: With positive sample mean, the joint MLE is finite and unique exactly when the variance with divisor n exceeds the mean; all-zero samples attain a point-mass maximum with nonunique coefficient pairs, and one-coordinate holds have the stated analytic or monotone solutions.

Grade: **proved**, by the published free-fit theorem and the conditional arguments in the dossier. Citation: Aragón, J., Eberly, D. and Eberly, S. (1992). “Existence and uniqueness of the maximum likelihood estimator for the two-parameter negative binomial distribution.” *Statistics & Probability Letters*, **15**(5), 375–379. DOI: [10.1016/0167-7152(92)90157-Z](https://doi.org/10.1016/0167-7152(92)90157-Z).

Verification used one web search, the [publisher result and abstract](https://www.sciencedirect.com/science/article/pii/016771529290157Z), and the [publisher-supplied bibliographic record](https://ideas.repec.org/a/eee/stapro/v15y1992i5p375-379.html). Authors, year, title, journal, volume, issue and pages agree. Direct full-text retrieval returned HTTP 403; this job did not independently audit the published proof.

For held r > 0, probability maximizes at r/(r + mean(x)). For held 0 < p < 1 and a positive count, the size score n log(p) + sum_i sum_{j=0}^{x_i−1} 1/(r+j) decreases strictly from infinity to a negative limit. Hence it has exactly one root. With all counts zero, the likelihood is p^(nr), and r = 0 or p = 1 attains one. Held r = 0 or p = 1 is incompatible with positive observations; on zeros the other coefficient is unidentified. The existing representative choices and silent boundary behavior are retained in R notes.

Status changes: none. README disagreement corrected in prose fields: the old Poisson `detect` condition also described all-zero samples, but the fitter correctly returns an attained zero point mass for them. The new condition requires a positive mean. Equality of positive mean and variance still gives the unattained Poisson supremum.

Open numerical certification gap: the existing fallback in `mle.hpp` returns `Underdispersed` when a nonnegative score persists at the large-size threshold `1e15 * max(1, mean(x))`, including after the initial dispersion check established overdispersion. A finite mathematical root can therefore be unresolved numerically without proving a Poisson supremum. A certified score bracket or a distinct numerical-failure outcome is needed before treating every such status as a nonexistence certificate. The fitter was not edited under this task's explicit ownership limits. The existing broad `fit.certified` field was also protected and unchanged. The theorem in the new existence field concerns mathematical maxima, not certification of every floating-point execution.

## Validation

Environment: Linux 7.0.0-38-generic; GCC 15.2.0; R 4.6.1; Python 3.14.5. Computational commands used `/tmp/umvl-cpu/run` or `run4`, which pin BLAS/OpenMP to one thread and run under `nice -n 10`; four-worker builds and suites held all four slots. No simbox, package installs, sub-agents, board edits or git writes were used.

Temporary environment for generation and R checks:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
```

All commands below finished before this report was written.

| Check | Result | Evidence |
|---|---|---|
| `/tmp/umvl-cpu/run4 just regen` | Pass, exit 0 | `build/scratch/regen.log` |
| Requested targeted `testthat::test_local` filter | FAIL 0, WARN 0, SKIP 5, PASS 7655 | `build/scratch/targeted.log` |
| `/tmp/umvl-cpu/run4 just cpp-test` | 105/105 dev shards passed; 0 failures | `build/scratch/cpp-dev.log` |
| Final `/tmp/umvl-cpu/run4 just test` | FAIL 0, WARN 0, SKIP 151, PASS 19843 | `build/scratch/r-dev.log` |
| Requested all-`ml*.Rd` `tools::checkRd` loop, excluding Non-ASCII diagnostics | No diagnostics for the five owned families; one unrelated title diagnostic for `ml_check_args.Rd` | `build/scratch/rd-check.log` |
| `/tmp/umvl-cpu/run python3 site/scripts/data.py` | Pass, exit 0; derived 98 families | `build/scratch/site-data.log` |
| Protected YAML metadata comparison against HEAD | Pass for all five families | `build/scratch/check-metadata.py` |
| `git diff --check` | Pass | Final worktree check |

The targeted command, run from `univariateML-r`, was:

```sh
/tmp/umvl-cpu/run4 env UMVL_DISTRIBUTIONS_DIR=$PWD/../univariateML-cpp/include/cppunivariate/distributions Rscript -e "testthat::test_local('.', filter = 'dist-(lnorm|invgauss|rayleigh|invgamma|nbinom)|conformance|restricted|generated-dpqr|parameter', stop_on_failure = TRUE)"
```

The help command, also from `univariateML-r`, was:

```sh
/tmp/umvl-cpu/run Rscript -e 'for (f in Sys.glob("man/ml*.Rd")) { r <- tools::checkRd(f); r <- r[!grepl("Non-ASCII", r)]; if (length(r)) print(r) }'
```

The unrelated help diagnostic is `ml_check_args.Rd:5: \\title should not end in a period`; its authored source is the untouched `bindings/r/R/code_helpers.R`. It was not suppressed or repaired outside scope. The five targeted skips and the 151 dev-suite skips are tier selections, not failures. The full tier was not run because this job changed documentation only, with no fitting, d/p/q/r or test-infrastructure changes.

The metadata comparison removes only the new existence field and allowed `limits.when`/`limits.detect` text before comparing parsed YAML against HEAD. It confirms that all other fields, including statuses and reference lists, are unchanged. Generated negative-binomial and collapse sentences were also inspected in the final Rd files.

No new probes, seeds or tolerances were introduced. Existing tests retain their declared seeds and tolerances; the family tests include seed 313, and tier-gated held-subset oracle tests use seed 81029 (those full-tier tests were skipped). Regression passes establish compatibility and tested behavior. The existence arguments and cited theorem support the mathematical claims; regression agreement alone is not their proof.
