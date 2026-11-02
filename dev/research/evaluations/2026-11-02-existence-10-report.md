# Existence-10: existence statements and reader-facing prose

Completed TASK-101.6.4.1 for `gb2`, `genbeta`, `trgamma`, `invtrgamma`, `pig` and `nsbeta` in worktree `existence-10`, branch `codex/existence-10`.

The tested base revision is `2dad9da3f2fe9e538e8acfa231cca83d77412ca5`. Changes are uncommitted, as requested. Six dossiers and two binding headers changed. No fitter, density, generator, golden, truth table, authored R test, other family, roadmap, status file or board was changed.

All six `existence.uniqueness` fields are `unknown`: a general joint uniqueness theorem is not asserted. This does not downgrade the conditional uniqueness proofs described below. Existence, parameter identification and numerical certification are stated separately.

## gb2

**Statement:** With all coefficients free, constant positive data have unbounded likelihood and nonconstant data may escape to a limiting model; with nonconstant data, holding both beta shapes, or holding power and scale, gives a unique finite MLE, while holding power, scale and either beta shape also permits a unique fit for constant data.

**Grade:** Joint existence criterion and uniqueness **unknown**; the stated conditional results and declared limiting paths are **proved**. The outer search remains **numerical**.

**Argument:** At fixed beta shapes, use power and the intercept on log observations as coordinates. The objective displayed in the dossier is strictly concave. After profiling the intercept it tends to negative infinity as power tends to zero or infinity for nonconstant data, giving attainment and uniqueness. With power and scale held, the logistic transform of log observations gives ordinary beta fitting. Holding one beta shape leaves a strictly decreasing digamma score with one root, including for constant transformed data. These arguments do not establish uniqueness of the outer beta-shape profile or completeness of its boundary comparisons.

**Status changes:** None. Limit statuses, firmness grades and numerical flags are unchanged.

**Disagreements:** None found between the revised existence statement, limits, holds and fitting-domain metadata. The proved limit paths are not presented as a global certificate for the numerical face comparisons.

**Open:** A complete existence criterion and uniqueness result for the free beta-shape profile; exhaustion of joint escape paths; a global certificate for the multibasin search. The [implementation report](2026-10-29-gb2-implementation-report.md) remains the source for the existing certification gap. The dated seed-subsampling and beta-coordinate precision notes were retained under Decision history.

## genbeta

**Statement:** With all coefficients free the likelihood is unbounded for every positive sample; with the endpoint held above the data, constant samples may still concentrate and nonconstant samples may escape to gamma or power limits, while holding endpoint and power reduces fitting to the unique beta MLE for nonconstant data, with a single free beta shape also admitting constant data.

**Grade:** Unrestricted non-existence, the stated beta-submodel results and the product-identification ridge are **proved**; general three-shape uniqueness is **unknown**, and its profile comparisons are **numerical**.

**Argument:** With the upper-end shape below one, moving the upper endpoint to a sample maximum tied (k) times gives a likelihood factor proportional to the endpoint gap raised to (k(q-1)), which diverges. At fixed endpoint and power, the transformed data have a beta likelihood. When (q=1), the density depends on (p,a) only through their product, so the two coefficients are not separately identified. The dossier retains the gamma and shifted-exponential limits on the negative-log scale, their Jacobian, the local gamma-face correction, mixed atomic limits and the qualifications on closure completeness.

**Status corrections:** The following exact held sets changed from `no-mle` to `not-implemented`:

| Held set | Counterexample to the old universal non-existence claim |
|---|---|
| `[shape2]` | Hold shape2 at one. The product shape1 times shape3 and the endpoint form a power model with a finite MLE for nonconstant positive data. |
| `[shape1, shape2]` | Hold both at one. Free shape3 and scale give the same power fit. |
| `[shape2, shape3]` | Hold both at one. Free shape1 and scale give the same power fit. |
| `[shape1, shape2, shape3]` | Hold all at one. The model is uniform on the interval from zero to scale, with scale estimated by the sample maximum. |

For the power examples, put (M=\max_i x_i). The endpoint is (M), and the fitted power is (1/(\log M-\overline{\log x})>0) for nonconstant samples. The declared upper-endpoint closure permits these finite fits. The ridge in the first row gives multiple parameter representations, but does not destroy attainment. The existing fitter continues to reject endpoint estimation; no new fit is enabled.

**Disagreements:** The former blanket `no-mle` entries were wrong for these four held sets. The revised advice and common binding message distinguish the unrestricted singularity from unimplemented held-shape fits. All limit statuses and firmness grades remain unchanged. There are no `fit_domain` bounds.

**Open:** A complete existence criterion and uniqueness classification for the three-shape fit at held endpoint; a certificate against missed narrow profile peaks and all joint boundary paths; classification and implementation of the remaining free-endpoint fits. The [family analysis](2026-10-29-p0-genbeta.md) remains linked, with the current dossier making the scope of the endpoint-singularity argument explicit.

## trgamma

**Statement:** With all coefficients free, constant data have unbounded likelihood and nonconstant data may have only a lognormal or power supremum; holding either shape with scale free gives a unique finite fit for nonconstant data, while the remaining single-coordinate fits have the stated constant-data exceptions.

**Grade:** The held-coordinate uniqueness results and limiting paths are **proved**; joint uniqueness is **unknown**, and the unresolved parts of the free and held-scale profiles remain **numerical**.

**Argument:** Holding the power gives a gamma fit. Holding the gamma shape and profiling scale gives a strictly concave objective in log-power for nonconstant data. With scale and power held, the gamma-shape score strictly decreases through zero. With scale and gamma shape held, write (z_i=\log(x_i/a)). The power likelihood has second derivative (-n/p^2-\sum_i z_i^2e^{p z_i}<0), and its score changes sign unless every observation equals the scale. In that exception, increasing the power makes likelihood unbounded. With both shapes held, scale is uniquely fitted even for constant data.

**Status changes:** None. No disagreement was found between the revised fields. The fixed-endpoint power comparison remains distinct from the free-scale power comparison, and density convergence is not substituted for a likelihood comparison at an endpoint.

**Open:** Joint uniqueness and complete control of the small-power end of the free profile; global certification of the held-scale two-shape search. The [profile derivation](2026-10-29-trgamma-derivation.md) documents the existing analytic bounds and the remaining numerical low-end treatment. The lognormal slope sign and asymptotic one-half boundary probability were retained. R notes retain the scale-representation failure and reported log scale; the C++ helper note moved to Decision history.

## invtrgamma

**Statement:** With all coefficients free, constant data have unbounded likelihood and nonconstant data may have only a lognormal or Pareto supremum; the held-coordinate existence and uniqueness results are the reciprocal counterparts of transformed gamma, including the exception when free power concentrates at the held scale.

**Grade:** Reciprocal transfer of the held-coordinate results and limiting paths is **proved**; joint uniqueness is **unknown**, with the same **numerical** profile qualifications as transformed gamma.

**Argument:** Transform observations to their reciprocals and invert scale. The original log likelihood differs from the transformed-gamma log likelihood by (-2\sum_i\log x_i), independent of the coefficients. This preserves maxima, attainment and uniqueness. At held scale the preceding power-score argument uses (z_i=\log(a/x_i)). The power limit becomes a Pareto limit, and the lognormal inward slope has the opposite reciprocal sign, as stated in the dossier.

**Status changes:** None. No disagreement was found between the revised fields. Limit firmness and numerical flags are unchanged.

**Open:** The corresponding joint-uniqueness and profile-certification gaps from transformed gamma. The numerical scale overflow near the lognormal limit is retained in R notes, rather than being treated as mathematical non-existence. The helper and derivation references were retained under Decision history.

## pig

**Statement:** With both coefficients free and positive sample mean, a finite MLE exists exactly when the variance with divisor sample size exceeds the mean; otherwise the supremum is Poisson, while all-zero data attain shape zero with unidentified mean, and the held cases have the stated Poisson, zero-limit or finite-profile conditions.

**Grade:** The existence criterion, attained zero member and held-positive-shape attainment argument are **proved** by the linked local derivation; finite-shape uniqueness is **unknown** and the implemented scalar search is **numerical**.

**Argument:** The [PIG likelihood derivation](2026-10-29-pig/analysis.md) profiles (t=\lambda/\mu) at fixed (z=\sqrt{\lambda(2+\lambda/\mu^2)}), giving the sample mean at a joint finite maximum. Its Bessel-ratio inequality and Jensen argument show that the reduced shape score is strictly positive under non-overdispersion. Under strict overdispersion the inward derivative from Poisson is positive, whereas likelihood vanishes at shape zero for samples containing a positive count, proving finite attainment. At fixed positive shape, the infinite-mean face has a strictly positive inward likelihood derivative, and the mean-zero endpoint loses all likelihood for a positive sample. At held mean, finite attainment is equivalent to some finite shape fitting at least as well as Poisson at that same held mean; the joint variance criterion is not applied to a different held mean.

**Status changes:** None. No disagreement was found among the active README fields. The shape-zero point mass is an attained member, not an escape. The positive-held-shape all-zero case remains an unattained mean-zero limit. The numerical Poisson tie tolerance can reject a very small finite improvement despite mathematical attainment, and this distinction is explicit.

**Open:** Uniqueness of the finite shape maximum and global certification of the scalar mesh/refinement search, including held-mean profiles. The older derivation's external-density discussion and positive-coefficient zero-limit discussion are historical; the current dossier uses the declared domain including shape zero. No changes to settled d/p/q/r or their reference tables were needed.

## nsbeta

**Statement:** With all four coefficients free, likelihood is unbounded; with both endpoints held and data strictly inside them, the two-shape MLE exists uniquely exactly for nonconstant data, and holding either shape also gives a unique fit for constant data, while free-endpoint fits with held shapes remain unimplemented rather than universally impossible.

**Grade:** The unrestricted singularity and supplied-endpoint beta results are **proved**; a general free-endpoint uniqueness classification is **unknown**.

**Argument:** With supplied endpoints, the affine transform gives a beta likelihood. Its log-normalizer Hessian is the positive definite covariance of the two log sufficient statistics. Nonconstant data give an interior mean sufficient statistic and a unique maximum. A single free shape has a strictly decreasing digamma score crossing zero. Constant data with both shapes free admit concentration at their common transformed value, with density growing at square-root order in the concentration. For a free endpoint whose associated shape is below one, the endpoint-gap argument makes likelihood unbounded.

**Status corrections:** These exact held sets changed from `no-mle` to `not-implemented`:

| Held set | Counterexample to the old universal non-existence claim |
|---|---|
| `[shape1, shape2]` | Hold both shapes at one. This is a uniform model; nonconstant data give a finite maximum with free endpoints at the sample extremes. |
| `[shape2, min]` | Hold shape2 at one and min below the data. The shifted observations have a power model with free shape and upper endpoint, whose finite MLE exists for nonconstant data. |
| `[shape1, shape2, min]` | Hold both shapes at one and min below the data. The uniform upper-endpoint MLE is the sample maximum. |
| `[shape1, max]` | Hold shape1 at one and max above the data. Reflection about max gives the corresponding power fit, with finite lower-endpoint MLE at the sample minimum. |
| `[shape1, shape2, max]` | Hold both shapes at one and max above the data. The uniform lower-endpoint MLE is the sample minimum. |

These examples use the declared closed endpoint convention and positive interval width. For each power example, the transformed positive data are nonconstant, so the reciprocal log-gap shape estimate given above is finite. The other seven unsupported held sets still leave a free endpoint with its associated shape free, permitting an endpoint singularity. No endpoint estimator was implemented or enabled.

**Disagreements:** The five former universal `no-mle` classifications contradicted these attained submodels. The new reason and advice distinguish that implementation gap from unbounded unrestricted likelihood. The supplied-endpoint `fit.certified` description remains conditional on that supported beta problem; it does not certify unsupported endpoint fits. The constant-data limit now explicitly requires both shapes free. There are no `fit_domain` bounds.

**Open:** Complete existence and uniqueness classification, and implementation, for free endpoints with shapes held. Fitting still requires observations strictly inside supplied endpoints, although density evaluation has the declared endpoint closure; this existing fitting restriction is retained and documented. The diagnostic implementation note moved to Decision history.

## Shared changes and remaining documentation discrepancies

The nine status changes above are the only changed machine-readable statuses. Held sets, supports, parameter domains and kinds, fitting bounds, controls, examples, relations, catalog fields, inference metadata, fit metadata, limit statuses and firmness/numerical flags are unchanged. No family in this job has a `fit_domain` restriction to revise.

The common binding headers for `genbeta` and `nsbeta` now repeat the identical new hold reason. Their control flow is byte-for-byte unchanged apart from replacing that string. Existing authored R regexes continue to match the new messages, so none was changed. The targeted and full conformance checks passed.

The generalized-beta/Pareto table in [ml-behaviour.tex](../ml-behaviour.tex), in its `genbeta` and `nsbeta` rows, still says that every free-endpoint held subset is `no-mle`. Those historical blanket statements conflict with the counterexamples above. **Open documentation follow-up:** revise those two rows to distinguish unrestricted singularity from the nine unimplemented held sets. That file was explicitly outside this job's edit scope and was not changed. The P0/A4 implementation policy alone is not a proof that every held-shape endpoint problem lacks an MLE.

No new external citation was added or used as a theorem justification. No web searches were needed. The arguments above and the linked local derivations are the mathematical evidence. Existing bibliographic entries were retained.

The existing globally uncertified numerical searches were not replaced or newly certified. Their concrete gaps are recorded above and in the dossiers; neither passing regression tests nor a small score is presented as a proof of global optimality. No new numerical search or changed numerical fit was introduced.

## Validation

All runs were local on the workstation, through the prescribed CPU-slot wrappers. Simbox was not used. Long jobs ran under the wrappers' `nice -n 10`; BLAS/OpenMP workers were pinned to one thread, builds and R suites used at most four slots, and temporary/runtime files were under this worktree's `build/tmp`. Environment: Linux, R 4.6.1 (2026-06-24), Python 3.14.5, Ubuntu GCC 15.2.0. Existing seeds, workloads, golden data and test tolerances were unchanged. No new simulation was run.

| Check | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just regen` | Passed; generated package and help successfully. |
| Requested targeted R filter | **FAIL 0, WARN 0, SKIP 10, PASS 8046**. Skips were full-tier oracle cases, subsequently covered by the full run. |
| Requested all-`ml*.Rd` scan, excluding Non-ASCII diagnostics | No diagnostics for the six owned families. The scan did print existing diagnostics for `mlskellam.Rd` and `mlst.Rd`, outside this job's scope. |
| `python3 site/scripts/data.py` | Passed, deriving **99 families**. Repeated successfully after the final prose clarification. |
| `just relations` | **157 edges, 0 failed**. |
| `/tmp/umvl-cpu/run4 just cpp-test` | **106/106 dev shards passed**, 50.28 seconds test wall time. |
| `/tmp/umvl-cpu/run4 just test-full` | **FAIL 0, WARN 0, SKIP 0, PASS 41724**, 247.892 seconds test elapsed time. |
| Final `/tmp/umvl-cpu/run4 just test` | **FAIL 0, WARN 0, SKIP 151, PASS 20121**, 123.807 seconds test elapsed time. The 151 dev-tier skips are not counted as passes. |
| Final strict six-family `tools::checkRd` recheck | Passed with **no diagnostics** after the last regeneration. |
| Final protected-field/message audit | Passed: exactly the nine listed status changes; all protected metadata and authored R tests unchanged; binding edits consist only of the identical reason-string replacements. |
| `git diff --check` | Passed before reporting; repeated for the finished report. |

The package-wide Rd scan is **not** claimed to be clean. Its out-of-scope `skellam` and `st` diagnostics remain open for their owners. Regeneration also emitted the existing link-generation/user-shell diagnostics; these are distinct from the zero-warning R test results. No unrelated files were edited to suppress them.

Reproduction from this worktree:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 TESTTHAT_CPUS=4
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run4 just regen
(
  cd univariateML-r
  /tmp/umvl-cpu/run4 env UMVL_DISTRIBUTIONS_DIR="$PWD/../univariateML-cpp/include/cppunivariate/distributions" \
    Rscript -e "testthat::test_local('.', filter = 'dist-(gb2|genbeta|trgamma|invtrgamma|pig|nsbeta)|conformance|restricted|generated-dpqr|parameter', stop_on_failure = TRUE)"
  /tmp/umvl-cpu/run Rscript -e 'for (f in Sys.glob("man/ml*.Rd")) { r <- tools::checkRd(f); r <- r[!grepl("Non-ASCII", r)]; if (length(r)) print(r) }'
)
/tmp/umvl-cpu/run python3 site/scripts/data.py
/tmp/umvl-cpu/run just relations
/tmp/umvl-cpu/run4 just cpp-test
/tmp/umvl-cpu/run4 just test-full
/tmp/umvl-cpu/run4 just test
```

The final source fingerprint is SHA-256 `9dd59440f32140b1efabbba5a95ddc0603bdae82b696987368a7f096db7de0b1`. It hashes the eight changed source files in lexicographic path order, appending each relative path, a NUL, its bytes, and a NUL. The report itself and ignored generated/build output are excluded. This identifies the tested uncommitted source state without creating a commit.

The artifact-pane `show` command was attempted after writing the report. The viewer returned HTTP 403 because this worktree is outside its configured readable roots. The report remains at the requested worktree path; no viewer configuration or file outside the worktree was changed.
