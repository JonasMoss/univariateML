# TASK-116.6: paralogis and invparalogis certificates

The free-common-shape joint and held-scale entrypoints now return a globally certified maximizer to the stated coefficient accuracy, or stop with a numerical, budget or nonexistence diagnostic. Reciprocal delegation preserves coefficients, Jacobians, statuses and feasible-candidate likelihood bounds. All required final development and full checks passed. Golden files are unchanged.

Tested base: `164d41784756606a48ddc0cedc67752acab4050b`, branch `codex/paralogis-cert`, with the uncommitted changes fingerprinted in [the source manifest](2026-11-06-paralogis-cert-data/source-manifest.json). No commits, git writes, board edits, dependency installs or changes to another worktree were made. Authored changes are confined to the two family dossiers/fitters/tests, their binding headers and R tests, and this report/evidence directory. The site check's incidental rewrite of an older screenshot was restored byte-for-byte from the base revision.

The [paralogis dossier](../../../univariateML-cpp/include/cppunivariate/distributions/paralogis/README.md) gives the objective, derivatives, initialization, arithmetic policy, proof and failure behavior. The [inverse dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invparalogis/README.md) records the reciprocal transport. The new [certificate header](../../../univariateML-cpp/include/cppunivariate/distributions/paralogis/certificate.hpp) reuses Burr's practical enclosure and small-matrix helpers; it does not call the unconstrained Burr fitter. Burr and invburr files were not changed.

For positive nonconstant joint samples, uniform log-density bounds exclude both shape ends and escaping log scale, proving existence of a finite maximum. Constant joint samples have unbounded likelihood. With scale held, the likelihood vanishes at both shape ends for every positive sample, including constant and one-point data. Held shape retains its monotone scale solve. There is no extra Hellinger-density closure member. The weak closure on the nonnegative real axis additionally contains point masses, including at zero; these supply no Lebesgue-density likelihood face. The dossier distinguishes tightness on the original and log axes.

BFGS or the historical safeguarded scalar solve supplies a proposal. Analytic Newton corrections and a verified contraction enclose its stationary root, with relative shape radius 1e-8 and a mapped scale interval of relative width below 1e-6. Explicit tail bounds reduce the problem to a compact domain. Likelihood bounds, free-score signs and mean-value fixed-point intersections then exclude every competing maximizer outside a larger verified neighborhood of the winning root. Initial shape slabs have endpoint ratio at most two, preventing practical padding from destroying positivity in very wide interval products. Exhaustion of 32768 boxes stops. No unimodality is assumed.

This certifies in exact arithmetic conditional on the computed comparisons, with the package's practical floating-point margin: 64 double epsilons times one plus operand magnitude per operation, plus the stated term-count/magnitude allowance for sums. It is not outward-rounded interval arithmetic. General analytic uniqueness for arbitrary samples is unproved; a successful instance certificate establishes its unique global maximum. Numerical oracle agreement is supporting validation, separate from this certificate argument. A proposed concavity shortcut was rejected because division by shape plus one does not yield the required softplus perspective; this decision is retained in the dossier.

The public NaN/free-shape holds entrypoints now route through the constrained paralogis paths rather than unconstrained Burr. Positive-data normalization overflow and unrepresentable reciprocals stop numerically. Original constant inverse samples retain `Unbounded`, including subnormal observations. Distinct observations whose reciprocals round to the same number stop numerically and clear the parent's spurious unbounded details; the permanent example is 1.9 and its next larger double. Bindings distinguish `nomle`, iteration/proof `budget`, and `numerical` stops, and check representation failures before optimizer exhaustion. A failed optimizer still needs the existing finite total-score gate before its proposal can even reach certification.

| Final command | Machine | Result | Measured elapsed |
|---|---|---|---:|
| `UMVL_TEST_TIER=full just cpp-test` | workstation | 127/127 shards, including the complete dev tier | 115.18 s |
| `just cpp-test-no-long-double` | workstation | 127/127 shards | 101.45 s |
| `just test` | workstation | FAIL 0, WARN 0, SKIP 154, PASS 20687 | 169.375 s |
| `just test-full` | workstation | FAIL 0, WARN 0, SKIP 0, PASS 43502 | 288.272 s |
| `just js-build` | workstation | pass | — |
| `just js-test` | workstation | 421/421, no failures or skips | 114.011 s |
| `just site-check` | workstation | pass: browser checks, MathML, resolving links, 99 C++ snippets | — |

C++ times are CTest elapsed times; R times are `test_local` elapsed times. These exclude CPU-lock waits and the preceding recipe generation/configuration steps. The requested C++ dev run also passed 106/106 earlier; the final exhaustive run includes every dev shard and the final rounding regression. There are no newly permitted failures. The 154 R dev skips are intentional tier selections; the full R tier has none. Existing unrelated compiler warnings were not changed.

Validation includes golden coefficients and likelihoods, finite-difference score checks, independent R multistart density oracles, every held subset, historical fixed-scale goldens, one-point/constant held-scale fits, reciprocal Jacobians and failure witnesses, insufficient proof/iteration budgets, normalization overflow, reciprocal overflow and reciprocal rounding ties. Full command results and machine distinctions are retained in [the validation summary](2026-11-06-paralogis-cert-data/validation-summary.txt).

Both frozen golden samples have n=500 (seeds 13307 and 13640). Maximum observed relative shape/scale changes were 4.02e-8 / 2.65e-7 for paralogis and 9.48e-8 / 4.92e-7 on the reciprocal inverse fixture. These are below the existing C++ and R golden tolerance, 3e-5. Neither ordinary nor historical golden fixtures were regenerated. No changes are needed in the settled d/p/q/r, special-function, truth or code-generation sources.

The [standalone checker](2026-11-06-paralogis-cert-data/certificate-check.cpp) certified all 20 ordinary fixtures: both golden samples and their held-scale fits; deterministic 30-point quantiles at shapes 0.3, 0.8, 1, 3 and 10; one-point and constant samples; a near-tie held-scale sample; and 100000 replicated quantiles. It compiles and links as plain C++17 with Boost headers and no R runtime. [Final workstation timings](2026-11-06-paralogis-cert-data/checks-workstation.tsv) are single runs under shared CPU reservations, not isolated statistical benchmarks.

| Fixture | Proposal ms | Certificate ms | Total ms | Boxes |
|---|---:|---:|---:|---:|
| paralogis joint, n=500 | 0.991 | 33.732 | 34.722 | 248 |
| paralogis scale held, n=500 | 0.147 | 2.393 | 2.540 | 18 |
| inverse golden on reciprocals, joint, n=500 | 0.913 | 25.275 | 26.187 | 189 |
| inverse golden on reciprocals, scale held, n=500 | 0.119 | 2.391 | 2.511 | 18 |
| replicated quantiles, joint, n=100000 | 102.296 | 7229.934 | 7332.230 | 290 |

Inverse timing rows measure the parent on reciprocal observations; reciprocal/Jacobian adapter overhead is excluded. The checker uses proposal maxit 1000, joint tolerance 1.220703e-4 and held-scale tolerance 1e-10. Final environment: GCC 15.2.0, R 4.6.1, Node 22.22.1, Emscripten 6.0.13. Local commands used `run`/`run4`, nice 10, and one BLAS/OpenMP thread per worker. The certificate's repeated linear data scans dominate its runtime.

Reproduce from the worktree root, with the CPU reservations shown:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test
/tmp/umvl-cpu/run4 just cpp-test-no-long-double
/tmp/umvl-cpu/run4 just test
/tmp/umvl-cpu/run4 just test-full
/tmp/umvl-cpu/run4 just js-build
/tmp/umvl-cpu/run just js-test
/tmp/umvl-cpu/run4 just site-check
```

Compile the retained checker or competing-mode reproducer through `run`, using `c++ -O2 -std=c++17`, `-IunivariateML-cpp/include`, `-IunivariateML-cpp/tests`, and the plain Boost include path (the original run obtained it with `Rscript -e 'cat(system.file("include",package="BH"))'`). The source filenames and TSV results are kept together in the evidence directory.

**Open numerical availability case.** On `x = exp(-0.001)` with scale held at 1, the historical scalar proposal is shape 2.89537978725, log likelihood -0.570099097106. The feasible shape 9000 has log likelihood 8.10021660294, so accepting the proposal would be incorrect. The [retained competing-mode check](2026-11-06-paralogis-cert-data/held-scale-competing-mode.cpp) passes because the certifier stops with `NumericalFailure` after 51 boxes; [its result](2026-11-06-paralogis-cert-data/held-scale-competing-mode.tsv) records the refusal. A finite maximum exists. Returning its higher mode rather than the permitted numerical stop remains open: it needs an alternative winning proposal and a verified narrow enclosure for that root. Increasing the original first-root Newton iteration limit does not establish globality. This limitation is also recorded in the canonical dossier.

**Open platform follow-up outside this task.** An earlier simbox R dev run showed an `mlst` df scale-equivariance mismatch: df about 1272, change -0.274 against relative tolerance 2e-4. The corresponding workstation dev and full checks passed. No st source or tolerance was changed. A pristine-base reproduction and compiler/platform comparison are still needed before assigning a cause. Earlier supplemental simbox checks passed C++ dev 106/106, full/no-long-double 127/127, and the two families' R full-tier tests (325 assertions). Its C++ compiler was GCC 13.3.0. A later optional simbox timing refresh dropped with exit 75; the prescribed local fallback passed. Earlier simbox timing data and the initial source snapshot are retained as historical evidence, while the table above and final source manifest describe the final workstation validation.
