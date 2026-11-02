# TASK-113: beta-binomial free size with alpha held

Tested source: `ddb03ef6e36a39e98e4b7018315f635953719c80` plus the uncommitted changes in this worktree. The held-alpha subset is implemented with a certified finite-fit outcome and conservative unresolved-face and budget stops. Both shapes free remain unsupported. Goldens and the settled d/p/q/r code are unchanged.

## Mathematics and procedure

The [beta-binomial dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md) states the formulas and proofs. For one observation, the ratio of probabilities at consecutive sizes is at most one exactly when `a*(N+1) >= x*(a+b-1)`. The fixed-size beta score gives the upper bound `(h*N-S)*(a+N)/S`, which is too weak to control this comparison. For nonconstant samples with positive counts, the profile maximizer instead satisfies `b/N -> a/xbar`: likelihood tends to zero when `b/N` tends to zero or infinity, and convergence on compact positive ratios gives the unique negative-binomial optimum. Thus no size-independent upper bound on the maximizer exists.

With alpha `a` held and mean `mu`, the limit along `b=N*a/mu` is negative binomial with shape `a` and mean `mu`. Its log likelihood is maximized at the sample mean. The implementation splits beta at `c*N`: a bound using the zero probability and exact probability ratios controls smaller beta; a comparison with the negative-binomial probability gives a uniform correction for larger beta. Both bounds decrease with size. When a finite candidate beats both bounds and the face, the subsequent integer tail is excluded. At each enumerated size the existing tangent/chord certificate bounds the free-beta profile; reflection is valid at that fixed size but does not transfer the held-beta size argument because the reflected data depend on size. Profile allowances and a cross-size ambiguity margin are retained.

The inequalities are proved in exact arithmetic, conditional on the computed comparisons. The implementation uses a practical floating-point margin of `2e-8*h` plus an epsilon-scaled term; this is not outward-rounded interval arithmetic. A failed profile or ambiguous size comparison stops. The size budget is 1000. To avoid a costly search without an initial tail certificate, the first profile must already exclude the tail beyond that budget; otherwise the fitter returns `SizeBudget`, even if a better intermediate profile might have made certification possible. If the first profile does not clearly exceed the negative-binomial face, it returns `FaceUnresolved`; this is not a finding that the face wins. Constant positive data have a point-mass supremum; all-zero data attain likelihood one at size zero with beta unidentified.

For both shapes free, arbitrary negative-binomial shapes and the Poisson limit enter the face supremum. The held-alpha correction grows like alpha squared and is not uniform in the large-alpha Poisson regime; a usable correction must also control small alpha, establish which other escaping paths retain likelihood, and compare the fixed-size binomial and endpoint faces while propagating profile allowances. An exhaustive closure proof and certified global search remain open; no implementation is claimed for this subset.

## Validation

The [seeded reproducer](2026-11-02-bbinom-alpha-data/check_bounds.py) checked 5,000 random samples and parameter combinations using Python double-precision log-gamma calculations, seed 113, with sizes through 250. The largest log-likelihood-minus-bound was `-0.2472866614` in the small-beta branch and `-0.0544120697` in the large-beta branch. These numerical comparisons do not prove the inequalities or floating-point error bounds.

C++ and R tests cover a certified finite fit, an unresolved negative-binomial face, the budget stop, constant and all-zero samples, and held metadata. Three seeded C++ binary-count samples compare the returned fit with the fixed-size certified profiles through size 20; the R test also compares a binary sample through size 30. These checks use the same fixed-size optimizer, not an independent optimizer, and cover only small counts. Open validation gaps are an independent high-precision sweep of the certificates, near-tie cases, and broader seeded finite-fit workloads with counts above one. The conservative early budget and face stops can also leave finite maxima unresolved.

All final commands ran locally on Linux x86-64 7.0.0-38-generic, GCC 15.2.0, R 4.6.1 and Python 3.14.5. Computation used `/tmp/umvl-cpu/run` or `run4`, with one BLAS/OpenMP thread per worker and `nice -n 10`; temporary files and logs were under the worktree's `build/`. No simbox, dependency installation, commit, push, or board edit was used; the R tests installed the assembled package into a temporary test library.

| Final command | Result |
|---|---|
| `just cpp-test` | Pass, 106/106 development shards. |
| `just cpp-test-no-long-double` | Pass, 127/127 tests. |
| `just test` | Pass: 20,275 passes, 0 failures, 0 warnings, 151 skips. |
| `just test-full` | Pass: 41,878 passes, 0 failures, 0 warnings, 0 skips. |
| `just js-build` | Pass. |
| `just js-test` | Pass, 413/413 tests. |
| `just site-check` | Pass. |
| `git diff --check` | Pass before writing this report. |
