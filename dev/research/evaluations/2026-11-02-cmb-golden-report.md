# CMB golden free-size search: TASK-114

2026-11-02. Source revision `47bf4e743a733092158a6637d5981d015c9b5029`; no fitter, binding, conformance expectation or golden file was changed. The worktree contains the [diagnostic programs](2026-11-02-cmb-golden-data/). Linux x86-64, GCC 15.2.0, R 4.6.1, installed BH headers. All calculations and suites ran locally via `/tmp/umvl-cpu/run` or `run4`, with the requested worktree temporary directories and single-thread BLAS/OpenMP. No simbox was used.

## Finding and open item

**Open: both golden free-size fits still stop with numerical failure.** The search exhausts its ten-million-support-term budget while enumerating fixed-size profiles, after calculating a very conservative tail cutoff. No certified improvement was established in this work. Returning the numerical grid maximum would leave unvisited sizes and the infinite tail unchecked. Resolving this requires a substantially sharper uniform tail bound or a certified way to exclude ranges of sizes without evaluating every full normalizer. Increasing the budget alone is not a satisfactory fix.

The reproducer [diagnose.cpp](2026-11-02-cmb-golden-data/diagnose.cpp) mirrors the search, including the CMP face charge; compile with C++17, the C++ include directory and the installed BH include directory, then run from the repository root. Its initial 33 sizes all pass their fixed-size entropy checks. The results below are numerical comparisons with the existing practical margins, not outward-rounded interval bounds.

| Probability | Initial best (size, log likelihood) | First usable truncation | Tail cutoff | First failed size | Charged terms at failure | Full fit elapsed |
| --- | --- | --- | --- | --- | --- | --- |
| Free | 16, -797.889642360016 | 8 | 326,548 | 1,154 | 9,999,122 | 0.96 s |
| Held at 0.2777362750324292 | 10, -798.130638928198 | 16 | 7,407,254 | 1,001 | 9,999,773 | 1.02 s |

The failed candidate has `IterationFailure` because another normalizer evaluation cannot be charged; it is not a rejected fixed-size entropy certificate. The search would have to consider sizes through cutoff minus one with the present bound. The free-probability CMP face calculation succeeds at log likelihood -798.170835535 and costs 847 terms; it does not stop the search. The held-probability sample has more than two observed categories, so the explicit endpoint-face comparison is inapplicable. These cutoff values describe the existing bound, not a proof that a true maximizer occurs near either cutoff or a minimal computational requirement for another algorithm.

I investigated tightening the superlevel dispersion bound by using all observed pairs `u < v` and all intermediate integers `j`, rather than just `(0,1,7)`. The same concavity and binary coarsening inequalities in the [CMB dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md) apply to every triple; taking the minimum remains an exact-arithmetic upper bound conditional on computed comparisons. [bounds.py](2026-11-02-cmb-golden-data/bounds.py) finds 3.3872 instead of 4.7518 for free probability, and 3.4391 instead of 4.9614 for held probability. This reduction is insufficient on its own: the positive correction remains proportional to that bound, so the cutoffs remain on the order of hundreds of thousands and millions, respectively. No change was made on the strength of this calculation.

## Numerical grid

[grid.py](2026-11-02-cmb-golden-data/grid.py) independently evaluates the log-sum-exp likelihood and uses SciPy moment root finding or scalar optimization at each integer size 7 through 500. All profiles met its numerical convergence check. The maxima were size 16, log likelihood -797.889642360013 with free probability, and size 10, -798.130638928196 with probability held. The runs took 1.90 s and 1.04 s respectively. These agree with the initial candidates, but are numerical finite-grid evidence only; they do not certify the unsearched tail. The existing conformance test accepts the explicit numerical stop for both subsets.

## Final checks

All commands ran locally through `run4`, sequentially, on the source revision above. No changes to fitter or test infrastructure followed these runs.

| Command | Result |
| --- | --- |
| `just cpp-test` | 106/106 shards passed |
| `just cpp-test-no-long-double` | 127/127 shards passed |
| `just test` | 20,486 passed, 0 failed, 0 warnings, 152 tier skips |
| `just test-full` | 42,103 passed, 0 failed, 0 warnings, 0 skips |
| `just js-build` | passed; pre-existing binomial compiler warning |
| `just js-test` | 413 passed, 0 failed, 0 skipped |

These regression passes do not prove a global maximum for the golden sample. The full suite's conformance expectation permits the numerical failure rather than demanding a certified free-size estimate.

## Round 2: block certificate

The open golden-sample failure above is resolved. The original round-one findings and enumeration cutoffs remain as historical measurements. The changed fitter checks ranges below the same tail cutoff using the block inequality proved in the [CMB dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md). For a block `[L,R]`, it compares the truncated entropy-dual profile at `L` with a correction based on the monotone change `c_R(k)-c_L(k)` and the existing superlevel dispersion bounds. It relaxes held odds to free odds, bisects a block that cannot be excluded, and checks an unexcluded singleton with the existing fixed-size certificate. It retains the two-outcome procedure, cutoff, budget and failure statuses. No unimodality of the profile is assumed.

The argument is exact in arithmetic conditional on computed comparisons. The entropy upper allowance is unchanged, and the block comparison adds `1e-9*(1+abs(anchor)+abs(correction))`; this is a practical floating-point margin, not an outward-rounded interval calculation. A failed entropy solve or exhausted work allowance still stops without an estimate.

On the golden sample the finished C++ search returned the following on the local workstation (one measured run, GCC 15.2.0 `-O2`):

| Subset | Certified size | Probability | Dispersion | Log likelihood | Elapsed |
| --- | ---: | ---: | ---: | ---: | ---: |
| Probability free | 16 | 0.0786449326946 | 2.04985979523 | -797.889642360016 | 0.019 s |
| Probability held | 10 | 0.277736275032 | 1.68359946659 | -798.130638928198 | 0.086 s |

The extended `diagnose.cpp` separately reproduces the block traversal: for free probability, truncation 8 excluded 883 blocks, with no singleton evaluations and 344,874 charged terms before singletons; for held probability, truncation 16 excluded 3,873 blocks and left 142 singleton sizes, with 3,152,994 charged terms before singleton evaluations. These diagnostic counts include the initial and tail-profile work, and do not include the singleton solves. The printed legacy enumeration failures in that program reproduce round one, not the outcome of the new fitter. The standalone numerical SciPy grid through size 500 from round one agrees with both results. The appended C++ golden test also compares the returned likelihood with entropy-checked profiles at every size 7 through 300. Existing full-tier R tests independently compare seeded samples and both subsets against numerical grids through size 300. The conformance exception for the golden free-size subsets was removed, so their numerical failures no longer pass conformance.

Final checks ran locally on source revision `47bf4e743a733092158a6637d5981d015c9b5029` plus the changes described here, using `run4`, worktree temporary directories and pinned one-thread BLAS/OpenMP. No simbox was used.

| Command | Result |
| --- | --- |
| `just cpp-test` | 106/106 shards passed |
| `just cpp-test-no-long-double` | 127/127 shards passed |
| `just test` | 20,491 passed, 0 failed, 0 warnings, 152 tier skips |
| `just test-full` | 42,108 passed, 0 failed, 0 warnings, 0 skips |
| `just js-build` | passed, with the existing binomial compiler warning |
| `just js-test` | 413 passed, 0 failed, 0 skipped |

The block strategy can still exhaust its budget for other samples or fail to establish a feasible entropy vector; such cases remain explicit numerical stops. Numerical grid agreement is evidence for the tested samples, not a proof of floating-point error bounds or general existence.
