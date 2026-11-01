# Azzalini skew-t implementation — TASK-110 (2026-11-01)

The `st` family is implemented in the C++ library, common binding and R
package. Its [canonical dossier](../../../univariateML-cpp/include/cppunivariate/distributions/st/README.md)
contains the graded existence arguments, limiting faces, references,
restriction and numerical fitting details. `mlst` uses direct Azzalini
parameters `xi`, `omega`, `alpha` and `nu`; the existing `sstd` is a different
construction. The previous [existence study](2026-11-01-skewt-data/existence.md)
is retained as historical evidence. Its recommendation to require a global
certificate before shipping was superseded by the user's round-2 instruction:
the package permits documented numerical searches with explicit failure and
face comparisons.

## Results and decisions

- For an unrestricted all-free fit, likelihood is unbounded for every
  nonempty finite sample: place the location at an observation, choose
  sufficiently small positive `nu`, and collapse scale. For `nu >= 1`, the
  likelihood is bounded when the largest tie has at most half the observations;
  this is not a proof of an attained maximum. With free scale, a strict
  collapse inequality returns `Unbounded`; equality returns `ScaleBoundary`.
- Free `nu` is restricted to `nu >= 1`, with no held-set exemptions claimed.
  Held positive values are accepted subject to the sample's collapse
  diagnostic. A selected free bound sets `restricted = c(nu = "lower")`
  and emits the package's restriction warning. A held bound is not flagged.
- The fitter standardizes data, performs location-scale profile searches at
  seven shape and six df starting points, and refines free coordinates. It
  compares the best observed finite profiles with skew-normal, translated
  half-t, reflected half-t and half-normal faces. Converged finite candidates
  must pass the free-coordinate score check; df at or very near one also gets
  an untransformed one-sided/boundary score check. Search failures and
  unresolved large-parameter regions stop with diagnostics. The shape search
  and face comparisons are numerical, **not global certificates**.
- The half-t endpoint, scale and df match `mlht`; its reflection is fitted on
  `-x`. At a held endpoint equal to an observation, the limiting likelihood
  includes the appropriate `log(2)` penalty. The skew-normal df face points
  to `mlsn`, and the joint face to `mlhnorm` or its reflection.
- The CDF uses a nonnegative angular integral on the thin side, reflection and
  the symmetric Student-t tail on the wide side. The quantile inverts log
  tails; sampling uses the skew-normal/chi-square mixture. The family-local
  mpmath truth route independently integrates the angular representation.
  The large-df density normalizer uses the gamma-ratio expansion also used by
  `ht`, avoiding cancellation near the skew-normal face.
- Adding a family required generated JavaScript catalog/type updates, the R
  family-count and affine-location test updates, and catalog counts derived
  from current site data. The inference catalog now creates explicitly
  unreviewed rows for current dossiers absent from its dated inventory; it does
  not rewrite that historical inventory or infer inference availability.

The univariate location/skewness information block at symmetry is nonsingular
for finite df, unlike the skew-normal limiting case. The dossier proves that
block statement only. Full four-parameter DQM, information rank, Hessian
suitability and estimator asymptotics by regime and hold subset remain open;
no inference procedure is claimed.

## Seeded fitter comparison

[Driver](2026-11-01-skewt-data/oracle_driver.cpp),
[oracle](2026-11-01-skewt-data/oracle.py) and
[per-case results](2026-11-01-skewt-data/oracle-results.json) use seed
20261103. The 300 independent samples cover `alpha` in `{-5,-2,0,2,5}`,
`nu` in `{2,5,15}`, sample sizes 30 and 100, and ten replicates per cell.
The independent SciPy comparison uses 15 finite-df BFGS starts and five
`nu=1` starts per sample, with 500 iterations and gradient tolerance 1e-6;
it is a numerical lower bound on the best finite likelihood, not a global
oracle or an independent optimization of each limiting face. Loss is oracle
log likelihood minus the library's returned finite or face log likelihood.

| Result | Cases | Range of oracle minus returned log likelihood |
|---|---:|---:|
| Finite fit | 187 | -2.24e-5 to 2.84e-14 |
| Skew-normal face | 58 | -7.56e-4 to -7.42e-8 |
| Right / left half-t | 17 / 15 | -0.895 to -0.00166 combined |
| Right / left half-normal | 11 / 12 | -1.866 to -0.00427 combined |
| Numerical failure | 0 | — |

Reproduce from the worktree root after creating `build/tmp` and
`build/scratch/skewt`:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run4 bash -c 'boost_dir=$(Rscript -e "cat(system.file(\"include\", package=\"BH\"))"); g++ -std=c++17 -O2 -I univariateML-cpp/include -I "$boost_dir" dev/research/evaluations/2026-11-01-skewt-data/oracle_driver.cpp -o build/scratch/skewt/oracle_driver && python3 dev/research/evaluations/2026-11-01-skewt-data/oracle.py'
```

The largest positive loss was 2.84e-14. Negative values mean the library's
candidate exceeded the best finite candidate found by this comparison; they
are not proof of global superiority. Explicit C++ cases also reproduce both
half-t directions, the infinite-df face, tie unboundedness, the df-boundary
flag and its one-sided score. Earlier iterations and the six large-df
numerical stops before the normalizer correction are preserved in
[initial](2026-11-01-skewt-data/oracle-results-initial.json) and
[before-normalizer](2026-11-01-skewt-data/oracle-results-before-normalizer.json)
results. The finite-difference df score is sensitive on very flat profiles:
in the seed-7 affine check, df near 1273 varied by at most 0.19 under unit
changes. That check uses a 2e-4 relative df tolerance, 2e-5 for the other
coefficients, and a separate 2e-8 per-observation likelihood tolerance.

## Validation

Tested on the workstation from source revision
`d8a62683b425204adcd3569ba7d1d30cc8140fc9` plus the uncommitted changes in
this worktree, using Python 3.14.5, R 4.6.1, g++ 15.2.0 and the installed BH
headers. All computations used `/tmp/umvl-cpu/run` or `run4`; those runners
pin BLAS/OpenMP to one thread and apply `nice -n 10`. Simbox was unreachable
on the earlier attempt, so the final runs below all used the workstation.
Existing site dependencies were copied as instructed; no package installation
was performed.

| Command | Final result |
|---|---|
| `UMVL_GOLDEN_IDS=st just goldens` | Pass; exactly one new `st` golden, no existing golden changed. |
| `just quality st` | Pass. Symbolic log-likelihood difference -7.96e-13; standardized score 4.06e-8; eight-start oracle gap -4.55e-13. The family-local symbolic specification uses numerical df derivatives. |
| `just dpqr-truth st` | Pass; eight parameter points, 152 value rows and 352 quantile rows, including two sampler points. |
| `just relations` | Pass; 157 edges, zero failed. |
| `just cpp-test` | Pass; 106/106 dev shards. |
| `just cpp-test-no-long-double` | Pass; 127/127 tests. |
| `just test` | Pass; 0 failures, 0 warnings, 151 skips, 20,042 passes. |
| `just test-full` | Pass; 0 failures, 0 warnings, 0 skips, 41,645 passes. |
| `just js-build` | Pass; one pre-existing binomial self-comparison compiler warning. |
| `just js-test` | Pass; 404 tests, zero failures or skips. |
| `just site-check` | Pass; 99 family pages, 158 inference scopes and 271 graph edges. |
| `node site/scripts/graph-browser.mjs` | Pass; 99 nodes, desktop/mobile and no-JavaScript checks. |

Targeted C++ and R runs were also used during iteration. The first C++ truth
consumer run exposed central-probability and exact-median errors; those were
fixed and the truth table regenerated. An initial full-suite run exposed the
large-df normalizer, family-count, equivariance-helper, JavaScript tolerance
and inference-catalog issues described above; all affected checks passed on
the final runs. The angular-integral research check uses 300 separately seeded
SciPy quadrature comparisons; it is distinct from the fitter study.

The mpmath fixtures, finite sample oracle, internal identities and golden
regression establish different, finite-scope evidence. They do not prove a
uniform floating-point error bound for the angular quadrature or exclude
unseen finite modes and joint escapes. Very extreme valid parameters can
still cause the quadrature or optimizer to stop numerically: the CDF returns
NaN on quadrature failure and the quantile propagates it; the fitter returns a
numerical-failure status rather than accepting an unchecked estimate. Full held-set existence,
limit-path closure, inference calibration and global optimality remain open.

## Tested artifact fingerprints

SHA-256 for the final uncommitted family sources and evidence:

```text
3e18c8f6a1ed091952e6417a0d81afdc0c6acb28aa65e2d98882aff649e3c452  st/mle.hpp
b2a3570f484989d68fbab26e0368e0d176acb6fa19a376d15f575eac4c349c21  st/dpqr.hpp
79fcc5beda627fa2c2858d7cca7842644f862c645f5bfddf5df900435e3bcdfb  st/wedge.hpp
1b71e69a3070b5e4cadd571ad903d2bd34b345c064b32355368792e60c56cb3c  st/README.md
d91686f0c7ad8bc6c8e78481d569412cca2c701e4f850583ba8e15765ebf37ff  st/truth.py
0d249faa57ca25507189824e0d808876dc2643d99abb809c5e5f50cee89c3e66  st/truth.json
f1657b3af1c91560bcd56a386c086793d07fcd7d2b10ca07feef7788c04e9262  st/golden.json
47d50d1b40d7dd180c88fcea972381ed066b5c10b3bd56329626289f5281794c  st/test.cpp
dc09905c4b043a93a805c2d36da28f452bb833a9df1527c122ec04f6429f4782  st/spec.py
2243519d14ebbd2fa4600779857d087ecb81f3cb87db337812bd0354b89cd29e  oracle.py
37ee683b440347dd9501592b3a96ec404d35bec98ce152dfa1f461338a0b5f3c  oracle_driver.cpp
22ead031a64d79d6740382f045d23c625e83fc9d4467b881f5e9f782f5447ff5  oracle-results.json
```
