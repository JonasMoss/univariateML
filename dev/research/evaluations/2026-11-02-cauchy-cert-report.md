# Cauchy free-fit geodesic certificate

Date: 2026-11-02. Worktree: `cauchy-cert`, branch
`codex/cauchy-cert`. Tested base revision:
`bbb37f337af143d1dcd3eb48881353fbf6aec69f`, plus the uncommitted changes
identified below. No commits, package installs, board edits, pushes or other
worktree changes were made.

## Result and mathematical scope

The canonical [Cauchy dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cauchy/README.md)
now states the free-fit certificate. Under the strict condition that every
sample multiplicity is less than half the sample size, an interior maximum
exists and there are at least three distinct observations. The negative
log likelihood has positive second derivative along every unit-speed
hyperbolic geodesic: vertical paths and semicircles orthogonal to the real
axis cover all connecting geodesics. Along a connecting geodesic from an
exact stationary point the derivative starts at zero and increases strictly.
Every other point therefore has strictly larger negative log likelihood.
This proves global maximality and uniqueness of an exact stationary point.

The equality case is distinct: two equally weighted distinct values attain
a continuum of maxima, and the existing explicit midpoint/half-distance
answer is retained. At half mass with more than two distinct values the
supremum is finite and unattained; above half mass it is unbounded.
These existence decisions and their code are unchanged.

The final numerical gate uses the total log-likelihood score in
`(mu, t=log(scale))`. With `z=(x-mu)/scale`, it checks

```text
a = scale * U_mu = sum(2*z/(1+z*z))
b = U_t          = sum((z*z-1)/(1+z*z))
hypot(a,b) < 1e-3
```

This is the hyperbolic gradient norm, with the location-coordinate score
multiplied by scale. It is dimensionless, affine invariant in exact arithmetic,
and is not an average score. The same gate applies after an internal
normalization, in those normalized coordinates. Finite location, finite positive
scale, finite log likelihood and the existing iteration-convergence condition
remain necessary. Failed/nonfinite score comparisons produce
`IterationFailure`. The existing common binding stops on that status.
The explicit balanced two-point maximum bypasses the numerical Newton route.

This follows the package standard: a certificate in exact arithmetic,
conditional on computed comparisons, with a practical floating-point margin.
It does not turn a nonzero numerical score into a proof of exact stationarity.
There is no uniform positive strong-convexity constant on the whole plane;
even vertical-geodesic curvature can vanish toward the boundary. A lower
Hessian bound `m>0` along the path to the optimum would imply a gap bounded
by `score_norm^2/(2*m)`, but no such bound is computed. The chosen practical
margin is explicitly documented instead, as permitted by the brief.

## Implementation and metadata

Changed only:
- `cauchy/mle.hpp`: final finite total-score norm gate; Newton initialization,
  safeguards, step tolerance, search path and default controls unchanged.
- `cauchy/test.cpp`: appended adversarial score and rejected-loose-tolerance tests.
- `bindings/r/distributions/cauchy/test.R`: corresponding binding checks and
  likelihood consistency checks.
- `cauchy/README.md`: proof, gate coordinates and limitations; corrected stale
  fit metadata.

The existing `convex-solve` vocabulary describes the geodesically convex
primary route, so no schema or loader changes were needed. The note now
explicitly certifies the free fit under the conditions above and the held-location
fit by its verified monotone-score bracket and bisection. The aggregate
`fit.certified` is `"partial"`, because README_SCHEMA requires `"yes"`
to cover **every supported subset** and the held-scale location scan remains
uncertified. Calling the aggregate `"yes"` would misstate that unchanged route.
The old note incorrectly described the now-repaired held-location bracket as
unchecked; it now agrees with the existing implementation.

No golden files, d/p/q/r, special functions, truth tables, code generators or
non-Cauchy family sources changed. Successful default estimates are unchanged:
the new code only adds an acceptance check.

## Reproducible validation

All runs used the local workstation, the shared `/tmp/umvl-cpu/run4` lock,
four build/test workers at most, BLAS/OpenMP pinned to one thread by the lock,
and `nice -n 10`. CPU-slot waits were allowed to finish. No simbox runs.
Environment: Ubuntu g++ 15.2.0, C++17, R 4.6.1, Node v22.22.1;
site validation reported Python 3.14.5 and SymPy 1.14.0.
The JS build used the existing Emscripten SDK and worktree-local cache.

Preparation:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
```

Each command below ran through `/tmp/umvl-cpu/run4 nice -n 10`.
The final four commands ran sequentially under one reservation, with shell
`set -e`; all completed with exit status zero.

| Command | Final result |
|---|---|
| `just cpp-test` | 106/106 dev shards passed; 0 failed |
| `just test` | FAIL 0, WARN 0, SKIP 152, PASS 20463 |
| `just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 42080 |
| `just js-build` | WASM build and native reference build succeeded |
| `just js-test` | 413/413 passed; 0 failures, cancellations or skips |
| `just site-check` | passed, including browser checks, numerical theory checks, 99 C++17 snippets and HTML spot checks |

R dev skips are the tier-controlled skips; full ran without skips.
The C++ build emitted warnings in existing non-Cauchy code and CMake;
shell startup also reported the missing `~/.cargo/env`. These were not test
failures. No permitted failures or new Cauchy warnings were introduced.
`git diff --check` passed.

New deterministic samples (no RNG seed required):
- near half-mass ties: `[0,0,0,0,.01,1,2,3,4]`;
- wide spread: `[-1e6,-100,-1,0,1,100,1e6]`;
- two clusters: `[-101,-100,-99,98,99,100,101]`.

Each is required to converge with `tol=1e-8, maxit=1000` and pass an
independently accumulated total-score norm below `1e-3`. R additionally checks
the reported likelihood against `stats::dcauchy` at tolerance `1e-10`.
The sample `[-8,-2,0,1,3,20,100]` with deliberately loose step tolerance
`1e6` must fail rather than accept its first nonstationary step; both C++
status and R error are checked. Existing tests cover the balanced two-point,
half-mass rejection, golden, held-location bracket and scale-equivariance cases.
These finite regression/identity checks support the gate implementation;
the global claim rests on the mathematical proof, not a multistart search.

Diagnostic logs are disposable under `build/scratch/`:
`cpp-test.log`, `r-dev.log`, `r-full.log`, `js-build.log`,
`js-test.log`, `site-check.log`. The commands, workloads, tolerances and
results above retain the evidence without depending on those ignored logs.

Tested source SHA-256:

```text
cauchy/mle.hpp   12eb4dab58b9020fef30731cf28580dcccf63345923810812f462764ee7c2e26
cauchy/test.cpp  34a243d55d205d4b874905a5683c9e346662d453f24a1640bc0c8ba5123c303a
cauchy/README.md 155b116baa482f93fd30b66ccaa9e93994249ad0bb3cbbc62b8cdc6060d5b22f
R cauchy/test.R 3e53a141edb9a7475d0badc6bddd18f95f6e1bdfab42b867c739430932761367
```

## Open scope

No implementation item in this brief remains open. A quantitative,
sample-specific likelihood-gap certificate would require a verified curvature
or distance bound in addition to the score gate; it is not supplied here.
The held-scale location search still lacks root isolation/global certification,
as required by the brief's unchanged-scope instruction. Neither limitation is
hidden by the metadata.
