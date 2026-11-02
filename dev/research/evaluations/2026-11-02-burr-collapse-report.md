# Burr collapse with held scale

Completed 2026-11-02 in `codex/burr-collapse`, based on
`422733c85731847958aae9caed0b302e6604ff29`. No commit was made.

## Result

`mlburr(rep(0.5, 3), scale = 1)` and
`mlinvburr(rep(2, 3), scale = 1)` now stop with the existing diagnostic:

> Likelihood is unbounded for this constant sample; no MLE.

The fixed-scale C++ entry points return their existing `Unbounded` status,
positive infinite log-likelihood and zero iterations before normalization or
search. Detection requires positive finite observations, exact equality of all
observations, and a common value at or below held Burr scale, or at or above
held inverse Burr scale. Equality at scale was already detected; the open-side
case is new. No tolerance or logarithmic equality test is used.

The change is confined to the two `mle.hpp` files and appended C++ and R tests.
The generic holds entry points reach the same detection when both shapes are
free. Existing common bindings already map `Unbounded` to the diagnostic above,
so neither binding implementation needed an edit. Both dossiers' `limits.detect`
records already cover the exact held subsets; their existence text and proofs
also already describe this path, so no README edit was needed. See the
[Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md),
[inverse Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/README.md)
and the earlier [existence analysis](2026-11-02-existence-7-report.md#burr-and-inverse-burr).

Default fitting algorithms, goldens, d/p/q/r, code generators and other families
were not changed.

## Mathematical scope

Write Burr shapes as `k = shape1`, `c = shape2`, and held scale as `s`.
For constant positive data `x0 < s`, set `z = (x0/s)^c` and profile
`k = 1/log1p(z)`. At the observation the density is

`f(x0) = c/(e*x0) * z/((1+z)*log1p(z)) ~ c/(e*x0)`.

As `c` tends to infinity, `z` tends to zero and the displayed ratio tends to
one. Thus the density and the likelihood for any positive sample size diverge.
For inverse Burr, reciprocal transformation gives the mirror case `x0 > s`;
the Jacobian at each fixed observation is a positive constant, so it preserves
divergence. These are analytic proofs, rather than conclusions from a numerical
search.

Holding `k` removes this off-scale path. For fixed positive `k` and `s`, Burr
has density `k*c*z/(x0*(1+z)^(k+1))`. Off scale it tends to zero at infinity:
below scale it decays as `c*exp(-c*abs(log(x0/s)))`, and above scale as
`c*exp(-k*c*log(x0/s))`, up to positive constant factors. It also tends to zero
as `c` tends to zero. Continuity and positivity therefore give an attained
finite maximum over `c`. At `x0 = s`, however, the density is
`k*c/(x0*2^(k+1))`, which diverges even with `k` held. Reciprocal transformation
again proves the corresponding inverse Burr statements.

With `c` and `s` held, the unique shape estimate is
`k = 1/log1p((x0/s)^c)` for Burr, using `(s/x0)^c` for inverse Burr.
With all coefficients held the density is finite. The relevant held-scale
subsets are therefore:

| Held coefficients | Burr constant below scale / inverse Burr constant above scale | Constant at scale |
|---|---|---|
| `scale` | Unbounded; newly detected before search | Unbounded; existing detection |
| `shape1, scale` | Finite fit; must not report collapse | Unbounded; existing detection |
| `shape2, scale` | Finite fit | Finite fit |
| `shape1, shape2, scale` | Finite density evaluation | Finite density evaluation |

No numerical optimizer, cap, convergence rule or certificate was added. The
new decision uses direct comparisons of the supplied floating-point values;
there is no floating-point margin that merges distinct observations. The
nonconstant fitting paths retain their existing guarantees and limitations.

## Regression coverage

New C++ cases, appended at the ends of the two family test files, check:

- Constant off-scale samples at scales `0.3`, `1` and `3`, through both the
  fixed-scale and generic holds entry points: both return `Unbounded`; the
  fixed-scale checks additionally assert positive infinite log-likelihood,
  preserved scale and zero iterations.
- Each additional held-shape subset in the table: finite fits instead of false
  unbounded statuses.
- A one-ULP perturbation of one observation: it must not trigger the new exact
  constant-sample decision. This assertion does not claim the near-degenerate
  numerical fit converges.
- Invalid data containing infinity: `InvalidInit`.

New R cases check the diagnostic, each additional held-shape subset, preserved
held values and a finite fit for nonconstant observations on the same side of
the held scale, at all three scales. Existing endpoint-collapse cases remain
in both suites. Existing default and historical fixed-scale golden tests,
conformance hold sweeps and full-tier independent oracle tests passed unchanged.
These regressions support preservation on the tested workloads; they are not a
proof about every nonconstant sample.

## Final validation

All runs used the local workstation, through `/tmp/umvl-cpu/run4`; simbox was
not used. Commands ran in the foreground and were awaited to completion.
The slot runner applies `nice -n 10`, four build slots, and single-thread
BLAS/OpenMP workers. The first C++ invocation additionally used `nice -n 10`.
Temporary and runtime directories were set to `$PWD/build/tmp`:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp
export XDG_RUNTIME_DIR=$PWD/build/tmp
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
/tmp/umvl-cpu/run4 nice -n 10 just cpp-test
/tmp/umvl-cpu/run4 just cpp-test-no-long-double
```

The remaining four commands ran sequentially in one foreground shell under a
single `run4` reservation, with no nested slot calls or non-computational waits:
`just test`, `just test-full`, `just js-build`, `just js-test`.

| Command | Machine | Final result |
|---|---|---|
| `just cpp-test` | Local | 106/106 shards passed; 0 failed |
| `just cpp-test-no-long-double` | Local | 127/127 shards passed; 0 failed |
| `just test` | Local | FAIL 0, WARN 0, SKIP 151, PASS 20172 |
| `just test-full` | Local | FAIL 0, WARN 0, SKIP 0, PASS 41775 |
| `just js-build` | Local | Exit 0 |
| `just js-test` | Local | 413/413 passed; 0 failed, cancelled, skipped or todo |

The dev-tier skips are exercised by the full run, which has no skips. There
were no permitted test failures. Native compilation emitted existing warnings
in unchanged code. `git diff --check` also passed.

Environment: Ubuntu 26.04.1 LTS, Linux 7.0.0-38-generic, GCC 15.2.0,
R 4.6.1 (2026-06-24), Node v22.22.1. Tests use the repository's existing
fixtures and seeds; the new cases are deterministic. Status and preserved-scale
checks are exact; the other new R held-value comparisons use testthat defaults.
Existing golden and conformance tolerances are unchanged.
Logs are scratch evidence under `build/scratch/` and are not required to retain
this report's conclusions.

The tested source is the base revision above plus the six changed fitter/test
files. SHA-256 of `git diff --binary --` restricted to those six paths, in Git's
path order, is
`8e9120623646f522941da15554322e2540cf01571cce0619e68cefda2193fe47`.
Only this report was added after the final runs.

## Open items

None within this job's scope. No changes are needed in the settled d/p/q/r,
truth fixtures or code generation to implement this diagnostic.

The artifact viewer rejected this worktree path as outside its configured
readable roots. The report remains available at its repository path; no viewer
configuration or files outside this worktree were changed.
