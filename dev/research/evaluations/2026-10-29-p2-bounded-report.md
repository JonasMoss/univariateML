# TASK-81: p2-bounded

Implemented all eight missing coefficient holds. All five family suites pass.
The full R suite has one additional, contradictory shared assertion beyond
its three permitted simbox failures; the shared file remains untouched.

## Files changed

Under `univariateML-cpp/include/cppunivariate/distributions/`:

- `beta/`, `kumar/`, `logitnorm/`, `unif/`: `README.md`, `mle.hpp`,
  `adapter.cpp`, `test.R`, `test.cpp` in each folder.
- `triang/`: `README.md`, `mle.hpp`, `test.R`, `test.cpp`.
- This report. No golden, shared source, dpqr, board, or Git history changes.

## Holds and fitting methods

| Family | Implemented subsets | Method |
|---|---|---|
| beta | shape1; shape2; both | Strictly decreasing digamma score, bracketed bisection; both held evaluates likelihood. Stable digamma increments and local log-beta arithmetic avoid concentrated-fit cancellation. |
| kumar | a; b; both | Held a gives exact b profile; held b uses the unique score root; both held evaluates likelihood. Stable log-complements retain tiny survival terms. |
| logitnorm | mu; sigma; both | Normal likelihood on logits: mean with sigma held, RMS distance with mu held, direct evaluation with both held. |
| unif | min; max; both | Free endpoints equal sample extremes; feasibility checks on held endpoints; positive-width all-held evaluation. |
| triang | Existing seven nonempty subsets retained | Added exact constant-sample exterior-mode endpoint profiles, including reflected cases and a second held endpoint. |

New adapter arguments have exact coefficient names and default to NULL.
Held values are preserved exactly and returned in `fixed`. New arguments
are documented in `r.control`; existence and incompatibility conditions
are declared in README prose and `limits`. All five `holds` lists are empty.

Converted to `no-mle`: none. No missing hold remains in scope. Continuous
point masses remain excluded as requested (P3). Infeasible uniform holds
stop explicitly rather than returning an invalid interval.

## Existence and evidence

The P0 proofs establish existence/uniqueness for the beta one-shape slices,
existence for Kumaraswamy one-shape slices, the normal-transform solutions,
and the uniform width minimizers. For held Kumaraswamy b, strict concavity
also follows from t^2 exp(t)/expm1(t)^2 <= 1; the score has one positive root.
Both-free beta/Kumaraswamy constant data remain unbounded. A free logit-normal
scale collapses exactly when all logits equal the fitted/held location.
A uniform constant sample collapses unless a held endpoint prevents it.

For triangular constant data z with held c < z, a free a equals c and a
free b equals z + sqrt((z-a)*(z-c)); reflection gives the other case.
These formulas follow directly from the conditional log-density derivative.
The existing general triangular enumeration/pruning solver is retained.

Independent R density-based multistart nlminb/optim checks cover every new
hold subset on n=20 and n=100 samples, ties, small and constant samples,
uniform-member data and near-endpoint samples. The triangular checks search
mode candidates with independent endpoint optimization. These numerical
oracles are checks, not independent proofs of global numerical convergence.
Doctests include score residuals, exact solutions, exhaustion, feasibility,
and concentrated beta/Kumaraswamy analytic regressions.

## Final validation

Commands ran from this worktree through `/tmp/umvl-codex/simbox-run.sh`.

- `just cpp-test`: six CTest targets; five pass. Distribution target:
  423 cases, 421 pass, two permitted failures; 513,755 assertions,
  513,753 pass. Only betapr/test.cpp:33 and std/test.cpp:40 fail.
  All 29 cases across the five assigned family files pass.
- `just test`: 12,093 expectations pass, four fail, zero warnings/skips.
  Three failures are the permitted sstd frozen quantile and bnbinom/std
  goldens. The fourth is the obsolete shared Kumaraswamy assertion below.
- Assigned R suites: beta 530, kumar 566, logitnorm 319, triang 230,
  unif 106: **1,751 expectations pass**, no failures/warnings/skips.
- `just quality beta kumar logitnorm unif triang`: **5/5 pass**.
- Generic hold conformance and all five default golden regressions pass.
  Golden files and default fitting paths are unchanged.
- `git diff --check`: clean.

Logs: `/tmp/umvl-codex/p2-bounded/{cpp-test-final,test-final,quality-final}.log`.

## Open question / shared-test blocker

`bindings/r/tests/testthat/test_argument_names.R:9` requires
`mlkumar(rbeta(50, 2, 3), a = 2)` to throw “cannot hold a”. This contradicts
TASK-81's required implementation. The requested five-failure allowance
cannot be met while preserving both this assertion and the requested hold.
The user explicitly forbade shared-test edits, so none has been made.

The concrete correction is to replace that one assertion with:

```r
expect_identical(unname(mlkumar(rbeta(50, 2, 3), a = 2)["a"]), 2)
```

Approval for that narrow scope exception is pending. The full R run must
then be repeated. No other shared change is needed.
