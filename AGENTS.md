# univariateML

## Purpose

univariateML is a proof of concept for **solid maximum likelihood**: across
about a hundred univariate families, every fit returns a verified MLE, a
plainly flagged unverified one, or says correctly and specifically why there is
none. The library is the evidence.
The research behind it covers escapes to limit models, nonexistence, multiple
maxima, when standard inference holds, and eventually numerical certificates.
See [the ML-difficulties paper plan](dev/roadmap/2026-11-02-ml-difficulties-paper.md).

## Principles

1. **No silent failures.** A fit ends in one of a fixed set of outcomes, the
   same for every family: an estimate (certified, or uncertified and flagged),
   an escape to a named limit model ("use `mlX`", proved or flagged as decided
   by a margin), a proved nonexistence, an exhausted budget, or a reported
   failure. Proved non-unique maxima are returned in full with a flag. A wrong
   answer with status `Ok` is the worst bug; an unflagged uncertified answer is
   the next. The shared outcome type is being introduced:
   [consolidation plan](dev/design/consolidation.md).
2. **Fitters follow the likelihood's structure.** Use what is known about it:
   concavity, exponential-family form, monotone scores, escape analysis.
   Heuristic starts and ad hoc profiling are not a design.
3. **Legacy carries no weight.** Algorithms, starts and outputs inherited from
   the original CRAN univariateML have no standing. Goldens detect drift only;
   regenerate them after deliberate changes and inspect the diff.
4. **Claims name their evidence.** Proof, rigorous or high-precision comparison,
   independent oracle and golden regression support different claims; say which
   one backs a claim. See the [validation guide](dev/validation/README.md).

## Layout

```
univariateML-cpp/include/cppunivariate/   C++17 core, header-only
  core/            span, reductions, optimizers, work budgets
  special/         Boost.Math seam (special.hpp) and tail-accurate helpers
  distributions/<id>/  README.md (dossier), mle.hpp, dpqr.hpp, test.cpp,
                   golden.json, truth.py, truth.json
univariateML-cpp/tests/   shared doctest targets
bindings/r/               R package inputs: scripts, distributions/<id>/ adapters and tests
univariateML-r/           generated R package (gitignored; `just regen`)
site/                     catalogue website
dev/                      STATUS.md, KNOWN_ISSUES.md, roadmap/, research/, design/, validation/, tools/
```

A family's `README.md` front matter is the single source for its R wrapper,
docs, validation harness and site page
([schema and new-family steps](univariateML-cpp/include/cppunivariate/distributions/README_SCHEMA.md)).
`lomax/` is the worked example.

## C++ rules

- C++17, header-only, depending only on the standard library and Boost.Math.
  Boost only through `special/special.hpp`, with its no-throw policy.
- No exceptions, printing or logging. Report outcomes through each fitter's
  `<Id>Status` enum. `Rcpp::stop` belongs in R adapters only.
- No `using` declarations, no raw pointers (take `cppunivariate::span`), no
  Armadillo.
- Accuracy contract ([design](dev/design/accuracy-contract.md)): an `Ok`
  fit is within relative `tol` (default `1e-10`) of a maximizer in every free
  coefficient, and its stopping rule must justify that. Target controls are
  `tol` and `iterlim` only (C++ `tol`, `maxit`); the CRAN-era `reltol`,
  `abstol`, `gradtol` and `<param>0` are being removed for 2.0.
- The core must build, test, fit, evaluate and sample without R. R RNG state
  stays in the bindings.
- Failure handling, score gates and work caps:
  [diagnostics](dev/validation/diagnostics.md#wrapper-failure-handling-rule-r6)
  and [work budgets](dev/design/work-budgets.md). Parameter holds, defaults and
  restrictions: [parameter rules](dev/roadmap/2026-10-29-parameter-rules.md).
- Every `dpqr.hpp` is tail-accurate (`special/tails.hpp`) and checked against
  its mpmath truth table.

## R package

ML-first: it exports fitters, `dml`/`pml`/`qml`/`rml`, model selection,
bootstrap and plots, and no d/p/q/r of its own. Every family's d/p/q is
generated from `dpqr.hpp` at assembly; the 14 families that used to call
`stats` are being moved over (TASK-198).
Dependencies: Rcpp and BH only.

## Out of scope

Dependencies beyond Rcpp + BH (R) and the standard library + Boost.Math (C++);
Bayesian inference, regression, MCMC; families whose parameter vector has a
user-chosen length (finite mixtures, categorical).

## Commands

| command | what |
|---|---|
| `just cpp-test` | C++ ctest targets |
| `just test-changed [base]` | changed families and transitive header dependants (default base: master) |
| `just test` | default R tier (reserve all four CPU slots first) |
| `just test-full` | exhaustive pre-merge suite with slow oracles |
| `just quality [id ...]` | fitter goldens against symbolic densities and a multistart oracle |
| `just dpqr-truth <id>` | regenerate a truth table |
| `just goldens` | create golden samples for new families (existing samples are frozen) |
| `just golden-reference [--write] <id>` | independent 50-digit reference MLE for a golden sample |
| `just regen` / `just gen` | assemble the R package / regenerate wrappers |
| `python3 dev/tools/check_links.py` | check local Markdown links |

Open items and test status: [dev/STATUS.md](dev/STATUS.md). Decisions:
[dev/roadmap/decisions.md](dev/roadmap/decisions.md). Inference work: read the
[inference plan](dev/roadmap/2026-10-30-inference-and-dqm.md) first.

## Working rules

- **Never push or publish** (`git push`, `gh pr create`, `gh pr merge`, CRAN)
  without an explicit request. This tree has diverged from the published
  package on purpose. Local commits, branches and merges are fine.
- Keep layout moves separate from numerical changes. Substantive algorithmic
  changes update the family's dossier and leave a reproducible check
  ([recording evidence](dev/validation/README.md#recording-evidence-and-decisions)).
- Record lasting findings in the repository, not only in chat or task notes;
  "later" ideas go in `dev/roadmap/` as one dated file each.
- Coordinate through the board (`~/.local/bin/board`); check In Progress tasks
  before touching the same files.
- Board statuses: To Do is ready to start; Later is scoped work deliberately
  deferred, with its trigger in a comment. Unscheduled ideas live only in
  `dev/roadmap/`; their card is created when they are scheduled.
- After merging a job branch, delete the branch (`git branch -d`), remove its
  worktree once `git status --short` shows nothing outside ignored build
  products, and remove its simbox mirror (`simbox:umvl-wt/<name>`). Unmerged
  branches stay until Jonas decides.
- Use at most four cores in total, BLAS/OpenMP pinned to one thread per
  parallel worker, and `nice -n 10` for long runs.
