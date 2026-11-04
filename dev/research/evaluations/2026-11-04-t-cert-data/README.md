# TASK-140 retained evidence

Run from the worktree root; no package installation is needed. All computation
uses the user's slot locks and remains on the local workstation.

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-04-t-cert-data/run.sh
```

- [derivation.md](derivation.md): exact-arithmetic fixed-df geometry,
  containment, whole-profile envelopes, unsigned tail and refitted seam.
- [exact.py](exact.py), [exact.json](exact.json): rational negative-slope
  finite-winner proof without an optimizer.
- [local-tail.md](local-tail.md), [local-tail.py](local-tail.py),
  [local-tail.json](local-tail.json): stronger nuisance-ball tail certificates
  and independent 80-digit checks.
- [signed-tail.md](signed-tail.md): conservative signed terminal exclusion.
- [study.py](study.py): seeded workloads and research value-enclosure engine.
- [inputs.json](inputs.json): frozen binary64 samples, including both goldens.
- [results.json](results.json): all profile outcomes and proposal diagnostics.
- [independent.py](independent.py), [independent.json](independent.json):
  70-digit roots, likelihood checks, seam coefficients and counterexample.
- [signed-tail.py](signed-tail.py), [signed-tail.json](signed-tail.json):
  110-digit checks at the conservative signed exclusion thresholds.
- [baseline.cpp](baseline.cpp), [baseline.py](baseline.py),
  [baseline.json](baseline.json): unchanged production C++ fits and timings.
- [environment.json](environment.json): revision, versions, seed and identities.
- [validate.py](validate.py), [validation.json](validation.json): final
  regression results, checksums and compact timing/outcome summary.

The Python engine supplies conditional **value** enclosures to absolute
log-likelihood tolerance `0.001`, with a default 768 cached-profile budget.
It is not a production coefficient fitter. Proposals in JSON must not be
interpreted as certified estimates. Its numerical allowances have not yet
been justified operation by operation under the package certificate contract.
The 33-point initial grid and scalar proposal refinement do not certify
anything; endpoint convexity bounds and the whole-tail inequality provide
the research enclosure, subject to those arithmetic allowances.

`baseline.cpp` discards one warmup and averages three subsequent calls per
family/sample. Python timings include proposal solves and branch-and-bound,
with no corresponding warmup/repetition protocol. This is a language- and
workload-dependent cost comparison, not a production port benchmark.

The scratch executable and verbose regression logs live under ignored
`build/scratch/`. Every result needed by the report is retained here.

The native baseline preserves the existing integer statuses. For lst, 0 is
`Ok`, 1 `BfgsFailure`, 3 `NoMLE`, 4 `NormalBoundary`, 6 `Unbounded`, 7
`ScaleBoundary` and 8 `IterationFailure`. For std, 0 is `Ok`, 1
`BfgsFailure`, 3 `NormalBoundary`, 4 `DfTwoBoundary`, 6 `Unbounded`, 7
`ScaleBoundary` and 8 `IterationFailure`. Their numerical classification
is recorded as existing behavior, not endorsed as a new global certificate.
