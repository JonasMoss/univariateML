# TASK-140 root and face evidence

The [derivation](derivation.md) states the exact-arithmetic arguments and
the practical conditional arithmetic used in this research implementation.
There is no C++ fitter dispatch in this directory.

Reproduction from the worktree root:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp" PYTHONDONTWRITEBYTECODE=1
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-05-t-root-data/run.sh
sh dev/research/evaluations/2026-11-05-t-root-data/suites.sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-05-t-root-data/validation.py
```

The suite launcher acquires its own four-slot allocation separately for
each command. Do not wrap it in another allocation. All runs are foreground
commands. The shared launchers set nice 10 and single-threaded BLAS/OpenMP.
Scratch, generated binaries and verbose logs stay in this worktree's build/.

* [roots.py](roots.py), [root boxes](roots.json): score contraction,
  public coefficient widths, and larger uniform nuisance/curvature boxes.
* [global.py](global.py), [profile decisions](global.json): exhaustive
  finite-interval envelopes, signed or unsigned whole-tail bounds, relative
  endpoint chords, and explicit refusal outcomes.
* [verify.py](verify.py), [independent checks](independent.json): 80-digit
  root containment, independent numerical differentiation of the Hessian,
  whole-box golden tolerances, endpoint limits, exact moments, outcome
  invariants and deliberate profile-budget refusals.
* [adversarial.py](adversarial.py), [multimodal checks](adversarial.json):
  root inclusion for the five stationary points in the two previous
  multimodal examples, and a separate global search for each family.
* [shallow.py](shallow.py), [shallow-case diagnostics](shallow.json):
  focused reproduction of the shallow normal-0 workload. Its search uses
  the current root and profile implementation.
* [baseline.py](baseline.py), [native timing](baseline.json): unchanged
  C++ fits, one discarded warmup followed by three timed repetitions.
  The native C++ source is the retained
  [previous diagnostic](../2026-11-04-t-cert-data/baseline.cpp).
* [validation.py](validation.py), [validation record](validation.json):
  completed suite summaries, environment, timing summaries and SHA256
  identities of inputs, scripts and relevant source files.

The [31 frozen inputs](../2026-11-04-t-cert-data/inputs.json) and the
[previous multimodal inputs](../2026-11-04-t-closure-data/multimodality.json)
are reused without rewriting them. Seed for the frozen random workloads:
14020261104. The two golden samples and their estimates remain frozen.
No statistical inference or general shape-unimodality claim is made.
