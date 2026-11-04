# Burr root certificate evidence

Source: `498f4178c0361cbdb0dd597c408a18a797a9fb2b`, branch `codex/burr-root`.
The parent [report](../2026-11-04-burr-root-report.md) distinguishes exact
proofs, practical conditional certificates, independent numerical checks,
and the production work that remains open.

- [derivation.md](derivation.md): root-box theorem, uniform nuisance
  containment, negative profile curvature, global exclusion, bracketed
  Weibull allowances, exact dual repair, and the practical arithmetic contract.
- [isolate.py](isolate.py), [isolation.json](isolation.json): all 33 previous
  stationary proposals checked by a three-score box; 27 local maximum slabs.
- [global_check.py](global_check.py), [global.json](global.json): strict
  global exclusion on 49 of 50 workloads; one explicit subdivision budget.
- [exact_root.py](exact_root.py), [exact-root.json](exact-root.json): exact
  rational golden root isolation and coefficient compatibility.
- [verify.py](verify.py), [verification.json](verification.json): independent
  80-digit root and face checks, signed-tail constants, and near-zero decisions.
- [benchmark.cpp](benchmark.cpp), [benchmark.py](benchmark.py),
  [benchmark.json](benchmark.json): ten repetitions of each unchanged C++ fit,
  alongside research Python global-search times. This is not a port benchmark.
- [run.sh](run.sh): full research reproducer, without nested CPU locks.

The 50 observations sets are read from
[the retained seam study](../2026-11-03-burr-xi-data/samples.json).
The proposal solver is imported read-only from
[the earlier certificate study](../2026-11-03-burr-cert-data/study.py).
Its empirical dual padding is discarded by the new upper bound.
Exact exp/log enclosures are imported read-only from
[the closure certificates](../2026-11-03-burr-closure-data/cert.py).
No previous evidence is overwritten. No sample is regenerated; seeds and
sampling regimes are those recorded in the earlier studies.

From the worktree root:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-04-burr-root-data/run.sh
```

Python dependencies are the existing NumPy, SciPy and mpmath installations.
The exact check uses Python Fraction arithmetic; no interval library or new
package installation is needed. The C++ benchmark uses C++17 and the existing
BH include path, with shared ccache and `-O2`. Thread limits and nice priority
are set by the slot runner. All files are small; no figures or large scratch
outputs are required.

[summary.json](summary.json), produced by [summarize.py](summarize.py), records
source/script/input hashes, environment, aggregate outcomes and timings.

[validation.json](validation.json), checked by [validation.py](validation.py),
retains the final dev/full suite summaries and compiler/R/BH versions.
