# Certified joint Skellam fit — TASK-116

Worktree `codex/skellam-cert`, base revision `6fea876e`; tested source is
that revision plus the uncommitted changes in this worktree. All calculations
and final runs below used the local workstation, not simbox. No golden was
regenerated. The canonical argument and algorithm are in the
[Skellam dossier](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/README.md).

## Result and guarantee

The joint fitter now certifies its mean-constrained profile by
branch-and-bound, including the two zero-rate faces and the infinite tail.
Its reported likelihood is within `1e-8 * n` of the global maximum in exact
arithmetic, conditional on the computed comparisons. The bound is a
practical floating-point margin: the mass and score evaluations are not
outward rounded, so it is not an interval-arithmetic proof. Unresolved
nonfinite calculations, unrepresentable adjacent counts, exhausted work,
and midpoint stagnation stop with `IterationFailure`. The existing one-held
rate solver uses strict concavity and a bracketed score; both-held and
zero-rate special cases retain their exact treatment.

For each fixed `s`, the theta score enforces `mu1 - mu2 = mean(x)`. Let
`t = min(mu1, mu2)`, `d = abs(mean(x))`, and `q = t(t+d)`. Conditional on
an observed difference `k`, the paired Poisson count `J` has weights
`q^j / (j! (j+|k|)!)`. The sequence `(j+1)p[j+1]/p[j]` decreases, so the
size-biased count minus one is stochastically dominated by `J` and
`Var(J) <= E(J)`. Its ratio also gives `E(J) <= q/(|k|+1)`. Differentiating
the positive series along the constrained line gives

```
d² log p_k(t) / dt² <= 2 E(J) / q <= 2 / (|k|+1).
```

The sum of these bounds supplies a quadratic upper envelope for each
interval. The midpoint score uses the two neighboring mass ratios. Grid
points and local refinements nominate candidates; every remaining interval
must have an upper bound within `0.5e-8*n` of the best candidate. A face
may win a computed tie within another `0.5e-8*n`. The positive series
bounds `[0, exp(-32)]`, and the maximum Poisson mass bounds the tail.
Exhausting `1000 * iterlim` intervals fails rather than accepting an
uncertified point. The grid is an initializer, not the certificate.

This argument does **not** establish that the profile is concave or
unimodal in `s`; that question remains open here. Bessel ratio and
Turán inequalities are relevant background (for example, [Baricz and
Ponnusamy, 2010](https://arxiv.org/abs/1010.3346)), but the certificate
above does not rely on an unproved profile uniqueness assertion.

## Comparison and cost

The [comparison driver and fixtures](2026-11-02-skellam-cert-data/) use
`std::mt19937` seed 116, 600 independent samples of size 20–80, and twelve
rate pairs from `(0,4)` through `(25,25)`, including balanced, strongly
asymmetric, one-sided, two-sided, and near-face samples. It builds the
original fitter from `6fea876e` and compares status, rates and likelihood.
Both C++ and the R binding returned 600 successful fits; there were zero
reported disagreements and maximum absolute likelihood difference zero.
The C++ and R golden regressions also passed. Faces are included in the
new finite-grid checks in both languages. Agreement with the old fitter and
a finite grid is numerical evidence, not a proof of global optimality.

One local timing comparison used a single seeded `n=500` sample generated
at rates `(8,4)`, five sequential repetitions per fitter, compiled with
`c++ -O2 -std=c++17`: old 0.08667 s, certified 0.66189 s in total, ratio
**7.64×**. This exceeds the requested 2× target. It is a small benchmark,
not a universal cost estimate; optimizing the certificate is open work.
Reproduce under a CPU slot with
`/tmp/umvl-cpu/run nice -n 10 bash dev/research/evaluations/2026-11-02-skellam-cert-data/reproduce.sh`;
after `just test`, run the adjacent `compare.R` under a slot. Rate
comparisons use `1e-4*(1+old rate)` and likelihood comparisons use
`1e-8*n`.

## Final validation

The following were run sequentially on the local workstation through
`/tmp/umvl-cpu/run4`, with OpenMP and BLAS pinned to one thread per worker
and `nice -n 10`. The C++ compiler was GCC 15.2.0; R was 4.6.1 with BH.

| Command | Result |
| --- | --- |
| `just cpp-test` | PASS, 106/106 dev shards |
| `just cpp-test-no-long-double` | PASS, 127/127 shards |
| `just test` | PASS: 20,106, FAIL 0, WARN 0, SKIP 151 |
| `Rscript dev/research/evaluations/2026-11-02-skellam-cert-data/compare.R` | PASS, 600 samples |
| `just test-full` | PASS: 41,709, FAIL 0, WARN 0, SKIP 0 |
| `just js-build` | PASS |
| `just js-test` | PASS, 413/413 |
| `just site-check` | PASS, 99 family pages and browser, MathML, link, relation and snippet checks |

The first prescribed `cp -al` of site dependencies failed with cross-device
links in the sandbox. A normal local copy succeeded; no packages were
installed. R documentation generation emitted the existing unresolved-link
diagnostics, and the C++ and WebAssembly builds emitted compiler warnings;
these were not testthat warnings or test failures. The new C++ regression
also has a harmless range-loop copy warning. The 151 dev skips are the
baseline tier exclusions; the full R run skipped none.

## Remaining limitations

- No proof or disproof of profile concavity or unimodality is claimed.
- The floating-point margin is conditional on computed masses, scores and
  comparisons; it has not been established by directed rounding or a
  comprehensive worst-case numerical error analysis. Extreme inputs may
  stop with numerical failure.
- The measured `n=500` cost is 7.64×, above the requested 2× target; further
  optimization and broader performance measurements remain open.
