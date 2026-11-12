# Zipf–Mandelbrot (finite support)

**Status:** banked, 2026-11-12, from the
[sads survey](../research/evaluations/2026-11-12-sads-survey.md). Not
scheduled; no implementation authorized by this entry.

## Model

$$
p(k)\propto(k+v)^{-s},\qquad k=1,\dots,N,
$$

with $N$ known (design, as in `zipf`). sads (`dmand`) takes $s>0$, $v\ge0$;
the weights stay finite for $v>-1$, which is the natural range to study.

| slice or limit | law | family here |
|---|---|---|
| $v=0$ | finite Zipf | `zipf` |
| $s=0$ | uniform on $\{1,\dots,N\}$ | `zipf` at $s=0$ |
| $v\to\infty$, $s/v\to c$ | $\propto e^{-ck}$: geometric truncated to $\{1,\dots,N\}$ (sads `gs`) | geometric + truncation modifier ([TASK-191](2026-11-11-zero-and-truncation-modifiers.md)) |
| $v\downarrow-1$, $s>0$ | point mass at 1 | — |

The limit follows from $(k+v)^{-s}\propto(1+k/v)^{-s}\to e^{-ck}$.

## Why it is interesting

For fixed $v$ it is a one-parameter exponential family in $s$ (statistic
$\sum\log(x_i+v)$), so the inner problem is concave and the fit reduces to a
one-dimensional profile in $v$. It is not an exponential family in $v$:
whether the profile can have several local maxima, and when the supremum is
the truncated-geometric limit rather than an interior point, are open. That
is an escape to a limit model that is not a boundary face, a case the
[ML-difficulties paper](2026-11-02-ml-difficulties-paper.md) wants.

## Against

A rank-abundance model with users mainly in linguistics and ecology. The
infinite-support variant ($N=\infty$, $s>1$, Hurwitz-zeta normaliser, not in
Boost.Math) is the more general count model and should be considered
alongside, not instead.

## Open questions

1. Profile shape in $v$: unimodal? Find a counterexample or a proof.
2. Exact condition for the geometric escape, analogous to the zeta face test
   in [power-bend](2026-11-12-powbend.md).
