# Discrete quantiles lose ties: `q(p(k))` is often `k + 1` (TASK-215)

Finding of 2026-11-13 on master `1e0f5185`. Measured; nothing was changed.

## What was run

[probe.cpp](2026-11-13-discrete-quantile-ties-data/probe.cpp) calls the C++
`p` and `q` of pois, binom, geom, nbinom and hyper on a grid of coefficients
and every support point `k` with `0 < F(k) < 0.999` whose computed `F(k)`
differs from `F(k - 1)`, and counts how often `q(F(k))` is not `k`.
[stats.R](2026-11-13-discrete-quantile-ties-data/stats.R) does the same with
R's `stats` functions (R 4.6.1). The grid is small and hand-picked; the shares
are for this grid, not a rate over all coefficients.

| family | cases | C++ returns `k + 1` | `stats` returns `k + 1` |
|---|---:|---:|---:|
| pois | 318 | 41 (12.9%) | 0 |
| binom | 590 | 61 (10.3%) | 0 |
| geom | 424 | 113 (26.7%) | 0 |
| nbinom | 1,612 | 355 (22.0%) | 0 |
| hyper | 291 | 0 | not run |

No case returned a value below `k`. Outputs:
[probe.out](2026-11-13-discrete-quantile-ties-data/probe.out),
[stats.out](2026-11-13-discrete-quantile-ties-data/stats.out).

Separately, `qhyper(0.5, m = 5, N = 10, k = 5)` returns 3. The masses are
`[1, 25, 100, 100, 25, 1]/252`, so `F(2) = 1/2` exactly and the quantile is 2.

## Cause

`special::discrete_quantile` compares log tails: it searches for the first `x`
whose smaller log tail reaches that of `p`. `p(k)` returns `exp` of the log
tail, rounded, and the logarithm of the rounded value can exceed the log tail
by an ulp. The comparison then fails at `k` and the search returns `k + 1`.
Checked on the 41 Poisson failures
([cause.cpp](2026-11-13-discrete-quantile-ties-data/cause.cpp)): in 39,
`log(p(k))` exceeds the lower log tail at `k`; the other two were not traced
and presumably compare the upper tail. hyper compares
the computed probability with `p` itself, so its round trip holds, but its
computed `F(2)` falls a rounding error below `1/2`.

`stats` avoids both with a left-continuity fuzz: `p` is reduced by a relative
`64 * DBL_EPSILON` before the search. The price is that a `p` within that band
above a jump gets the lower value (21 of 21 cases in `stats.out`).

## Who sees it

R's `qml` has used the C++ quantiles for these families since the stats
routing was merged on 2026-11-12 (TASK-198, `d46a1b21`); before that it called
`stats`. The d/p/q truth consumer accepts either neighbour at a numerically
ambiguous jump, so no suite reports it. The other discrete families were not
measured; all that go through `discrete_quantile` share the cause.

## Options

1. **Fuzz as in `stats`.** One change in `discrete_quantile` and in hyper's
   predicate. The round trip holds and exact ties resolve downwards; a `p`
   within `64 eps` (relative) above a jump returns the lower value. Results
   agree with `stats`.
2. **Compare on the scale `p` was given.** The round trip then holds by
   construction with no band, but an exact tie such as the hyper case stays
   wrong unless the CDF is correctly rounded.
3. **Option 2 plus a tie band equal to the CDF's own error bound** (a few
   ulp). The narrowest band, at the cost of an error bound per family.

In all three the truth consumer gains the round trip `q(p(k)) = k` and exact
rational ties as required cases.
