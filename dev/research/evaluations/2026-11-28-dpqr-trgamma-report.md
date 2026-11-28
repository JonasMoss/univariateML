# Transformed-gamma evaluation at very large shape

TASK-464 · 2026-11-28 · Baseline `5f1317e288f557d9b373385838d74442765f9d3f`

## Density and distribution functions

With gamma shape $a$, power $c$, and scale $s$, the centered argument is
$z=c\log(x/s)-\log a$ for the forward law and $z=c\log(s/x)-\log a$ for
the inverse law. The Stirling form of the log density subtracts
$a(\exp(z)-1-z)$. An error $\delta z$ changes that penalty to first order
by $a\,\operatorname{expm1}(z)\,\delta z$. For archive 001625, rounding
the powered log ratio and $\log a$ separately loses digits when subtracting
two values near 53 to obtain $z$ of order $10^{-11}$.

For $a\ge10^5$, the shared kernel builds the log ratio from the original
binary64 operands and centers it in a three-word expansion. It uses
640 fractional bits as a fallback when $a\ge10^{40}$ and the expansion
gives $|z|<10^{-25}$. The fixed-point logarithm reduces its positive
ratio to a power of two times a mantissa in $[1,2)$, giving an
atanh-series argument of magnitude at most $1/3$. The omitted series
after 208 terms is below $2^{-650}$. Distinct binary64 operands bound
the positive log ratio below by $2^{-54}$, so the power fits the
64-bit integer part in the fallback region. For small $|z|$ the penalty
is evaluated using $w=\sqrt a\,z$ and the regular ratio
$(\exp(z)-1-z)/z^2$. This keeps the scaling by $a$ ahead of the square.
The series bound is analytic. End-to-end binary64 accuracy is measured
below. [Density and probability kernel](../../../univariateML-cpp/include/cppunivariate/distributions/trgamma/dpqr.hpp) · [Fixed-point logarithms](../../../univariateML-cpp/include/cppunivariate/special/fixed_log.hpp)

The former CDF path exponentiated a rounded log-gamma argument before
centering it. The corrected path passes $w$ directly to the existing
centered gamma tail kernel. Large-shape quantiles invert those tails in
$w$, using gamma Chernoff bounds for the bracket. They combine the
latent displacement with the physical log scale before exponentiating.
The kernels below $a=10^5$ retain their arithmetic for finite latent
arguments. An overflowing latent argument for $a\ge1$ returns the
limiting log density $-\infty$.

## High-precision comparison

A 569-row grid evaluates both orientations against the original density
formula at 420 mpmath digits and exact binary64 inputs. It spans central
offsets, offsets out to $z=\pm550$, powers derived from adjacent ratio
operands, and the largest finite binary64 shape. The error is
$|\widehat\ell-\ell|/\max(1,|\ell|)$. The CSV records actual $z$ after
rounding the inputs, since target offsets within a gamma standard deviation
can coalesce on the physical binary64 lattice. [Grid script](2026-11-28-dpqr-trgamma-data/density-grid.py) · [All grid results](2026-11-28-dpqr-trgamma-data/density-grid.csv)

| Gamma shape | Maximum error before | Maximum error after |
| ---: | ---: | ---: |
| $10^2$ | 4.36e-15 | 4.36e-15 |
| $10^4$ | 1.14e-13 | 1.14e-13 |
| $10^8$ | 1.85e-10 | 4.66e-15 |
| $10^{16}$ | 1.57e-7 | 2.63e-16 |
| $\approx10^{23}$ | 4.03e-4 | 8.50e-16 |
| $10^{28}$ | 2.46e-1 | 5.31e-16 |
| $10^{30}$ | 3.56 | 3.92e-16 |
| $10^{100}$ | 3.94 | 4.76e-16 |
| $10^{200}$ | 1.32e7 | 4.73e-16 |
| $10^{300}$ | 1.00 | 2.07e-16 |
| Largest finite binary64 | 1.00 | 2.22e-16 |

The local baseline probe sums to `127.81654984957379` at the five
001625 observations and the literal coefficients in the brief. The
patched sum is `127.81430798268818`, the rounded mpmath result. A second
regression uses a square whose exact value exceeds the rounded shape by
$2^{896}$ and gives $z\approx4.93\times10^{-32}$. Both orientations
match the rounded reference log density of about $-1.30235\times10^{238}$.
[C++ regressions](../../../univariateML-cpp/include/cppunivariate/distributions/invtrgamma/test.cpp)

The regenerated truth tables add $a=10^{30}$ in both orientations and
include all five exact 001625 observations at its archived coefficients.
Every prior value and quantile row retains its oracle result. On the added
parameter points, the largest scaled log-CDF error fell from 4.78 to
$2.45\times10^{-14}$. The largest relative quantile error on those points
fell from $7.10\times10^{-15}$ to $1.98\times10^{-16}$.
[Probability probe](2026-11-28-dpqr-trgamma-data/probability-fixtures.py) · [Probability results](2026-11-28-dpqr-trgamma-data/probability-fixtures.csv)

## Archive replay and held-power limit scale

The replay refits the 12 named `fpareto` and `gb2` inputs via the
binding-neutral snapshot probe and evaluates the resulting payloads with
`fit-density.py` and its original strict tolerances. With the density
fix alone, independent mpmath evaluation confirmed six fresh payload
mismatches in each precision mode. The largest relative difference
between the reported likelihood and the likelihood at its emitted
coefficients was $3.18\times10^{-8}$. For fresh 001625, the scale
`0x1.8b8751ffb6254p+38` was eight binary64 steps from the nearest
conditional optimum at the emitted shape and power.
[Replay before scale correction](2026-11-28-dpqr-trgamma-data/archive-before-scale.jsonl)

The shared held-power face path now carries the physical log scale
through exponentiation in the existing three-word expansion. The
face comparison and profile likelihood calculation use their original
formulas. Default fits use the ordinary search path. The final replay
passed all 24 certified limits and 24 candidate intervals across native
and double-only modes. The largest relative reported-to-summed
difference was $3.86\times10^{-11}$ in each mode.
[Held-power scale reconstruction](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp) · [Replay script](2026-11-28-dpqr-trgamma-data/replay.py) · [Final records and hexadecimal coefficients](2026-11-28-dpqr-trgamma-data/archive-replay.jsonl)

## Validation

The workstation ran the final checks with GCC 15.2, pinned numerical
threads, and the local CPU reservations. Simbox returned exit 75.
The fresh archive replay used `python3 -u` on the linked `replay.py`.

| Check | Result |
| --- | --- |
| `just dpqr-truth trgamma invtrgamma` | Both tables regenerated |
| Native CTest via `just test-changed --no-long-double` | 136/136 |
| Double-only CTest via the same command | 179/181, with two baseline failures described below |
| Focused CTest for `fpareto`, `gb2`, `invtrgamma` and `trgamma` | 4/4 native and 4/4 double-only |
| `just test-full` | 36 oracle checks passed. R had 68,175 passes, one skip, zero failures and zero warnings |
| `just js-build` and `just js-test` | WASM rebuilt and 601/601 tests passed |
| Strict density replay | 48/48 comparisons passed |

### Open checks

- The double-only `distribution_lst_slow` and `distribution_std_slow`
  tests fail on held-golden certification assertions. An independent
  rebuild from the untouched baseline headers and golden inputs
  reproduced all three assertions: `lst` returns `certified=true`
  at masks 1 and 2, and `std` returns `certified=true` at mask 1.
  The wrapper stopped there, so the full R, JavaScript, and archive
  checks ran separately.
  [Baseline probe](2026-11-28-dpqr-trgamma-data/baseline-certification.py) · [Output](2026-11-28-dpqr-trgamma-data/baseline-certification.txt)
- `just site-check` generated the catalogue data and built WASM.
  The site build stopped at `astro: not found` because the worktree
  lacks `site/node_modules`.
