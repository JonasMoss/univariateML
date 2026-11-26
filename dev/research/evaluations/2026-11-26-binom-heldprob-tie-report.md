# Exact held-probability binomial size ties

TASK-330, based on master `2d89fd4a117a7ee7850558e6a04b4c4d74b87589`.

With counts `c(0, 6)` and held probability `0.5`, the master binding reports size 7, `certified = TRUE`, `unique = NA`, and an empty maximizer set. The older R reproduction cited by the task reported `unique = TRUE`. In both cases size 8 is missing. The revised fit returns size 7 as representative and the two certified points `(7, 0.5)` and `(8, 0.5)` with `unique = FALSE`. Their common likelihood is exactly `7 / 2^14`. The shared binding delivers both points and their common log likelihood to R. [Master replay](2026-11-26-binom-heldprob-tie-data/master-native.json), [revised replay](2026-11-26-binom-heldprob-tie-data/fixed-native.json).

## Independent reference

The replay uses Python `Fraction` and `math.comb` to enumerate exact joint masses at feasible sizes. It uses the exact dyadic value of each binary64 hold and evaluates the logarithms with mpmath at 100 decimal digits, recording 85 significant digits. Enumeration passes the held-probability tail crossing `max(x) / p`. The same script compiles and queries the public fit-snapshot binding probe for each input. [Replay script](2026-11-26-binom-heldprob-tie-data/replay.py), [master double-only](2026-11-26-binom-heldprob-tie-data/master-double.json), [revised double-only](2026-11-26-binom-heldprob-tie-data/fixed-double.json).

| Counts | Held probability | Exact maximizing sizes | Maximized log likelihood |
|---|---:|---|---:|
| 0, 6 | 0.5 | 7 and 8 | −7.758150378783921026735896956971292 |
| 0, 0, 7 | 0.5 | 7 and 8 | −14.556090791758851497761874550621708 |
| 0, 7 | 0.25 | 15 and 16 | −7.551241074199968625436606011670816 |
| 1, 1, 1, 1 | 0.5 | 1 and 2 | −2.772588722239781237668928485832706 |
| 0, 6 | `0x1.fffffffffffffp-2` | 8 | −7.758150378783920582646687106908775 |
| 0, 6 | `0x1.0000000000001p-1` | 7 | −7.758150378783921470825106807034254 |
| 0, 1, 6 | `0x1.5555555555555p-2` | 9 | −9.171546806660522314667972576844090 |

For the second through fourth rows, the common joint masses are respectively `2^-21`, `6435 * 3^23 / 4^30`, and `1/16`. Master omitted the second point in the first three rows and returned both in the fourth. It returned the correct sizes for the binary64 predecessor and successor of 0.5 and left their cardinalities unknown. Their 7-to-8 log increments are approximately 2.22045 × 10⁻¹⁶ and −4.44089 × 10⁻¹⁶, respectively.

For `c(0, 1, 6)`, master returned size 8 at the binary64 approximation to $1/3$, with log likelihood −9.171546806660522397934699423730829. The exact held value is $1/3-1/(3\cdot2^{54})$. At the exact rational hold $1/3$ the ratio for the move from 8 to 9 is one. At the binary64 hold it is $(1+2^{-55})^3>1$, giving a log increment of about 8.32667 × 10⁻¹⁷. Size 9 is the unique winner. This strict near miss is corrected by the same certificate.

## Certificate

For feasible size $k$, write $t=k+1$ and $q=1-p$. The likelihood increment from $k$ to $k+1$ has the sign of `q^n × ∏ᵢ [t/(t−xᵢ)] − 1`.

Every factor $t/(t-x_i)$ decreases with $t$, strictly when $x_i>0$. With at least one positive count, equality gives exactly the two adjacent maximizers. A positive incoming sign, where the predecessor is feasible, and a negative outgoing sign give a singleton. At the support boundary the outgoing sign suffices. With all counts zero and an interior hold the ratio is $q^n<1$ at every size. [Binomial tie derivation](2026-11-25-binom-ties-math-report.md#what-this-changes-for-the-fitter), [Lean crossing proof](../../lean/UnivariateML/Family/Discrete.lean#L380).

- `binom/mle.hpp:79` compares products using eight base $2^{32}$ digits. The largest digit multiply plus the stored digit and carry is $2^{64}-1$. Overflow makes the ordering indeterminate. The hold is decoded exactly as an odd integer divided by a power of two, and its complement is formed in integers whenever needed.
- `binom/mle.hpp:122` tests equality when the signed products exceed capacity. For reduced $q=A/B$, equality requires $(At/B)^n=\prod_i(t-x_i)$. An integral power of a positive rational implies the rational is integral, so $B\mid t$. When that condition holds, GCD cancellation matches the $n$ copies of $At/B$ against the observation factors without forming any large product. Each cancellation is exact. The factors are no larger than $t$, and the work allowance bounds the scan. Constant counts retain the exact fused sign test for $x-pt$. Every exact input to that difference is a multiple of the smallest binary64 subnormal, so a nonzero difference cannot round to zero.
- `binom/mle.hpp:145` falls back to a practical sign margin when integer ordering is indeterminate. The observation term `log1p(x_i/(t-x_i))` uses an exactly representable integer denominator. Its relative sensitivity to rounding in the division is at most one. Compensated summation of nonnegative terms avoids a first-order error growing with sample length. Compare the two magnitudes with margin `32 ε max(Σᵢ log1p(x_i/(t-x_i)), −n log1p(−p))`. The margin allows usual binary64 division, libm and summation error. It is a practical floating-point comparison, without directed-rounding bounds. It can order an increment. Exact arithmetic alone can declare equality.
- `binom/mle.hpp:307` checks both feasible neighbors after a successful search. A contrary sign moves the candidate one step and reuses the known crossing sign. If that step fails to establish a valid winner, the fitter reports the existing iteration failure. If the comparisons remain unresolved without refuting the search result, it returns the candidate with `certified = FALSE` and unknown cardinality. The arithmetic uses at most three shares of the `128 * min(iterlim, 100000)` certificate allowance. The paired result at `binom/mle.hpp:192` contains two typed points with exactly the held probability. Both outcomes use the existing PMF-based likelihood evaluation.
- `binom/mle.hpp:583` propagates the certification flag through `FitOutcome::unknown_cardinality`. The existing singleton inventory entry for `result.singleton_proved=singleton_proved` still applies. Its dossier now cites the signed-crossing argument and describes the fallback. [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/binom/mle.hpp), [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/binom/README.md), [singleton inventory](../../validation/singleton-proof-sites.csv).

## Routes and regression tests

The nearby grid uses lengths two and three, with a minimum in 0–2 and remaining counts drawn with replacement from 5–8. In both precision modes, all 42 held-size fits had a singleton, and all 42 fits holding both coefficients at feasible values had a singleton. For the both-free route, exact dispersion classified 35 Poisson escapes and seven finite fits. Exact rational profile comparisons matched all seven finite sizes and found no positive-mean tie in the grid. Separate probes covered incompatible probability endpoints, the maximizing size axis at held zero probability, the constant sample at held unit probability, and the all-zero singleton at an interior hold. Existing C++ and R tests also exercise the all-free all-zero set and the size-zero probability axis.

The appended C++ and R tests pin the seven rows above. The C++ cases verify the typed two-point payload, the GCD path on 64 copies of `c(0, 6)`, and the uncertified fallback when that exact calculation exhausts its allowance. The R cases verify both public point rows, the preserved hold, paired likelihood bounds, and the singleton outcomes on the strict near misses. [C++ tests](../../../univariateML-cpp/include/cppunivariate/distributions/binom/test.cpp#L767), [R tests](../../../bindings/r/distributions/binom/test.R#L335).

## Final checks

All final runs used the workstation. Native and double-only C++ used GCC 15.2.0.

| Check | Result |
|---|---|
| Ordinary snapshot, native and double-only | 94 records per mode, zero differences or missing records |
| Modifier-route snapshot, native and double-only | 677 records per mode across window truncation, zero inflation, zero modification and zero truncation, zero differences or missing records |
| `fit-density.py` and `fit-consistency.py` | Zero new failures in both |
| `strict-uniqueness.py` | 218 registered proof sites pass |
| `just test-changed --no-long-double` | C++ 9/9 in each precision mode. R dev tier 2,725 passes, 14 skips, 0 failures and 0 warnings. JS build passed and JS tests 263/263 |
| `just site-check` | Passed, including the binomial page, 86 family pages and 86 C++ snippets |

The four diffs used the stored batch-19 baselines filtered to the replayed family or routes and `build/scratch/diff_baseline.py`. None of the newly corrected examples occurs in those snapshots. The targeted runner selected `bbinom, binom, cmb, geom, lgser, nbinom, pois`. Its assembly step regenerated four derived binomial certification rows in `dev/validation/capabilities.csv`. The generated CSV was restored to keep the patch within the assigned files. Subsequent assembly derives the updated rows from the dossier.

## Open

- The general existence of a positive-mean, nonconstant both-free adjacent tie remains open. The local grid and the wider searches in the math report give bounded exclusions. A proof over all feasible sizes and samples or an explicit feasible integer sample would settle the question. [Both-free searches](2026-11-25-binom-ties-math-report.md#both-size-and-probability-free).
- When checked signed products exceed capacity and the practical margin cannot order the increment, an exact ordering method with greater capacity or a tighter certified bound would resolve more strict near misses. The current outcome keeps the unrefuted estimate and exposes the unresolved cardinality.
