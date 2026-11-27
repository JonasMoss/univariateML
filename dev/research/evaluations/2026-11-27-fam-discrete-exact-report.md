# Exact discrete boundary and tie decisions

TASK-383, based on `4a014db6`. The changes are in the `fam-discrete-exact` worktree and were left uncommitted under the task's ground rules.

## TASK-245 · Fixed-size beta-binomial

The former detector needed a signed neighbourhood expansion and numerical coverage of the remaining concentration profile. This could leave exact equidispersion unresolved. At held size 3 with counts `(0, 1, 2)`, both sides of the boundary inequality are 18.

With both shapes free and an interior observation, the fitter now compares $mn\sum_i x_i(x_i-1)$ with $(m-1)(\sum_i x_i)^2$ in checked integer arithmetic. A smaller or equal left side returns a certified binomial limit immediately. The integer test uses an unsigned 128-bit type where available and an unsigned 64-bit type otherwise. Overflow leaves the comparison to the existing profile search. The direct route follows the strict dominance and supremum theorems `BetaBinomial.necessity` and `BetaBinomial.binomial_sup_of_nonpos`. [Lean proof](../../lean/UnivariateML/Family/BetaBinomial.lean) · [Analysis](2026-11-25-bbinom-math-report.md#global-necessity-proof)

The regression asserts zero solver iterations, certified `BinomialLimit`, and probability $1/3$. Snapshots with underdispersion or exact equidispersion may change to a certified limit or lose finite-search candidates and iterations. Positive-dispersion fits continue through the interior search.

## TASK-373 · Hypergeometric adjacent maxima

For $N=5$, $k=2$, and counts `(0, 2)`, the observation likelihood ratios from $m=2$ to $m=3$ are $1/3$ and 3. The old detector required each ratio to be one, so the R regression expected an unresolved comparison.

The scalar route now cancels integer factors in the exact rational product of ratios at the two adjacent candidates. Equality produces both maximizers in typed `Points` with `unique = FALSE`. Cancellation uses checked unsigned 64-bit arithmetic. A bounded product that overflows, or a numerical overlap without proved equality, leaves the tie unconfirmed. The composition and draw proofs use strictly decreasing adjacent ratios. The population proof uses the strict single crossing after its ratio reaches one. [Lean proof and source gap](2026-11-26-lean-hyper-report.md#adjacent-ratios-and-equality)

The C++ regression covers both the composition and symmetric draw fits. The existing R regression now expects a certified two-point maximizer set with compositions 2 and 3. This is the expected snapshot change for mixed-ratio ties whose products close within the integer range.

## TASK-366 · Conway–Maxwell-binomial central limit

With size 3 and probability $3/4$ held, an upper-central singleton at 2 gave a limit on `{2, 3}` with mass $1/4$ at 2. The correct limit has support `{1, 2}` with mass $3/4$ at 2. The two central binomial coefficients agree, so the odds of the upper count are the held odds. The fixed-size witness now obtains both support points from the design. `Discrete3CMB.odd_center_supremum` proves that this limit strictly dominates every finite dispersion, including for either singleton. [Lean proof](../../lean/UnivariateML/Family/Discrete3CMB.lean) · [Held-size analysis](2026-11-25-cmb-math-report.md#held-size)

The regression repeats each singleton twice and checks the certified support, probability, and log likelihood. Affected snapshots keep `AdjacentLimit` and correct its support and likelihood payload. The separately proved free-size tie for `(0, 6)` at $(p,\nu)=(1/2,1)$ can still be unresolved in the fitter, as allowed by the card.

## TASK-350 · PIG held-mean Poisson boundary

For `(0, 0, 0, 0, 0, 1, 1, 2, 2)`, the all-free fit has exact equality and escapes to Poisson. Holding the binary64 rounding of $2/3$ gives the strictly positive numerator $(3\mu-2)^2=2^{-106}$ and a finite shape near $2.16\times10^{32}$. The numerical comparison formerly left the held fit unresolved. The small-score expression also used equality with the rounded sample mean to select the all-free constant.

The boundary calculation now uses the integer moments $n,S,F$ and the exact dyadic hold. Its centered numerator is $nF-S^2+(n\mu-S)^2$, with only $nF-S^2$ remaining for a free mean. The small-score formula carries the resulting constant into the shape solve. Checked-range failures retain a padded comparison. The held-mean score theorem and the all-free variance theorem establish existence and uniqueness in their respective positive-score regimes. [Lean proof](../../lean/UnivariateML/Family/Discrete3PIG.lean) · [Exact predicates](2026-11-25-pig-math-report.md#exact-predicates-and-examples)

The C++ regression checks the two masks and verifies shape to relative tolerance. The existing C++, R, and JavaScript regressions for `(0, 1, 2)` with held mean `0x1.93cd3a2c8198ep+0` now expect a certified Poisson limit. Exact rational evaluation makes $3\mu^2-6\mu+2$ negative there and positive at the next binary64 value, which the C++ regression checks as a certified finite fit. These are the expected changes from margin-flagged Poisson outcomes to a certified finite estimate or certified limit, depending on the held mean.

## TASK-307 · Truncated-binomial moment and size comparisons

On effective window $[1,\infty)$ with size and probability free, the previous routes needed numerical coverage against the mean-matched Poisson face. For `(1, 1, 1, 6)`, $T=10/3$ exceeds the fitted rate. The fitter now accumulates the two integer sums for $T$ and compares the positive-Poisson mean at $T$ with the empirical mean using a floating-point pad. Monotonicity gives the same sign as $T-\lambda$. A positive result certifies the face without a profiled finite prefix or tail cutoff. The zero-truncation payload retains one evaluated finite witness. A negative result proves that a finite size beats the face and retains the finite search. [Moment envelope](2026-11-25-tbinom2-math-report.md#the-moment-envelope-for-pure-zero-truncation) · [Finite improvement criterion](2026-11-25-tbinom-math-report.md)

For a lower-only window with retained lower count $L\ge1$ and a matched Poisson rate below $6/5$, the fitter can certify a proposed size through strict signs at its feasible neighbours. It verifies the rate bound using a finite positive-Poisson sum at the binary64 value just below $6/5$. It bounds each profile value using a checked probability enclosure, the curvature bound in log odds, and a numerical margin. Strict concavity in the transformed size coordinate orders the adjacent steps. A comparison that overlaps leaves this certificate inconclusive, so the existing coverage or uncertified fallback can decide the fit. [Small-rate proof](2026-11-25-tbinom2-math-report.md#lower-only-integer-profiles-at-small-matched-rates)

The regressions check certified `PoissonLimit` and `TpoisLimit` payloads on the same sample and two certified finite winners. Counts `(1^6, 2^3, 3)` select size 3, and `(2^28, 3^6, 4)` on $[2,\infty)$ select size 6. The expected snapshot changes are earlier certified Poisson limits and known uniqueness for finite small-rate fits with separated neighbouring profile values.

## Open · Exact isolated adjacent truncated-binomial ties

An exact tie between two nonconstant lower-only profiles would require exact equality of the optimized likelihoods at two separately determined probabilities. Strict concavity certifies the pair as global if that equality is established. The available proof and regressions do not provide an isolated pair.

An exact rational scan used all adjacent size pairs $(N,N+1)$ with $3\le N\le16$, all $1\le L\le N-2$, and probabilities $a/256$ for $1\le a\le255$. For each of the 105 size and window pairs, it intersected the conditional means computed from integer weights $\binom{N}{j}a^j(256-a)^{N-j}$. The only shared mean was $242/29$ for sizes 10 and 11, $L=8$, and probabilities $5/8$ and $1/2$. The three successive probability ratios on counts 8 through 10 are $55/54$, $11/12$, and $11/10$. Every ratio has 11-adic valuation one, so no nonempty sample can have product one. The scan leaves the general algebraic probability case open. A typed two-point result needs a bounded exact profile-equality test and a reproducible isolated example. The known constant-sample ridge and bounded-window graph do not supply such an example. [Exact ties and open cases](2026-11-25-tbinom2-math-report.md#exact-size-ties)

## Verification

All runs used the workstation CPU locks. Each item passed `just test-changed` before work moved to the next. On the final diff, the changed-family recipe selected the full C++ suites because the JavaScript regression and singleton proof inventory are shared files.

| Final check | Result |
| --- | --- |
| `just cpp-test` via `just test-changed --no-long-double` | 136/136 |
| `just cpp-test-no-long-double` via the same recipe | 181/181 |
| R development tier via the same recipe | 38,294 passed, 157 skipped, zero failures or warnings |
| JavaScript build and test via the same recipe | 601/601 |
| Existence atlas regeneration and `--check`, `existence_keys.py`, and `strict-uniqueness.py` | Passed |
| `just site-check` and `git diff --check` | Passed |

Golden samples were retained. Snapshot changes above are expected from the named regressions and detector scopes. The stored snapshots were not regenerated.
