# TASK-113: beta-binomial free-size tail investigation

Source revision `1fc8af0a5e4cf62b04d77e647182143b27903f65`, plus the uncommitted changes in this worktree. No free-size shape subset was enabled. The supported fixed-size fits and their goldens are unchanged. The held-beta diagnostic now correctly states that the bound tends to zero; it does not imply that the likelihood is eventually monotone.

## Held-beta bound

Let `h` be the sample size, `m` the number of positive observations, `M` the maximum observation, `b > 0` the held beta, and `N >= 2*M`. Write `H(N) = sum(j=0,...,N-1) 1/(b+1+j)`. The individual probability inequalities in [the earlier held-beta report](2026-10-30-holds-bbinom-report.md) imply that, uniformly over `a > 0`, the joint likelihood is at most

```text
max( 2^sum(x) * (m/(e*h*H(N)))^m,
     exp(-h*H(N)) * product_{x_i>0} (2^(x_i+1)-1) ).
```

For `a <= 1`, multiply the bounds `P(0) <= exp(-a*H)` and `P(x) <= 2^x*a*exp(-a*H)` for positive `x`; maximize `a^m*exp(-h*a*H)` over all positive `a`. For `a >= 1`, use the stochastic-monotonicity bound from the earlier report, including `P(0) <= exp(-H)`, and multiply the individual bounds. Both terms decrease with `N`, so a positive candidate likelihood yields an explicit finite tail cutoff. This is an exact-arithmetic bound, not a claim that the profile is monotone.

The first term is `O((log N)^(-m))`, improving on applying the earlier one-observation `O(1/log N)` bound to the whole likelihood. Its power is sharp for a fixed sample: along `a = t/log N` with fixed `t > 0`, `P(0) -> exp(-t)` and `P(x) ~ exp(-t)*t/(x*log N)` for fixed positive integer `x`. Consequently the likelihood times `(log N)^m` approaches `exp(-h*t)*t^m/product_{x_i>0} x_i`. This does not establish that the displayed constants are optimal. Depending on the data and `b`, the cutoff can be useful or far beyond a feasible enumeration budget.

The [reproducer](2026-11-01-bbinom-size-data/check_bound.py), run through `/tmp/umvl-cpu/run` with Python and mpmath at 60 decimal digits, used seed 113 for 3,000 random samples, shapes and sizes through 2,000. The largest log-likelihood-minus-bound was `-0.71808414`; all inequalities passed. On the path above, the scaled likelihood divided by its asymptotic limit was 2.9951, 1.7315, 1.3161 and 1.1473 at sizes `10^5`, `10^10`, `10^20` and `10^40`, respectively. These numerical checks are not a proof or an outward-rounded implementation certificate.

## Faces and open implementation work

At fixed size, the unrestricted shape fit has binomial, endpoint two-point and point-mass faces; a held shape has endpoint point-mass faces. With size growing, held alpha admits a negative-binomial face with that alpha, and both shapes free admit arbitrary negative-binomial and Poisson faces. Held beta has the zero likelihood tail above when a positive observation occurs. Paths with mass escaping to infinity also exist. These are known faces, not an exhaustive closure proof; the earlier report gives the parameter paths and special cases. All-zero and constant samples and unidentified size zero or one require separate treatment.

**Open: held beta.** The bound above is sufficient in principle to cut off the integer tail, but the search and its finite-size comparisons have not been implemented. In particular, the existing fixed-size optimizer certifies its numerical profile only to a nonzero allowance; a global search must propagate those upper bounds, compare finite-size and point-mass faces, resolve or report numerical ties, enforce a work budget, and account for floating-point error. The present work does not establish those comparisons. The practical cutoff and enumeration costs need measurement on varied data.

**Open: held alpha and neither shape held.** A uniform, computable correction to the appropriate maximized negative-binomial/Poisson faces, valid over the free shape parameters, is missing. The unrestricted search must also compare all fixed-size binomial and endpoint faces. No closure or global optimality claim follows from the tested finite grids in prior reports. These subsets remain guarded as not implemented.

The requested C++ and R finite-fit, face-stop, tie, work-budget and seeded brute-force tests for newly supported subsets therefore do not exist. No conformance expectations or metadata were changed to advertise support. A valid implementation needs those tests, including explicit maximum enumeration sizes and numerical tolerances. No `dpqr.hpp`, special function, truth fixture, generated d/p/q/r, or golden was changed.

## Validation

All commands ran separately through `/tmp/umvl-cpu/run4`, which pins BLAS/OpenMP to one thread and uses at most four slots. Temporary and log files were under `build/tmp` and `build/scratch`; the pre-existing site dependencies were copied into this worktree before the site check. The compute locks caused substantial waiting between commands. Results:

| Command | Result |
|---|---|
| `just cpp-test` | Pass, 105/105 development shards. |
| `just cpp-test-no-long-double` | Pass, 126/126 tests. |
| `just test` | Pass: 19,898 passes, 0 failures, 0 warnings, 151 skips. |
| `just test-full` | Pass: 41,344 passes, 0 failures, 0 warnings, 1 skip (optional `sn` unavailable). |
| `just js-build` | Pass. |
| `just js-test` | Pass, 400/400. |
| `just site-check` | Pass. |

These suites verify the existing code and the changed diagnostic, not the absent free-size algorithms. The C++ development suite completed successfully, though the shell line intended to print its exit status subsequently failed because `status` is a read-only zsh variable; its log records 105/105 passing.
