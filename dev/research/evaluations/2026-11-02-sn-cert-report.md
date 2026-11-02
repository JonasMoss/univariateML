# TASK-116: skew-normal outer shape search

Date: 2026-11-02. Tested source: `2dad9da3f2fe9e538e8acfa231cca83d77412ca5` plus the uncommitted changes in this worktree. No commit or fixture regeneration was made.

The free-location, free-shape fit now bounds the entire profile, including both half-normal faces. Its initial grid and local refinements provide feasible likelihood candidates; a branch-and-bound check then covers finite shape intervals, while separate bounds cover both infinite tails. An exhausted 20,000-bisection budget, an unresolved tail bound or an unresolvable interval returns `IterationFailure`. The R binding reports failure rather than returning an unchecked candidate. The practical certificate is conditional on computed special functions and comparisons, not an outward-rounded interval certificate.

The [skew-normal dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sn/README.md) gives the derivation and implementation scope; [mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/sn/mle.hpp) implements it. `fit.certified` remains `partial`: the default free-location shape search and the held-scale/free-location shape search have the stated global gap bound, while fixed-location and fixed-shape routes retain numerical root or Newton stopping checks without a separate likelihood-gap certificate.

## Mathematics and stopping rule

For $z_i=\eta x_i-h$ and any reference arguments $t_i$, concavity of $\log\Phi$ supplies the tangent bound $\log\Phi(\alpha z_i)\leq\log\Phi(t_i)+r_i(\alpha z_i-t_i)$, with $r_i=\phi(t_i)/\Phi(t_i)$. For fixed $(\eta,h)$ the right-hand likelihood bound is affine in shape, so its maximum on an interval lies at an endpoint. Maximizing each endpoint bound analytically over the free inverse scale and location gives an upper bound for the full interval. The minimum of two such upper bounds remains an upper bound. Inner convergence is not needed to form a tangent: any finite reference point suffices.

For the positive tail $\alpha\geq A$, use $\Phi(t)\leq1$ for $t\geq0$ and $\Phi(t)\leq\exp(-t^2/2)$ for $t<0$. The relaxed objective reduces to a convex piecewise quadratic in the location, followed by a closed-form inverse-scale maximization, or evaluation at the held inverse scale. Its limit as $A\to\infty$ is the right translated half-normal face likelihood. Reflection gives the negative tail and left face. These are exact-arithmetic inequalities, rather than evidence from the grid or from the experiments below. The dossier states the formulas and assumptions in detail.

Finite intervals and each tail are pruned when their computed upper bound is at most $5\cdot10^{-9}n$ above the best candidate or face. The existing face tie rule allows a face within $10^{-8}n$ of the best finite candidate, giving an exact-arithmetic likelihood-gap allowance of at most $1.5\cdot10^{-8}n$. The stated practical allowance is $2\cdot10^{-8}n$, conditional on computed comparisons. Floating-point rounding, the continued-fraction normal-tail evaluation and the practical margin have not been bounded with outward-rounded interval arithmetic. A budget failure is a numerical failure, not evidence of non-existence.

The inner maximization remains a numerical Newton solve. Fixed shape is strictly concave in inverse scale and scaled location for nonconstant data, but the stopping score is not itself a global likelihood-gap certificate. With fixed location, the shape score separates and is solved by numerical bisection; the two face conditions include the penalty for observations equal to the held location. Those held routes are explicitly left numerical. No inference or asymptotic guarantee is claimed.

## Comparisons and performance

The original fitter at `2dad9da3` and the new fitter were run on the same 600 generated samples, each of size 100, using C++ `std::mt19937` seed 20261102. Shapes cycled through -100, -20, -5, -1, -0.001, 0, 0.001, 1, 5, 20, 100 and 1e8, including near-normal and near-face samples. The C++ comparison found 390 finite fits and 210 boundary results in each version, with no status changes, no new failures and zero finite log-likelihood difference at 17-digit output precision. The same samples were streamed through the R binding and agreed with the old core in all 600 cases, including the boundary direction; the largest finite log-likelihood gap was zero at reported precision. The new R family tests also compare free-shape fits against several fixed-shape fits. These are finite regression checks, not a proof of globality.

On the 500-observation golden sample, both versions returned xi 0.0712306862893739, omega 0.9008377142396523, alpha 2.241606711280416 and log likelihood -456.8240018413871 at printed precision. Three alternating optimized single-core fits took 0.193–0.209 s for the old fitter and 0.222–0.244 s for the new fitter, approximately 1.1–1.3 times as long on this machine. Profile evaluation counts were 202 and 269 respectively. Compilation and input parsing were outside the timed region. The golden file was not changed. [Drivers and reproduction notes](2026-11-02-sn-cert-data/README.md) retain the seed, sample generation, old/new comparison and benchmark source; regenerable sample and output tables remain under ignored `build/scratch/`.

The independent numerical bound spot check used seed 116, 200 datasets and 8,000 feasible probes; no sampled likelihood exceeded its bound. It tests the inequalities numerically and cannot establish their validity between probes. C++ tests exercise the interval and tail bounds, including a zero tail quadratic with held scale. The existing golden, score, failure, hold and face tests remain in place. The 600-pair comparisons and these tests did not force exhaustion of the new 20,000-bisection budget; direct budget-exhaustion coverage remains open.

## Final validation

All final runs were local on `JonasBI`, Linux x86-64, GCC 15.2.0, R 4.6.1, BH/Boost 1.90 and Node 22.22.1. The Python bound check used Python 3.14.5, NumPy 2.5.0 and SciPy 1.18.0. Final commands ran separately through `/tmp/umvl-cpu/run4`, with `TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` set to this worktree's `build/tmp`; the lock sets BLAS/OpenMP threads to one. No simbox run was used.

| Command | Result |
|---|---|
| `just cpp-test` | Pass, 106/106 dev shards. |
| `just cpp-test-no-long-double` | Pass, 127/127 tests. This tests the Boost portability configuration; it is not an outward-rounding test. |
| `just test` | Pass: 0 failures, 0 warnings, 151 skips, 20,148 passes. |
| `just test-full` | Pass: 0 failures, 0 warnings, 0 skips, 41,751 passes. |
| `just js-build` | Pass. |
| `just js-test` | Pass: 413/413, no skips. |
| `just site-check` | Pass on retry: 99 family pages, MathML and links checked; 99 generated C++ snippets compiled and linked. |

The first site check failed because the baseline paralogis and inverse-paralogis dossiers each contained two bell characters in `\alpha` expressions. The four characters were corrected in those two dossier prose passages, and the retry passed. The other six suite results predate this prose-only correction; the skew-normal source and tests were unchanged between those runs and the site retry. `git diff --check` passed. The original site failure and retry logs, as well as the other run logs, are retained under ignored `build/scratch/` in this worktree.

## Remaining limits

- Floating-point comparisons and the $2\cdot10^{-8}n$ practical allowance are not backed by a directed-rounding error proof; extreme or ill-conditioned inputs may return `IterationFailure`.
- Held-location/free-shape and held-shape routes retain numerical stopping checks rather than the free-location likelihood-gap certificate. The metadata therefore remains `partial`.
- The finite branch-budget exhaustion path has not been forced by a dedicated regression test. The existing low-iteration tests cover inner-optimizer failure, a distinct path.
- The 600 paired samples, golden, bound probes and platform runs are finite numerical evidence. They do not establish validity for every dataset, machine, or floating-point implementation.
