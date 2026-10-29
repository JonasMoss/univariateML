# Probe: own C++ d/p/q vs extraDistr/actuar vs an independent truth

Throwaway probe (2026-10-29) for the "own d/p/q/r, drop extraDistr and
actuar" estimate. Scripts still use their original `/tmp/dpqr-probe/` paths.

- `gen.R` writes an Rcpp shim over `distributions/<id>/dpqr.hpp` for the
  external-reference families whose C++ argument order matches `README.md`
  (54 of 66; the other 12 are density-only or have a different order).
- `probe2.R <tag> <ids...>` evaluates d, p (both tails, natural and log) and
  q (both tails, natural and log input) at one parameter point per family
  (the golden's `sampler_args`): x at reference quantiles from 1e-300 to
  1 - 1e-300, u from 1e-300 to 0.95, log-u to -1e4.
- Truth: continuous p by integrating the density in log space over the
  smaller tail (log substitution towards infinite ends); discrete p by
  cumulative log-sum-exp of the pmf; q is correct if the true probability at
  q, or at its neighbouring doubles, brackets the target.
- `results.csv`: one row per family and check; `ours_wrong`/`ref_wrong`
  count points off the truth by more than 1e-6 (relative, log scale).

Known false positives: hcauchy (heavy-tail integration; ours matches the
closed form), dnorm (truth uses extraDistr's pmf, which is wrong far out),
and possibly heavy-tailed discrete q (bnbinom). zmnbinom and ztnbinom did not
finish. One parameter point per family only.
