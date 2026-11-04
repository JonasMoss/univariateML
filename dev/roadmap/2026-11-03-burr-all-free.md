# Burr XII with all coefficients free (TASK-138; implemented 2026-11-04)

Parked 2026-11-03 after two research rounds; the companion to
`2026-11-03-bbinom-free-size.md`, and probably the easier of the two.

- [Closure](../research/evaluations/2026-11-03-burr-closure-report.md),
  proved: only Weibull and Pareto keep likelihood; a finite MLE exists iff
  some finite point reaches `max(P, W)` (equality needed); compact
  feasibility region; Pareto cusp `H(k) = P + nk log k + ...`; at least half
  the sample at the minimum means no finite MLE; Weibull seam coefficients
  `A_W`, `C_W`; certified counterexamples to local classifiers.
- [Certificate attempt](../research/evaluations/2026-11-03-burr-cert-report.md),
  proved: fixed-k concavity, small-k exclusion, uniform Weibull tail
  `F(k) <= W + n^2(k+1)/(2(k+1-n)^2)`, entropy-dual upper bounds, convex
  chord envelopes in k. The value enclosure closes on 27 of 39 workloads;
  near-Weibull samples exhaust the budget.

- [Seam coordinate](../research/evaluations/2026-11-03-burr-xi-report.md),
  proved: in `xi = 1/k` the profile extends to Weibull at 0 and
  `H(xi) <= W + A xi + B xi^2` near 0, uniformly over re-optimized nuisances;
  with `A < 0` this excludes all large `k` against Weibull itself. Value
  enclosure now closes on all 50 workloads.

- [Root boxes](../research/evaluations/2026-11-04-burr-root-report.md),
  2026-11-04: stationary maxima isolated by contraction boxes with a proved
  negative profile curvature; exact rational certificate for the golden root;
  global outcomes certified on 49 of 50 workloads in a Python research engine
  under a stated arithmetic contract. Remaining: the guarded C++ port, the
  binding/invburr status mapping, and speed (research engine ~100 ms per fit).

Superseded list (after burr-xi): enclosure and isolation of the
maximizing stationary point with coefficient accuracy, production error
margins (bracketed Weibull root propagated into the seam constants), and the
degenerate seam slope `A = 0`. Earlier list: a signed Weibull tail bound (to certify
Weibull winners and shorten the k range), certified enclosure of the
maximizing stationary point (the k-profile is not concave), and production
error margins. Joint uniqueness is open.

## Implemented 2026-11-04

[burr-port](../research/evaluations/2026-11-04-burr-port-report.md): the
certified all-free fit is the default (`burr_mle`), with outcomes Ok,
WeibullBoundary, ParetoBoundary, CertificateTie, CertificateBudget and
NumericalFailure. About three times slower than the legacy fitter.
Follow-ups: invburr (Burr on 1/x) and all-free pareto4 still use
`burr_mle_legacy`; held subsets are uncertified (held-power Weibull
comparison is numerical); exact ties and the nearly constant budget case.
