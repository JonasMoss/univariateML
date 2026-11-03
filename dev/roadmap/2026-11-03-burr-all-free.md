# Burr XII with all coefficients free (later; TASK-138)

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

Missing for a production fit: a signed Weibull tail bound (to certify
Weibull winners and shorten the k range), certified enclosure of the
maximizing stationary point (the k-profile is not concave), and production
error margins. Joint uniqueness is open.
