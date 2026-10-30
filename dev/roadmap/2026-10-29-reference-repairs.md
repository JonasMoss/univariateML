# Reference repairs adopted

**Status:** superseded 2026-10-30 (TASK-78.9, merge 8471607): extraDistr and actuar are no longer used and every runtime repair is deleted; kept as history. Earlier: adopted by user decision, 2026-10-29. Upstream PRs may follow
later; extraDistr's maintainer is unresponsive, actuar's response is unknown.

The package keeps runtime-checked repairs in
`univariateML_reference_fixes` (`bindings/r/R/code_helpers.R`).
Every entry checks its defect on every call and switches off once the
reference function is fixed. Fitting and log likelihoods remain C++ operations.

| Reference | Adopted repair |
|---|---|
| extraDistr ptriang | Correct the lower endpoint (existing) |
| extraDistr pdweibull | Floor the argument before calling the reference (existing) |
| actuar d/p/q/r trgamma and invtrgamma | Gamma backend composed with power transforms; log-CDF anchor for underflow |
| extraDistr d/p/q tnorm | Integrate the parent normal relative to the interval mode, with recycled vector arguments |
| extraDistr qgev and rgev | Stable Gumbel transform using expm1, including finite endpoints |
| extraDistr pgev | Stable upper-tail log probability |

The two held-back patch files have been incorporated and removed.
The truncated-normal repair also serves mlhnorm, preserving its
`mean = mu, sd = sigma, a = mu` map, open density endpoint and sampler.
Sampler defect checks restore the RNG state, including absence of a seed.

Visibility is recorded in KNOWN_ISSUES.md, each affected model's Details,
the package help page and NEWS. Regression tests live with each model.

The sads repairs were removed on 2026-10-29 when Zipf moved to the
package's own d/p/q/r and sads left Imports.
