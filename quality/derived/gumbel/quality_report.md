---
id: gumbel
spec_hash: 99dc94aa6aa45be7
mpmath_dps: 50
claimed_level: validated
---

# gumbel — quality report

## Symbolic vs C++ round-trip

- ✓ nll_at_theta: max_rel = 1.072e-15 (tol 1e-12, n=17)
- ✓ grad_at_theta: max_rel = 2.257e-14 (tol 1e-12, n=34)
- ✓ hess_at_theta: max_rel = 1.872e-15 (tol 1e-12, n=68)

## Provenance

`spec_hash` is the SHA-256 (16 hex) of the canonical sympy `srepr(log_density)`. It identifies the math, not the implementation:
a re-spelling of the same expression that simplifies to itself (e.g. swapping `log(x*y)` for `log(x) + log(y)`) yields a different
hash, which is the desired behaviour — it forces an explicit reroll of the derived artifacts when the math changes.

