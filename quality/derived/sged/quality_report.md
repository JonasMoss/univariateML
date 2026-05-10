---
id: sged
spec_hash: 6b4ef86038b440ca
mpmath_dps: 25
claimed_level: validated
---

# sged — quality report

## Symbolic vs C++ round-trip

- ✓ nll_at_theta: max_rel = 1.081e-15 (tol 1e-12, n=17)
- · grad_at_theta: max_rel = 1.015e-08 (SKIP — FD grad, advisory only, n=68)
- ✓ hess_at_theta: max_rel = 6.028e-16 (tol 1e-12, n=272)

## Provenance

`spec_hash` is the SHA-256 (16 hex) of the canonical sympy `srepr(log_density)`. It identifies the math, not the implementation:
a re-spelling of the same expression that simplifies to itself (e.g. swapping `log(x*y)` for `log(x) + log(y)`) yields a different
hash, which is the desired behaviour — it forces an explicit reroll of the derived artifacts when the math changes.

