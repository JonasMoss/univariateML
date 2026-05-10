---
id: std
spec_hash: e9e0dca5a4eb0a27
mpmath_dps: 50
claimed_level: validated
---

# std — quality report

## Symbolic vs C++ round-trip

- ✓ nll_at_theta: max_rel = 1.498e-15 (tol 1e-12, n=17)
- · grad_at_theta: max_rel = 1.703e-05 (SKIP — FD grad, advisory only, n=51)
- ✓ hess_at_theta: max_rel = 1.459e-13 (tol 1e-12, n=153)

## Provenance

`spec_hash` is the SHA-256 (16 hex) of the canonical sympy `srepr(log_density)`. It identifies the math, not the implementation:
a re-spelling of the same expression that simplifies to itself (e.g. swapping `log(x*y)` for `log(x) + log(y)`) yields a different
hash, which is the desired behaviour — it forces an explicit reroll of the derived artifacts when the math changes.

