---
id: naka
spec_hash: 5e3d9b7064f1d4ae
mpmath_dps: 50
claimed_level: validated
---

# naka — quality report

## Symbolic vs C++ round-trip

- ✓ nll_at_theta: max_rel = 2.841e-15 (tol 1e-12, n=17)
- ✓ grad_at_theta: max_rel = 3.950e-13 (tol 1e-12, n=34)
- ✓ hess_at_theta: max_rel = 1.297e-13 (tol 1e-12, n=68)

## Provenance

`spec_hash` is the SHA-256 (16 hex) of the canonical sympy `srepr(log_density)`. It identifies the math, not the implementation:
a re-spelling of the same expression that simplifies to itself (e.g. swapping `log(x*y)` for `log(x) + log(y)`) yields a different
hash, which is the desired behaviour — it forces an explicit reroll of the derived artifacts when the math changes.

