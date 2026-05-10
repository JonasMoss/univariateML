---
id: zipf
spec_hash: e3752728a0897f6d
mpmath_dps: 50
claimed_level: validated
---

# zipf — quality report

## Symbolic vs C++ round-trip

- ✓ nll_at_theta: max_rel = 2.094e-16 (tol 1e-12, n=17)
- ✓ grad_at_theta: max_rel = 4.084e-15 (tol 1e-12, n=17)
- ✓ hess_at_theta: max_rel = 6.710e-16 (tol 1e-12, n=17)

## Provenance

`spec_hash` is the SHA-256 (16 hex) of the canonical sympy `srepr(log_density)`. It identifies the math, not the implementation:
a re-spelling of the same expression that simplifies to itself (e.g. swapping `log(x*y)` for `log(x) + log(y)`) yields a different
hash, which is the desired behaviour — it forces an explicit reroll of the derived artifacts when the math changes.

