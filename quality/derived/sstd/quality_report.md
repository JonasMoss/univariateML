---
id: sstd
spec_hash: 4a8822eef8260116
mpmath_dps: 25
claimed_level: validated
---

# sstd — quality report

## Symbolic vs C++ round-trip

- ✓ nll_at_theta: max_rel = 1.274e-15 (tol 1e-12, n=17)
- · grad_at_theta: max_rel = 5.457e-08 (SKIP — FD grad, advisory only, n=68)
- ✓ hess_at_theta: max_rel = 1.554e-15 (tol 1e-12, n=272)

## Provenance

`spec_hash` is the SHA-256 (16 hex) of the canonical sympy `srepr(log_density)`. It identifies the math, not the implementation:
a re-spelling of the same expression that simplifies to itself (e.g. swapping `log(x*y)` for `log(x) + log(y)`) yields a different
hash, which is the desired behaviour — it forces an explicit reroll of the derived artifacts when the math changes.

