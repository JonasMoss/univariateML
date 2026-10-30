# Fernandez-Steel skewed family in the harness (closed 2026-05-10)

**Status:** closed — all three (snorm, sstd, sged) validate at 1e-12.

## Resolution

After landing zip via a data-conditional `sp.Piecewise`, the lesson
was that parameter-conditional Piecewise is also tractable *if the
two branches agree at the boundary*. F-S satisfies this: at `z = 0`
both `xi^sign(z)` branches give 1, so the moving-boundary DiracDelta
contribution is zero in the limit and the harness's
`DiracDelta → 0` substitution gives correct second derivatives.

Three changes together (see
[2026-05-10-validation-harness.md](2026-05-10-validation-harness.md)
section A for the full story) unlocked the family:

1. Rewrite `xi^sign(z)` as `sp.Piecewise((branch_pos, z > 0),
   (branch_neg, True))`. F-S branches agree at `z = 0`, so this
   isn't the dangerous parameter-conditional case the original
   investigation feared.
2. Drop `sp.simplify` from `print_derivatives`. simplify hangs
   indefinitely on the lgamma+polygamma+Piecewise expressions
   sstd/sged produce; the pretty-print helper isn't load-bearing
   so just print the raw expression.
3. `MPMATH_DPS` 50 → 25 (5× faster per op, still 13 digits over
   the 1e-12 tolerance) + exploit Hessian symmetry (only lambdify +
   evaluate the upper triangle, mirror at the end).

Pilot results:

  - snorm: nll 4.1e-16, hess 1.4e-15
  - sstd:  nll 1.3e-15, hess 1.6e-15
  - sged:  nll 1.1e-15, hess 6.0e-16

`Spec.cost_has_gradient = False` for all three (BFGS uses
`finite_diff_cost` wrapper, no analytical gradient to validate at
1e-12; FD gradient is advisory in the report).

The text below is preserved as-is from the original deferral entry
for the historical context. None of the three "approaches to try"
were the right move — the actual fix was simpler than they
suggested.

---

## (original deferral notes follow)

## Which distros

`snorm`, `sstd`, `sged`. Also any future distribution built on the
F-S skew construction.

## Why they don't fit the v1 harness pattern

The F-S skew construction (`special/skewed_densities.hpp`) writes
the skew density as

```
log f_skew(z; xi) = log(2/(xi + 1/xi)) + log f_sym(z / xi^sign(z))
```

where `z` is itself a function of `(mu, sigma, xi)` via the
standardisation `z = sigma_u(...)·(x − mu)/sigma + mu_u(...)`, and
`f_sym` is the symmetric core (normal / Student-t / GED).

When sympy differentiates this expression once (in symbolic form),
it gets `Piecewise`s and `xi^sign(z)` factors. Twice — for the
Hessian — the chain rule explodes. Empirically (`snorm` attempt,
2026-05-10) `sp.diff(log_density, log_xi, log_xi)` with substitution
through `z` produced expressions large enough that the harness's
mpmath-50 evaluation didn't finish in 2 minutes.

Paralogis and GED, the other two phi-space pilots, both work fine
because they don't have the `sign(...)`-pivoted piecewise structure.

## What's already proven

The phi-space + FD-grad pattern in the harness works:

- `dev/validation/quality/math/paralogis.py` — log-log reparam, no skew. Validates
  at 1e-13.
- `dev/validation/quality/math/ged.py` — symmetric GED, abs-value-via-(a²)^(nu/2)
  workaround, harness substitutes `DiracDelta → 0` on lambdify.
  Validates at 1e-13.

The `cost_has_gradient=False` Spec flag and the `eval_fd_grad<Cost>`
template-evaluator in `univariateML-cpp/tests/test_validation.cpp`
both work as intended for these.

## Approaches to try when revisited

1. **Piecewise spec**: write the log-density as an explicit
   `sp.Piecewise((branch_pos, z > 0), (branch_neg, True))` with two
   branches that don't use `sign`. Each branch is a smooth function
   of all parameters; symbolic diff stays bounded.

   Risk: `sp.diff` on Piecewise still expands across branches, and
   our DiracDelta → 0 substitution doesn't handle the boundary
   between branches. Worth trying first; cheap to write.

2. **Two-phase symbolic derivation**: fix `xi^sign(z)` numerically by
   evaluating `sign(z)` per data point at each grid theta, then
   substitute the resulting `+1/-1` into a parameterised symbolic
   expression with no sign. Effectively "pre-bake" the sign at each
   evaluation.

3. **Bypass sympy for the F-S family**: write the analytical
   gradient and Hessian of the F-S skew density by hand (or once via
   sympy then save the simplified expressions), and wire them into
   the spec as opaque numerical functions. Less elegant; works.

4. **Compare numerically against finite-difference of the spec's
   nll**: if the symbolic Hessian is too expensive to evaluate, drop
   to FD on a high-precision (mpmath-50) `nll` evaluation. The C++
   side's analytical Hessian then must match an FD-of-truth Hessian
   at, say, 1e-6 relative — looser than 1e-12, but still meaningful.

Recommended order: try (1) first; fall back to (3) if (1) explodes.

## Trigger to revisit

- When the package ships a feature that would expose `Cost::hessian`
  output for the F-S family to users (e.g. asymptotic SEs for snorm
  fits).
- A user reports a numerical inconsistency in any of the three.
- A natural follow-on after dpqr-validation lands (the F-S family's
  d/p/q/r needs cross-check anyway).

## Verification when handled

- Each of `snorm`, `sstd`, `sged` has `dev/validation/quality/math/<id>.py`
  + `dev/validation/quality/derived/<id>/quality_report.md` claiming `validated`.
- If approach (4) is used, `validate.py` records the looser
  tolerance in `quality_report.md` rather than silently relaxing
  the 1e-12 default.
