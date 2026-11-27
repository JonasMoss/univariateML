# Discrete Weibull existence and uniqueness in Lean

TASK-379 · 2026-11-27 · Base `0bfa829c`

The Lean module proves all four targets for the exact-real type I discrete Weibull.
A nonempty sample has a unique joint maximum exactly when its range is at least
two. The held-parameter criteria, the positive-cell Hessian and the unattained
constant and adjacent suprema are also proved. [Lean module](../../lean/UnivariateML/Family/DiscreteWeibull.lean)

## Source discrepancy

**The November 25 math report describes an older coefficient-restoration
rule.** Its numerical example for `(8,10)` measures the change caused by a
shape refit at rounded probability. At this worktree’s base, the C++ `finish`
function preserves the joint shape, evaluates the likelihood at the rounded
public pair, and reports `Unrepresentable` if the restored probability hits
an endpoint. The replay therefore illustrates the sensitivity of a
conditional refit. It cannot establish a shape displacement for the current
return path. I found no mathematical gap in the dossier’s existence or
concavity statements. [Math report](2026-11-25-dweibull-math-report.md#numerical-evidence-and-coefficient-restoration) · [Current fitter](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/mle.hpp#L472) · [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/README.md#why-a-single-profile-bracket-is-global)

## Theorems

### Model

```lean
theorem mass_eq_rpow {q β : ℝ} (hq : 0 < q) (hq1 : q < 1) (hβ : 0 < β)
    (k : ℕ) :
    mass k (log (-log q), β) = q ^ ((k : ℝ) ^ β) - q ^ (((k + 1 : ℕ) : ℝ) ^ β)
```

The log cumulative hazard parametrization gives the stated probability mass at every count, including zero. The map from the public probability to the first coordinate is `log (-log q)`, with inverse `exp (-exp α)`.

### Held coefficients

```lean
theorem fixedShape_existsUnique_iff {x : Fin n → ℕ} (hn : 0 < n) {β : ℝ} (hβ : 0 < β) :
    (∃! p, p ∈ fixedShape β ∧ IsMaxOn (logLik x) (fixedShape β) p) ↔
      ∃ i, 0 < x i
```

A valid held shape gives a unique finite probability fit exactly when the sample contains a positive count. For an all-zero sample, increasing the first coordinate strictly improves every finite fit.

```lean
theorem fixedProbability_existsUnique_iff {x : Fin n → ℕ} (α : ℝ)
    (hx : ∃ i, 0 < x i) :
    (∃! p, p ∈ fixedProbability α ∧ IsMaxOn (logLik x) (fixedProbability α) p) ↔
      ∃ i, 2 ≤ x i
```

With probability held and some count positive, the shape fit is unique and finite exactly when some count is at least two. On zero and one data with a one present, increasing shape strictly improves every finite fit.

```lean
theorem fixedProbability_all_zero {x : Fin n → ℕ} (hx : ∀ i, x i = 0)
    (α : ℝ) {p : ℝ × ℝ} (hp : p ∈ fixedProbability α) :
    logLik x p = n * log (1 - exp (-exp α)) ∧
      IsMaxOn (logLik x) (fixedProbability α) p
```

At a held probability, every positive shape maximizes the all-zero log likelihood. The theorem gives their common value.

### Curvature

```lean
theorem endpoint_hessian_neg {u v du dv z : ℝ}
    (huv : v + z * dv < u + z * du) (hd : du ≠ 0 ∨ dv ≠ 0) :
    deriv (deriv (fun s ↦ endpointLogMass (u + s * du) (v + s * dv))) z < 0
```

At ordered endpoints the second derivative along every nonzero direction is negative. The proof differentiates the endpoint log mass twice, verifies the entries and determinant, and completes the square.

```lean
theorem strictConcave_log_mass {k : ℕ} (hk : 0 < k) :
    StrictConcaveOn ℝ domain (fun p ↦ log (mass k p))
```

For any positive count, the endpoint map carries nonzero directions in the model coordinates to nonzero directions in endpoint coordinates. The log mass is strictly concave on the entire parameter domain.

```lean
theorem strictConcave_logLik {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) :
    StrictConcaveOn ℝ domain (logLik x)
```

A single positive observation makes the sum strictly concave. Each zero-cell term is concave and depends only on the first coordinate.

### Joint attainment and excluded limits

```lean
theorem existsUnique_joint_of_gap {x : Fin n → ℕ} {i j : Fin n}
    (hgap : x i + 2 ≤ x j) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik x) domain p
```

Two occupied counts with a gap of at least two force a compact superlevel box. Continuity supplies the maximum and strict concavity gives uniqueness.

```lean
theorem constant_sup {x : Fin n → ℕ} (hn : 0 < n) {m : ℕ} (hx : ∀ i, x i = m) :
    sSup (logLik x '' domain) = 0 ∧ ¬ ∃ p ∈ domain, IsMaxOn (logLik x) domain p
```

A constant sample has log-likelihood supremum zero, approached on an explicit path to its point mass. Every finite pair gives a strictly smaller value.

```lean
theorem adjacent_sup {x : Fin n → ℕ} {m : ℕ}
    (hx : ∀ i, x i = m ∨ x i = m + 1)
    (hl : ∃ i, x i = m) (hr : ∃ i, x i = m + 1) :
    let w := upperFraction x m
    sSup (logLik x '' domain) = ∑ i, log (if x i = m + 1 then w else 1 - w) ∧
      ¬ ∃ p ∈ domain, IsMaxOn (logLik x) domain p
```

For two occupied adjacent cells, the limit law with empirical fractions gives the exact supremum. The proof uses the tangent bound for the logarithm to compare any finite fit with the limit, and an explicit path to approach the supremum.

```lean
theorem joint_existsUnique_iff {x : Fin n → ℕ} (hn : 0 < n) :
    (∃! p, p ∈ domain ∧ IsMaxOn (logLik x) domain p) ↔
      ∃ i j, x i + 2 ≤ x j
```

A nonempty sample has a unique finite joint fit exactly when two counts are separated by at least two. A sample with no such pair is constant or occupies two adjacent cells.

## Deviations and remaining scope

The criterion uses two indices in a vector of natural numbers. For a nonempty
sample this is equivalent to a maximum-minus-minimum of at least two. The
attainment argument bounds the magnitude of the derivative of endpoint
survival by `1`. The math report uses `1/e`. The weaker bound still keeps
shape uniformly positive on the superlevel set. The held cases use affine
fibers of the same box argument, and the constant-zero joint path sends both
shape and the first coordinate to infinity.

Every requested target is closed. The larger source claims about signed
profile-score brackets, geometric contact, classification of every proper
boundary limit, and floating-point certification are outside these statements.
The held nonattainment proofs establish strict improvement at every finite fit.
The exact limiting log-likelihood values are formalized for the two jointly
free escape cases.

## Effort by target

| Target | Proof work |
|---|---|
| 1. Held cases | Restricted the box and strict concavity to each affine fiber. Proved strict improvement in the excluded cases and computed the entire flat fiber’s value. |
| 2. Strict concavity | Computed second directional derivatives, checked the Hessian determinant and sign, proved the endpoint map preserves nonzero directions, and added zero-cell concavity via the decreasing derivative. |
| 3. Range two | Derived three cell-mass bounds, constructed a compact box, formalized both boundary paths, and proved the strict comparison with the empirical adjacent law. |
| 4. Source audit | Checked the existence predicates and Hessian against the dossier and traced probability restoration and the public result in the fitter. |

## Verification

The direct module check exited zero without diagnostics on the workstation.
The final `lake build` also exited zero with **7,462 jobs**. Its linter
warnings were in existing modules. The coordinator owns the root import, so
the direct check is the verification of the new module and the library build
checks the current import tree. All runs used the workstation slot wrapper.

```sh
cd dev/lean
/tmp/umvl-cpu/run nice -n 10 /home/jonas/.elan/bin/lake env lean UnivariateML/Family/DiscreteWeibull.lean
/tmp/umvl-cpu/run nice -n 10 /home/jonas/.elan/bin/lake build
```
