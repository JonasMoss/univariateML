# Kumaraswamy MLE formalization

TASK-323. All four targets are proved in exact real arithmetic. The module
uses a finite, nonempty sample `x : Fin n → ℝ`, with every observation
in the open unit interval. Joint existence and uniqueness require two
different observations. Either held-shape fit also covers constant data.

## Source findings

**The dossier's analytic strict-concavity proof is sound.** The Lean proof
uses the same two curvature bounds. It replaces the infinite exponential
series with finite Cauchy–Schwarz. For one summand
$g(a)=-\log(1-q)$ with $q=e^{-at}$, we have $(g')^2=qg''$, $g''>0$,
and $q\le g$. Therefore $(G')^2\le G''G$. The strict bound
$G''<n/a^2$ follows from $u<\sinh u$ for $u>0$.

**Two numerical details in the dossier are stale.** The certificate ledger
uses `certificate_budget(maxit)`, which gives 128 evaluations per unit of
the clamped iteration limit. The dossier prose gives 16. It also describes
selecting historical Newton coefficients or a midpoint, while the current
final refinement uses a midpoint and can select a secant point. The final
loop updates its endpoints by the unpadded score sign. The practical sign
allowance is applied during the earlier bracketing stage.
[Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/kumar/README.md),
[certificate implementation](../../../univariateML-cpp/include/cppunivariate/distributions/kumar/certificate.hpp),
[work-budget helper](../../../univariateML-cpp/include/cppunivariate/core/work_budget.hpp)

**The linked historical analysis reverses one weak limit.** As $b\to0$ at
held $a>0$, the CDF $1-(1-x^a)^b$ tends to zero for every $0<x<1$.
The limit is the point mass at 1. The historical analysis gives the point
mass at 0 for this case. This is separate from the likelihood claims.
[Historical family analysis](2026-10-29-p0-bounded.md#kumaraswamy)

## Theorems

The statements below are in the `UnivariateML.Kumaraswamy` namespace.
`G` is the sum of the negative log terms, `shape` is the profiled
second shape, and `domain` is the positive quadrant.

### `logLik_eq_sum`

```lean
theorem logLik_eq_sum (x : Fin n → ℝ) (hx : ∀ i, x i ∈ Ioo 0 1) (a b : ℝ) :
    logLik x a b = ∑ i, (log a + log b + (a - 1) * log (x i) +
      (b - 1) * log (1 - (x i) ^ a))
```

The transformed expression equals the total log-density for the original sample.

### `held_a`

```lean
theorem held_a (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    {a : ℝ} (ha : 0 < a) :
    0 < shape x a ∧ ∀ b > 0, b ≠ shape x a → logLik x a b < profile x a
```

At each positive held first shape, `shape x a = n / G x a` is positive and strictly improves on every other positive second shape. This is the dossier formula $-n/\sum_i\log(1-x_i^a)$.

### `profile_strictConcave`

```lean
theorem profile_strictConcave (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo 0 1) : StrictConcaveOn ℝ (Ioi 0) (profile x)
```

Profiling the second shape gives a strictly concave function on the positive first-shape axis, including for constant data.

### `profile_tendsto_zero`

```lean
theorem profile_tendsto_zero (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo 0 1) : Tendsto (profile x) (𝓝[>] 0) atBot
```

The profile tends to minus infinity as the first shape approaches zero from above for every nonempty interior sample.

### `profile_tendsto_atTop`

```lean
theorem profile_tendsto_atTop (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    (hne : ∃ i j, x i ≠ x j) : Tendsto (profile x) atTop atBot
```

The profile tends to minus infinity as the first shape grows without bound whenever two observations differ.

### `held_b`

```lean
theorem held_b (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    {b : ℝ} (hb : 0 < b) :
    ∃! a, a ∈ Ioi 0 ∧ IsMaxOn (fun a ↦ logLik x a b) (Ioi 0) a
```

Every positive held second shape has exactly one positive first-shape maximizer, including for constant samples.

### `existsUnique_joint`

```lean
theorem existsUnique_joint (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, x i ∈ Ioo 0 1)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! p, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p
```

A nonconstant interior sample has exactly one joint maximizer in the positive quadrant.

### `constant_tendsto`

```lean
theorem constant_tendsto (hn : 0 < n) {c : ℝ} (hc : c ∈ Ioo 0 1) :
    Tendsto (fun a ↦ logLik (fun _ : Fin n ↦ c) a (c ^ (-a))) atTop atTop
```

For data all equal to $c$, the total log-likelihood tends to plus infinity along the dossier path $b=c^{-a}$.

### `constant_unbounded`

```lean
theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : c ∈ Ioo 0 1) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ c) p.1 p.2) '' domain)
```

For constant interior data, the total log-likelihood has no upper bound over the positive quadrant.

## Deviations and scope

The two profile endpoint theorems state the limits needed for coercivity.
At zero the proof controls the residual after cancelling the logarithms
through the derivative of $1-e^{-at}$ at zero. At infinity it uses a linear
upper bound from the smallest transformed observation and the strict gap
between the total and $n$ times the minimum. The dossier's stronger
big-O expansions are outside the formal statements. For held $b$ the proof
uses the dossier's curvature bound, with endpoint limits of the objective
and compact attainment in place of the endpoint score argument.

No requested MLE result remains open. Weak convergence of the fitted
distributions and the floating-point certification path are outside these
theorems. The root import is pending coordinator integration.

## Effort by target

| Target | Proof work |
| --- | --- |
| 1. Held first shape | Short. Apply the strict logarithm inequality to the ratio of a candidate second shape to the analytic optimum. |
| 2. Strict profile concavity | Largest share. Derive both derivatives of each summand and formalize the hyperbolic sine bound, finite Cauchy–Schwarz and the profile curvature calculation. |
| 3. Joint existence and uniqueness | Substantial. Prove both endpoint limits, restrict to a compact interval, then lift the profile maximizer with the generic profiling lemmas. |
| 4. Constant data and held second shape | Substantial. Bound the constant-path penalty using the logarithm of a reciprocal. Reuse the curvature and limit estimates for the held-shape proof. |

## Verification

Both final checks ran on the workstation through the single-slot wrapper in
`dev/lean/`.

- `nice -n 10 /home/jonas/.elan/bin/lake env lean UnivariateML/Family/Kumaraswamy.lean`
  exited 0 with no diagnostics. The module contains no proof placeholders.
- `nice -n 10 /home/jonas/.elan/bin/lake build` exited 0 with 7,362 jobs.
  Existing modules emitted linter or tactic diagnostics. This run built the
  current library root. The direct check above covers Kumaraswamy pending
  the root import.
