# Beta MLE formalization

TASK-317 · 2026-11-25

Lean checks all five targets in `UnivariateML.Beta`. The sample is a nonempty
finite indexed collection of observations strictly between zero and one.
`domain` is the positive shape quadrant, `logBeta` is the logarithm of the
Gamma-ratio normalizer, and `logLik` is the total log likelihood.

## Dossier qualification

**The Details claim that the ray score has one root needs a nonconstant-sample
qualification.** On the ray whose mean equals a constant sample, the log
likelihood diverges to positive infinity. A zero derivative of a differentiable
concave function would give a global maximum along that ray. The fitter
already detects constant data and returns `Degenerate` before the free fit.
The dossier's existence statement and both held-shape claims agree with the
formal results.

## Theorems

### 1. Strict convexity

```lean
theorem logBeta_strictConvex : StrictConvexOn ℝ domain logBeta
```

The log normalizer is strictly convex jointly in the two positive shapes. Mathlib supplies the complex beta integral and its Gamma-ratio identity. The proof transfers them to a real integral and applies strict convexity of the exponential to the two normalized kernels. Distinct shape pairs have distinct normalized log kernels at some interior point, as their derivatives in the observation determine both shapes.


### 2. Coercivity

```lean
theorem logLik_coercive (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) 1) (hne : ∃ i j, x i ≠ x j) :
    Tendsto (fun p : domain ↦ logLik x p) (cocompact domain) atBot
```

For nonconstant data the log likelihood tends to minus infinity on escape from every compact subset of the positive quadrant. Gamma’s strict Jensen lemma puts the two geometric means strictly inside the relevant boundary. A fixed subinterval then gives a lower bound on the normalizer that is uniform in both shapes. The Gamma recurrence bounds the normalizer near the axes inside a bounded box. Together the bounds confine each superlevel set to a compact box within the positive quadrant, including for mixed escapes.


### 3. Joint MLE

```lean
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) 1) (hne : ∃ i j, x i ≠ x j) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik x) domain p
```

A nonconstant sample has exactly one maximizing positive shape pair. The proof applies the generic coercive maximization theorem to the strictly concave total log likelihood.

```lean
theorem jointMLE_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) 1) :
    (∃! p, p ∈ domain ∧ IsMaxOn (logLik x) domain p) ↔ ∃ i j, x i ≠ x j
```

A nonempty interior sample has a unique joint MLE exactly when two observations differ. The implication from existence to nonconstancy uses the constant-sample result below.


### 4. Constant samples

```lean
theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : c ∈ Ioo (0 : ℝ) 1) :
    Tendsto (fun t ↦ logLik (fun _ : Fin n ↦ c) (t * c, t * (1 - c))) atTop atTop
```

The total log likelihood diverges to positive infinity along the fixed-mean ray through the constant observation. The integral of the kernel divided by its value at the observation tends to zero by dominated convergence. The integrand tends to zero off the observation and is dominated by the same ratio at precision one.

```lean
theorem constant_noMLE (hn : 0 < n) {c : ℝ} (hc : c ∈ Ioo (0 : ℝ) 1) :
    ¬ ∃ p ∈ domain, IsMaxOn (logLik (fun _ : Fin n ↦ c)) domain p
```

There is no maximizing positive shape pair for a constant interior sample.


### 5. Held shapes

```lean
theorem heldFirst (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) 1) {h : ℝ} (hh : 0 < h) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun z ↦ logLik x (h, z)) (Ioi 0) b
```

Holding the first shape positive leaves a unique maximizing positive second shape even for constant data. The held term can be shifted by an additive constant in the objective, allowing the joint compact-box bound to apply on the held line.

```lean
theorem heldSecond (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) 1) {h : ℝ} (hh : 0 < h) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun z ↦ logLik x (z, h)) (Ioi 0) a
```

Holding the second shape gives the symmetric result by swapping the shapes and complementing the observations.

## Scope and remaining work

All five requested targets are proved. The constant-sample theorem states
divergence along the dossier's ray. Its proof does not establish the
dossier's Stirling growth rate. The held-shape proofs use coercivity and
strict concavity. They do not give the digamma score limits or verify the
fitter's floating-point comparisons. The root-module import is reserved for
the coordinator.

## Validation

All checks ran on the workstation from `dev/lean/` through
`/tmp/umvl-cpu/run` with `nice -n 10`.

- `lake env lean UnivariateML/Family/Beta.lean` exited 0 with no diagnostics.
- `lake build UnivariateML +UnivariateML.Family.Beta` exited 0 and built
  Beta explicitly, completing 7,370 jobs.
- The final plain `lake build` exited 0, completing 7,294 jobs.
  The library builds emitted existing diagnostics in Frechet, FoldedNormal,
  Weibull and GPD. Beta compiled cleanly.
- The Beta module contains no `sorry`, `admit`, `axiom` or `native_decide`.

## Effort by target

The figures are approximate active minutes. Shared reading, elaboration
and build waits overlap the targets.

| Target | Active effort | Main work |
| --- | ---: | --- |
| 1. Strict convexity | 2–3 min | Beta integral transfer, strict integral comparison and separation of shape pairs |
| 2. Coercivity | 2–3 min | Jensen gap, uniform interval bound and control near the axes |
| 3. Joint MLE | Under 1 min | Generic maximization and the equivalence with nonconstancy |
| 4. Constant sample | 1–2 min | Kernel ratio, dominated convergence and exclusion of a maximizer |
| 5. Held shapes | About 1 min | Reduction to the compact-box bound and symmetry |
