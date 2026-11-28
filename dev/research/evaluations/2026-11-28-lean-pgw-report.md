# TASK-500 · PGW and fatigue-life likelihoods

The six requested atlas claims have Lean proofs. The PGW all-free criterion and uniqueness claim share a joined theorem that gives the complete set of maximizing triples. The fatigue-life results cover the subcritical, critical, and supercritical minimum counts and both constant-sample divergences. [Atlas fragment](../existence-atlas/lean-map.d/lean-pgw.csv)

| Target | Checked argument | Approximate active effort |
|---|---|---:|
| PGW, shape held, criterion | The optimized nuisance section extends analytically through inverse exponent zero. A strictly monotone right collar and the vanishing far tail leave a finite closed-window stationary list. A retained value equal to Gompertz suffices for attainment on nonconstants. | 9 min |
| PGW, all free, criterion | Both faces are optimized. The joint nuisance Hessian is negative definite and the analytic inverse-exponent profile has a strict collar at zero. Its large-inverse-exponent tail loses strictly to the optimized threshold. | 10 min |
| PGW, all free, failure | The joint superlevel bounds exhaust bounded- and large-shape sequences. At a moving threshold, tied minima contribute the exact contact factor. A maximizing sequence in a nonattaining sample has a subsequence at an optimized winning face. Constants concentrate. | 12 min |
| PGW, all free, uniqueness | Selection of the highest qualifying roots and a bijection from those roots to the physical maximizers preserve all finite ties. The constant branch has an empty maximizing set. | 5 min |
| Fatigue life, beta held, criterion | The conditional squared rational profile vanishes at infinity. A vanishing threshold trace ensures attainment. At positive finite trace, a finite equality with the trace is necessary and sufficient. | 7 min |
| Fatigue life, beta held, failure | The joint shape penalty and positive scaled residual yield all three threshold exponents, exhaustion of competitive sequences, and the two constant-data divergence mechanisms. | 6 min |

The effort estimates allocate shared proof work to its main use and exclude time waiting for the final aggregate build.

## Proof scope and source comparison

For PGW the all-free objective uses real log observations and log scale. Exponentiating these coordinates recovers all positive physical observations and scales, and the log Jacobian is a sample-only additive constant. At bounded shape, competitive points enter a compact box in shape, rate, and inverse exponent. The two nuisance coordinates remain positive when inverse exponent reaches zero. Large shape bounds the product of shape and exponent away from zero and infinity. At a minimum contact with multiplicity $m$, the limiting log likelihood includes $m\log\rho$, where $\rho$ is the limit of the logistic factor. Every $0<\rho\le 1$ is reachable. A maximizing threshold subsequence in a nonattaining sample must have the threshold converge to the minimum and $\rho$ converge to one. [Written PGW proof](2026-11-25-pgw-math-report.md#bounds-and-exhaustive-competitive-escapes)

Both PGW profiles use analytic inversion of the nuisance score. The removable logarithmic quotient in the joint chart is a divided difference. The held-shape score has negative derivative in its nuisance coordinate. In the all-free section the Hessian quadratic form is the sum of the negative log-shape curvature and the nonpositive contributions from the affine observation arguments. Together they give a strict inequality in every nonzero direction. The identity theorem and continuity of the score give the signed collar and finite retained list. The held-shape constant exclusion uses the Base9 comparison, and the all-free equality branch uses compactification with a finite witness. The fixed-exponent nuisance optimum reuses the existing coercivity result from the generic escape development. [Written stationary-list proof](2026-11-25-pgw-math-report.md#gompertz-decisions-and-the-finite-critical-list)

For fatigue life, $g_i=x_i-\min x$ and $d=\min x-\mu$, so the gap chart covers every real sample and feasible threshold. The objective rescales the squared total likelihood by the common positive factor $(8\pi e)^n$. The profile has edge exponent $n-3j$, with $j$ counting every tied minimum. At the critical count a positive-distance root of the profile-minus-trace equation is the source's equality condition. The denominator on the nonconstant branch is positive, so the equation is equivalent to a root after clearing denominators. The exact multiplier $[u e^{1-u}]^n$ distinguishes the envelope from the actual likelihood along an arbitrary joint shape and threshold path. This distinction matters particularly when the envelope diverges. [Written fatigue proof](2026-11-27-lsheld-atlas-math-report.md#fatigue-held-beta)

The statements retain the source likelihood scope. The PGW list gives exact comparisons and cardinality through a bijection. Effective critical-value equality tests, an explicit Taylor coefficient formula for the analytic collar, numerical root-isolation budgets, and the separate fatigue defective-law limit fall outside the six atlas targets.

## Verification

- Each of the five new modules passed `nice -n 10 /home/jonas/.elan/bin/lake env lean <file>` on the workstation through `/tmp/umvl-cpu/run`.
- The aggregate `lake build` with all five temporary imports passed on the workstation with **8,622 jobs**. Lake reused the Mathlib artifacts. Existing modules emitted lint diagnostics.
- After restoring the aggregate file, `git diff --exit-code dev/lean/UnivariateML.lean` passed.
- `#print axioms` on the five joined target declarations returned only `propext`, `Classical.choice`, and `Quot.sound`.
- `python3 dev/tools/existence_atlas.py --check` passed. The fragment has 43 theorem entries with coverage left empty for independent review.

## Lean declarations
### `UnivariateML.PGWHeldShape.held_shape_criterion`

PGW shape-held criterion. The finite retained scalar stationary list decides attainment by comparison with the fitted Gompertz face, including equality and the constant branch. [PGWHeldShape.lean](../../lean/UnivariateML/Family/PGWHeldShape.lean#L352)

```lean
theorem held_shape_criterion (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 < b) :
    ∃ (F : ℝ → ℝ) (ε H : ℝ) (R : Set ℝ),
      AnalyticOnNhd ℝ F (Ici 0) ∧ EqOn F (profile hn hx hb) (Ici 0) ∧
      0 < ε ∧ ε ≤ H ∧ (∀ h ∈ Ioo 0 ε, deriv F h ≠ 0) ∧
      (StrictMonoOn F (Ioo 0 ε) ∨ StrictAntiOn F (Ioo 0 ε)) ∧
      (∀ h > H, F h < F 0) ∧
      R = Icc ε H ∩ {h | deriv F h = 0} ∧ R.Finite ∧
      ((∃ p ∈ Weibull.domain, IsMaxOn
        (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x b q.1 q.2) Weibull.domain p) ↔
        (∃ i j, x i ≠ x j) ∧ ∃ h ∈ R, F 0 ≤ F h)
```

### `UnivariateML.PGWJointProfile.all_free_criterion_and_uniqueness`

PGW all-free criterion and uniqueness. Constants do not attain. On nonconstants the finite retained list decides attainment against the larger optimized face and enumerates all finite winners without duplication. [PGWJointProfile.lean](../../lean/UnivariateML/Family/PGWJointProfile.lean#L547)

```lean
theorem all_free_criterion_and_uniqueness (hn : 0 < n) (y : Fin n → ℝ) :
    ((∀ i j, y i = y j) → ¬ ∃ p ∈ physicalDomain, IsMaxOn (value y) physicalDomain p) ∧
    ((∃ i j, y i ≠ y j) →
      ∃ (i j : Fin n) (hij : y i < y j), (∀ l, y i ≤ y l) ∧ (∀ l, y l ≤ y j) ∧
      ∃ (F : ℝ → ℝ) (k ε H : ℝ) (R : Set ℝ),
        AnalyticOnNhd ℝ F (Ici 0) ∧ EqOn F (profile hij) (Ici 0) ∧
        (∃ g ∈ Weibull.domain,
          IsMaxOn (fun p : ℝ × ℝ ↦ gompertzProduct y p.1 p.2) Weibull.domain g ∧
            F 0 = gompertzProduct y g.1 g.2) ∧
        0 < k ∧ IsMaxOn (thresholdProduct y (y i)) (Ioi 0) k ∧
        0 < ε ∧ ε ≤ H ∧ (∀ h ∈ Ioo 0 ε, deriv F h ≠ 0) ∧
        (StrictMonoOn F (Ioo 0 ε) ∨ StrictAntiOn F (Ioo 0 ε)) ∧
        (∀ h > H, F h < thresholdProduct y (y i) k) ∧
        R = Icc ε H ∩ {h | deriv F h = 0} ∧ R.Finite ∧
        let C := max (F 0) (thresholdProduct y (y i) k)
        let W := {h ∈ R | C ≤ F h ∧ ∀ t ∈ R, F t ≤ F h}
        ((∃ p ∈ physicalDomain, IsMaxOn (value y) physicalDomain p) ↔
          ∃ h ∈ R, C ≤ F h) ∧
        BijOn (fit hij) W {p ∈ physicalDomain | IsMaxOn (value y) physicalDomain p})
```

### `UnivariateML.PGWEnvelope.all_free_escape_classification`

PGW all-free failure. Constants concentrate, and a nonattaining nonconstant sample has only optimized Gompertz or unpenalized threshold subsequential limits along maximizing sequences. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L1339)

```lean
theorem all_free_escape_classification (hn : 0 < n) (y : Fin n → ℝ) :
    let L := fun p : ℝ × ℝ × ℝ ↦ logProduct y p.1 p.2.1 p.2.2
    let V := L '' physicalDomain
    ((∀ i j, y i = y j) →
      Tendsto (fun b ↦ logProduct y b 1 (y ⟨0, hn⟩)) atTop atTop ∧ ¬ BddAbove V) ∧
    ((∃ i j, y i ≠ y j) →
      ∃ i : Fin n, (∀ j, y i ≤ y j) ∧
      ∃ g ∈ Weibull.domain, ∃ k > 0,
        IsMaxOn (fun q : ℝ × ℝ ↦ gompertzProduct y q.1 q.2) Weibull.domain g ∧
        IsMaxOn (thresholdProduct y (y i)) (Ioi 0) k ∧
        let G := gompertzProduct y g.1 g.2
        let T := thresholdProduct y (y i) k
        BddAbove V ∧
        ((¬ ∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) ↔
          (∀ p ∈ physicalDomain, L p < max G T) ∧ sSup V = max G T) ∧
        ((¬ ∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) →
          ∀ {b c μ : ℕ → ℝ}, (∀ᶠ t in atTop, 0 < b t) → (∀ᶠ t in atTop, 0 < c t) →
          Tendsto (fun t ↦ logProduct y (b t) (c t) (μ t)) atTop (𝓝 (sSup V)) →
          (∃ p ∈ Weibull.domain, ∃ φ : ℕ → ℕ, StrictMono φ ∧
            Tendsto (fun t ↦ (b (φ t), c (φ t) * exp (-b (φ t) * μ (φ t)), (c (φ t))⁻¹))
              atTop (𝓝 (p.1, p.2, 0)) ∧ gompertzProduct y p.1 p.2 = sSup V) ∨
          (∃ v > 0, ∃ φ : ℕ → ℕ, StrictMono φ ∧ Tendsto (b ∘ φ) atTop atTop ∧
            Tendsto (μ ∘ φ) atTop (𝓝 (y i)) ∧
            Tendsto (fun t ↦ b (φ t) * c (φ t)) atTop (𝓝 v) ∧
            Tendsto (fun t ↦ logistic (b (φ t) * (y i - μ (φ t)))) atTop (𝓝 1) ∧
            thresholdProduct y (y i) v = sSup V)))
```

### `UnivariateML.PGWFatigue.held_beta_criterion`

Fatigue beta-held criterion. Nonconstant data attain below critical multiplicity, or at critical multiplicity when the rational profile meets the positive trace at a finite threshold. [PGWFatigue.lean](../../lean/UnivariateML/Family/PGWFatigue.lean#L480)

```lean
theorem held_beta_criterion (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b) :
    (∃ p ∈ Fatigue.domain, IsMaxOn (fun p : ℝ × ℝ ↦
      Fatigue.logLik (shifted g p.1) p.2 b) Fatigue.domain p) ↔
      (∃ i j, g i ≠ g j) ∧
        (3 * multiplicity g < n ∨
          (3 * multiplicity g = n ∧ ∃ d > 0, rationalProfile g b d = edgeFactor g b 0))
```

### `UnivariateML.PGWFatigue.held_beta_failure`

Fatigue beta-held failure. The joined failure result gives the threshold exponent, both regular shape ends, the zero infinite-threshold tail and both constant-data divergences. [PGWFatigue.lean](../../lean/UnivariateML/Family/PGWFatigue.lean#L764)

```lean
theorem held_beta_failure (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b) :
    (∀ d > 0, rationalProfile g b d =
      edgeFactor g b d * (d ^ n / d ^ (3 * multiplicity g))) ∧
    Tendsto (rationalProfile g b) atTop (𝓝 0) ∧
    Tendsto (fun d ↦ Fatigue.shape (shifted g d) b) (𝓝[>] 0) atTop ∧
    (3 * multiplicity g < n → Tendsto (rationalProfile g b) (𝓝[>] 0) (𝓝 0)) ∧
    (3 * multiplicity g = n → 0 < edgeFactor g b 0 ∧
      Tendsto (rationalProfile g b) (𝓝[>] 0) (𝓝 (edgeFactor g b 0))) ∧
    (n < 3 * multiplicity g → Tendsto (rationalProfile g b) (𝓝[>] 0) atTop) ∧
    ((∃ i j, g i ≠ g j) →
      (∀ d > 0, (∀ a > 0, squaredLik g b (d, a) ≤ rationalProfile g b d) ∧
        ∃ a > 0, squaredLik g b (d, a) = rationalProfile g b d) ∧
      (∀ {ι : Type} {l : Filter ι} {d a : ι → ℝ} {r : ℝ}, 0 < r →
        Tendsto d l (𝓝 r) → (∀ᶠ t in l, 0 < a t) →
        (Tendsto a l (𝓝 0) ∨ Tendsto a l atTop) →
        Tendsto (fun t ↦ squaredLik g b (d t, a t)) l (𝓝 0)) ∧
      ∀ {d a : ℕ → ℝ} {M : ℝ}, 0 < M →
        (∀ᶠ t in atTop, 0 < d t) → (∀ᶠ t in atTop, 0 < a t) →
        (∀ᶠ t in atTop, M ≤ squaredLik g b (d t, a t)) →
        ∃ φ : ℕ → ℕ, StrictMono φ ∧
          (Tendsto (d ∘ φ) atTop (𝓝 0) ∨
            ∃ d₀ > 0, ∃ a₀ > 0, Tendsto (d ∘ φ) atTop (𝓝 d₀) ∧
              Tendsto (a ∘ φ) atTop (𝓝 a₀))) ∧
    ((∀ i, g i = 0) →
      Tendsto (fun a ↦ Fatigue.logLik (shifted g b) a b) (𝓝[>] 0) atTop ∧
      Tendsto (fun d ↦ squaredLik g b (d, Fatigue.shape (shifted g d) b))
        (𝓝[>] 0) atTop ∧
      ∀ {d a : ℕ → ℝ}, (∀ᶠ t in atTop, 0 < d t) → (∀ᶠ t in atTop, 0 < a t) →
        Tendsto (fun t ↦ squaredLik g b (d t, a t)) atTop atTop →
        ∃ φ : ℕ → ℕ, StrictMono φ ∧
          (Tendsto (d ∘ φ) atTop (𝓝 0) ∨
            (Tendsto (d ∘ φ) atTop (𝓝 b) ∧ Tendsto (a ∘ φ) atTop (𝓝 0))))
```

### `UnivariateML.PGWHeldShape.joint_section_forms`

Shared support for the PGW shape-held and all-free criteria. Each fixed inverse-exponent section separates into a log-shape term and affine arguments of a base with negative second derivative. [PGWHeldShape.lean](../../lean/UnivariateML/Family/PGWHeldShape.lean#L109)

```lean
theorem joint_section_forms (y : Fin n → ℝ) {h : ℝ} (hh : 0 ≤ h) :
    ∃ (K d : ℝ) (f : ℝ → ℝ),
      (∀ v, deriv (deriv f) v < 0) ∧ (∀ v, DifferentiableAt ℝ f v) ∧
      ∀ {b : ℝ}, 0 < b → ∀ v,
        chartObjective y b (h, v) =
          (n : ℝ) * log b + K + ∑ i, f (v + b * y i + d)
```

### `UnivariateML.PGWHeldShape.section_has_max`

Shape-held criterion support. At each positive held shape the nuisance optimum exists on all inverse-exponent slices, including zero. [PGWHeldShape.lean](../../lean/UnivariateML/Family/PGWHeldShape.lean#L202)

```lean
theorem section_has_max (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b h : ℝ} (hb : 0 < b) (hh : 0 ≤ h) :
    ∃ v, IsMaxOn (fun w ↦ chartObjective (fun i ↦ log (x i)) b (h, w)) univ v
```

### `UnivariateML.PGWHeldShape.analytic_profile_extension`

Shape-held criterion support. The optimized held-shape profile analytically extends around every nonnegative inverse exponent. [PGWHeldShape.lean](../../lean/UnivariateML/Family/PGWHeldShape.lean#L296)

```lean
theorem analytic_profile_extension (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b h : ℝ} (hb : 0 < b) (hh : 0 ≤ h) :
    ∃ F : ℝ → ℝ, AnalyticAt ℝ F h ∧
      F =ᶠ[𝓝[Ici 0] h] profile hn hx hb
```

### `UnivariateML.PGWEnvelope.physical_eq`

Shared support for the PGW shape-held and all-free criteria. The log-observation objective differs from the physical PGW log likelihood by the log Jacobian. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L79)

```lean
theorem physical_eq {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b c a : ℝ} (hb : 0 < b) (hc : 0 < c) (ha : 0 < a) :
    logProduct (fun i ↦ log (x i)) b c (log a) =
      ShapeFacesPGW.logLik x b a c + ∑ i, log (x i)
```

### `UnivariateML.PGWEnvelope.logProduct_le`

Shared support for the PGW shape-held and all-free criteria. The log likelihood is bounded above by sample size times either the log shape or the log product rate. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L109)

```lean
theorem logProduct_le {y : Fin n → ℝ} {b c μ : ℝ}
    (hb : 0 < b) (hc : 0 < c) :
    logProduct y b c μ ≤ (n : ℝ) * log b ∧
      logProduct y b c μ ≤ (n : ℝ) * log (b * c)
```

### `UnivariateML.PGWEnvelope.bounded_shape_envelope`

Shared support for the PGW shape-held and all-free criteria. At bounded shape, any competitive likelihood point has a compact positive shape and rate chart with a closed inverse-exponent end. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L124)

```lean
theorem bounded_shape_envelope (hn : 0 < n) {y : Fin n → ℝ} (j : Fin n)
    {b c μ B M : ℝ} (hb : 0 < b) (hc : 0 < c) (hB : b ≤ B)
    (hM : M ≤ logProduct y b c μ) :
    let m := exp (M / n)
    let C := (n : ℝ) * log B - M
    b ∈ Icc m B ∧ m ≤ b * c ∧ c⁻¹ ∈ Icc 0 (B / m) ∧
      c * exp (b * (y j - μ)) ∈ Icc (exp (-C)) (exp C * exp (exp (2 * C)))
```

### `UnivariateML.PGWEnvelope.large_shape_envelope`

All-free criterion and failure support. At large shape, competitive points have a compact product rate and a minimum logistic factor bounded away from zero. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L253)

```lean
theorem large_shape_envelope {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j)
    (M : ℝ) :
    ∃ B K U : ℝ, 0 < B ∧ 0 < K ∧ 0 < U ∧
      ∀ {b c μ : ℝ}, B < b → 0 < c → M ≤ logProduct y b c μ →
        exp (M / n) ≤ b * c ∧ b * c ≤ K ∧
        M - (n : ℝ) * log K ≤ log (logistic (b * (y i - μ))) ∧
        c * softplus (b * (y j - μ)) ≤ U
```

### `UnivariateML.PGWEnvelope.strict_threshold_tail`

All-free criterion and failure support. Every competitive point with sufficiently large shape loses strictly to the closed threshold at its own product rate. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L381)

```lean
theorem strict_threshold_tail {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l) (hmax : ∀ l, y l ≤ y j)
    (M : ℝ) :
    ∃ B > 0, ∀ {b c μ : ℝ}, B < b → 0 < c → M ≤ logProduct y b c μ →
      logProduct y b c μ < thresholdProduct y (y i) (b * c)
```

### `UnivariateML.PGWEnvelope.threshold_contact`

All-free criterion and failure support. An arbitrary large-shape threshold approach has a limiting penalty equal to the contact factor to the power of the minimum multiplicity. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L503)

```lean
theorem threshold_contact {ι : Type*} {l : Filter ι}
    {y : Fin n → ℝ} {i : Fin n} (hmin : ∀ j, y i ≤ y j)
    {b c μ : ι → ℝ} {s k ρ : ℝ} (hs : s ≤ y i) (hk : 0 < k) (hρ : 0 < ρ)
    (hb : Tendsto b l atTop) (hbc : Tendsto (fun t ↦ b t * c t) l (𝓝 k))
    (hμ : Tendsto μ l (𝓝 s))
    (hcontact : s = y i → Tendsto (fun t ↦ logistic (b t * (y i - μ t))) l (𝓝 ρ)) :
    Tendsto (fun t ↦ logProduct y (b t) (c t) (μ t)) l
      (𝓝 (thresholdProduct y s k +
        if s = y i then ((Finset.univ.filter fun j ↦ y j = y i).card : ℝ) * log ρ else 0))
```

### `UnivariateML.PGWEnvelope.threshold_contact_reachable`

All-free criterion and failure support. Every positive logistic contact factor up to and including one is realized by a moving free-scale path. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L578)

```lean
theorem threshold_contact_reachable {y : Fin n → ℝ} {i : Fin n}
    (hmin : ∀ j, y i ≤ y j) {k ρ : ℝ} (hk : 0 < k) (hρ : ρ ∈ Ioc 0 1) :
    ∃ b c μ : ℝ → ℝ,
      (∀ᶠ t in atTop, 0 < b t) ∧ (∀ᶠ t in atTop, 0 < c t) ∧
      Tendsto b atTop atTop ∧ Tendsto μ atTop (𝓝 (y i)) ∧
      Tendsto (fun t ↦ b t * c t) atTop (𝓝 k) ∧
      Tendsto (fun t ↦ logProduct y (b t) (c t) (μ t)) atTop
        (𝓝 (thresholdProduct y (y i) k +
          ((Finset.univ.filter fun j ↦ y j = y i).card : ℝ) * log ρ))
```

### `UnivariateML.PGWEnvelope.analytic_chart`

Shared support for the PGW shape-held and all-free criteria. The joint likelihood in the inverse-exponent chart is analytic through zero at each positive nuisance point. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L668)

```lean
theorem analytic_chart (y : Fin n → ℝ) {p : ℝ × ℝ × ℝ} (hp : p ∈ chartDomain) :
    AnalyticAt ℝ (chart y) p
```

### `UnivariateML.PGWEnvelope.chart_physical`

Shared support for the PGW shape-held and all-free criteria. The positive physical model maps into the compactified chart with identical likelihood. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L696)

```lean
theorem chart_physical (y : Fin n → ℝ) {b c μ : ℝ} (hb : 0 < b) (hc : 0 < c) :
    chart y (b, c * exp (-b * μ), c⁻¹) = logProduct y b c μ
```

### `UnivariateML.PGWEnvelope.chart_surjective`

Shared support for the PGW shape-held and all-free criteria. Each positive inverse-exponent chart point is realized at finite physical coefficients. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L713)

```lean
theorem chart_surjective (y : Fin n → ℝ) {b r h : ℝ}
    (hb : 0 < b) (hr : 0 < r) (hh : 0 < h) :
    chart y (b, r, h) = logProduct y b h⁻¹ (-(log (h * r)) / b)
```

### `UnivariateML.PGWEnvelope.faces_le_of_upper_bound`

All-free criterion and failure support. Every upper bound on the finite likelihood also bounds both reachable faces. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L773)

```lean
theorem faces_le_of_upper_bound {y : Fin n → ℝ} {i : Fin n}
    (hmin : ∀ l, y i ≤ y l) {M : ℝ}
    (hbound : ∀ p ∈ physicalDomain, logProduct y p.1 p.2.1 p.2.2 ≤ M) :
    (∀ b > 0, ∀ r > 0, gompertzProduct y b r ≤ M) ∧
      (∀ k > 0, thresholdProduct y (y i) k ≤ M)
```

### `UnivariateML.PGWEnvelope.all_free_attains_iff_witness`

All-free criterion and failure support. A nonconstant sample attains exactly when a finite point ties or beats every point on both faces. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L810)

```lean
theorem all_free_attains_iff_witness {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l) (hmax : ∀ l, y l ≤ y j) :
    let D : Set (ℝ × ℝ × ℝ) := Ioi 0 ×ˢ (Ioi 0 ×ˢ univ)
    let L := fun p : ℝ × ℝ × ℝ ↦ logProduct y p.1 p.2.1 p.2.2
    (∃ p ∈ D, IsMaxOn L D p) ↔
      ∃ p ∈ D, (∀ b > 0, ∀ r > 0, gompertzProduct y b r ≤ L p) ∧
        (∀ k > 0, thresholdProduct y (y i) k ≤ L p)
```

### `UnivariateML.PGWEnvelope.large_shape_subsequence`

All-free criterion and failure support. A competitive large-shape sequence has a convergent threshold, product rate and positive minimum contact factor along a subsequence. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L872)

```lean
theorem large_shape_subsequence {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l) (M : ℝ)
    {b c μ : ℕ → ℝ} (hb : Tendsto b atTop atTop)
    (hc : ∀ᶠ t in atTop, 0 < c t)
    (hM : ∀ᶠ t in atTop, M ≤ logProduct y (b t) (c t) (μ t)) :
    ∃ (s k ρ : ℝ) (φ : ℕ → ℕ), StrictMono φ ∧ s ≤ y i ∧ 0 < k ∧ ρ ∈ Ioc 0 1 ∧
      Tendsto (μ ∘ φ) atTop (𝓝 s) ∧
      Tendsto (fun t ↦ b (φ t) * c (φ t)) atTop (𝓝 k) ∧
      Tendsto (fun t ↦ logistic (b (φ t) * (y i - μ (φ t)))) atTop (𝓝 ρ) ∧
      Tendsto (fun t ↦ logProduct y (b (φ t)) (c (φ t)) (μ (φ t))) atTop
        (𝓝 (thresholdProduct y s k +
          if s = y i then ((Finset.univ.filter fun l ↦ y l = y i).card : ℝ) * log ρ else 0))
```

### `UnivariateML.PGWEnvelope.bounded_shape_subsequence`

All-free criterion and failure support. Competitive points with a frequent bounded-shape subsequence have a limit in the closed inverse-exponent chart. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L950)

```lean
theorem bounded_shape_subsequence {y : Fin n → ℝ} (j : Fin n)
    {b c μ : ℕ → ℝ} {B M : ℝ}
    (h : ∃ᶠ t in atTop, 0 < b t ∧ 0 < c t ∧ b t ≤ B ∧
      M ≤ logProduct y (b t) (c t) (μ t)) :
    ∃ p ∈ chartDomain, ∃ φ : ℕ → ℕ, StrictMono φ ∧
      Tendsto (fun t ↦ (b (φ t), c (φ t) * exp (-b (φ t) * μ (φ t)), (c (φ t))⁻¹))
        atTop (𝓝 p)
```

### `UnivariateML.PGWEnvelope.competitive_subsequence`

All-free criterion and failure support. Every competitive sequence has a subsequence in the closed Gompertz chart or at an exact threshold contact trace. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L989)

```lean
theorem competitive_subsequence {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l) (M : ℝ)
    {b c μ : ℕ → ℝ} (hb : ∀ᶠ t in atTop, 0 < b t) (hc : ∀ᶠ t in atTop, 0 < c t)
    (hM : ∀ᶠ t in atTop, M ≤ logProduct y (b t) (c t) (μ t)) :
    (∃ p ∈ chartDomain, ∃ φ : ℕ → ℕ, StrictMono φ ∧
        Tendsto (fun t ↦
          (b (φ t), c (φ t) * exp (-b (φ t) * μ (φ t)), (c (φ t))⁻¹))
          atTop (𝓝 p) ∧
        Tendsto (fun t ↦ logProduct y (b (φ t)) (c (φ t)) (μ (φ t))) atTop
          (𝓝 (chart y p))) ∨
      (∃ (s k ρ : ℝ) (φ : ℕ → ℕ), StrictMono φ ∧ s ≤ y i ∧ 0 < k ∧ ρ ∈ Ioc 0 1 ∧
        Tendsto (b ∘ φ) atTop atTop ∧
        Tendsto (μ ∘ φ) atTop (𝓝 s) ∧
        Tendsto (fun t ↦ b (φ t) * c (φ t)) atTop (𝓝 k) ∧
        Tendsto (fun t ↦ logistic (b (φ t) * (y i - μ (φ t)))) atTop (𝓝 ρ) ∧
        Tendsto (fun t ↦ logProduct y (b (φ t)) (c (φ t)) (μ (φ t))) atTop
          (𝓝 (thresholdProduct y s k +
            if s = y i then ((Finset.univ.filter fun l ↦ y l = y i).card : ℝ) * log ρ
            else 0)))
```

### `UnivariateML.PGWEnvelope.all_free_failure`

All-free criterion and failure support. Constants concentrate without bound. On nonconstants the finite supremum is unattained exactly when all finite points lose to the larger face supremum. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L1046)

```lean
theorem all_free_failure (hn : 0 < n) (y : Fin n → ℝ) :
    let L := fun p : ℝ × ℝ × ℝ ↦ logProduct y p.1 p.2.1 p.2.2
    ((∀ i j, y i = y j) →
      Tendsto (fun b ↦ logProduct y b 1 (y ⟨0, hn⟩)) atTop atTop ∧
      ¬ BddAbove (L '' physicalDomain)) ∧
    ((∃ i j, y i ≠ y j) →
      ∃ i : Fin n, (∀ j, y i ≤ y j) ∧
        let F := (fun p : ℝ × ℝ ↦ gompertzProduct y p.1 p.2) '' Weibull.domain ∪
          thresholdProduct y (y i) '' Ioi 0
        BddAbove (L '' physicalDomain) ∧ BddAbove F ∧
        ((¬ ∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) ↔
          (∀ p ∈ physicalDomain, L p < sSup F) ∧ sSup (L '' physicalDomain) = sSup F))
```

### `UnivariateML.PGWEnvelope.gompertz_face_attains`

All-free criterion and failure support. The transformed Gompertz face has a finite maximizer on every nonconstant sample. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L1121)

```lean
theorem gompertz_face_attains {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j) :
    ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ gompertzProduct y q.1 q.2) Weibull.domain p
```

### `UnivariateML.PGWEnvelope.threshold_face_attains`

All-free criterion and failure support. The threshold envelope has an optimizing positive rate and its best threshold is the sample minimum. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L1168)

```lean
theorem threshold_face_attains {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l) :
    ∃ k ∈ Ioi 0, IsMaxOn (thresholdProduct y (y i)) (Ioi 0) k ∧
      ∀ s ≤ y i, ∀ v > 0, thresholdProduct y s v ≤ thresholdProduct y (y i) k
```

### `UnivariateML.PGWEnvelope.gompertz_nuisance_compact`

All-free criterion and failure support. A compact positive nuisance box contains all competitive points sufficiently close to inverse exponent zero. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L1200)

```lean
theorem gompertz_nuisance_compact {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j)
    (M : ℝ) :
    ∃ ε > 0, ∃ S : Set (ℝ × ℝ), IsCompact S ∧ S ⊆ Weibull.domain ∧
      ∀ {b c μ : ℝ}, 0 < b → 0 < c → c⁻¹ < ε → M ≤ logProduct y b c μ →
        (b, c * exp (-b * μ)) ∈ S
```

### `UnivariateML.PGWEnvelope.strict_inverse_exponent_tail`

All-free criterion and failure support. The full finite profile lies strictly below the optimized threshold at sufficiently large inverse exponent. [PGWEnvelope.lean](../../lean/UnivariateML/Family/PGWEnvelope.lean#L1226)

```lean
theorem strict_inverse_exponent_tail {y : Fin n → ℝ} {i j : Fin n}
    (hij : y i < y j) (hmin : ∀ l, y i ≤ y l) (hmax : ∀ l, y l ≤ y j)
    {k : ℝ}
    (hm : IsMaxOn (thresholdProduct y (y i)) (Ioi 0) k) :
    ∃ H > 0, ∀ {b h μ : ℝ}, 0 < b → H < h →
      logProduct y b h⁻¹ μ < thresholdProduct y (y i) k
```

### `UnivariateML.PGWJointProfile.nuisance_hessian_invertible`

All-free criterion and uniqueness support. At every nonnegative inverse exponent the joint nuisance score has an invertible derivative in the nuisance coordinates. [PGWJointProfile.lean](../../lean/UnivariateML/Family/PGWJointProfile.lean#L181)

```lean
theorem nuisance_hessian_invertible (hn : 0 < n) (y : Fin n → ℝ) {h : ℝ}
    {p : ℝ × ℝ} (hp : p ∈ nuisanceDomain) (hh : 0 ≤ h) :
    (fderiv ℝ (score y) (h, p) ∘L ContinuousLinearMap.inr ℝ ℝ (ℝ × ℝ)).IsInvertible
```

### `UnivariateML.PGWJointProfile.section_attains`

All-free criterion and uniqueness support. Every nonconstant all-free sample has an optimizer on each inverse-exponent section, including the Gompertz section. [PGWJointProfile.lean](../../lean/UnivariateML/Family/PGWJointProfile.lean#L254)

```lean
theorem section_attains {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j)
    {h : ℝ} (hh : 0 ≤ h) :
    ∃ p ∈ nuisanceDomain, IsMaxOn (fun q ↦ objective y (h, q)) nuisanceDomain p
```

### `UnivariateML.PGWJointProfile.analytic_profile_extension`

All-free criterion and uniqueness support. The optimized all-free profile analytically extends around every nonnegative inverse exponent. [PGWJointProfile.lean](../../lean/UnivariateML/Family/PGWJointProfile.lean#L307)

```lean
theorem analytic_profile_extension {y : Fin n → ℝ} {i j : Fin n} (hij : y i < y j)
    {h : ℝ} (hh : 0 ≤ h) :
    ∃ F : ℝ → ℝ, AnalyticAt ℝ F h ∧ F =ᶠ[𝓝[Ici 0] h] profile hij
```

### `UnivariateML.PGWProfile.maximizer_bijection`

All-free uniqueness support in physical nuisance coordinates. In physical nuisance coordinates, global maximizers of the free fits correspond bijectively to maximizing stationary inverse exponents. [PGWProfile.lean](../../lean/UnivariateML/Family/PGWProfile.lean#L57)

```lean
theorem maximizer_bijection (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    let W := {h ∈ Ioi 0 | IsMaxOn (profile hn hx hne) (Ioi 0) h}
    let P := {p ∈ finiteDomain | IsMaxOn (finiteObjective x) finiteDomain p}
    BijOn (fun h ↦ (bestNuisance hn hx hne h, h)) W P ∧
      ∀ h ∈ W, deriv (profile hn hx hne) h = 0
```

### `UnivariateML.PGWProfile.analytic_nuisance_branch`

Shared support for the PGW shape-held and all-free criteria. A locally analytic score with invertible nuisance derivative has a locally unique analytic implicit solution. [PGWProfile.lean](../../lean/UnivariateML/Family/PGWProfile.lean#L90)

```lean
theorem analytic_nuisance_branch {E : Type*} [NormedAddCommGroup E]
    [NormedSpace ℝ E] [CompleteSpace E] {S : ℝ × E → E} {u : ℝ × E}
    (ha : AnalyticAt ℝ S u)
    (hi : (fderiv ℝ S u ∘L ContinuousLinearMap.inr ℝ ℝ E).IsInvertible) :
    ∃ ψ : ℝ → E, AnalyticAt ℝ ψ u.1 ∧ ψ u.1 = u.2 ∧
      (∀ᶠ h in 𝓝 u.1, S (h, ψ h) = S u) ∧
      (∀ᶠ v in 𝓝 u, S v = S u ↔ ψ v.1 = v.2)
```

### `UnivariateML.PGWProfile.analytic_collar_and_finite`

Shared support for the PGW shape-held and all-free criteria. A nontrivial analytic score has a stationary-free right collar and a finite zero set in each retained compact window. [PGWProfile.lean](../../lean/UnivariateML/Family/PGWProfile.lean#L106)

```lean
theorem analytic_collar_and_finite {S K : Set ℝ} {score : ℝ → ℝ}
    (ha : AnalyticOnNhd ℝ score S) (hs : IsPreconnected S) (h₀ : (0 : ℝ) ∈ S)
    (hne : ∃ t ∈ S, score t ≠ 0) (hK : IsCompact K) (hKS : K ⊆ S) :
    (∃ ε > 0, ∀ t ∈ Ioo 0 ε, score t ≠ 0) ∧ (K ∩ {t | score t = 0}).Finite
```

### `UnivariateML.PGWProfile.signed_collar`

Shared support for the PGW shape-held and all-free criteria. Continuity and nonvanishing of the analytic derivative give strict monotonicity across the collar. [PGWProfile.lean](../../lean/UnivariateML/Family/PGWProfile.lean#L131)

```lean
theorem signed_collar {F : ℝ → ℝ} {ε : ℝ} (hε : 0 < ε)
    (ha : AnalyticOnNhd ℝ F (Ici 0))
    (hn : ∀ h ∈ Ioo 0 ε, deriv F h ≠ 0) :
    StrictMonoOn F (Ioo 0 ε) ∨ StrictAntiOn F (Ioo 0 ε)
```

### `UnivariateML.PGWProfile.extend_nonnegative`

Shared support for the PGW shape-held and all-free criteria. Local analytic extensions give a common analytic representative of the profile on the nonnegative inverse-exponent half-line. [PGWProfile.lean](../../lean/UnivariateML/Family/PGWProfile.lean#L161)

```lean
theorem extend_nonnegative {f : ℝ → ℝ}
    (hlocal : ∀ h ≥ 0, ∃ F : ℝ → ℝ, AnalyticAt ℝ F h ∧ F =ᶠ[𝓝[Ici 0] h] f) :
    ∃ F : ℝ → ℝ, AnalyticOnNhd ℝ F (Ici 0) ∧ EqOn F f (Ici 0)
```

### `UnivariateML.PGWFatigue.squared_identity`

Beta-held criterion and failure support. The normalized squared likelihood equals the rational profile times the exact joint shape penalty. [PGWFatigue.lean](../../lean/UnivariateML/Family/PGWFatigue.lean#L51)

```lean
theorem squared_identity (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    {b d a : ℝ} (hb : 0 < b) (hd : 0 < d) (ha : 0 < a) (hq : 0 < Q g b d) :
    let u := Q g b d / a ^ 2
    squaredLik g b (d, a) = rationalProfile g b d * (u * exp (1 - u)) ^ n
```

### `UnivariateML.PGWFatigue.joint_envelope`

Beta-held criterion and failure support. The rational profile is an attainable upper envelope over all positive shapes at each nonsingular threshold. [PGWFatigue.lean](../../lean/UnivariateML/Family/PGWFatigue.lean#L100)

```lean
theorem joint_envelope (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    {b d : ℝ} (hb : 0 < b) (hd : 0 < d) (hq : 0 < Q g b d) :
    (∀ a > 0, squaredLik g b (d, a) ≤ rationalProfile g b d) ∧
      ∃ a > 0, squaredLik g b (d, a) = rationalProfile g b d
```

### `UnivariateML.PGWFatigue.infinite_threshold`

Beta-held criterion and failure support. The rational profile tends to zero as the threshold distance goes to infinity. [PGWFatigue.lean](../../lean/UnivariateML/Family/PGWFatigue.lean#L132)

```lean
theorem infinite_threshold (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    {b : ℝ} (hb : 0 < b) : Tendsto (rationalProfile g b) atTop (𝓝 0)
```

### `UnivariateML.PGWFatigue.threshold_shape_diverges`

Beta-held criterion and failure support. The conditional shape tends to infinity as the threshold contacts the sample minimum. [PGWFatigue.lean](../../lean/UnivariateML/Family/PGWFatigue.lean#L298)

```lean
theorem threshold_shape_diverges (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b) :
    Tendsto (fun d ↦ Fatigue.shape (shifted g d) b) (𝓝[>] 0) atTop
```

### `UnivariateML.PGWFatigue.threshold_trace`

Beta-held criterion and failure support. The squared profile has a positive finite factor times distance to the sample size minus triple the minimum multiplicity. [PGWFatigue.lean](../../lean/UnivariateML/Family/PGWFatigue.lean#L317)

```lean
theorem threshold_trace (hn : 0 < n) {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b) :
    0 < edgeFactor g b 0 ∧
      Tendsto (edgeFactor g b) (𝓝[>] 0) (𝓝 (edgeFactor g b 0)) ∧
      ∀ d > 0, rationalProfile g b d =
        edgeFactor g b d * (d ^ n / d ^ (3 * multiplicity g))
```

### `UnivariateML.PGWFatigue.constant_divergence_subsequence`

Beta-held criterion and failure support. A divergent constant sequence has a subsequence at threshold contact or at the held-scale collapse with vanishing shape. [PGWFatigue.lean](../../lean/UnivariateML/Family/PGWFatigue.lean#L669)

```lean
theorem constant_divergence_subsequence (hn : 0 < n) {b : ℝ} (hb : 0 < b)
    {d a : ℕ → ℝ} (hd : ∀ᶠ t in atTop, 0 < d t) (ha : ∀ᶠ t in atTop, 0 < a t)
    (hL : Tendsto (fun t ↦ squaredLik (fun _ : Fin n ↦ 0) b (d t, a t)) atTop atTop) :
    ∃ φ : ℕ → ℕ, StrictMono φ ∧
      (Tendsto (d ∘ φ) atTop (𝓝 0) ∨
        (Tendsto (d ∘ φ) atTop (𝓝 b) ∧ Tendsto (a ∘ φ) atTop (𝓝 0)))
```

### `UnivariateML.PGWFatigue.competitive_subsequence`

Beta-held criterion and failure support. Every nonconstant competitive sequence has a subsequence at a finite interior fit or threshold contact. [PGWFatigue.lean](../../lean/UnivariateML/Family/PGWFatigue.lean#L718)

```lean
theorem competitive_subsequence (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hne : ∃ i j, g i ≠ g j) {b : ℝ} (hb : 0 < b)
    {d a : ℕ → ℝ} {M : ℝ} (hM : 0 < M)
    (hd : ∀ᶠ t in atTop, 0 < d t) (ha : ∀ᶠ t in atTop, 0 < a t)
    (hL : ∀ᶠ t in atTop, M ≤ squaredLik g b (d t, a t)) :
    ∃ φ : ℕ → ℕ, StrictMono φ ∧
      (Tendsto (d ∘ φ) atTop (𝓝 0) ∨
        ∃ d₀ > 0, ∃ a₀ > 0, Tendsto (d ∘ φ) atTop (𝓝 d₀) ∧
          Tendsto (a ∘ φ) atTop (𝓝 a₀))
```

