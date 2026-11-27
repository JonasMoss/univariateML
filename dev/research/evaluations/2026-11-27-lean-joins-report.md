# Lean joins and held fits

TASK-398 · 2026-11-27

The work adds 35 public theorems in 13 modules. The atlas fragment records each declaration with coverage blank for independent review. [Atlas fragment](../existence-atlas/lean-map.d/lean-joins.csv)

## Source discrepancies and open proof interfaces

- **Burr Pareto endpoint.** The atlas says the scale “stays at its endpoint.” The literal fixed-scale path has the prescribed diverging power and positive limiting shape product, but its mean log likelihood loses `(zeroCount / n) * log 2` at endpoint contacts. Lean proves the fixed-scale limit and a strict upper bound below the optimized Pareto face. The separate path approaching the minimum from below reaches that optimized value, as in the written face analysis. The two limits differ even if the sample minimum occurs only once. [Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md) · [Written analysis](2026-11-25-burr-math-report.md#face-comparisons-and-equality)
- **Grouped-normal CDF transport remains open.** The proof establishes the Gaussian integral for the CDF difference at the standardized working endpoints. It separately proves positivity of the existing working cell masses from the public endpoint-information theorem, the exact working-product log normalization, and equality of the existing working product and working log argmax sets on each fiber. The held-location criterion uses the CDF product and includes the strict contact supremum. To transfer held-fit uniqueness to that product, the missing equality is `intervalMass a b = ∫ z in a..b, kernel z`. The existing `intervalMass` has an unexposed body, and its integral conversion is private in `Discrete3DNorm`. Making that conversion public would supply the equality needed by the two checked identities. [Grouped-normal source](../../lean/UnivariateML/Family/Discrete3DNorm.lean) · [Coverage audit](2026-11-27-atlas-coverage2-report.md#where-the-declarations-stop)
- **PIG chart interface.** The positive held-shape fit is proved for the explicit log-polynomial objective of the written derivation, in the Bessel argument and in the physical mean. The all-zero limit is joined to the same physical likelihood. Positive fixed likelihood factors are omitted, which preserves the maximizers. A definitional bridge to the older opaque `chartLogLik` is still open. An exported evaluation equation for `chartLogLik` and the identity `besselPolynomial 1 = 1` would let the explicit objective be substituted into that declaration. The all-free classification uses the existing chart on positive counts and the existing shape-zero likelihood on all-zero data. The separate atlas seam for shape held at zero is outside the positive-held-shape theorem. [PIG source](../../lean/UnivariateML/Family/Discrete3PIG.lean) · [Held-shape derivation](2026-11-25-pig-math-report.md)
- **Skew-normal naming.** The skew-normal join grades belong to `sn`, the Azzalini distribution with a real slant. The Fernández–Steel two-piece normal has dossier ID `snorm` and a different objective. [Azzalini dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sn/README.md) · [Two-piece dossier](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/README.md)

## Effort and checks

| Target | Effort and scope |
| --- | --- |
| 1. Joins | Sixteen declarations in the primary EMG, skew-normal, half-t, CMB and Burr modules and the reciprocal and translated Burr joins. The additional discrete sweep joined CMP, both Double Pareto fibers, ordinary binomial size, and discrete Weibull. The grouped-normal contact and PIG zero-count joins are recorded with those families. The reused extrema are witnesses available for every nonempty finite sample, including ties. The Burr fixed-endpoint calculation additionally needed a limit of the scale-profile normalizer. |
| 2. Positive held PIG shape | The main work derived log-polynomial derivatives from the public variance result. The mean-degree sign uses polynomial growth at infinity and the negative derivative at any hypothetical zero. Those results give a monotone crossing and a bijection to mean coordinates, with all-zero data handled by the mean-zero limit. |
| 3. Held GEV scale | The main work formalized the density on the shape interval, including the Gumbel seam and upper semicontinuity at reverse-exponential contact. Uniform standardized density bounds give bounded location superlevels. Their compactness yields a positive global maximum for every nonempty sample. No uniqueness conclusion is asserted for this hold. |
| 4. Grouped-normal working chart | Public Gaussian CDF differentiation gives the normalized integral formula. Public endpoint information gives positivity for the existing working chart and permits the independent exact normalization and argmax transfer. The remaining API dependency is stated above. |

The sweep leaves separate claims requiring more than combining existing branches at their previous coverage. Examples include CMB global face comparison, Bhattacharjee complementary width branches, the Skellam constant-cardinality branch, and GEV held-location envelopes and global ties. Numerical fitter certificates and stopping rules are outside these real-arithmetic statements.

Each module passed `lake env lean` on the workstation under the single-slot launcher. The final `lake build` with temporary root imports succeeded with 7,944 jobs. The last build emitted no warnings in the new modules and reused the Mathlib cache. The temporary imports were removed, and `git diff --exit-code -- dev/lean/UnivariateML.lean` passed. An axiom audit of all 35 theorems found only `propext`, `Classical.choice`, and `Quot.sound`.

## EMG

### `UnivariateML.JoinsEMG.all_free_criterion`

A finite EMG maximum exists exactly on the nonconstant branch with a stationary value at least as large as both compactified face values. [Lean source](../../lean/UnivariateML/Family/JoinsEMG.lean#L30)

```lean
theorem all_free_criterion {n : ℕ} (x : Fin n → ℝ) (i : Fin n)
    (hi : ∀ j, x i ≤ x j) :
    (∃ q ∈ (Ioo 0 1 ×ˢ LocationScale.domain),
      IsMaxOn (fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2)
        (Ioo 0 1 ×ˢ LocationScale.domain) q) ↔
    LocationScale.Nonconstant x ∧
      ∃ t ∈ Ioo 0 1, HasDerivAt (compactProfile x i) 0 t ∧
        max (compactProfile x i 0) (compactProfile x i 1) ≤ compactProfile x i t
```

### `UnivariateML.JoinsEMG.all_free_failure`

The image of the log likelihood is unbounded on constant data. On nonconstant data without a finite fit its supremum is the larger face. [Lean source](../../lean/UnivariateML/Family/JoinsEMG.lean#L47)

```lean
theorem all_free_failure {n : ℕ} (x : Fin n → ℝ) (i : Fin n)
    (hi : ∀ j, x i ≤ x j) :
    let D := Ioo (0 : ℝ) 1 ×ˢ LocationScale.domain
    let L := fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2
    (¬ LocationScale.Nonconstant x → ¬ BddAbove (L '' D)) ∧
    (LocationScale.Nonconstant x →
      ((¬ ∃ q ∈ D, IsMaxOn L D q) ↔
        ∀ t ∈ Ioo 0 1, HasDerivAt (compactProfile x i) 0 t →
          compactProfile x i t < max (compactProfile x i 0) (compactProfile x i 1)) ∧
      ((¬ ∃ q ∈ D, IsMaxOn L D q) →
        IsLUB (L '' D) (max (compactProfile x i 0) (compactProfile x i 1))))
```

## SN

### `UnivariateML.JoinsSN.all_free_criterion`

For the Azzalini skew normal, finite attainment requires nonconstant data and a finite point reaching both optimized half-normal bounds. [Lean source](../../lean/UnivariateML/Family/JoinsSN.lean#L27)

```lean
theorem all_free_criterion {n : ℕ} (x : Fin n → ℝ) (i j : Fin n)
    (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    (∃ p : ℝ × LocationScale.inverseDomain,
      IsMaxOn (fun ap : ℝ × LocationScale.inverseDomain ↦ objective x ap.1 ap.2) univ p) ↔
    LocationScale.Nonconstant x ∧
      ∃ (α : ℝ) (p : LocationScale.inverseDomain),
        max (face x (x i)) (face x (x j)) ≤ objective x α p
```

### `UnivariateML.JoinsSN.all_free_failure`

Constant data have unbounded log likelihood. On nonconstant data, a strict loss at every finite point is equivalent to nonattainment, and the larger optimized half-normal value is then the supremum. [Lean source](../../lean/UnivariateML/Family/JoinsSN.lean#L46)

```lean
theorem all_free_failure {n : ℕ} (x : Fin n → ℝ) (i j : Fin n)
    (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    let L := fun ap : ℝ × LocationScale.inverseDomain ↦ objective x ap.1 ap.2
    let F := max (face x (x i)) (face x (x j))
    (¬ LocationScale.Nonconstant x → ¬ BddAbove (range L)) ∧
    (LocationScale.Nonconstant x →
      ((¬ ∃ p, IsMaxOn L univ p) ↔ ∀ p, L p < F) ∧
      ((¬ ∃ p, IsMaxOn L univ p) → IsLUB (range L) F))
```

## HalfT

### `UnivariateML.JoinsHalfT.all_free_failure`

Strict majority at the minimum is precisely the unbounded branch. Otherwise nonattainment is equivalent to the absence of an eligible finite candidate reaching the larger half-normal or collapse face, which is then the exact supremum. [Lean source](../../lean/UnivariateML/Family/JoinsHalfT.lean#L18)

```lean
theorem all_free_failure {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) :
    let L := fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2
    let F := max (normalFace x (x i)) (collapseFace x (x i))
    ((¬ BddAbove (L '' domain x)) ↔ n < 2 * TDist.multiplicity x (x i)) ∧
    (2 * TDist.multiplicity x (x i) ≤ n →
      ((¬ ∃ p ∈ domain x, IsMaxOn L (domain x) p) ↔
        ¬ ∃ ν ≥ (1 : ℝ), 0 < TDist.exponent x (x i) ν ∧
          (ν = 1 ∨ HasDerivAt (scaleProfile x (x i)) 0 ν) ∧
          F ≤ scaleProfile x (x i) ν) ∧
      ((¬ ∃ p ∈ domain x, IsMaxOn L (domain x) p) → IsLUB (L '' domain x) F))
```

## CMB

### `UnivariateML.JoinsCMB.held_size_criterion`

Every compatible size-zero or size-one sample attains a maximum. For larger held sizes, attainment requires an endpoint-constant sample or an empirical moment pair strictly inside the moment hull. [Lean source](../../lean/UnivariateML/Family/JoinsCMB.lean#L18)

```lean
theorem held_size_criterion {N n : ℕ} (hn : 0 < n) (x : Fin n → Fin (N + 1)) :
    (∃ p ∈ Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ),
      IsMaxOn (fun p : ℝ × ℝ ↦ closedLikelihood x p.1 p.2) (Icc 0 1 ×ˢ univ) p) ↔
    N ≤ 1 ∨ (∀ i, x i = 0) ∨ (∀ i, x i = Fin.last N) ∨
      empirical x ∈ interior (convexHull ℝ (range (statistic N)))
```

## Burr

### `UnivariateML.JoinsBurr.constant_path`

With any fixed positive first shape, taking the power to infinity and keeping scale at the common observation gives divergence. [Lean source](../../lean/UnivariateML/Family/JoinsBurr.lean#L25)

```lean
theorem constant_path {n : ℕ} (hn : 0 < n) {v k : ℝ} (hv : 0 < v) (hk : 0 < k) :
    (∀ᶠ t : ℝ in atTop, (k, t, v) ∈ physicalDomain) ∧
    Tendsto (fun t : ℝ ↦ logLik (fun _ : Fin n ↦ v) k t v) atTop atTop
```

### `UnivariateML.JoinsBurr.pareto_path`

In physical coordinates the optimized Pareto path has diverging power, constant positive product of shapes, and scale approaching the sample minimum strictly from below. [Lean source](../../lean/UnivariateML/Family/JoinsBurr.lean#L45)

```lean
theorem pareto_path {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (j : Fin n) (hj : ∀ i, x j ≤ x i) :
    let y := logExcess x j
    let a := (avg y)⁻¹
    let s := fun t : ℝ ↦ exp (log (x j) - t⁻¹)
    0 < a ∧
    (∀ᶠ t : ℝ in atTop, (a / t^2, t^2, s t) ∈ physicalDomain ∧
      (a / t^2) * t^2 = a) ∧
    Tendsto (fun t : ℝ ↦ t^2) atTop atTop ∧
    Tendsto s atTop (𝓝[<] x j) ∧
    Tendsto (fun t : ℝ ↦ logLik x (a / t^2) (t^2) (s t)) atTop
      (𝓝 (BurrFree.paretoValue y - avg (fun i ↦ log (x i))))
```

The scale tends to the endpoint from below. The limit is the optimized Pareto likelihood.

### `UnivariateML.JoinsBurr.fixed_endpoint_penalty`

Keeping the scale equal to the sample minimum imposes a strict gap below the optimized, moving-endpoint Pareto face. [Lean source](../../lean/UnivariateML/Family/JoinsBurr.lean#L103)

```lean
theorem fixed_endpoint_penalty {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (j : Fin n) (hj : ∀ i, x j ≤ x i) (hpos : ∃ i, x j < x i) :
    ∃ B < BurrFree.paretoValue (logExcess x j) - avg (fun i ↦ log (x i)),
      ∀ k > 0, ∀ c > 0, logLik x k c (x j) < B
```

An exact uniform gap separates the literal fixed-endpoint path from the optimized face.

### `UnivariateML.JoinsBurr.pareto_fixed_endpoint_path`

The literal fixed-endpoint Burr path has diverging power and positive limiting product of shapes. Its likelihood tends to the Pareto trace with the contact penalty included. [Lean source](../../lean/UnivariateML/Family/JoinsBurr.lean#L166)

```lean
theorem pareto_fixed_endpoint_path {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (j : Fin n) (hj : ∀ i, x j ≤ x i) (hpos : ∃ i, x j < x i) :
    let z := logRatios x (x j)
    let k := bestK z
    (∀ᶠ c : ℝ in atTop, (k c, c, x j) ∈ physicalDomain) ∧
    0 < (avg z)⁻¹ ∧
    Tendsto (fun c : ℝ ↦ c) atTop atTop ∧
    Tendsto (fun c : ℝ ↦ k c * c) atTop (𝓝 ((avg z)⁻¹)) ∧
    Tendsto (fun c : ℝ ↦ logLik x (k c) c (x j)) atTop
      (𝓝 (BurrHeld.paretoValue z - avg (fun i ↦ log (x i))))
```

The scale stays at the endpoint. The likelihood includes the contact penalty.

### `UnivariateML.JoinsBurr.all_free_criterion`

Constant positive data have no finite maximum. For nonconstant data attainment is equivalent to reaching the better optimized face value. [Lean source](../../lean/UnivariateML/Family/JoinsBurr.lean#L204)

```lean
theorem all_free_criterion {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (j : Fin n) (hj : ∀ i, x j ≤ x i) :
    let L := fun p : ℝ × ℝ × ℝ ↦ logLik x p.1 p.2.1 p.2.2
    let y := logExcess x j
    (∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) ↔
      (∃ i k, x i ≠ x k) ∧
      ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
        ∃ p ∈ physicalDomain,
          max (BurrFree.paretoValue y) (weibullLik y c b) - avg (fun i ↦ log (x i)) ≤ L p
```

### `UnivariateML.JoinsBurr.all_free_failure`

Constants have unbounded Burr likelihood. For nonconstant samples without finite attainment, the larger optimized face value is the unattained supremum, even when both faces give the same value. [Lean source](../../lean/UnivariateML/Family/JoinsBurr.lean#L255)

```lean
theorem all_free_failure {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (j : Fin n) (hj : ∀ i, x j ≤ x i) :
    let L := fun p : ℝ × ℝ × ℝ ↦ logLik x p.1 p.2.1 p.2.2
    let y := logExcess x j
    ((∀ i, x i = x j) → ¬ BddAbove (L '' physicalDomain)) ∧
    ((∃ i k, x i ≠ x k) →
      ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
        ((¬ ∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) →
          IsLUB (L '' physicalDomain)
            (max (BurrFree.paretoValue y) (weibullLik y c b) -
              avg (fun i ↦ log (x i))) ∧
          ∀ p ∈ physicalDomain, L p <
            max (BurrFree.paretoValue y) (weibullLik y c b) - avg (fun i ↦ log (x i))))
```

## BurrTransfers

### `UnivariateML.JoinsBurrTransfers.inverse_burr_criterion`

Constant inverse Burr data diverge. On nonconstant data a finite maximum exists exactly when it reaches the larger reciprocal face. [Lean source](../../lean/UnivariateML/Family/JoinsBurrTransfers.lean#L49)

```lean
theorem inverse_burr_criterion {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (j : Fin n) (hj : ∀ i, x i ≤ x j) :
    let L := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesInverseBurr.logLik x p.1 p.2.1 p.2.2
    let y := logExcess (Transfer.reciprocal x) j
    let J := ∑ i, Real.log (x i)
    ((∀ i, x i = x j) → ¬ BddAbove (L '' physicalDomain)) ∧
    ((∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) ↔
      (∃ i k, x i ≠ x k) ∧
      ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
        ∃ p ∈ physicalDomain, n * max (paretoValue y) (weibullLik y c b) - J ≤ L p)
```

### `UnivariateML.JoinsBurrTransfers.paretoIV_held_threshold_criterion`

At a held Pareto IV threshold with positive excesses, constants diverge. On nonconstant excesses a finite maximum exists exactly when it reaches the larger translated Burr face. [Lean source](../../lean/UnivariateML/Family/JoinsBurrTransfers.lean#L72)

```lean
theorem paretoIV_held_threshold_criterion {n : ℕ} {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) (j : Fin n) (hj : ∀ i, x j ≤ x i) :
    let L := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ p.1 p.2.1 p.2.2
    let e := ShapeFacesParetoIV.excess x μ
    let y := logExcess e j
    let J := ∑ i, Real.log (e i)
    ((∀ i, x i = x j) → ¬ BddAbove (L '' physicalDomain)) ∧
    ((∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) ↔
      (∃ i k, x i ≠ x k) ∧
      ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
        ∃ p ∈ physicalDomain, n * max (paretoValue y) (weibullLik y c b) - J ≤ L p)
```

### `UnivariateML.JoinsBurrTransfers.paralogistic_criterion`

The paralogistic finite fit exists exactly on nonconstant positive samples. On constant samples the log likelihood diverges. [Lean source](../../lean/UnivariateML/Family/JoinsBurrTransfers.lean#L104)

```lean
theorem paralogistic_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    let L := fun p : ℝ × ℝ ↦ Paralogistic.logLik x p.1 p.2
    ((∃ p ∈ Weibull.domain, IsMaxOn L Weibull.domain p) ↔ ∃ i j, x i ≠ x j) ∧
    ((∀ i, x i = x ⟨0, hn⟩) → ¬ BddAbove (L '' Weibull.domain))
```

### `UnivariateML.JoinsBurrTransfers.inverse_paralogistic_criterion`

Inverse paralogistic has the same finite-attainment condition and constant-sample divergence in physical coordinates. [Lean source](../../lean/UnivariateML/Family/JoinsBurrTransfers.lean#L117)

```lean
theorem inverse_paralogistic_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    let L := fun p : ℝ × ℝ ↦ InvParalogistic.logLik x p.1 p.2
    ((∃ p ∈ Weibull.domain, IsMaxOn L Weibull.domain p) ↔ ∃ i j, x i ≠ x j) ∧
    ((∀ i, x i = x ⟨0, hn⟩) → ¬ BddAbove (L '' Weibull.domain))
```

## CMP

### `UnivariateML.JoinsCMP.held_rate`

Positive held rate has a unique fit exactly when some count exceeds one. Binary samples instead have the strict Bernoulli supremum. At zero rate every dispersion maximizes the flat likelihood, which is positive exactly for compatible all-zero data. [Lean source](../../lean/UnivariateML/Family/JoinsCMP.lean#L19)

```lean
theorem held_rate {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {r : ℝ} (hr : 0 ≤ r) :
    let D := {ν : ℝ | (r, ν) ∈ CMP.rateDomain}
    let M := fun ν ↦ ν ∈ D ∧ IsMaxOn (CMP.likelihood x r) D ν
    (0 < r →
      ((∃ ν, M ν) ↔ ∃ i, 1 < x i) ∧
      ((∃ i, 1 < x i) → ∃! ν, M ν) ∧
      ((∀ i, x i ≤ 1) →
        let b := ∏ i, r ^ x i / (1 + r)
        IsLUB (CMP.likelihood x r '' D) b ∧ ∀ ν ∈ D, CMP.likelihood x r ν < b)) ∧
    (r = 0 → ∀ ν ∈ D, M ν ∧ CMP.likelihood x r ν = (if ∀ i, x i = 0 then 1 else 0))
```

## DoublePareto

### `UnivariateML.JoinsDoublePareto.held_left_rate`

With kink and left rate held, the right-rate maximum is unique when the right deviation is positive. At zero the power face is the unattained supremum. [Lean source](../../lean/UnivariateML/Family/JoinsDoublePareto.lean#L25)

```lean
theorem held_left_rate {L R a : ℝ} (ha : 0 < a) (hR : 0 ≤ R) :
    ((∃ b ∈ Ioi (0 : ℝ), IsMaxOn (objective L R a) (Ioi 0) b) ↔ 0 < R) ∧
    (0 < R → ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (objective L R a) (Ioi 0) b) ∧
    (R = 0 → IsLUB (objective L R a '' Ioi 0) (log a - a * L))
```

### `UnivariateML.JoinsDoublePareto.held_right_rate`

With kink and right rate held, the left-rate maximum is unique when the left deviation is positive. At zero the Pareto face is the unattained supremum. [Lean source](../../lean/UnivariateML/Family/JoinsDoublePareto.lean#L43)

```lean
theorem held_right_rate {L R b : ℝ} (hb : 0 < b) (hL : 0 ≤ L) :
    let f := fun a ↦ objective L R a b
    ((∃ a ∈ Ioi (0 : ℝ), IsMaxOn f (Ioi 0) a) ↔ 0 < L) ∧
    (0 < L → ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn f (Ioi 0) a) ∧
    (L = 0 → IsLUB (f '' Ioi 0) (log b - b * R))
```

## BinomialSize

### `UnivariateML.JoinsBinomialSize.all_free_criterion`

The joint binomial fit exists on all-zero data and exactly under underdispersion on positive-mean data. On all-zero data both feasible parameter axes comprise the maximizing set. [Lean source](../../lean/UnivariateML/Family/JoinsBinomialSize.lean#L19)

```lean
theorem all_free_criterion {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m) :
    let L := fun q : ℕ × ℝ ↦ Discrete.Binomial.sampleLikelihood x q.1 q.2
    ( {q ∈ parameterDomain | IsMaxOn L parameterDomain q}.Nonempty ↔
      (∀ i, x i = 0) ∨ variance x < mean x) ∧
    ((∀ i, x i = 0) → ∀ q ∈ parameterDomain,
      IsMaxOn L parameterDomain q ↔ q.1 = 0 ∨ q.2 = 0)
```

## DiscreteWeibull

### `UnivariateML.JoinsDiscreteWeibull.all_free_classification`

The gap branch has a unique finite fit. Without a gap, the whole sample is constant or occupies two adjacent cells. In those cases the point mass or the empirical two-cell law gives the unattained supremum. [Lean source](../../lean/UnivariateML/Family/JoinsDiscreteWeibull.lean#L18)

```lean
theorem all_free_classification {n : ℕ} (x : Fin n → ℕ) (hn : 0 < n) :
    let M := ∃ p ∈ domain, IsMaxOn (logLik x) domain p
    ((∃ i j, x i + 2 ≤ x j) → ∃! p, p ∈ domain ∧ IsMaxOn (logLik x) domain p) ∧
    (¬ (∃ i j, x i + 2 ≤ x j) → ¬ M ∧
      ((∃ m, (∀ i, x i = m) ∧ sSup (logLik x '' domain) = 0) ∨
        ∃ m, (∀ i, x i = m ∨ x i = m + 1) ∧ (∃ i, x i = m) ∧ (∃ i, x i = m + 1) ∧
          sSup (logLik x '' domain) =
            ∑ i, log (if x i = m + 1 then upperFraction x m else 1 - upperFraction x m)))
```

## PIG

### `UnivariateML.JoinsPIG.log_polynomial_derivative`

The log-polynomial contribution for a count of at least two has argument derivative `-E(1/z)/z`. [Lean source](../../lean/UnivariateML/Family/JoinsPIG.lean#L107)

```lean
theorem log_polynomial_derivative {k : ℕ} (hk : 2 ≤ k) {z : ℝ} (hz : 0 < z) :
    HasDerivAt (fun v ↦ log ((besselPolynomial k).eval v⁻¹)) (-degreeMean k z⁻¹ / z) z
```

### `UnivariateML.JoinsPIG.weighted_degree_derivative`

The derivative of the argument-weighted degree contribution is the degree mean minus its variance. [Lean source](../../lean/UnivariateML/Family/JoinsPIG.lean#L119)

```lean
theorem weighted_degree_derivative {k : ℕ} (hk : 2 ≤ k) {z : ℝ} (hz : 0 < z) :
    HasDerivAt (fun v ↦ v * degreeMean k v⁻¹)
      (degreeMean k z⁻¹ - degreeVariance k z⁻¹) z
```

### `UnivariateML.JoinsPIG.held_shape_derivative`

For arbitrary mixed counts, the derivative of the held-shape log likelihood in the Bessel argument is the difference of the two sides of the stationary equation divided by the squared argument. [Lean source](../../lean/UnivariateML/Family/JoinsPIG.lean#L133)

```lean
theorem held_shape_derivative {n : ℕ} (x : Fin n → ℕ) {lam z : ℝ}
    (hl : 0 < lam) (hz : sqrt (2 * lam) < z) :
    let S := ∑ i, (x i : ℝ)
    let t := sqrt (z^2 - 2 * lam)
    let q := t / z
    let D := S + ∑ i, if 2 ≤ x i then degreeMean (x i) z⁻¹ else 0
    HasDerivAt
      (fun v ↦ -(n : ℝ) * (v - sqrt (v^2 - 2 * lam)) - S * log v +
        ∑ i, if 2 ≤ x i then log ((besselPolynomial (x i)).eval v⁻¹) else 0)
      ((2 * n * lam / (q * (1 + q)) - z * D) / z^2) z
```

### `UnivariateML.JoinsPIG.weighted_sample_degree_strictMono`

The left side of the held-shape stationary equation is strictly increasing for every sample with at least one positive count. [Lean source](../../lean/UnivariateML/Family/JoinsPIG.lean#L185)

```lean
theorem weighted_sample_degree_strictMono {n : ℕ} (x : Fin n → ℕ) (hx : ∃ i, 0 < x i) :
    StrictMonoOn
      (fun z : ℝ ↦ z * ((∑ i, (x i : ℝ)) +
        ∑ i, if 2 ≤ x i then degreeMean (x i) z⁻¹ else 0)) (Ioi 0)
```

### `UnivariateML.JoinsPIG.held_shape_unique`

Every sample with a positive count has a unique maximum of the held-shape log likelihood in the Bessel argument. [Lean source](../../lean/UnivariateML/Family/JoinsPIG.lean#L228)

```lean
theorem held_shape_unique {n : ℕ} (x : Fin n → ℕ) (hx : ∃ i, 0 < x i)
    {lam : ℝ} (hl : 0 < lam) :
    let S := ∑ i, (x i : ℝ)
    let b := sqrt (2 * lam)
    let L := fun z : ℝ ↦ -(n : ℝ) * (z - sqrt (z^2 - 2 * lam)) - S * log z +
      ∑ i, if 2 ≤ x i then log ((besselPolynomial (x i)).eval z⁻¹) else 0
    ∃! z, z ∈ Ioi b ∧ IsMaxOn L (Ioi b) z
```

### `UnivariateML.JoinsPIG.held_shape_classification`

At positive held shape, a positive count gives a unique positive mean fit. On all-zero samples the zero-mean limit is the strictly unattained supremum. [Lean source](../../lean/UnivariateML/Family/JoinsPIG.lean#L408)

```lean
theorem held_shape_classification {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {lam : ℝ} (hl : 0 < lam) :
    let L := heldShapeLikelihood x lam
    ((∃ i, 0 < x i) → ∃! μ, μ ∈ Ioi (0 : ℝ) ∧ IsMaxOn L (Ioi 0) μ) ∧
    ((∀ i, x i = 0) →
      Tendsto L (𝓝[>] 0) (𝓝 1) ∧ IsLUB (L '' Ioi 0) 1 ∧ ∀ μ > 0, L μ < 1)
```

Positive held shape. The explicit reduced likelihood is in physical mean coordinates.

### `UnivariateML.JoinsPIG.all_free_classification`

The all-free PIG classification includes the unidentified mean at shape zero for all-zero samples and the exact variance cut otherwise. [Lean source](../../lean/UnivariateML/Family/JoinsPIG.lean#L457)

```lean
theorem all_free_classification {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    ((∀ i, x i = 0) →
      let D := Ioi (0 : ℝ) ×ˢ Ici 0
      ∀ p ∈ D, IsMaxOn (fun q : ℝ × ℝ ↦ HeldSweepPIG.zeroLikelihood n q.1 q.2) D p ↔
        p.2 = 0) ∧
    ((∃ i, 0 < x i) →
      ((∃ p ∈ positiveChart, IsMaxOn (chartLogLik x) positiveChart p) ↔
        sampleMean x < sampleVariance x) ∧
      (sampleMean x < sampleVariance x →
        ∃! p, p ∈ positiveChart ∧ IsMaxOn (chartLogLik x) positiveChart p) ∧
      (sampleVariance x ≤ sampleMean x →
        IsLUB (chartLogLik x '' positiveChart) (poissonLogLik x (sampleMean x)) ∧
        ∀ p ∈ positiveChart, chartLogLik x p < poissonLogLik x (sampleMean x)))
```

Positive counts use the Bessel chart and all-zero data use the closed physical seam.

## GEV

### `UnivariateML.JoinsGEV.held_scale_upperSemicontinuous`

The support-extended likelihood is upper semicontinuous in location and fitted shape, including shape minus one at contact. [Lean source](../../lean/UnivariateML/Family/JoinsGEV.lean#L416)

```lean
theorem held_scale_upperSemicontinuous {n : ℕ} (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s) :
    UpperSemicontinuousOn (heldScaleLikelihood x s)
      ((univ : Set ℝ) ×ˢ Icc (-1 : ℝ) 1)
```

### `UnivariateML.JoinsGEV.held_scale_compact`

Every positive likelihood superlevel is compact at fixed positive scale, uniformly over the fitted shape interval. [Lean source](../../lean/UnivariateML/Family/JoinsGEV.lean#L438)

```lean
theorem held_scale_compact {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s)
    {e : ℝ} (he : 0 < e) :
    IsCompact (((univ : Set ℝ) ×ˢ Icc (-1 : ℝ) 1) ∩
      heldScaleLikelihood x s ⁻¹' Ici e)
```

### `UnivariateML.JoinsGEV.held_scale_attainment`

With scale held positive, a nonempty sample attains a maximum on the fitted shape interval with the closed reverse-exponential endpoint. [Lean source](../../lean/UnivariateML/Family/JoinsGEV.lean#L483)

```lean
theorem held_scale_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {s : ℝ}
    (hs : 0 < s) :
    let D := {p : ℝ × ℝ | p.2 ∈ Icc (-1 : ℝ) 1 ∧ (p.1,s) ∈ GEV.domain x p.2}
    ∃ p ∈ D, IsMaxOn (fun q ↦ GEV.logLik x q.2 (q.1,s)) D p
```

## DNorm

### `UnivariateML.JoinsDNorm.working_cdf_integral`

In the working coordinates, a normalized Gaussian CDF difference is the standardized Gaussian interval integral divided by `sqrt (2π)`. [Lean source](../../lean/UnivariateML/Family/JoinsDNorm.lean#L37)

```lean
theorem working_cdf_integral (k : ℝ) (p : ℝ × ℝ) :
    let a := k * p.2 - p.1
    let b := (k + 1) * p.2 - p.1
    Φ b - Φ a = (∫ z in a..b, kernel z) / sqrt (2 * π)
```

Identity for the normal CDF difference in working coordinates. The existing intervalMass conversion is separate.

### `UnivariateML.JoinsDNorm.working_log_normalization`

The log of the normalized working likelihood differs from the existing working log likelihood by the Gaussian normalization only. [Lean source](../../lean/UnivariateML/Family/JoinsDNorm.lean#L83)

```lean
theorem working_log_normalization {n : ℕ} (x : Fin n → ℤ)
    {p : ℝ × ℝ} (hp : p ∈ workingDomain) :
    log (workingLikelihood x p) = workingLogLik x p - n * log (sqrt (2 * π))
```

The existing workingLikelihood, which uses intervalMass, is the probability objective here.

### `UnivariateML.JoinsDNorm.working_argmax_iff`

Maximization of the existing normalized working product is equivalent to maximization of the working log objective on any fiber. [Lean source](../../lean/UnivariateML/Family/JoinsDNorm.lean#L102)

```lean
theorem working_argmax_iff {n : ℕ} (x : Fin n → ℤ) {D : Set (ℝ × ℝ)}
    (hD : D ⊆ workingDomain) {p : ℝ × ℝ} (hp : p ∈ D) :
    IsMaxOn (workingLikelihood x) D p ↔ IsMaxOn (workingLogLik x) D p
```

Applies to every working-domain subset, including the two held fibers.

### `UnivariateML.JoinsDNorm.held_location_classification`

At a held location, a positive scale fits exactly when some observed closed cell excludes that location. Otherwise the contact product gives the strictly unattained zero-scale supremum. [Lean source](../../lean/UnivariateML/Family/JoinsDNorm.lean#L120)

```lean
theorem held_location_classification {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) (m : ℝ) :
    let L := HeldSweepDNorm.heldLocationLikelihood x m
    ((∃ B ∈ Ioi (0 : ℝ), IsMaxOn L (Ioi 0) B) ↔
      ∃ i, m ∉ Icc (x i : ℝ) ((x i : ℝ) + 1)) ∧
    ((∀ i, m ∈ Icc (x i : ℝ) ((x i : ℝ) + 1)) →
      let u := ∏ i, (if m = (x i : ℝ) ∨ m = (x i : ℝ) + 1 then (1 : ℝ) / 2 else 1)
      Tendsto L atTop (𝓝 u) ∧ IsLUB (L '' Ioi 0) u ∧ ∀ B > 0, L B < u)
```
